#!/usr/bin/env bb

(require '[clojure.string :as str]
         '[clojure.set :as set]
         '[clojure.edn :as edn]
         '[babashka.process :refer [shell]]
         '[babashka.cli :as cli]
         '[clojure.java.io :as io])

(defonce lang-patterns
  {:clj {:file #".*\.cljc?$"
         :path #"^(src|test)/(clj|cljc)/"
         :ext #"\.cljc?$"
         :globs ["-g" "*.clj" "-g" "*.cljc"]
         :includes ["--include=*.clj" "--include=*.cljc"]}
   :cljs {:file #".*\.clj(c|s)$"
          :path #"^(src|test)/(cljc|cljs)/"
          :ext #"\.clj(c|s)$"
          :globs ["-g" "*.cljc" "-g" "*.cljs"]
          :includes ["--include=*.cljc" "--include=*.cljs"]}})

(defn- run-search
  [rg-args grep-args default]
  (try
    (->> (apply shell {:out :string
                       :continue true} rg-args)
         :out
         str/split-lines
         (remove str/blank?)
         vec)
    (catch clojure.lang.ExceptionInfo _
      (try
        (->> (apply shell {:out :string
                           :continue true} grep-args)
             :out
             str/split-lines
             (remove str/blank?)
             vec)
        (catch clojure.lang.ExceptionInfo _
          (println "Neither ripgrep nor grep available. Install one.")
          (System/exit -1)
          default)))))

(defn- changed-files
  [lang]
  (let [{:keys [:file]} (lang-patterns lang)]
    (-> (shell {:out :string} "git diff --name-only --diff-filter=d main...HEAD")
        :out
        str/split-lines
        (->> (filter #(and (re-matches file %)
                           (str/starts-with? % "src/")))
             vec))))

(defn- file->ns
  [^String f lang]
  (let [{:keys [:path :ext]} (lang-patterns lang)]
    (-> f
        (str/replace path "")
        (str/replace ext "")
        (str/replace #"/" ".")
        (str/replace #"_" "-"))))

(defn- ns->test-ns
  [n]
  (str n "-test"))

(defn- find-dependents
  [nss lang]
  (when (seq nss)
    (let [escaped-nss (map #(str/replace % "." "\\.") nss)
          bracket-pattern (str "\\[(" (str/join "|" escaped-nss) ")\\b")
          qualified-pattern (str "\\b(" (str/join "|" escaped-nss) ")/")
          pattern (str "(" bracket-pattern "|" qualified-pattern ")")
          {:keys [globs includes]} (lang-patterns lang)]
      (->> (run-search
            (into [] (concat ["rg" "-l"] globs [pattern "src" "test"]))
            (into [] (concat ["grep" "-Er" "-l"] includes [pattern "src" "test"]))
            [])
           (map #(file->ns % lang))
           set))))

(defn- transitive-dependents
  [nss lang]
  (loop [all-deps (set nss)
         new-deps (set nss)]
    (if (empty? new-deps)
      all-deps
      (let [deps (find-dependents new-deps lang)
            deps-diff (set/difference deps all-deps)]
        (recur (set/union all-deps deps-diff) deps-diff)))))

(defn- test-ns-exists?
  [test-ns lang]
  (let [pattern (str "\\(ns\\s+" test-ns "\\b")
        {:keys [globs includes]} (lang-patterns lang)]
    (boolean (seq (run-search
                   (vec (concat ["rg" "-l"] globs [pattern "test"]))
                   (vec (concat ["grep" "-Er" "-l"] includes [pattern "test"]))
                   [])))))

(defn- normalize-path
  [^String path]
  (let [file (io/file path)]
    (when-not (.exists file)
      (println (format "Error: Directory does not exist: %s" path))
      (System/exit 1))
    (when-not (.isDirectory file)
      (println (format "Error: Not a directory: %s" path))
      (System/exit 1))
    (let [abs-path (.getAbsolutePath file)
          normalized (str/replace abs-path #"^~" (System/getProperty "user.home"))]
      (if (str/ends-with? normalized "/")
        normalized
        (str normalized "/")))))

(defn- show-help
  [spec]
  (cli/format-opts (merge spec {:order (vec (keys (:spec spec)))})))

(def cli-spec
  {:spec
   {:selector {:desc "Test selector (:default, :unit or :integration)"
               :default ":default"
               :coerce :keyword
               :alias :s}
    :dir {:desc "Project directory path"
          :default (System/getProperty "user.dir")
          :alias :d}
    :lang {:desc "Language to test (:clj, :cljs or :both)"
           :default :both
           :alias :l
           :coerce :keyword
           :validate {:pred #(contains? #{:clj :cljs :both} %)
                      :ex-msg "lang must be :clj, :cljs or :both"}}
    :help {:desc "Show help"
           :alias :h}}
   :error-fn
   (fn [{:keys [typ cause msg option]}]
     (when (= :org.babashka/cli typ)
       (case cause
         :require (println (format "Missing required argument: %s\n" option))
         :validate (println (format "Validation error for --%s: %s\n" option msg)))))})

(defn- dashed-line
  [n]
  (apply str (repeat n "-")))

(defn- print-block
  [lang files test-nss]
  (let [lang-str (case lang :clj " Clojure " :cljs " ClojureScript ")]
    (println (dashed-line 50))
    (println (str "Changed" lang-str "namespaces:"))
    (println (str/join "\n" (map #(file->ns % lang) files)))
    (println (str "\nRunning" lang-str "tests for:"))
    (println (str/join "\n" test-nss))
    (println (dashed-line 50))))

(defn- main
  [& args]
  (let [opts (cli/parse-opts args cli-spec)
        {:keys [selector dir lang help]} opts
        project-dir (normalize-path dir)
        shadow-config-path (str project-dir "shadow-cljs.edn")
        run-clj? (contains? #{:clj :both} lang)
        run-cljs? (contains? #{:cljs :both} lang)
        files-clj (when run-clj? (changed-files :clj))
        files-cljs (when run-cljs? (changed-files :cljs))]
    (when help
      (println "Usage: test_runner.clj [OPTIONS]\n")
      (println "Run tests for changed namespaces and their dependents\n")
      (println (show-help cli-spec))
      (System/exit 0))
    (when (and run-cljs? (not= selector ":default"))
      (println "Warning: Selectors aren't supported for CLJS tests; using default behavior."))
    (when (and (empty? files-clj) (empty? files-cljs))
      (println (str "No changed" (case lang :clj " .clj/.cljc " :cljs " .cljc/.cljs " :both " .clj/.cljc/.cljs ") "files"))
      (System/exit 0))
    (letfn [(run-clj-tests []
              (when-let [test-nss (seq (->> (transitive-dependents (map #(file->ns % :clj) files-clj) :clj)
                                            (map ns->test-ns)
                                            (filter #(test-ns-exists? % :clj))))]
                (print-block :clj files-clj test-nss)
                (apply shell {:continue true
                              :dir project-dir} "lein" "eftest" selector test-nss)))
            (run-cljs-tests []
              (when-let [test-nss (seq (->> (transitive-dependents (map #(file->ns % :cljs) files-cljs) :cljs)
                                            (map ns->test-ns)
                                            (filter #(test-ns-exists? % :cljs))))]
                (print-block :cljs files-cljs test-nss)
                (let [ns-pattern (str "(" (str/join "|" (map #(str/replace % "." "\\.") test-nss)) ")")
                      shadow-config (edn/read-string (slurp shadow-config-path))
                      modified-config (assoc-in shadow-config [:builds :test :ns-regexp] ns-pattern)]
                  (spit shadow-config-path modified-config)
                  (try
                    (shell {:continue true
                            :dir project-dir} "npx" "shadow-cljs" "compile" "test")
                    (shell {:continue true
                            :dir project-dir} "npx" "karma" "start" "karma.conf.js" "--single-run")
                    (finally
                      (spit shadow-config-path shadow-config))))))]
      (when run-clj? (run-clj-tests))
      (when run-cljs? (run-cljs-tests)))))

(apply main *command-line-args*)
