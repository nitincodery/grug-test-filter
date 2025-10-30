#!/usr/bin/env bb

(require '[clojure.string :as str]
         '[clojure.set :as set]
         '[babashka.process :refer [shell]]
         '[babashka.cli :as cli]
         '[clojure.java.io :as io])

(defn run-search
  [rg-args grep-args default]
  (try
    (->> (apply shell {:out :string :continue true} rg-args)
         :out
         str/split-lines
         (remove str/blank?)
         vec)
    (catch clojure.lang.ExceptionInfo _
      (try
        (->> (apply shell {:out :string :continue true} grep-args)
             :out
             str/split-lines
             (remove str/blank?)
             vec)
        (catch clojure.lang.ExceptionInfo _
          (println "Neither ripgrep nor grep available. Install one.")
          (System/exit -1)
          default)))))

(defn changed-files
  [lang]
  (let [pattern (case lang
                  :clj #".*\.cljc?$"
                  :cljs #".*\.clj(c|s)$")]
    (-> (shell {:out :string} "git diff --name-only --diff-filter=d main...HEAD")
        :out
        str/split-lines
        (->> (filter #(and (re-matches pattern %)
                           (str/starts-with? % "src/")))
             vec))))

(defn file->ns
  [f lang]
  (let [pattern (case lang
                  :clj #"^(src|test)/(clj|cljc)/"
                  :cljs #"^(src|test)/(cljc|cljs)/")
        ext-pattern (case lang
                      :clj #"\.cljc?$"
                      :cljs #"\.clj(c|s)$")]
    (-> f
        (str/replace pattern "")
        (str/replace ext-pattern "")
        (str/replace #"/" ".")
        (str/replace #"_" "-"))))

(defn ns->test-ns
  [n]
  (str n "-test"))

(defn find-dependents
  [nss lang]
  (when (seq nss)
    (let [escaped-nss (map #(str/replace % "." "\\.") nss)
          bracket-pattern (str "\\[(" (str/join "|" escaped-nss) ")\\b")
          qualified-pattern (str "\\b(" (str/join "|" escaped-nss) ")/")
          pattern (str "(" bracket-pattern "|" qualified-pattern ")")
          [rg-globs grep-includes] (case lang
                                     :clj [["-g" "*.clj" "-g" "*.cljc"]
                                           ["--include=*.clj" "--include=*.cljc"]]
                                     :cljs [["-g" "*.cljc" "-g" "*.cljs"]
                                            ["--include=*.cljc" "--include=*.cljs"]])]
      (->> (run-search
            (concat ["rg" "-l"] rg-globs [pattern "src" "test"])
            (concat ["grep" "-Er" "-l"] grep-includes [pattern "src" "test"])
            [])
           (map #(file->ns % lang))
           set))))

(defn transitive-dependents
  [nss lang]
  (loop [all-deps (set nss)
         new-deps (set nss)]
    (if (empty? new-deps)
      all-deps
      (let [deps (find-dependents new-deps lang)
            deps-diff (set/difference deps all-deps)]
        (recur (set/union all-deps deps-diff) deps-diff)))))

(defn test-ns-exists?
  [test-ns lang]
  (let [pattern (str "\\(ns\\s+" test-ns "\\b")
        [rg-globs grep-includes] (case lang
                                   :clj [["-g" "*.clj" "-g" "*.cljc"]
                                         ["--include=*.clj" "--include=*.cljc"]]
                                   :cljs [["-g" "*.cljc" "-g" "*.cljs"]
                                          ["--include=*.cljc" "--include=*.cljs"]])]
    (boolean (seq (run-search
                   (concat ["rg" "-l"] rg-globs [pattern "test"])
                   (concat ["grep" "-Er" "-l"] grep-includes [pattern "test"])
                   [])))))

(defn normalize-path
  "Convert path to absolute and ensure it ends with /"
  [path]
  (let [file (io/file path)
        abs-path (.getAbsolutePath file)
        normalized (str/replace abs-path #"^~" (System/getProperty "user.home"))]
    (if (str/ends-with? normalized "/")
      normalized
      (str normalized "/"))))

(defn show-help
  [spec]
  (cli/format-opts (merge spec {:order (vec (keys (:spec spec)))})))

(def cli-spec
  {:spec
   {:selector {:desc "Test selector (:default, :unit or :integration)"
               :default ":default"
               :alias :s}
    :dir {:desc "Project directory path"
          :default "."
          :alias :d
          :validate {:pred #(or (= % ".") (io/.exists (io/file %)))
                     :ex-msg "Directory does not exist"}}
    :lang {:desc "Language to test (:clj, :cljs or :both)"
           :default :both
           :alias :l
           :coerce :keyword
           :validate {:pred #(contains? #{:clj :cljs :both} %)
                      :ex-msg "lang must be :clj, :cljs or :both"}}
    :help {:desc "Show help"
           :alias :h}}
   :error-fn
   (fn [{:keys [type cause msg option]}]
     (when (= :org.babashka/cli type)
       (case cause
         :require
         (println (format "Missing required argument: %s\n" option))
         :validate
         (println (format "Validation error for --%s: %s\n" option msg)))))})

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

    (when (or help (:h opts))
      (println "Usage: test_runner.clj [OPTIONS]\n")
      (println "Run tests for changed namespaces and their dependents\n")
      (println (show-help cli-spec))
      (System/exit 0))

    (when (and (empty? files-clj) (empty? files-cljs))
      (println (str "No changed" (case lang :clj " .clj/.cljc " :cljs " .cljc/.cljs ") "files"))
      (System/exit 0))

    ;; Clojure tests
    (when run-clj?
      (let [changed-nss (map #(file->ns % :clj) files-clj)
            all-affected (transitive-dependents changed-nss :clj)
            test-nss (->> all-affected
                          (map ns->test-ns)
                          (filter #(test-ns-exists? % :clj)))]
        (when (seq test-nss)
          (println "Changed CLJ namespaces:" (str/join ", " changed-nss))
          (println "Running Clojure tests for:" test-nss)
          (apply shell {:continue true :dir project-dir} "lein" "eftest" selector test-nss))))

    ;; ClojureScript tests
    (when run-cljs?
      (let [changed-nss (map #(file->ns % :cljs) files-cljs)
            all-affected (transitive-dependents changed-nss :cljs)
            test-nss (->> all-affected
                          (map ns->test-ns)
                          (filter #(test-ns-exists? % :cljs)))]
        (when (seq test-nss)
          (println "Changed CLJS namespaces:" (str/join ", " changed-nss))
          (println "Running ClojureScript tests for:" test-nss)

          ;; Modify shadow config with test namespaces pattern
          (let [ns-pattern (str "(" (str/join "|" (map #(str/replace % "." "\\\\.") test-nss)) ")")
                shadow-config (slurp shadow-config-path)
                modified-config (str/replace shadow-config
                                             #":ns-regexp \"-test\""
                                             (str ":ns-regexp \"" ns-pattern "\""))]
            (spit shadow-config-path modified-config)
            (try
              (shell {:continue true :dir project-dir} "npx" "shadow-cljs" "compile" "test")
              (shell {:continue true :dir project-dir} "npx" "karma" "start" "karma.conf.js" "--single-run")
              (finally
                (spit shadow-config-path shadow-config)))))))))

(apply main *command-line-args*)
