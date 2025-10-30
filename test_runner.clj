#!/usr/bin/env bb

(require '[clojure.string :as str]
         '[clojure.set :as set]
         '[babashka.process :refer [shell]])

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

(defn main
  []
  (let [selector (or (first *command-line-args*) ":default")
        project-dir (or (second *command-line-args*) "./")
        shadow-config-path (str project-dir "shadow-cljs.edn")
        files-clj (changed-files :clj)
        files-cljs (changed-files :cljs)]
    (when (and (empty? files-clj) (empty? files-cljs))
      (println "No changed .clj/.cljc/.cljs files")
      (System/exit 0))

    ;; Clojure tests
    (let [changed-nss (map #(file->ns % :clj) files-clj)
          all-affected (transitive-dependents changed-nss :clj)
          test-nss (->> all-affected
                        (map ns->test-ns)
                        (filter #(test-ns-exists? % :clj)))]
      (when (seq test-nss)
        (println "Changed CLJ namespaces:" (str/join ", " changed-nss))
        (println "Running Clojure tests for:" test-nss)
        (apply shell {:continue true} "lein" "eftest" selector test-nss)))

    ;; ClojureScript tests
    (let [changed-nss (map #(file->ns % :cljs) files-cljs)
          all-affected (transitive-dependents changed-nss :cljs)
          test-nss (->> all-affected
                        (map ns->test-ns)
                        (filter #(test-ns-exists? % :cljs)))]
      (when (seq test-nss)
        (println "Changed CLJS namespaces:" (str/join ", " changed-nss))
        (println "Running ClojureScript tests for:" test-nss)
        ;; Generate temp shadow config
        (let [ns-pattern (str "(" (str/join "|" (map #(str/replace % "." "\\.") test-nss)) ")")
              shadow-config (slurp shadow-config-path)
              modified-config (str/replace shadow-config
                                           #":ns-regexp \"-test\""
                                           (str ":ns-regexp \"" ns-pattern "\""))]
          (spit shadow-config-path modified-config)
          (try
            (shell {:continue true :dir project-dir} "npx" "shadow-cljs" "compile" "test")
            (shell {:continue true :dir project-dir} "npx" "karma" "start" "karma.conf.js" "--single-run")
            (finally
              (spit shadow-config-path shadow-config))))))))

(main)
