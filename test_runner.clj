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
  []
  (-> (shell {:out :string} "git diff --name-only --diff-filter=d main...HEAD")
      :out
      str/split-lines
      (->> (filter #(and (re-matches #".*\.cljc?$" %)
                         (str/starts-with? % "src/")))
           vec)))

(defn file->ns
  [f]
  (-> f
      (str/replace #"^(src|test)/(clj|cljc)/" "")
      (str/replace #"\.cljc?$" "")
      (str/replace #"/" ".")
      (str/replace #"_" "-")))

(defn ns->test-ns
  [n]
  (str n "-test"))

(defn find-dependents
  [nss]
  (when (seq nss)
    (let [escaped-nss (map #(str/replace % "." "\\.") nss)
          ;; Pattern 1: [namespace ...]
          bracket-pattern (str "\\[(" (str/join "|" escaped-nss) ")\\b")
          ;; Pattern 2: namespace/symbol (fully qualified calls)
          qualified-pattern (str "\\b(" (str/join "|" escaped-nss) ")/")
          ;; Combined pattern
          pattern (str "(" bracket-pattern "|" qualified-pattern ")")]
      (->> (run-search
            ["rg" "-l" "-g" "*.clj" "-g" "*.cljc" pattern "src" "test"]
            ["grep" "-Er" "-l" "--include=*.clj" "--include=*.cljc" pattern "src" "test"]
            [])
           (map file->ns)
           set))))

(defn transitive-dependents
  [nss]
  (loop [all-deps (set nss)
         new-deps (set nss)]
    (if (empty? new-deps)
      all-deps
      (let [deps (find-dependents new-deps)
            deps-diff (set/difference deps all-deps)]
        (recur (set/union all-deps deps-diff) deps-diff)))))

(defn test-ns-exists?
  [test-ns]
  (let [pattern (str "\\(ns\\s+" test-ns "\\b")]
    (boolean (seq (run-search
                   ["rg" "-l" "-g" "*.clj" "-g" "*.cljc" pattern "test"]
                   ["grep" "-Er" "-l" "--include=*.clj" "--include=*.cljc" pattern "test"]
                   [])))))

(defn main
  []
  (let [selector (or (first *command-line-args*) ":default")
        files (changed-files)]
    (when (empty? files)
      (println "No changed .clj/.cljc files")
      (System/exit 0))
    
    (let [changed-nss (map file->ns files)
          all-affected (transitive-dependents changed-nss)
          test-nss (->> all-affected
                        (map ns->test-ns)
                        (filter test-ns-exists?))]

      (println "Changed namespaces:" (str/join ", " changed-nss))
      
      (when (empty? test-nss)
        (println "No test files found for affected namespaces")
        (System/exit 0))

      (println "Running tests for:" test-nss)
      (apply shell {:continue true} "lein" "eftest" selector
             test-nss))))

(main)

