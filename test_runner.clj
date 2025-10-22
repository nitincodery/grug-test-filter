#!/usr/bin/env bb

(require '[clojure.string :as str]
         '[clojure.set :as set]
         '[babashka.process :refer [shell]])

(defn changed-files
  []
  (-> (shell {:out :string} "git diff --name-only --diff-filter=d main...HEAD")
      :out
      str/split-lines
      (->> (filter #(re-matches #".*\.cljc?$" %)))))

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
          pattern (str "\\[(?:" (str/join "|" escaped-nss) ")\\b\\]")]
      (->> (shell {:out :string :continue true} 
                  "rg" "-l" "--type" "clojure" pattern "src" "test")
           :out
           str/split-lines
           (remove str/blank?)
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
  (try
    (let [result (shell {:out :string :continue true}
                        "rg" "-l" "-g" "*.clj" "-g" "*.cljc"
                        (str "\\(ns\\s+" test-ns "\\b"))]
      (not (str/blank? (:out result))))
    (catch clojure.lang.ExceptionInfo _  ; rg not found or failed
      (try
        (let [pattern (str "\\(ns\\s+" test-ns "\\b")
              result (shell {:out :string :continue true}
                            "grep" "-r" "-l" "-P" "--include=*.clj" "--include=*.cljc"
                            pattern ".")]
          (not (str/blank? (:out result))))
        (catch clojure.lang.ExceptionInfo _  ; grep not found
          (println "Neither ripgrep nor grep available. Install one.")
          (System/exit -1)
          false)))))

(defn main
  []
  (let [files (changed-files)]
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
      (apply shell {:continue true} "lein" "eftest" ":unit"
             test-nss))))

(main)

