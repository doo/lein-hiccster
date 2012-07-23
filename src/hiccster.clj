(ns hiccster
  (:import java.util.regex.Pattern
           java.io.File))

(def ^{:dynamic true} *request*)

(def root-namespace (atom nil))

(defn- ns-ref [ns var-name]
  (get (ns-publics ns) var-name))

(defn ns-sym->file-name [ns-sym suffix]
  (let [ext (ns-ref (find-ns ns-sym) 'file-extension)
        suffix (if ext (str "." (deref ext)) suffix)]
    (-> (str ns-sym)
        (.replaceFirst (str "^" (Pattern/quote (str @root-namespace "."))) "")
        (.replace \. File/separatorChar)
        (str suffix))))

(defn page-ns-sym->file-name [ns-sym]
  (ns-sym->file-name ns-sym ".html"))
