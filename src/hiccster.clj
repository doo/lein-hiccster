(ns hiccster
  (:import java.util.regex.Pattern
           java.io.File))

(def ^{:dynamic true} *request*)

(def root-namespace (atom nil))

(defn ns-sym->file-name [ns-sym suffix]
  (let [ext (get (meta (find-ns ns-sym)) :file-extension)
        suffix (if ext (str "." ext) suffix)]
    (-> (str ns-sym)
        (.replaceFirst (str "^" (Pattern/quote (str @root-namespace "."))) "")
        (.replace \. File/separatorChar)
        (str suffix))))

(defn page-ns-sym->file-name [ns-sym]
  (ns-sym->file-name ns-sym ".html"))
