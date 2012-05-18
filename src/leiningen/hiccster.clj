(ns leiningen.hiccster
  (:use hiccster
        ns-tracker.core
        hiccup.core
        ring.middleware.params
        ring.middleware.file
        ring.middleware.file-info
        ring.util.response
        ring.adapter.jetty)
  (:import java.io.File))

(def ^{:dynamic true :private true}
  *modified-namespaces* nil)

(def *pages* (atom #{}))

(def root-namespace (atom nil))

(defn- log [& msg]
  (println (str "[" (java.util.Date.) "]")
           (apply str msg)))

(defn- reload-modified-namespaces [quiet?]
  (doseq [ns-sym (*modified-namespaces*)]
    (when-not (= (name ns-sym) "hiccster.init")
      (when-not quiet?
        (log "loading " ns-sym))
      (require ns-sym :reload)
      (let [ns      (find-ns ns-sym)
            publics (and ns (ns-publics ns))
            page    (get publics 'page)]
        (if (and page (fn? @page))
          (swap! *pages* conj ns-sym)
          (swap! *pages* disj ns-sym))))))

(defn- html-response [page]
  (-> (html page)
      (response)
      (content-type "text/html; charset=UTF-8")))

(defn- ns-ref [ns var-name]
  (get (ns-publics ns) var-name))

(defn- file-name->ns-sym [filename]
  (-> (.substring (str filename) 0 (.lastIndexOf (str filename) "."))
      (.replaceAll (str File/separatorChar) ".")))

(defn- ns-sym->file-name [ns-sym]
  (-> (str ns-sym)
      (.replaceAll (str "^" (java.util.regex.Pattern/quote (str @root-namespace "."))) "")
      (.replace \. File/separatorChar)
      (.concat ".")
      (.concat (or (ns-ref (find-ns ns-sym) 'file-extension) "html"))))

(defn- page-index []
  (html-response
   [:html [:head [:title "hiccster page index"]]
    [:body
     [:h1 "pages"]
     [:ul (map (fn [page]
                 (let [file (ns-sym->file-name page)]
                   [:li [:a {:href (str "/" file)} file]]))
               (sort (fn [a b] (compare (str a) (str b)))
                     (deref *pages*)))]]]))

(defn- handle-page [req]
  (reload-modified-namespaces false)
  (if (= (:uri req) "/")
    (page-index)
    (let [ns-sym (->> (.substring (:uri req) 1)
                      (file-name->ns-sym)
                      (str @root-namespace ".")
                      (symbol))
          ns (find-ns ns-sym)]
      (if-let [page (and ns (ns-resolve ns-sym 'page))]
        (-> (binding [*request* req]
              (page))
            (html-response))
        (-> (response (str "var " ns-sym "/page not found"))
            (status 404))))))

(defn- wrap-logging [handler]
  (fn [req]
    (log (:uri req))
    (handler req)))

(def ^{:private true}
  handler
  (-> handle-page
      (wrap-params)
      (wrap-file "static")
      (wrap-file-info)
      (wrap-logging)))

(defn init! [dirs & [quiet?]]
  (try (require 'hiccster.init)
       (catch java.io.FileNotFoundException e))
  (alter-var-root (var *modified-namespaces*)
                  (constantly (ns-tracker dirs 0)))
  (reload-modified-namespaces quiet?))

(defn hiccster-config []
  (let [file (File. ".hiccster")]
    (when (.exists file)
      (with-open [f (java.io.PushbackReader. (java.io.FileReader. file))]
        (read f)))))

(defn hiccster
  ([project] (hiccster project "src"))
  ([project & dirs]
     (init! dirs)
     (reset! root-namespace (:hiccster-root-namespace project))
     (run-jetty #'handler {:port (get (hiccster-config) :port 8765)})))
