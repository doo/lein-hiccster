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

(def *pages* (ref #{}))
(def page-ns->file-name (ref {}))
(def file-name->page-ns (ref {}))

(defn- log [& msg]
  (println (str "[" (java.util.Date.) "]")
           (apply str msg)))

(defn update-pages [ns-sym coll-op map-op]
  (let [file-name (page-ns-sym->file-name ns-sym)]
    (dosync
     (commute *pages* coll-op file-name)
     (commute page-ns->file-name map-op ns-sym file-name)
     (commute file-name->page-ns map-op file-name ns-sym))))

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
          (update-pages ns-sym conj assoc)
          (update-pages ns-sym disj dissoc))))))

(defn- html-response [page]
  (-> (html page)
      (response)
      (content-type "text/html; charset=UTF-8")))

(defn- page-index []
  (html-response
   [:html [:head [:title "hiccster page index"]]
    [:body
     [:h1 "pages"]
     [:ul (map (fn [page]
                 [:li [:a {:href (str "/" page)} page]])
               (sort (fn [a b] (compare (str a) (str b)))
                     (deref *pages*)))]]]))

(defn- handle-page [req]
  (reload-modified-namespaces false)
  (if (= (:uri req) "/")
    (page-index)
    (let [ns-sym (->> (.substring (:uri req) 1)
                      (@file-name->page-ns))]
      (if-let [page (and ns-sym (find-ns ns-sym) (ns-resolve ns-sym 'page))]
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
     (reset! root-namespace (:hiccster-root-namespace project))
     (init! dirs)
     (run-jetty #'handler {:port (get (hiccster-config) :port 8765)})))
