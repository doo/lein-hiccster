(ns leiningen.hiccster
  (:use hiccster
        ns-tracker.core
        hiccup.core
        ring.middleware.params
        ring.middleware.file
        ring.middleware.file-info
        ring.util.response
        ring.adapter.jetty))

(def ^{:dynamic true}
  *modified-namespaces* nil)

(def *pages* (atom #{}))

(defn- log [& msg]
  (println (str "[" (java.util.Date.) "]")
           (apply str msg)))

(defn- reload-modified-namespaces []
  (doseq [ns-sym (*modified-namespaces*)]
    (log "loading " ns-sym)
    (require ns-sym :reload)
    (let [ns      (find-ns ns-sym)
          publics (and ns (ns-publics ns))
          page    (get publics 'page)]
      (if (and page (fn? @page))
        (swap! *pages* conj ns-sym)
        (swap! *pages* disj ns-sym)))))

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
                 [:li [:a {:href (str "/" page)}
                       (str page)]])
               (deref *pages*))]]]))

(defn- handle-page [req]
  (reload-modified-namespaces)
  (let [ns-sym (->> (.substring (:uri req) 1)
                    (symbol))
        ns (find-ns ns-sym)]
    (let [page (and ns (ns-resolve ns-sym 'page))]
      (cond page
            (-> (binding [*request* req]
                  (page))
                (html-response))
            (= (:uri req) "/")
            (page-index)
            :else
            (-> (response (str "var " ns-sym "/page not found"))
                (status 404))))))

(defn- wrap-logging [handler]
  (fn [req]
    (log (:uri req))
    (handler req)))

(def handler
  (-> handle-page
      (wrap-params)
      (wrap-file "static")
      (wrap-file-info)
      (wrap-logging)))

(defn hiccster
  ([] (hiccster "src"))
  ([& args]
     (alter-var-root (var *modified-namespaces*)
                     (constantly (ns-tracker args 0)))
     (reload-modified-namespaces)
     (run-jetty #'handler {:port 8765})))