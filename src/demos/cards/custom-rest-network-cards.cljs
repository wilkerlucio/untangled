(ns cards.custom-rest-network-cards
  (:require
    [devcards.core :as dc :include-macros true]
    [untangled.client.cards :refer [untangled-app]]
    [om.next :as om :refer [defui]]
    [om.dom :as dom]
    [untangled.client.core :as uc]
    [untangled.client.data-fetch :as df]
    [untangled.client.mutations :as mut]
    [untangled.client.network :as net]))

(defui Post
  static om/IQuery
  (query [this] [:id :title])
  static om/Ident
  (ident [this {:keys [id]}] [:post/by-id id])
  Object
  (render [this]
    (let [{:keys [id title]} (om/props this)]
      (dom/div nil "id: " id ", title: " title))))
(def ui-post (om/factory Post))

(defui LatestPosts
  static uc/InitialAppState
  (initial-state [_ _] {:rest/limit 3})
  static om/IQuery
  (query [this] [:rest/limit {:rest/latest-posts (om/get-query Post)}])
  static om/Ident
  (ident [this props] [:posts :latest])
  Object
  (render [this]
    (let [{:rest/keys [limit latest-posts]} (om/props this)]
      (dom/div nil
        (dom/label nil "latest-posts"
          (dom/input #js {:value limit :onChange #(mut/set-integer! this :rest/limit :event %)}))
        (dom/button #js {:onClick #(df/load this :rest/latest-posts Post
                                     {:target (conj (om/get-ident this) :rest/latest-posts)
                                      :remote :rest ;; IMPORTANT, targets your custom remote
                                      :params {:limit limit}})}
          "GET")
        (map ui-post latest-posts)))))
(def ui-latest-posts (om/factory LatestPosts))

(defui Root
  static uc/InitialAppState
  (initial-state [_ _] {:posts/latest (uc/get-initial-state LatestPosts nil)})
  static om/IQuery
  (query [this] [{:posts/latest (om/get-query LatestPosts)}])
  Object
  (render [this]
    (let [{:posts/keys [latest]} (doto (om/props this) (prn "-- ROOT PROPS"))]
      (dom/div nil
        (ui-latest-posts latest)))))

(defmulti parse-query (fn [ast-frag _] (:dispatch-key ast-frag)))

(defmethod parse-query :rest/latest-posts [{{:keys [limit] :or {limit 5}} :params} app-state]
  {:url (cond-> "https://jsonplaceholder.typicode.com/posts?"
          limit (str "&_limit=" limit))
   :request-method :get})

(defmulti parse-response (fn [_ ast-frag _] (:dispatch-key ast-frag)))

(defmethod parse-response :rest/latest-posts [json _ app-state]
  {:rest/latest-posts (js->clj (js/JSON.parse json) :keywordize-keys true)})

(dc/defcard get-latest-posts "# Custom REST network"
  (untangled-app Root
    :networking {:remote (net/make-untangled-network "/api" :global-error-callback (constantly nil))
                 :rest   (net/make-rest-network :ast-frag->request parse-query :response->edn parse-response)})
  {}
  {:inspect-data true})
