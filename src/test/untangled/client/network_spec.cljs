(ns untangled.client.network-spec
  (:require
    [goog.events :as events]
    [om.next :as om]
    [untangled.client.network :as net]
    [untangled-spec.core :refer-macros [specification behavior assertions provided component when-mocking]])
  (:import [goog.net EventType]))

(specification "Networking"
  (component "Construction of networking"
    (let [url "/some-api"
          atom? (fn [a] (= (type a) Atom))
          n (net/make-untangled-network url :request-transform :transform :global-error-callback (fn [status body] status))]
      (assertions
        "sets the URL"
        (:url n) => url
        "records the request transform"
        (:request-transform n) => :transform
        "records the global error callback"
        (@(:global-error-callback n) 200 "Body") => 200)))

  (behavior "Send"
    (let [body-sent (atom nil)
          headers-sent (atom nil)
          network (net/make-untangled-network "/api")
          fake-xhrio (js-obj "send" (fn [url typ body headers]
                                      (reset! body-sent body)
                                      (reset! headers-sent headers)))]

      (when-mocking
          (net/make-xhrio) => fake-xhrio
          (events/listen & _) => nil

          (net/send network {:original 1} nil nil))

      (assertions
        "Sends the original body if no transform is present"
        (js->clj @body-sent) => "[\"^ \",\"~:original\",1]"
        "Uses content-type for transit by default"
        (js->clj @headers-sent) => {"Content-Type" "application/transit+json"}))

    (let [body-sent (atom nil)
          headers-sent (atom nil)
          network (net/make-untangled-network "/api" :request-transform (fn [{:keys [request headers]}]
                                                                          {:body {:new 2}
                                                                           :headers {:other 3}}))
          fake-xhrio (js-obj "send" (fn [url typ body headers]
                                      (reset! body-sent body)
                                      (reset! headers-sent headers)))]

      (when-mocking
          (net/make-xhrio) => fake-xhrio
          (events/listen & _) => nil

          (net/send network {:original 1} nil nil))

      (assertions
        "Request transform can replace body"
        (js->clj @body-sent) => "[\"^ \",\"~:new\",2]"
        "Request transform can replace headers"
        (js->clj @headers-sent) => {"other" 3}))))

(specification "RestNetwork"
  (when-mocking
    (om/app-state ::reconciler) => (atom ::test-app-state)
    (let [sent-request (atom nil)]
      (when-mocking
        (net/make-xhrio) => (js-obj "send" (fn [url method body headers] (reset! sent-request [url method body headers])))
        (events/listen & _) => nil
        (component "ast-frag->request"
          (assertions "takes an ast fragment, app state, and returns a request"
            (-> (net/make-rest-network :ast-frag->request
                  (fn test-ast-frag->request [ast-frag app-state]
                    (assertions "ast-frag->request receives correct arguments"
                      (namespace (:dispatch-key ast-frag)) => (namespace ::_)
                      app-state => ::test-app-state)
                    {:url "/fake-url/325" :request-method :post :body ::test-body :headers {"test" "headers"}}))
              (net/start {:reconciler ::reconciler})
              (net/send '[::fake.query ::second.fragment] nil nil)
              (do @sent-request))
            => ["/fake-url/325" "post" ::test-body {"test" "headers"}]))))

    (let [response-edn (atom nil)]
      (when-mocking
        (net/make-xhrio) => (js-obj "send" (constantly ::ignored)
                                    "dispose" (constantly ::ignored)
                                    "getResponse" (constantly ::response))
        (events/listen xhrio evt-type callback) => (when (= (.-SUCCESS EventType) evt-type) (callback))
        (component "response->edn"
          (assertions "takes the response, an ast fragment, app-state and returns the response parsed into edn"
            (-> (net/make-rest-network :ast-frag->request (constantly {:url "/always-success" :request-method :get})
                  :response->edn (fn test-response->edn [response ast-frag app-state]
                                   (assertions "response->edn receives correct arguments"
                                     response => ::response
                                     (namespace (:dispatch-key ast-frag)) => (namespace ::_)
                                     app-state => ::test-app-state)
                                   {::test ::response}))
              (net/start {:reconciler ::reconciler})
              (net/send '[::fake.query ::second.fragment] (partial reset! response-edn) nil)
              (do @response-edn))
            => {::test ::response}))))

    (let [om-error (atom nil)
          mock-xhrio (js-obj "send" (constantly ::ignored)
                             "dispose" (constantly ::ignored)
                             "getResponse" (constantly ::response))]
      (when-mocking
        (net/make-xhrio) => mock-xhrio
        (events/listen xhrio evt-type callback) => (when (= (.-ERROR EventType) evt-type) (callback))
        (component "on-network-error"
          (assertions
            "takes the xhrio, app-state, an ast fragment, and returns an error for the om error callback"
            (-> (net/make-rest-network :ast-frag->request (constantly {:url "/always-error" :request-method :get})
                  :on-network-error (fn test-on-network-error [xhrio app-state ast-frag]
                                      (assertions "on-network-error receives correct arguments"
                                        xhrio => mock-xhrio
                                        app-state => ::test-app-state
                                        (namespace (:dispatch-key ast-frag)) => (namespace ::_))
                                      {:type ::unknown-error}))
              (net/start {:reconciler ::reconciler})
              (net/send '[::fake.query ::second.fragment] nil (partial reset! om-error))
              (do @om-error))
            => {:type ::unknown-error}
            "defaults to default-on-network-error"
            (:on-network-error (net/make-rest-network)) => net/default-on-network-error))))))
