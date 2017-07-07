(ns untangled.client.network
  (:refer-clojure :exclude [send])
  (:require
    [untangled.client.logging :as log]
    [cognitect.transit :as ct]
    #?(:cljs [goog.events :as events])
    [om.next :as om]
    [om.transit :as t]
    [clojure.string :as str])
  #?(:cljs (:import [goog.net XhrIo EventType])))

(declare make-untangled-network)

#?(:cljs
   (defn make-xhrio "This is here (not inlined) to make mocking easier." [] (XhrIo.)))

(defprotocol NetworkBehavior
  (serialize-requests? [this] "Returns true if the network is configured to desire one request at a time."))

(defprotocol ProgressiveTransfer
  (updating-send [this edn done-callback error-callback update-callback] "Send EDN. The update-callback will merge the state
  given to it. The done-callback will merge the state given to it, and indicates completion. See
  `untangled.client.ui.file-upload/FileUploadNetwork` for an example."))

(defprotocol UntangledNetwork
  (send [this edn done-callback error-callback]
    "Send EDN. Calls either the done or error callback when the send is done. You must call one of those only once.
     Implement ProgressiveTransfer if you want to do progress updates during network transmission.")
  (start [this complete-app]
    "Starts the network, passing in the app for any components that may need it."))

(defprotocol IXhrIOCallbacks
  (response-ok [this xhrio ok-cb] "Called by XhrIo on OK")
  (response-error [this xhrio err-cb] "Called by XhrIo on ERROR"))

#?(:cljs
   (defn parse-response
     "An XhrIo-specific implementation method for interpreting the server response."
     ([xhrio] (parse-response xhrio nil))
     ([xhrio read-handlers]
      (try (let [text          (.getResponseText xhrio)
                 base-handlers {"f" (fn [v] (js/parseFloat v)) "u" cljs.core/uuid}
                 handlers      (if (map? read-handlers) (merge base-handlers read-handlers) base-handlers)]
             (if (str/blank? text)
               (.getStatus xhrio)
               (ct/read (t/reader {:handlers handlers})
                 (.getResponseText xhrio))))
           (catch js/Object e {:error 404 :message "Server down"})))))

(defrecord Network [url request-transform global-error-callback complete-app transit-handlers]
  NetworkBehavior
  (serialize-requests? [this] true)
  IXhrIOCallbacks
  (response-ok [this xhrio valid-data-callback]
    ;; Implies:  everything went well and we have a good response
    ;; (i.e., got a 200).
    #?(:cljs
       (try
         (let [read-handlers  (:read transit-handlers)
               query-response (parse-response xhrio read-handlers)]
           (when (and query-response valid-data-callback) (valid-data-callback query-response)))
         (finally (.dispose xhrio)))))
  (response-error [this xhrio error-callback]
    ;; Implies:  request was sent.
    ;; *Always* called if completed (even in the face of network errors).
    ;; Used to detect errors.
    #?(:cljs
       (try
         (let [status                 (.getStatus xhrio)
               log-and-dispatch-error (fn [str error]
                                        ;; note that impl.application/initialize will partially apply the
                                        ;; app-state as the first arg to global-error-callback
                                        (log/error str)
                                        (error-callback error)
                                        (when @global-error-callback
                                          (@global-error-callback status error)))]
           (if (zero? status)
             (log-and-dispatch-error
               (str "UNTANGLED NETWORK ERROR: No connection established.")
               {:type :network})
             (log-and-dispatch-error
               (str "SERVER ERROR CODE: " status)
               (parse-response xhrio transit-handlers))))
         (finally (.dispose xhrio)))))
  UntangledNetwork
  (send [this edn ok error]
    #?(:cljs
       (let [xhrio     (make-xhrio)
             handlers  (or (:write transit-handlers) {})
             headers   {"Content-Type" "application/transit+json"}
             {:keys [body headers]} (cond-> {:body edn :headers headers}
                                      request-transform request-transform)
             post-data (ct/write (t/writer {:handlers handlers}) body)
             headers   (clj->js headers)]
         (.send xhrio url "POST" post-data headers)
         (events/listen xhrio (.-SUCCESS EventType) #(response-ok this xhrio ok))
         (events/listen xhrio (.-ERROR EventType) #(response-error this xhrio error)))))
  (start [this app]
    (assoc this :complete-app app)))

(defn make-untangled-network
  "Build an Untangled Network object using the default implementation.

  Features:

  - Can configure the target URL on the server for Om network requests
  - Can supply a (fn [{:keys [body headers] :as req}] req') to transform arbitrary requests (e.g. to add things like auth headers)
  - Supports a global error callback (fn [status-code error] ) that is notified when a 400+ status code or hard network error occurs
  - `transit-handlers`: A map of transit handlers to install on the reader, such as

   `{ :read { \"thing\" (fn [wire-value] (convert wire-value))) }
      :write { Thing (ThingHandler.) } }`

   where:

   (defrecord Thing [foo])

   (deftype ThingHandler []
     Object
     (tag [_ _] \"thing\")
     (rep [_ thing] (make-raw thing))
     (stringRep [_ _] nil)))
  "
  [url & {:keys [request-transform global-error-callback transit-handlers]}]
  (map->Network {:url                   url
                 :transit-handlers      transit-handlers
                 :request-transform     request-transform
                 :global-error-callback (atom global-error-callback)}))


(defrecord MockNetwork
  [complete-app]
  UntangledNetwork
  (send [this edn ok err] (log/info "Ignored (mock) Network request " edn))
  (start [this app]
    (assoc this :complete-app app)))

(defn mock-network [] (map->MockNetwork {}))


(defn- on-ok [{:keys [response->edn app-state]} xhrio om-ok-cb query]
  (try
    (when-let [response (response->edn (.getResponse xhrio) query @app-state)]
      (om-ok-cb response))
    (finally (.dispose xhrio))))

(defn- on-error [{:keys [app-state on-network-error on-network-error]} xhrio om-error-cb query]
  (try
    (om-error-cb (on-network-error xhrio @app-state query))
    (finally (.dispose xhrio))))

(defn default-on-network-error [{:keys [on-network-error]} xhrio om-error-cb query]
  (try
    (let [status (.getStatus xhrio)]
      (log/error (str "ERROR: RestNetwork failed to process: " query "."))
      (om-error-cb (if (zero? status) {:type :network} {})))
    (finally (.dispose xhrio))))

(defrecord RestNetwork [app-state om-query->request response->edn on-network-error serialize-requests?]
  NetworkBehavior
  (serialize-requests? [this] serialize-requests?)
  UntangledNetwork
  (send [this query ok-cb error-cb]
    #?(:cljs
       (let [xhrio (make-xhrio), {:keys [url request-method headers body]} (om-query->request query @app-state)]
         (.send xhrio url (name request-method) body headers)
         (events/listen xhrio (.-SUCCESS EventType)
           #(on-ok this xhrio ok-cb query))
         (events/listen xhrio (.-ERROR EventType)
           #(on-error this xhrio error-cb query)))))
  (start [this app]
    (assoc this :app-state (om/app-state (:reconciler app)))))

(defn make-rest-network
  "Takes as kw arguments:
   - :om-query->request              - (fn [query app-state] ... request)

   - :response->edn                  - (fn [json query app-state] ... edn)

   - :on-network-error (OPTIONAL)    - (fn [xhrio app-state query] ... om-error)
      - defaults to `default-on-network-error`
      - for docs see https://developers.google.com/closure/library/docs/xhrio

   - :serialize-requests? (OPTIONAL) - Boolean
      - defaults to true

   Returns an UntangledNetwork component, for use in untangled client networking as an additional remote."
  [& {:as opts}] (map->RestNetwork (merge {:on-network-error default-on-network-error :serialize-requests? true} opts)))
