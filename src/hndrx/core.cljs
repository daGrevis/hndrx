(ns hndrx.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [put! chan <!]]
            [reagent.core :as reagent :refer [atom]]
            [clojure.set :refer [difference]]
            [schema.core :as schema :include-macros true])
  (:import [goog.string format]))

(enable-console-print!)

(defn uuid4!
  "https://catamorphic.wordpress.com/2012/03/02/generating-a-random-uuid-in-clojurescript/"
  []
  (let [r (repeatedly 30 (fn [] (.toString (rand-int 16) 16)))]
    (apply str (concat (take 8 r) ["-"]
                       (take 4 (drop 8 r)) ["-4"]
                       (take 3 (drop 12 r)) ["-"]
                       [(.toString  (bit-or 0x8 (bit-and 0x3 (rand-int 15))) 16)]
                       (take 3 (drop 15 r)) ["-"]
                       (take 12 (drop 18 r))))))

; Convenience wrapper for PeerJS. I should move this out.

(defn connect-to-peerserver
  ([peer-id]
   (connect-to-peerserver peer-id {:host "localhost" :port 9000}))

  ([peer-id options]
   (js/Peer. peer-id (clj->js options))))

(defn on-peer-connection [peer callback]
  (.on peer "connection" callback))

(defn on-peer-error [peer callback]
  (.on peer "error" callback))

(defn connection->peer-id [connection]
  (.-peer connection))

(defn connections->peer-ids [connections]
  (map connection->peer-id connections))

(defn new-connection [peer peer-id-to-connect-to]
  (.connect peer peer-id-to-connect-to))

(defn send-data! [connection data]
  (.send connection (clj->js data)))

(defn on-connection-data [connection callback]
  (.on connection "data" #(callback (js->clj % :keywordize-keys true))))


(defn prepare-data [data-type data-source]
  {:data-type data-type
   :data-source data-source})

(defn send-message! [connections message]
  (let [data (prepare-data "message" message)]
    (doseq [connection connections]
      (send-data! connection data))))

(defn get-involved-peer-ids [peer-ids peer-id-for-follower]
  (difference (set peer-ids) (set [peer-id-for-follower])))

(def Message {:body schema/Str
              :from schema/Str
              :to [schema/Str]})

(def connections (atom []))
(def messages (atom []))

(def connections-chan (chan))
(def data-chan (chan))
(def messages-chan (chan))

; Called when a connection is made. Called once when follower connects to the leader on both sides.
(go
  (loop []
    (let [connection (<! connections-chan)]
      (on-connection-data connection #(put! data-chan %))

      (swap! connections conj connection))

    (recur)))

(go
  (loop []
    (let [data (<! data-chan)
          {:keys [data-type data-source]} data]
      (cond
        (= data-type "message") (put! messages-chan data-source)))

    (recur)))

; There is no difference between local messages and ones that are sent to you over the network.
(go
  (loop []
    (let [message (<! messages-chan)]
      (schema/validate Message message)

      (swap! messages conj message))

    (recur)))

(def peer-id (subs (uuid4!) 0 36))
(def peer (connect-to-peerserver peer-id))

(defn on-connection-to-leader [connection]
  (put! connections-chan connection)

  ; TODO: Send request to the follower to connect to other followers that are involved.
  (println (get-involved-peer-ids (connections->peer-ids @connections) (connection->peer-id connection))))

(on-peer-connection peer on-connection-to-leader)

(on-peer-error peer #(println (.-type %)))

(defn connections-component []
  [:div
   [:h2 "Connections"]
   [:ul
    (for [peer-id (connections->peer-ids @connections)]
      ^{:key peer-id} [:li peer-id])]])

(defn connecting-component []
  (let [peer-id-to-connect-to (atom "")]
    (fn []
      [:form {:on-submit (fn [e]
                           (.preventDefault e)
                           (let [connection (new-connection peer @peer-id-to-connect-to)]
                             (.on connection "error" #(println "Could not connect to other peer"))
                             (.on connection "open" #(put! connections-chan connection)))
                           (reset! peer-id-to-connect-to ""))}
       [:input {:type "text"
                :placeholder "peer-id"
                :value @peer-id-to-connect-to
                :on-change #(reset! peer-id-to-connect-to (.. % -target -value))}]])))

(defn messages-component []
  [:div
   [:h2 "Messages"]
   [:ul
    (for [{:keys [body from]} @messages]
      [:li body " (by " [:code from] ")"])]])

(defn messaging-component []
  (let [value (atom "")]
    (fn []
      [:form {:on-submit (fn [e]
                           (.preventDefault e)

                           (let [message {:body @value
                                          :from peer-id
                                          :to (connections->peer-ids @connections)}]
                             (schema/validate Message message)

                             (send-message! @connections message)

                             (put! messages-chan message))
                           (reset! value ""))}
       [:input {:type "text"
                :placeholder "Message"
                :value @value
                :on-change #(reset! value (.. % -target -value))}]])))

(defn root-component []
  [:div
   [:h1 "Hndrx"]
   [:p "Your peer-id is " [:code peer-id]]
   [connections-component]
   [connecting-component]
   [messages-component]
   [messaging-component]])

(reagent/render-component [root-component] (.-body js/document))
