(ns hndrx.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [put! chan <!]]
            [reagent.core :as reagent :refer [atom]]
            [schema.core :as schema :include-macros true])
  (:import [goog.string format]))

; Utilities.

(defn in? [coll x]
  (not (nil? (some #(= x %) coll))))

(defn debug! [what]
  (.debug js/console (str what)))

(defn error! [what]
  (.error js/console (str what)))

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

; Schemas.

(def Message {:body schema/Str
              :from schema/Str
              :to [schema/Str]})

(def PeerIds [schema/Str])

; Safe functions.

(defn wrap-data [kind contents]
  {:kind kind
   :contents contents})

(defn undecided? [role]
  (= role :undecided))
(defn leader? [role]
  (= role :leader))
(defn follower? [role]
  (= role :follower))

(defn get-role-text [role]
  (get {:undecided "undecided"
        :leader "leader"
        :follower "follower"}
       role))

(defn get-undiscovered-peer-ids [peer-id peer-ids-currently-discovered peer-ids-from-leader]
  (filter #(and (not= % peer-id)
                ((complement in?) peer-ids-currently-discovered %))
          peer-ids-from-leader))

; App state.

(def role
  (atom :undecided))
(def peer-id (atom (subs (uuid4!) 0 36)))
(def peer (atom (connect-to-peerserver @peer-id)))
(def connections (atom []))
(def messages (atom []))

; Unsafe functions.

(defn send-data-to-connections! [connections data]
  (doseq [connection connections]
    (send-data! connection data)))


(defn connect-to-peer-id! [peer peer-id-to-connect-to connections-chan]
  (let [connection (new-connection peer peer-id-to-connect-to)]
    (.on connection "error" #(error! "Could not connect to other peer"))
    (.on connection "open" (fn []
                             (put! connections-chan [connection false])))))

; Channels and events.

(def connections-chan (chan))
(def data-chan (chan))

(defn on-incoming-connection-to-undecided []
  (reset! role :leader))

(defn on-incoming-connection-to-leader []
  (let [peer-ids (connections->peer-ids @connections)]
    (schema/validate PeerIds peer-ids)

    (send-data-to-connections! @connections (wrap-data "peer-ids" peer-ids))))

(defn on-incoming-connection-to-follower []
  ; It happens when follower connects to other follower because leader said it should.
  (debug! "someone connected to follower"))

(defn on-outgoing-connection-to-leader []
  (reset! role :follower))

;; There is no difference between local messages and ones that are sent to you over the network.
(defn on-message [message]
  (schema/validate Message message)

  (swap! messages conj message))

(defn on-peer-ids-to-follower [peer-ids]
  (doseq [peer-id (get-undiscovered-peer-ids @peer-id (connections->peer-ids @connections) peer-ids)]
    (connect-to-peer-id! @peer peer-id connections-chan)))

(on-peer-connection @peer #(put! connections-chan [% true]))
(on-peer-error @peer #(error! (.-type %)))

;; Called on any kind of new connection.
(go
  (loop []
    (let [[connection incoming?] (<! connections-chan)
          outgoing? (not incoming?)]
      (swap! connections conj connection)

      (on-connection-data connection #(put! data-chan %))

      (cond
        (and incoming? (undecided? @role)) (on-incoming-connection-to-undecided)
        (and incoming? (leader? @role)) (on-incoming-connection-to-leader)
        (and incoming? (follower? @role)) (on-incoming-connection-to-follower)
        outgoing? (on-outgoing-connection-to-leader)))

    (recur)))

(go
  (loop []
    (let [{:keys [kind contents]} (<! data-chan)]
      (cond
        (= kind "message") (on-message contents)
        (= kind "peer-ids") (on-peer-ids-to-follower contents)))

    (recur)))

; Components.

(defn connections-component []
  [:div
   [:h2 "Connections"]
   [:ul
    (for [peer-id (connections->peer-ids @connections)]
      ^{:key peer-id}
      [:li peer-id])]])

(defn stats-component []
  [:div
   [:p "Your peer-id is " [:code#peer-id @peer-id]]
   [:p "Your role is " [:code#role (get-role-text @role)]]])

(defn connecting-component []
  (let [peer-id-to-connect-to (atom "")]
    (fn []
      [:form {:on-submit (fn [e]
                           (.preventDefault e)
                           (connect-to-peer-id! @peer @peer-id-to-connect-to connections-chan)
                           (reset! peer-id-to-connect-to ""))}
       [:input {:type "text"
                :placeholder "peer-id"
                :value @peer-id-to-connect-to
                :on-change #(reset! peer-id-to-connect-to (.. % -target -value))}]])))

(defn messages-component []
  [:div
   [:h2 "Messages"]
   [:ul#messages
    (for [{:keys [body from]} @messages]
      ^{:key [body from]}
      [:li body " (by " [:code from] ")"])]])

(defn messaging-component []
  (let [value (atom "")]
    (fn []
      [:form {:on-submit (fn [e]
                           (.preventDefault e)

                           (let [message {:body @value
                                          :from @peer-id
                                          :to (connections->peer-ids @connections)}]
                             (schema/validate Message message)

                             (send-data-to-connections! @connections (wrap-data "message" message))

                             (on-message message))
                           (reset! value ""))}
       [:input#message {:type "text"
                        :placeholder "Message"
                        :value @value
                        :on-change #(reset! value (.. % -target -value))}]])))

(defn root-component []
  [:div
   [:h1 "Hndrx"]
   [stats-component]
   [connections-component]
   [connecting-component]
   [messages-component]
   [messaging-component]])

;; Takeoff for graphics!
(reagent/render-component [root-component] (.-body js/document))
