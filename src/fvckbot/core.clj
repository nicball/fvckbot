(ns fvckbot.core
  (:use [compojure.core :only (defroutes GET POST)]
        [ring.adapter.jetty :only (run-jetty)]
        [compojure.route :only (not-found)]
        [fvckbot.brainfuck :only (brainfuck)])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-http.client :as client]
            [clojure.data.json :as json]
            [environ.core :refer [env]]))

(def tgbot-token (env :tgbot-token))
(def tgbot-api-url (str "https://api.telegram.org/bot" tgbot-token))

(def pia-database (atom {}))

(defn tg-post-command [cmd params]
  (client/post (str tgbot-api-url "/" cmd) {:form-params params}))

(defn send-message [chat-id text & {:keys [reply-to-message]}]
  (let [cmd "sendMessage"
        params {:chat_id chat-id
                :text text}]
    (tg-post-command cmd
                     (if (nil? reply-to-message)
                       params
                       (assoc params :reply_to_message_id reply-to-message)))))

(defn reply-to-message [message text]
  (send-message (get-in message [:chat :id])
                text
                :reply-to-message (:message-id message)))

(defn get-command [^String s]
  (let [begin ^String (first (.split s "[ @]"))]
    (str/lower-case 
      (if (.startsWith begin "/")
        (.substring begin 1)
        begin))))

(defn skip-command [^String s]
  (let [idx (.indexOf s " ")]
    (if (= -1 idx)
      ""
      (str/trim (.substring s (inc idx))))))

(defn add-pia-database [q a]
  (swap! pia-database 
         (fn [db]
           (if-let [as (get db q)]
             (assoc db q (conj as a))
             (assoc db q [a])))))

(defn pia [message ^String params]
  (if (not= -1 (.indexOf params "%"))
    (let [[q a :as ps] (map str/trim (.split params "%"))]
      (if (or (not= 2 (count ps))
              (= "" q))
        (reply-to-message message "语法错误")
        (do
          (add-pia-database q a)
          (reply-to-message message "喵~"))))
    (if-let [answer (get @pia-database params)]
      (reply-to-message message (rand-nth answer))
      (reply-to-message message "(￣ε(#￣)☆╰╮o(￣▽￣///)"))))

(defn bf [^String params]
  (if (not= -1 (.indexOf params "%"))
    (let [[code input :as ps] (.split params "%")]
      (if (not= 2 (count ps))
        "语法错误"
        (brainfuck code input 400)))
    (brainfuck params "" 400)))


(defn bot [{message :message :as update}]
  (when-let [text (:text message)]
    (condp = (get-command text)
      "ping" (reply-to-message message "ping你妹")
      "pia" (pia message (skip-command text))
      "dump" (reply-to-message message (str @pia-database))
      "bf" (reply-to-message message (bf (skip-command text)))
      nil)))

(defn pretty-keyword [s]
  (keyword (str/replace s \_ \-)))

(defroutes app
  (POST (str "/" tgbot-token) {body :body}
    (with-open [rd (io/reader body)]
      (bot (json/read rd :key-fn pretty-keyword)))
    {:status 202 :headers {} :body "202 Accepted"})
  (not-found "<h1>Sorry, there is nothing here.</h1>"))

(defn -main [port]
  (run-jetty app {:port (Integer/parseInt port) :join? false}))
