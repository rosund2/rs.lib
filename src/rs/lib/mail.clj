(ns foo1.mail
  (:import [javax.mail Authenticator]))

;;C-c M-o clear repl
;;C-c C-k Eval entire buffer
;;C-c C-x


(defprotocol Notify
  (notify-send! [this msg]))

(defrecord Smtp [session]
  Notify
  (notify-send! [this msg]
    (do
      (let [mm (javax.mail.internet.MimeMessage. (:session this))]
        (doto mm
          (.setFrom (javax.mail.internet.InternetAddress. (:from msg)))          
          (.setRecipients  
           (javax.mail.Message$RecipientType/TO)
           (javax.mail.internet.InternetAddress/parse (:recipients msg)))          
          (.setSubject (:subject msg))
          (.setText (:text msg)))
        (javax.mail.Transport/send mm)))))


(defn make-smtp [{:keys [host port user password ssl auth] :or {ssl true auth true}}]
  {:pre [host port user password (number? port)]}
  (let [props (java.util.Properties.)]
    (doto props
      (.put "mail.smtp.host" host)
      (.put "mail.smtp.port" port)
      (.put "mail.smtp.user" user)
      (.put "mail.smtp.socketFactory.port" port)
      (.put "mail.smtp.auth" (.toString auth)))

    (if  (= ssl true)
      (doto props
       (.put "mail.smtp.starttls.enable" "true")
       (.put "mail.smtp.socketFactory.class" 
             "javax.net.ssl.SSLSocketFactory")
       (.put "mail.smtp.socketFactory.fallback" "false")))

    (if auth
      (Smtp.
       (javax.mail.Session/getInstance
        props
        (proxy [javax.mail.Authenticator] [] 
          (getPasswordAuthentication 
            []
            (javax.mail.PasswordAuthentication. 
             user password)))))
      (Smtp.
       (javax.mail.Session/getInstance
        props)))))


(defonce smtp
  (make-smtp {:host "smtp.mail.com" :port 21 :password "test" :user "nisse" :ssl false :auth false}))

(notify-send! smtp
              {:subject "asdasd"
               :from "frommer@mail.com"
               :text "hei"
               :recipients "toer@mail.com"})


(defrecord PostMark [session]
    Notify
    (notify-send! [this msg]))

