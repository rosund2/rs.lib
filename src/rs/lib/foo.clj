(ns rs.lib.foo
  (:require [net.cgrand.enlive-html :as html]))


(def uri "http://holdemtight.com/pgs/od/oddpgs/3-169holdemhands.htm")

(defonce res (html/html-resource (java.net.URL. uri)))

(def qwe (butlast (rest (html/select res [(html/attr= :id "Hold'em hands") :tr]))))

;; http://cgrand.github.io/enlive/syntax.html
(def asd (mapv (fn [x] (first (:content x))) (html/select qwe [:tr (html/nth-child 2)])))

(def asddsa (map (fn [x y] (let [s (str "\"" x "\" ")]
                             (if (= 0 (mod y 10))
                               (print (str s "\n"))
                               (print s )))) asd (range)) )


