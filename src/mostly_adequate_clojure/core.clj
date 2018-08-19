(ns mostly-adequate-clojure.core
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clj-http.client :as http]))

;; Box type
(defprotocol BoxOp
  (bmap [_ f])
  (fold [_ f] [_ f g]))
(defrecord Box [v]
  BoxOp
  (bmap [this f] (->Box (f (:v this))))
  (fold [this f] (f (:v this))))

(defn money2float [^String s]
  (-> (->Box s)
      (bmap #(Float/valueOf ^String %))))

(defn percent2float [^String s]
  (-> (->Box s)
      (bmap #(Float/valueOf ^String %))
      (bmap #(* 0.01 %))))

(defn discount [money percent]
  (-> (money2float money)
      (fold (fn [m]
              (fold (percent2float percent) (fn [p]
                                              (- m (* p m))))))))
(defn test-discount [] (discount "50" "40"))

;; Either type

(defprotocol Chain (chain [_ f]))
; Same as Box
(defrecord Right [v]
  BoxOp
  (bmap [this f] (->Right (f (:v this))))
  (fold [this _f g] (g (:v this)))
  Chain
  (chain [this f] (f (:v this))))

(defrecord Left [v]
  BoxOp
  (bmap [this _f] this)
  (fold [this f _g] (f (:v this)))
  Chain
  (chain [this _f] this))

(defn test-either []
  (let [r (-> (->Right 4)
              (bmap inc)
              (bmap #(/ % 2))
              (fold (constantly "error") identity))
        l (-> (->Left 4)
              (bmap inc)
              (bmap #(/ % 2))
              (fold (constantly "error ") identity))]
    [r l]))

(def ^:dynamic colors {:red "#ff4444" :blue "#3b5998" :yellow "#fff68f"})
(defn find-color [k] (get colors k))
(defn test-find-color [k] (-> k find-color (subs 1) .toUpperCase))
(test-find-color :red)                                      ; returns "FF4444"
; (test-find-color :green)                                    ; NPE !

(defn find-color-either [k]
  (let [result (get colors k)]
    (if result
      (->Right result)
      (->Left result))))

(defn find-color-either-str [k] (-> k find-color-either
                                    (bmap #(subs % 1))
                                    (fold (constantly "no color") #(.toUpperCase %))))
(defn test-find-color-either []
  (let [s (find-color-either-str :blue)
        f (find-color-either-str :no-code)]
    [s f]))
; => ["3B5998" "no color"]

(defn trycatch [f]
  (try (->Right (f)) (catch Exception e (->Left e))))

(def ^:dynamic good-config "{:port \"3000\"}")
(def ^:dynamic bad-config "{:port \"3000\" invalid-value}")
(def ^:dynamic some-bad-config "{:port \"300d\"}")

(defn get-port [config]
  (try (-> config
           (edn/read-string)                                    ; can throw exception
           :port
           Integer/parseInt)
       (catch Exception e 8888)))

(defn get-port-try [config]
  (-> (trycatch #(-> config (edn/read-string)))
      (bmap :port)
      (chain (fn [x] (trycatch #(Integer/parseInt x))))
      (fold (constantly 8888) identity)
      ))

;; currying, partial
(defn max-old [xs]
  (reduce (fn [acc x] (if (> x acc) x acc)) Integer/MIN_VALUE, xs))
(max-old [8 3 4 0 1])                                       ; => 8
(defn highest [x y] (if (> x y) x y))
(defn max-new [xs] (reduce highest Integer/MIN_VALUE, xs))
(def max-new2 (partial reduce highest Integer/MIN_VALUE))

;; composing
(defn url [tag]
  (str "https://api.flickr.com/services/feeds/photos_public.gne?tags="
       tag "cats&format=json&jsoncallback=?"))
(defn get-json [callback url]
  (http/get
    "https://api.flickr.com/services/feeds/photos_public.gne?tags=cats&format=json&jsoncallback=?"
    {:accept :json :async? true}
    callback callback))

(def ^:dynamic flickr (comp (partial get-json) url))

;; IO
(defprotocol IOProto
  (iomap [_ f]))
(defrecord IO [unsafeF]
  IOProto
  (iomap [_ g] (->IO (comp g unsafeF))))

(defn some-url [] "http://example.com/hello?key1=val1&searchTerm=wafflehouse")
(def href-io (->IO some-url))
(defn split-amp [s] (string/split s #"&"))
(defn split-eq [s] (string/split s #"="))
(defn split-q [s] (string/split s #"\?"))
(def to-pairs ^:dynamic (comp (partial map split-eq) split-amp))
(def params ^:dynamic (comp to-pairs last split-q))
(defn find-param [k] (iomap href-io (comp (partial filter (comp (partial = k) first)) params)))
((:unsafeF (find-param "key1")))
; => (["key1" "val1"])
;; async tasks
