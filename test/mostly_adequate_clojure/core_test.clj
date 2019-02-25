(ns mostly-adequate-clojure.core-test
  (:require [clojure.test :refer :all]
            [mostly-adequate-clojure.core :refer :all]
            [clojure.algo.monads :as am]
            [cats.core :as m]
            [cats.monad.either :as eit]
            [cats.builtin]))

(defn m-bind [value function]
  (function value))

(defn id-m [n]
  (am/domonad am/identity-m
    [a n
     b (inc a)] (* a b)))

(defn maybe-m [n]
  (am/domonad am/maybe-m
    [a n
     b (inc a)] (* a b)))

(deftest m-bind-test
  (testing "m-bind test"
    (is (= (m-bind 1 (fn [a]
                     (m-bind (inc a) (fn [b]
                                       (* a b))))) 2)))
  (testing "identity-m"
    (is (= 6 (id-m 2))))
  (testing "identity-m fail"
    (is (thrown? NullPointerException (id-m nil))))
  (testing "maybe-m"
    (is (= nil (maybe-m nil)))))

(defn m-bind-first-try [sequence function]
  (apply concat (map function sequence)))

(m-bind-first-try (range 5)  (fn [a]
                               (m-bind-first-try (range a)  (fn [b]
                                                              (list (* a b))))))

(map (fn [a]
       (map (fn [b]
              (list (* a b))) (range a))) (range 5))

(am/with-monad am/sequence-m
  (defn mystery
    [f xs]
    ( (am/m-lift 1 f) xs )))

(mystery #(str "hello " %) (range 5))

(am/with-monad am/sequence-m
  (defn ntuples [n xs]
    (am/m-seq (repeat n xs))))

(ntuples 2 [1 2 3])

(eit/right 200)
