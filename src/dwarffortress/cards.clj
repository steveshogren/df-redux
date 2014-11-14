(ns dwarffortress.cards
  (:require [dwarffortress.percentages :as p])
  (:use clojure.test))

(def location-cards {:residence 16 :superstore 4
                     :grocery 4 :pawn-shop 4
                     :gas-station 4})

(defn pct-of [x out-of]
  (-> x (* 100) (/ out-of)))

(defn next-location []
  (let [count (reduce + (vals location-cards))]
    (p/if-percent
     (pct-of (:residence location-cards) count) :residence
     (pct-of (:superstore location-cards) count) :superstore
     (pct-of (:grocery location-cards) count) :grocery 
     (pct-of (:pawn-shop location-cards) count) :pawn-shop 
     (pct-of (:gas-station location-cards) count) :gas-station)))

