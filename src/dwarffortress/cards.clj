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

(defn next-card [card-list]
  (let [keys (keys card-list)
        count (reduce + (vals card-list))
        cards (mapcat (fn [k] [(pct-of (k card-list) count) (fn [] k)])
                      keys)]
    (apply p/if-percent-fn cards)))

(def item-cards {
                 :baricade-hearts-8 2
                 :baricade-hearts-4 2
                 :baricade-hearts-6 2
                 :baricade-hearts-10 2
                 :baricade-diamonds-8 2
                 :baricade-diamonds-4 2
                 :baricade-diamonds-6 2
                 :baricade-diamonds-10 2
                 :baricade-spades-8 2
                 :baricade-spades-4 2
                 :baricade-spades-6 2
                 :baricade-spades-10 2
                 :baricade-clubs-8 2
                 :baricade-clubs-4 2
                 :baricade-clubs-6 2
                 :baricade-clubs-10 2

                 :weapon-hearts-5 2
                 :weapon-hearts-7 2
                 :weapon-diamonds-5 2
                 :weapon-diamonds-7 2

                 :weapon-mod-clubs-5 2
                 :weapon-mod-clubs-7 2
                 :weapon-mod-spades-5 2
                 :weapon-mod-spades-7 2
                 :weapon-mod-diamonds-5 2
                 :weapon-mod-diamonds-7 2
                 :weapon-mod-hearts-5 2
                 :weapon-mod-hearts-7 2

                 :food 16
                 :ammo 16
                 :gas 8 
                 :car-part 8

                 })
