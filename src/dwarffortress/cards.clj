(ns dwarffortress.cards
  (:require [dwarffortress.percentages :as p])
  (:use clojure.test))

(def location-cards {:residence {:count 16 :cost 4}
                     :superstore {:count 8 :cost 8}
                     :grocery {:count 8 :cost 6}
                     :pawn-shop {:count 8 :cost 6}
                     :gas-station {:count 8 :cost 6}})

(defn pct-of [x out-of]
  (-> x (* 100) (/ out-of)))

(defn next-card [card-list]
  (let [keys (keys card-list)
        count (reduce + (map :count (vals card-list)))
        cards (mapcat (fn [k] [(pct-of (:count (get card-list k)) count)
                               (fn [] k)])
                      keys)]
    (apply p/if-percent-fn cards)))

;; (next-location)
(defn next-location []
  (next-card location-cards))

(def item-cards {
                 :baricade-hearts-8 {:count 2}
                 :baricade-hearts-4 {:count 2}
                 :baricade-hearts-6 {:count 2}
                 :baricade-hearts-10 {:count 2}
                 :baricade-diamonds-8 {:count 2}
                 :baricade-diamonds-4 {:count 2}
                 :baricade-diamonds-6 {:count 2}
                 :baricade-diamonds-10 {:count 2}
                 :baricade-spades-8 {:count 2}
                 :baricade-spades-4 {:count 2}
                 :baricade-spades-6 {:count 2}
                 :baricade-spades-10 {:count 2}
                 :baricade-clubs-8 {:count 2}
                 :baricade-clubs-4 {:count 2}
                 :baricade-clubs-6 {:count 2}
                 :baricade-clubs-10 {:count 2}

                 :weapon-hearts-5 {:count 2}
                 :weapon-hearts-7 {:count 2}
                 :weapon-diamonds-5 {:count 2}
                 :weapon-diamonds-7 {:count 2}

                 :weapon-mod-clubs-5 {:count 2}
                 :weapon-mod-clubs-7 {:count 2}
                 :weapon-mod-spades-5 {:count 2}
                 :weapon-mod-spades-7 {:count 2}
                 :weapon-mod-diamonds-5 {:count 2}
                 :weapon-mod-diamonds-7 {:count 2}
                 :weapon-mod-hearts-5 {:count 2}
                 :weapon-mod-hearts-7 {:count 2}

                 :food {:count 16}
                 :ammo {:count 16}
                 :gas {:count 8} 
                 :car-part {:count 8}

                 })
;; (next-item)
(defn next-item []
  (next-card item-cards))

(def action-cards
  (apply merge (map (fn [num] {[:hearts num] {:count 2}
                               [:spades num] {:count 2}
                               [:diamonds num] {:count 2}
                               [:clubs num] {:count 2}})
                    (range 1 14))))

;; (next-action-card)
(defn next-action-card []
  (next-card action-cards))

