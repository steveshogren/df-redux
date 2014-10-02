(ns dwarffortress.rnc-test
  (:use midje.sweet)
  (:require [dwarffortress.rnc :as r]))

(facts "Roman Numerals"
       (fact "Single digits" (r/parse "I") => 1)
       (fact "Single digits" (r/parse "V") => 5)
       (fact "Single digits" (r/parse "X") => 10)
       (fact "" (r/parse "III") => 3)
       (fact "" (r/parse "XVIII") => 18)
       (fact "" (r/parse "IV") => 4)
       (fact "" (r/parse "IX") => 9)
       (fact "" (r/parse "XX") => 20)
       (fact "" (r/parse "XXXX") => 0)
       )


