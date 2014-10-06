(ns dwarffortress.rnc-test
  (:use midje.sweet)
  (:require [dwarffortress.rnc :as r]))

(facts "Roman Numerals"
       (fact "" (r/parse "I") => 1)
       (fact "" (r/parse "V") => 5)
       (fact "" (r/parse "X") => 10)
       (fact "" (r/parse "L") => 50)
       (fact "" (r/parse "C") => 100)
       (fact "" (r/parse "D") => 500)
       (fact "" (r/parse "M") => 1000)
       (fact "" (r/parse "III") => 3)
       (fact "" (r/parse "XVIII") => 18)
       (fact "" (r/parse "IV") => 4)
       (fact "" (r/parse "IX") => 9)
       (fact "" (r/parse "XX") => 20)
       (fact "" (r/parse "XXXX") => 0)
       )
(facts "To Roman"
       (fact "" (r/to-roman 1) => "I")
       (fact "" (r/to-roman 5) => "V")
       (fact "" (r/to-roman 10) => "X")
       (fact "" (r/to-roman 50) => "L")
       (fact "" (r/to-roman 100) => "C")
       (fact "" (r/to-roman 500) => "D")
       (fact "" (r/to-roman 1000) => "M")
       (fact "" (r/to-roman 3) => "III")
       (fact "" (r/to-roman 15) => "XV")
       (fact "" (r/to-roman 30) => "XXX")
       (fact "" (r/to-roman 18) => "XVIII")
       )


