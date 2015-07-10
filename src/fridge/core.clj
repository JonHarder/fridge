(ns fridge.core
  (:use fridge.ingredient)
  (:gen-class))

(def stock (ref {}))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [ing1 (make-ingredient "cheese" :ounces 3)
        ing2 (make-ingredient "cheese" :ounces 10)
        ing3 (make-ingredient "sugar" :liters 3)]
    (add-ingredient-to-stock stock ing1)
    (println @stock)
    (add-ingredient-to-stock stock ing2)
    (println @stock)
    (add-ingredient-to-stock stock ing3)
    (println @stock)))
