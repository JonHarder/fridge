(ns fridge.core
  (:gen-class))

(defn make-ingredient
  [name unit number]
  {:pre [(number? number)
         (keyword? unit)]}
  {:name (keyword name) :units (keyword unit) :number number})

(defn- get-units
  [ingredient]
  {:units (:units ingredient) :number (:number ingredient)})

(defn- update-units
  [ingredient unit-map]
  "updates the ingredients :units and :number values to that of
   unit-map, throwing out its own"
  (let [units (:units unit-map)
        number (:number unit-map)]
    (-> ingredient
       (assoc :units units)
       (assoc :number number))))

(defn- convert-to
  [unit-map unit]
  "takes an map of {:units unit :number number}
   and a unit keyword ex. :lbs, :ounces, :cans
   and returns a new map for ingredient converted to
   the unit specified
   ex. (convert-units {:units :mililiter :number 500} :liter) -> {:units :liter :number 0.5}"
  (let [converter {:mililiters {:liters     #(/ % 1000)}
                   :liters     {:mililiters #(* % 1000)}}
        from (:units unit-map)
        to unit
        func (some-> converter
                     from
                     to)]
    (if func
      (let [new-number (func (:number unit-map))]
        (update-units unit-map {:units to :number new-number}))
      unit-map)))

(defmacro infix
  [proc]
  (list (second proc)
        (first proc)
        (second (rest proc))))

(defn- in
  [key col]
  (not (= (key col) nil)))

(defn- insert-ingredient
  [stock ingredient]
  "takes a map of maps of form {:ingredient-name {:unit unit :number number}}
   and an ingredient of the form {:name name :unit unit :number number}"
  (let [name (:name ingredient)
        val (dissoc ingredient :name)]
    (assoc stock name val)))


(defn- add-units
  [unit1 unit2]
  "add two maps of form {:units unit :number number} together,
   converting unit2 to the units of unit1, altering it's number
   accordingly"
  (let [units (:units unit1)
        converted (convert-to unit2 (:units unit1))]
    {:units units
     :number (+ (:number unit1)
                (:number converted))}))

(defn- add-ingredients
  [ingredient1 ingredient2]
  "adds the ingredients together, so units and number reflects
   the units of the first ingredient and the number of the sum
   of ingredient1 and ingredient 2."
  (assert (= (:name ingredient1)
             (:name ingredient2))
          "Cannot add disparate ingredients")
  (let [unit1 (get-units ingredient1)
        unit2 (get-units ingredient2)
        converted-units (add-units unit1 unit2)]
    (update-units ingredient1 converted-units)))

(defn add-ingredient-to-stock
  [stock ingredient]
  "takes a ref to a stock {:food-name {:units unit :number number}}
   and an ingredient {:name name :units unit :number number}
   and adds it to the stock ref, making a new entry if none of that
   ingredient were previously present or adding the ingredient to
   the supply previously there (converting to the units provided"
  (println (str "Adding " (:number ingredient) " " (name (:units ingredient))
                " of " (name (:name ingredient))))
  (dosync
   (let [name (:name ingredient)
         current (assoc (name @stock) :name name)]
     (if (infix (name in @stock))
       (alter stock insert-ingredient (add-ingredients current ingredient))
       (alter stock insert-ingredient ingredient)))))

(def stock (ref {}))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [ing1 (make-ingredient "cheese" :ounces 3)
        ing2 (make-ingredient "cheese" :ounces 10)]
    (add-ingredient-to-stock stock ing1)
    (add-ingredient-to-stock stock ing2)))
