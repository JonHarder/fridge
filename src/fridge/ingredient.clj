(ns fridge.ingredient)
; exports [make-ingredient in-stock? add-ingredient-to-stock]

; There are two basic units of this module: the "ammount" which has
; both units and number
; and ingredient which has a name, and ammount

(defn make-ingredient
  [name unit number]
  {:pre [(number? number)
         (keyword? unit)]}
  [(keyword name) (keyword unit) number])

(defn- i-name
  [i]
  (first i))

(defn- i-units
  [i]
  (if (= (count i) 3)
    (second i)
    (first i)))

(defn- i-number
  [i]
  (if (= (count i) 3)
    (second (rest i))
    (second i)))

(defn- fmap
  [ingredient func]
  (make-ingredient (i-name ingredient) (i-units ingredient) (func (i-number ingredient))))
;;; everything above is the api used for working with ingredients
;;; everything below should use these for interacting with them
;;; the internal implimentation of ingredient should not need to be known
(defn in-stock?
  [ingredient stock]
  (not (= nil ((i-name ingredient) stock))))

(defn- convert-to
  [ingredient units]
  "takes an ingredient and a unit to convert it to"
  (let [converter {:mililiters {:liters     #(/ % 1000)}
                   :liters     {:mililiters #(* % 1000)}
                   :ounces     {:pounds     #(/ % 16)}
                   :pounds     {:ounces     #(* % 16)}}
        conversion-map (some-> ingredient
                               i-units
                               converter)
        func (some-> units conversion-map)]
    (if func
      (fmap ingredient func)
      ingredient)))


(defn- add-units
  [i1 i2]
  "takes two ingredients, converts the second's ammount to the units
   of the first, and returns the ammount"
  (let [i2-converted (convert-to i2 (i-units i1))]
    (+ (i-number i1) (i-number i2-converted))))

(defn- add-ingredients
  [i1 i2]
  "adds two ingredients together, converting units to unify types"
  {:pre (= (i-name i1) (i-name i2))}
  (let [name (i-name i1)
        units (i-units i1)
        number (add-units i1 i2)]
  (make-ingredient name units number)))

(defn add-ingredient-to-stock
  [stock ingredient]
  (dosync
   (let [name (i-name ingredient)]
     (if (in-stock? ingredient @stock)
       (let [current (name @stock)]
         (alter stock assoc name (add-ingredients current ingredient)))
       (alter stock assoc name ingredient)))))
