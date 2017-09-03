(ns game.core
  (:gen-class)
  (:require [clojure.string :as str]))


(defrecord Item [name loc])
(def items
  [(->Item :frog :garden)
   (->Item :hammer :attic)
   (->Item :whiskey :living-room)
   (->Item :bucket :living-room)])

(defn describe-item [item]
  (format "You see a %s on the floor." (name (:name item))))

(defrecord Edge [destination direction object])
(defn describe-edge [edge]
  (format "There is a %s going %s."
  (name (get edge :object))
  (name (get edge :direction))))

(defrecord Location [name desc edges])
(def locations
  [(->Location :living-room "You are in a living room. A wizard is snoring on the couch."
               [(->Edge :garden :west :door)
                (->Edge :attic :upstairs :ladder)])
   (->Location :garden "You are in a beautiful garden. There is a well in front of you."
               [(->Edge :living-room :east :door)])

   (->Location :attic "You are in the attic. There is a giant welding torch in the corner."
               [(->Edge :living-room :downstairs :ladder)])])

(defn location-from-name [name locations]
  (first (filter (fn [loc] (= name (get loc :name)))
                 locations)))

(defn describe-paths [loc]
  (reduce (fn [x y] (str x "\n" y))
          (map describe-edge (get loc :edges))))

(defn items-at-location [loc items]
  (filter (fn [item] (= (get item :loc)
                        (get loc :name)))
          items))


(defn describe-items-at-location [loc items]
  (reduce str (map (fn [item] (str (describe-item item) "\n"))
                   (items-at-location loc items))))

(defn describe-location [loc items]
  (str
    "\n" (get loc :desc)
    "\n" (describe-paths loc)
    "\n" (describe-items-at-location loc items)))


(defn describe [loc items]
  (println (describe-location loc items)))


(def cur-location (location-from-name :living-room locations))

(defn change-location [new-location]
  (def cur-location new-location)
  (describe new-location items))

(defn action-in-location [action location]
  (cond
    (str/starts-with? action "go")
    (let [direction (str/trim (str/replace-first action #"go" ""))
          edges (get location :edges)
          edge (first (filter (fn [e] (= (name (get e :direction)) direction)) edges))]
      (if-not (nil? edge)
        (change-location (location-from-name (get edge :destination) locations))))
    (str/starts-with? action "pickup")
    (let [item (str/trim (str/replace-first action #"pickup" ""))]
      (if (= (name (get item :loc)) (name (get location :name)))
        ()))))

(def prompt "> ")

(defn game-step []
  (println "What do you want to do?")
  (print prompt)
  (flush)
  (let [user-input (str/trim (read-line))]
    (cond
      (= user-input "look around")
      (describe cur-location items)
      :default
      (action-in-location user-input cur-location))))



(defn -main [& args]
  (defn main-game-loop []
    (game-step)
    (recur))
  (main-game-loop))
