(ns aima.core
  (:require [aima.agents :refer :all :reload true]))

; Blind dog example
(def food (new-object :food))

(def water (new-object :water))

(defn percept
  [env agent]
  (list-things env (:location agent)))

(defn move-down!
  [env agent]
  (let [ag-name (:name agent)
        new-agent (update agent :location inc)
        dropped (filterv #(not (same-name? % ag-name)) (get-agent env))
        new-locations (conj dropped new-agent)]
    (println "Agent" ag-name "is moving down")
    (assoc env :agents new-locations)))

(defn consume-thing!
  [env location kind]
  (let [things (list-things env location kind)]
    (when (not-empty things)
      (remove-thing! env (first things)))))

(defn eat!
  [env agent]
  (let [location (:location agent)]
    (println "Agent" (:name agent) "eats food at" location)
    (consume-thing! env location :food)))

(defn drink!
  [env agent]
  (let [location (:location agent)]
    (println "Agent" (:name agent) "drinks water at" location)
    (consume-thing! env location :water)))

(defn execute!
  [env agent action]
  (case action
    :eat   (eat! env agent)
    :drink (drink! env agent)
    (move-down! env agent)))

(defn alive-left?
  [env]
  (some alive? (get-agent env)))

(defn edible-left?
  [env]
  (boolean (some #{:food :water} (map :name (:things env)))))

(defn program
  [percepts]
  (let [mapping {:water :drink
                 :food  :eat}]
    (mapping (:name percepts))))

(defn run-env
  [env steps]
  (doseq [a (range steps)]
    (swap! env stepper)))

(defn up-and-run
  []
  (def dog
    (new-agent program :blind-dog))

  (def park
    (new-environment :park
                     (fn [env] (done? env alive-left? edible-left?))
                     percept
                     execute!))

  (swap! park add-thing dog 1)
  (swap! park add-thing food 1)
  (swap! park add-thing water 2)

  (def watcher (atom []))

  (new-watcher park :Watchdog watcher)

  (run-env park 10)
  @park)
