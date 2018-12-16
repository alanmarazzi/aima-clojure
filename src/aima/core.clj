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
    (assoc env :agents new-locations)))

(defn consume-thing!
  [env location kind]
  (let [things (list-things env location kind)]
    (when (not-empty things)
      (remove-thing! env (first things)))))

(defn eat!
  [env agent]
  (let [location (:location agent)]
    (consume-thing! env location :food)))

(defn drink!
  [env agent]
  (let [location (:location agent)]
    (consume-thing! env location :water)))

(defn execute!
  [env agent action]
  (case action
    :eat   (let [e (eat! env agent)]
             (println (:name agent) "ate" (:name e)
                      "at" (:location agent))
             e)
    :drink (println (:name agent) "drank" (:name (drink! env agent))
                    "at" (:location agent))
    (let [l (move-down! env agent)]
      (println (:name agent) "decided to"
               "move down at" (:location agent))
      l)))

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
    (swap! env step)))

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
  (swap! park add-thing water 1)

  (run-env park 10)
  @park)
