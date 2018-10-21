(ns aima.core
  (:require [aima.agents :refer :all :reload true]))

; Blind dog example
(def food (new-object :food))

(def water (new-object :water))

(defn percept
  [env agent]
  (list-things env (:location agent)))

(defn move-down
  [env agent]
  (let [ag-name (:name agent)
        new-agent (update agent :location inc)
        dropped (filterv #(not (same-name? % ag-name)) (:agents @env))
        new-locations (conj dropped new-agent)]
    (swap! env assoc :agents new-locations)))

(defn consume-thing
  [env location kind]
  (let [things (list-things env location kind)]
    (when (not-empty things)
      (remove-thing env (first things))
      (first things))))

(defn eat
  [env agent]
  (let [location (:location agent)]
    (consume-thing env location :food)))

(defn drink
  [env agent]
  (let [location (:location agent)]
    (consume-thing env location :water)))

(defn execute
  [env agent action]
  (case action
    :eat (println (:name agent) "ate" (:name (eat env agent))
                  "at" (:location agent))
    :drink (println (:name agent) "drank" (:name (drink env agent))
                    "at" (:location agent))
    (do (println (:name agent) "decided to"
                 "move down" "at" (:location agent))
        (move-down env agent))))

(defn alive-left?
  [env]
  (some alive? (:agents @env)))

(defn edible-left?
  [env]
  (boolean (some #{:food :water} (map :name (:things @env)))))

(defn program
  [percepts]
  (let [mapping {:water :drink
                 :food  :eat}]
    (mapping (:name percepts))))

(defn run-env
  [env steps]
  (doseq [a (range steps)]
    (step env)))

(defn up-and-run
  []
  (def dog
    (new-agent program :blind-dog))

  (def park
    (new-environment :park
                     (fn [env] (done? env alive-left? edible-left?))
                     percept
                     execute))

  (add-thing park dog 1)
  (add-thing park food 5)
  (add-thing park water 7)

  (run-env park 10)
  @park)
