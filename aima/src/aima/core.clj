(ns aima.core
  (:require [aima.agents :refer :all :reload true]))

; Blind dog example
(def food (new-object :food))

(def water (new-object :water))

(defn percept
  [env agent]
  (list-things env (:location @agent)))

(defn move-down
  [agent]
  (swap! agent update :location inc))

(defn consume-thing
  [env agent kind]
  (let [things (list-things env (:location @agent) kind)]
    (when (not-empty things)
      (remove-thing env (first things))
      things)))

(defn eat
  [env agent]
  (consume-thing env agent "food"))

(defn drink
  [env agent]
  (consume-thing env agent "water"))

(defn execute
  [env agent action]
  (case action
    nil (do (println (:name @agent) "decided to"
                     action "at" (:location @agent))
            (move-down agent))
    :eat (println (:name @agent) "ate" (first (eat env agent))
                       "at" (:location @agent))
    :drink (println (:name @agent) "drank" (first (drink env agent))
                           "at" (:location @agent))))

(defn check-alive?
  [env]
  (not (some alive? (:agents @env))))

(defn check-edible?
  [env]
  (not (some #{"food" "water"} (:things @env))))

(defn program
  [percepts]
  (let [mapping {:water :drink
                 :food :eat}]
    (println percepts)
    (map #(mapping (:name %)) percepts)))

(defn up-env
  []
  (def dog
    (new-agent program :blind-dog))

  (def park
    (new-environment))

  (add-thing park dog 5)
  (add-thing park food 5)
  (add-thing park water 7))

(dorun 5 (step park
               (done? park check-alive? check-edible?)
               (execute park dog :eat)))
