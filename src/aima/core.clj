(ns aima.core
  (:require [aima.agents :as ag :reload true]))

; Blind dog example
(def food (ag/new-object :food))

(def water (ag/new-object :water))

(defn percept
  [env agent]
  (ag/list-things env (:location agent)))

(defn move-down!
  [env agent]
  (let [ag-name       (:name agent)
        new-agent     (update agent :location inc)
        dropped       (filterv 
                       #(not (ag/same-name? % ag-name)) 
                       (ag/get-agent env))
        new-locations (conj dropped new-agent)]
    (println "Agent" ag-name "is moving down")
    (assoc env :agents new-locations)))

(defn consume-thing!
  [env location kind]
  (let [things (ag/list-things env location kind)]
    (when (not-empty things)
      (ag/remove-thing! env (first things)))))

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
  (some ag/alive? (ag/get-agent env)))

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
  (doseq [_ (range steps)]
    (swap! env ag/stepper)))

(comment
  (def dog
    (ag/new-agent program :blind-dog))

  (def park
    (ag/new-environment :park
                        (fn [env] 
                          (ag/done? env alive-left? edible-left?))
                        percept
                        execute!))

  (swap! park ag/add-thing dog 3)
  (swap! park ag/add-thing food 3)
  (swap! park ag/add-thing water 5)

  (def watcher (atom []))

  (new-watcher park :Watchdog watcher)

  (run-env park 1)
  @park)
