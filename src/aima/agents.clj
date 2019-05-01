(ns aima.agents
  (:require [clojure.pprint :refer [pprint]]
            [clojure.data :as d]))

(defn new-object
  "A `thing`: can represent any inanimate object.
  To personalize it just `assoc` the resulting map"
  [name]
  {:alive    false
   :name     name
   :location nil
   :type     :things})

(defn new-agent
  "Creates a new agent"
  [program name]
  {:alive       true
   :name        name
   :bump        false
   :holding     []
   :performance 0
   :program     program
   :location    nil
   :type        :agents})

(defn new-watcher
  "Attach a watcher to an environment.
  Good for debugging & teaching purposes"
  [a k h]
  (add-watch a k
             (fn [k a o n]
               (pprint "=== CHANGE ===")
               (pprint (str "*** " k " ***"))
               (newline)
               (comment (pprint "OLD")
                        (pprint o)
                        (newline))
               (pprint "NEW")
               (pprint n)
               (newline)
               (pprint "DIFF")
               (pprint (first (d/diff n o)))
               (newline)
               (swap! h conj (first (d/diff n o))))))

(defn alive?
  "Is this thing alive?"
  [obj]
  (:alive obj))

(defn new-environment
  "Create a new environment to play with. `:name` is required,
  `:done?` can be added later and it holds a function to determine
  whether there's anything else to do in the environment, `:perceive`
  holds the function to let agents perceive their environment,
  `:execute` holds the function to let agents decide what to do
  considering their percepts"
  [name & [done? perceive execute]]
  (atom {:name   name
         :things []
         :agents []
         :step 0
         :max-steps 1000
         :done? done?
         :perceive perceive
         :execute execute
         :finished false}))

(defn perceive-&-run
  "Generic function to extract the program from an agent and if it is
  still alive run the program over percepts"
  [env agent]
  (let [f       (:program agent)
        percept (:perceive env)]
    (when (alive? agent)
      (map f (percept env agent)))))

(defn step
  ""
  [env]
  (let [done    (:done? env)
        execute (:execute env)
        agents  (:agents env)]
    (when-not (done env)
      (for [ag agents]
        (let [actions  (perceive-&-run env ag)
              executed (map #(execute env ag %) actions)]
          executed)))))

(defn stepper
  [env]
  (let [new-env (first (step env))]
    (if new-env
      (update new-env :step inc)
      (update env :step inc))))

(defn same-location?
  "Check whether a thing is at `location`"
  [obj location]
  (= (:location obj)
     location))

(defn same-name?
  "Does the obj have this name?"
  [obj name-compare]
  (= (:name obj)
     name-compare))

(defn list-things
  "Returns a sequence of things at the given location. If a kind
  is passed as an argument returns only things of that kind"
  ([env location]
   (let [things (:things env)]
     (filter #(same-location? % location) things)))
  ([env location kind]
   (let [things (list-things env location)]
     (filter #(same-name? % kind) things))))

(defn get-agent
  "Extract agents from the given `env`. If given a `name` returns
  only the agents with that `name`"
  ([env]
   (:agents env))
  ([env name]
   (first (filter #(same-name? % name) (get-agent env)))))

(defn location-empty?
  "Is this location empty?"
  [env location]
  (empty? (list-things env location)))

(defn set-location
  "Place `obj` (can be whatever) at `location`"
  [obj location]
  (assoc obj :location location))

(defn add-thing
  "Add `thing` to the proper `env` slot at `location`"
  [env thing location]
  (let [t (:type thing)]
    (update env t conj (set-location thing location))))

(defn remove-thing!
  "Remove a `thing` from the given `env`"
  [env thing]
  (let [t (:type thing)]
    (assoc env t (filterv (complement #{thing}) (t env)))))

(defn done?
  "Tests a sequence of `preds` that take an `env` as argument"
  [env & preds]
  (some false?
        (for [p preds]
          (p env))))
