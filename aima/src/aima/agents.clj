(ns aima.agents)

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
  [a k h]
  (add-watch a k
             (fn [k a o n]
               (println "=== CHANGE ===")
               (println (str k ": "))
               (println o)
               (println n)
               (newline)
               (swap! h update k conj o))))

(defn alive?
  [obj]
  (:alive obj))

(defn new-environment
  [name & [done? perceive execute]]
  (atom {:name   name
         :things []
         :agents []
         :step 0
         :max-steps 1000
         :done? done?
         :perceive perceive
         :execute execute}))

(defn perceive-&-run
  [env percept agent]
  (let [f (:program agent)]
    (when (alive? agent)
      (map f (percept env agent)))))

(defn step
  [env]
  (let [done     (:done? @env)
        perceive (:perceive @env)
        execute  (:execute @env)
        agents   (:agents @env)]
    (when-not (done env)
      (let [actions (mapcat #(perceive-&-run env perceive %) agents)]
        (if (seq actions)
          (for [ac actions
                ag agents]
            (execute env ag ac))
          (mapcat #(execute env % nil) agents))))
    (swap! env update :step inc)))

(defn same-location?
  [obj location]
  (= (:location obj)
     location))

(defn same-name?
  [obj name-compare]
  (= (:name obj)
     name-compare))

(defn list-things
  ([env location]
   (let [things (:things @env)]
     (filter #(same-location? % location) things)))
  ([env location kind]
   (let [things (list-things env location)]
     (filter (fn [thing]
               (= (:name thing) kind)) things))))

(defn get-agent
  ([env]
   (:agents @env))
  ([env name]
   (filter (fn [ag]
             (= (:name ag) name)) (:agents @env))))

(defn location-empty?
  [env location]
  (empty? (list-things env location)))

(defn set-location
  [obj location]
  (assoc obj :location location))

(defn add-thing
  [env thing location]
  (let [t (:type thing)]
    (swap! env update t conj
           (set-location thing location))))

(defn remove-thing
  [env thing]
  (let [t (:type thing)]
    (swap! env assoc t (filterv (complement #{thing}) (t @env)))))

(defn done?
  [env & preds]
  (some false?
        (for [p preds]
          (p env))))

(defn get-agent
  [env name]
  (first (filter #(= name (:name %)) (:agents @env))))
