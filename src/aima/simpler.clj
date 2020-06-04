(ns aima.simpler)

(defn object
  "A `thing`: can represent any inanimate object.
  To personalize it just `assoc` the resulting map"
  [name]
  {:alive    false
   :name     name
   :location nil
   :type     :things})

(defn agente
  "Creates a new agent"
  [& {:keys [program name]
      :or   {program identity
             name    (java.util.UUID/randomUUID)}}]
  {:alive       true
   :name        name
   :bump        false
   :holding     []
   :performance 0
   :program     program
   :location    nil
   :type        :agents})

(defn agente
  [name]
  {:alive       true
   :name        name
   :bump        false
   :holding     []
   :performance 0
   :location    nil
   :type        :agents})

(defn environment
  "Create a new environment to play with. `:name` is required,
  `:done?` can be added later and it holds a function to determine
  whether there's anything else to do in the environment, `:perceive`
  holds the function to let agents perceive their environment,
  `:execute` holds the function to let agents decide what to do
  considering their percepts"
  [name & {:keys [done? perceive execute]}]
  (atom {:name      name
         :things    []
         :agents    []
         :step      0
         :max-steps 1000
         :done?     done?
         :perceive  perceive
         :execute   execute
         :finished  false}))

(defn environment
  [name]
  (atom {:name      name
         :things    []
         :agents    []
         :step      0
         :max-steps 1000
         :finished  false}))

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

(defn perception
  [f]
  (fn [env ag]
    (let [loc (:location ag)]
      (apply f [env loc]))))

(defn program
  [ag percept]
  (case (:name ag)
    :cane ({:that :action :this :nothing} (:name percept))))

(defn lister
  [ag percepts]
  (for [a ag]
    {(keyword (name (:name a))
              (name (program a (first percepts)))) percepts}))

(defn run
  [env ag]
  (when (:alive ag)
    (->> ((perception list-things) env (:location ag))
         (mapv (partial program ag))
         )))

(defn runner
  [env perc]
  (fn [e ag]
    (let [p        (perception perc)
          percepts (p e ag)]
      )))

(defn execute
  [env actions]
  (let [[ag acts] actions]
    (when-not (empty? acts)
      (for [a acts]
        ((:execute env) env ag a)))))

(defn execute
  [env actions]
  (let [[ag acts] actions]
    (loop [a (first acts)
           r (rest acts)
           e env]
      (if a
        (recur (first r) (rest r) ((:execute e) e ag a))
        e))))

(defn perceive-&-run
  "Generic function to extract the program from an agent and if it is
  still alive run the program over percepts"
  [env]
  (let [ag (:agents env)]
    (into []
          (comp
           (map (partial run env))
           (map (partial execute env)))
          ag)))

; I'm trying to decompose the `step` action
(defn run-step
  [env]
  (let [{:keys [agents things step]} @env
        acts (perceive-&-run @env)]
    ()))

(defn program
  [percepts]
  :action)

(defn execute-ag
  [env ag ac]
  (update env :step inc))

(do
  (def env (environment :prova))

  (def cane {:alive true,
             :name :cane,
             :bump false,
             :holding [],
             :performance 0,
             :location 2
             :type :agents})
  (swap! env assoc :agents [cane])
  ;(swap! env update :agents #(conj % cane))
  (swap! env assoc :things [(assoc (object :that) :location 2)])
  (swap! env update :things #(conj % (assoc (object :this) :location 2)))
  )

(perceive-&-run @env)
