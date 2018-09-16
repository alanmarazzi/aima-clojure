(ns aima.agents)

(defn new-object
  "A `thing`: can represent any inanimate object.
  To personalize it `swap!` and `assoc` the atom"
  [& [name]]
  (atom {:alive false
         :name name
         :location nil
         :type :things}))

(defn new-agent
  "Creates a new agent"
  [program & [name]]
  (atom {:alive true
         :name name
         :bump false
         :holding []
         :performance 0
         :program program
         :location nil
         :type :agents}))

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
  (:alive @obj))

(defn new-environment
  []
  (atom {:things []
         :agents []}))

(defn perceive-&-run
  [agent percept]
  (let [f (:program @agent)]
    (when (alive? agent)
      (f (percept agent)))))

(defn step
  [env done? execute]
  (when-not (done? env)
    (let [actions (map perceive-&-run (:agents @env))]
      (map execute actions))))

(defn same-location?
  [obj location]
  (= (:location @obj)
     location))

(defn same-name?
  [obj name-compare]
  (= (:name @obj)
     name-compare))

; I don't care for types at the moment
(defn list-things
  ([env location]
   (let [things (:things @env)]
     (filter #(same-location? % location) things)))
  ([env location kind]
   (let [things (list-things env location)]
     (filter (fn [x]
               (= (:name @x) kind) things)))))

(defn location-empty?
  [env location]
  (empty? (list-things env location)))

(defn set-location
  [obj location]
  (swap! obj assoc :location location))

(defn add-thing
  [env thing location]
  (let [t (:type @thing)]
    (swap! thing assoc :location location)
    (swap! env update t conj thing)))

(defn remove-thing
  [env thing]
  (let [t (:type @thing)]
    (swap! env #(remove #{thing} (t %)))))

(defn done?
  [env & preds]
  ((apply every-pred preds) @env))
