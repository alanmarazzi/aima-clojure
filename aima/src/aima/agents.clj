(ns aima.agents)

(defn new-object
  "A `thing`: can represent any inanimate object.
  To personalize it `swap!` and `assoc` the atom"
  [& [name]]
  (atom {:alive false
         :name name}))

(defn new-agent
  "Creates a new agent"
  [program & [name]]
  (atom {:alive true
         :name name
         :bump false
         :holding []
         :performance 0
         :program program}))


