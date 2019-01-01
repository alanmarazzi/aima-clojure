(ns aima.agents-test
  (:require [aima.agents :refer :all]
            [clojure.test :refer :all]))

(deftest test-objects
  (are [n] (instance? clojure.lang.PersistentArrayMap n)
           (new-object :test)
           (new-agent nil :test))
  (is (= :test (:name (new-object :test))))
  (is (:alive (new-agent nil :test)))
  (is (= :test (:name (new-agent nil :test))))
  (is (function? (:program (new-agent #(println %) :test)))))

(deftest alive-test
  (are [p n] (alive? (new-agent p n))
             nil nil
             #(%) nil
             nil :myname
             :p :n))

(deftest new-environment-test
  (are [n d p e] (instance? clojure.lang.Atom (new-environment n d p e))
                 :test nil nil nil
                 nil #(%) nil nil
                 nil nil #(%) nil
                 nil nil nil #(%)
                 :test #(%) #(%) #(%))
  (are [n d p e] (instance? clojure.lang.PersistentHashMap
                            @(new-environment n d p e))
                 :test nil nil nil
                 nil #(%) nil nil
                 nil nil #(%) nil
                 nil nil nil #(%)
                 :test #(%) #(%) #(%))
  (is (= :test (:name @(new-environment :test))))
  (are [d p e] (map function?
                    (keep identity (select-keys
                                     @(new-environment :test d p e)
                                     [:done? :perceive :execute])))
               #(%) nil nil
               nil #(%) nil
               nil nil #(%)
               #(%) nil #(%)
               #(%) #(%) nil
               nil #(%) #(%)
               #(%) #(%) #(%)))

(deftest set-location-test
  (are [obj loc] (= loc (:location (set-location obj loc)))
                 {} 5
                 {} 0
                 {:whatever "str"} 3
                 {:location nil} 5
                 {:test 12} -1
                 {:doubles :a} 1.7
                 {} nil))

(defn mock-env
  [name & [done? perceive execute]]
  @(new-environment name done? perceive execute))

(deftest add-thing-test
  (let [env (mock-env :this)
        obj (new-object :nothing)
        agt (new-agent nil :generic)]
    (is (instance? clojure.lang.PersistentHashMap
                   (add-thing env obj 3)))
    (is (first (:things (add-thing env obj 3))))
    (is (first (:agents (add-thing env agt 2))))
    (is (= 2 (count (:things
                     (add-thing (add-thing env obj 3) obj 2)))))
    (is (= 2 (count (:agents
                     (add-thing (add-thing env agt 3) agt 2)))))
    (are [t loc]
        (= loc (:location
                          (first
                           ((:type t) (add-thing env t loc)))))
      obj 5
      agt 3
      obj nil
      agt nil
      obj 0
      agt 0
      obj -1
      agt -1
      obj 3.7
      agt 4.5)))

(deftest remove-thing-test
  (let [env       (mock-env :this)
        e         #(add-thing %1 %2 3)
        get-thing #(get-in %1 [%2 0])
        obj       (new-object :nothing)
        agt       (new-agent nil :generic)]
    (is (as-> (e env obj) nenv
            (remove-thing! nenv (get-thing nenv :things))
            (:things nenv)
            (empty? nenv)))
    (is (as-> (e env agt) nenv
            (remove-thing! nenv (get-thing nenv :agents))
            (:agents nenv)
            (empty? nenv)))
    (is (as-> (e env agt) nenv
            (e nenv obj)
            (remove-thing! nenv (get-thing nenv :things))
            (:agents nenv)
            (not-empty nenv)))
    (is (as-> (e env obj) nenv
            (e nenv (new-object :test))
            (remove-thing! nenv (get-thing nenv :things))
            (:things nenv)
            (count nenv)
            (= 1 nenv)))))

(deftest done-test
  (let [env (mock-env :test)
        agt (new-agent nil :generic)
        obj (new-object :nothing)
        nenv (-> (add-thing env agt 2)
                 (add-thing obj 3))]
    (is (done? nenv (fn [env]
                     (-> env :agents first alive? not))))
    (is (done? nenv
               (fn [env]
                 (-> env :agents first alive? not))
               (fn [env]
                 (-> env :things first :name (= :nothing)))))))

(deftest same-location-test
  (are [o l] (same-location? o l)
    {:location 0} 0
    {:location -1} -1
    {:location 1} 1))

(deftest same-name-test
  (are [o n] (same-name? o n)
    {:name "Al"} "Al"
    {:name :key} :key
    {:name 123} 123))

(deftest list-things-test
  (let [env (mock-env :test)
        agt (new-agent nil :generic)
        obj (new-object :nothing)]
    (is (-> (add-thing env obj 3)
            (list-things 3)
            count
            (= 1)))
    (is (-> (add-thing env obj 3)
            (add-thing agt 3)
            (list-things 3)
            count
            (= 1)))
    (is (-> (assoc env :things
                   (repeat 5 (assoc obj :location 5)))
            (list-things 5)
            count
            (= 5)))
    (is (-> (add-thing env
                       (assoc obj :name :something) 3)
            (update :things conj
                    (repeat 2 (assoc obj :location 3)))
            (update :things flatten)
            (list-things 3 :nothing)
            count
            (= 2)))))

(deftest get-agent-test
  (let [env   (mock-env :test)
        agt   (new-agent nil :generic)
        agt-2 (new-agent nil :trial)]
    (is (empty? (get-agent env)))
    (is (empty? (get-agent env :name)))
    (is (-> (add-thing env agt 3)
            (add-thing agt-2 5)
            (get-agent)
            count
            (= 2)))
    (is (-> (add-thing env agt 3)
            (add-thing agt-2 5)
            (get-agent :generic)
            (= (assoc agt :location 3))))
    (is (-> (add-thing env agt 3)
            (add-thing agt-2 5)
            (get-agent :trial)
            first
            :name
            (= :trial)))))

(deftest location-empty-test
  (let [env  (mock-env :test)
        agt  (new-agent nil :generic)
        obj  (new-object :trial)
        nenv (-> (add-thing env agt 3)
                 (add-thing obj 5))]
    (is (location-empty? nenv 1))
    (is (false? (location-empty? nenv 5)))
    (is (location-empty? nenv 3))))

(deftest perceive-&-run-test
  (let [per (fn [e a]
              (list-things e (:location a)))
        env (mock-env :test nil per)
        agt #(assoc (new-agent %1 :generic) :location %2)]
    (is (empty? (perceive-&-run env (agt identity 1))))
    ))
