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
  (are [n d p e] (instance? clojure.lang.PersistentArrayMap
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

(deftest add-thing-test
  (let [env #(new-environment :this)
        obj (new-object :nothing)
        agt (new-agent nil :generic)]
    (is (instance? clojure.lang.PersistentArrayMap
                   (add-thing (env) obj 3)))
    (is (first (:things (add-thing (env) obj 3))))
    (is (first (:agents (add-thing (env) agt 2))))
    (is (let [e (env)]
          (= 2 (count (:things
                        (do (add-thing e obj 2)
                            (add-thing e obj 3)))))))
    (is (let [e (env)]
          (= 2 (count (:agents
                        (do (add-thing e agt 2)
                            (add-thing e agt 3)))))))
    (are [t loc] (= loc (:location
                          (first
                             ((:type t) (add-thing (env) t loc)))))
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
  (let [env       (new-environment :this)
        e         #(add-thing env %1 3)
        get-thing #(get-in %1 [%2 0])
        obj       (new-object :nothing)
        agt       (new-agent nil :generic)]
    (do (e obj)
        (is (empty? (:things
                      (remove-thing env (get-thing @env :things))))))
    (do (e agt)
        (is (empty? (:agents
                      (remove-thing env (get-thing @env :agents))))))
    (do (e agt)
        (e obj)
        (is (not-empty (:agents
                         (remove-thing env (get-thing @env :things))))))
    (do (e obj)
        (is (empty? (:things
                      (remove-thing env (get-thing @env :things))))))
    (do (e obj)
        (e (new-object :test))
        (is (= 1 (count (:things
                          (remove-thing
                             env
                             (get-thing @env :things)))))))))

(deftest done-test
  (let [env (new-environment :test)
        agt (new-agent nil :generic)
        obj (new-object :nothing)
        _   (add-thing env agt 2)
        _   (add-thing env obj 3)]
    (is (done? env (fn [env]
                     (-> @env :agents first alive? not))))
    (is (done? env
               (fn [env]
                 (-> @env :agents first alive? not))
               (fn [env]
                 (-> @env :things first :name (= :nothing)))))))

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
  (let [env #(new-environment :test)
        agt (new-agent nil :generic)
        obj (new-object :nothing)]
    (is (let [e (env)
              _ (add-thing e obj 3)]
          (= 1 (count (list-things e 3)))))
    (is (let [e (env)
              _ (add-thing e obj 3)
              _ (add-thing e agt 3)]
          (= 1 (count (list-things e 3)))))
    (is (let [e (env)
              _ (doall (repeatedly 5 #(add-thing e obj 5)))]
          (= 5 (count (list-things e 5)))))
    (is (let [e (env)
              _ (doall (do (add-thing e (assoc obj :name :something) 3)
                           (repeatedly 2 #(add-thing e obj 3))))]
          (= 2 (count (list-things e 3 :nothing)))))))

(deftest get-agent-test
  (let [env   (new-environment :test)
        agt   (new-agent nil :generic)
        agt-2 (new-agent nil :trial)]
    (is (empty? (get-agent env)))
    (is (empty? (get-agent env :name)))
    (is (do (add-thing env agt 3)
            (add-thing env agt-2 5)
            (= 2 (count (get-agent @env)))))
    (is (= 1 (count (get-agent @env :generic))))
    (is (= :trial (:name (first (get-agent @env :trial)))))))

(deftest location-empty-test
  (let [env (new-environment :test)
        agt (new-agent nil :generic)
        obj (new-object :trial)
        _   (do (add-thing env agt 3)
                (add-thing env obj 5))]
    (is (location-empty? env 1))
    (is (false? (location-empty? env 5)))
    (is (location-empty? env 3))))

(deftest perceive-&-run-test
  (let [per (fn [e a]
              (list-things e (:location a)))
        env (new-environment :test)
        agt #(new-agent)]))
