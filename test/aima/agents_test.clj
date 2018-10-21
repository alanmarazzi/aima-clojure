(ns aima.agents-test
  (:require [aima.agents :refer :all]
            [clojure.test :refer :all]))

(deftest test-objects
  (are [n] (instance? clojure.lang.PersistentArrayMap n)
    (new-object :test)
    (new-agent  nil :test))
  (is (= :test (:name  (new-object :test))))
  (is (:alive (new-agent nil :test)))
  (is (= :test (:name  (new-agent nil :test))))
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

(deftest perceive-&-run-test
  )

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
  (let [env #(new-environment :this)
        e   #(add-thing (env) %1 3)
        obj (new-object :nothing)
        agt (new-agent nil :generic)]
    (is (empty? (:things (e obj))))))
