(ns app.core
  (:require [clojure.zip :as zip])
  (:use [clojure.walk]))

; protected divison
(defn pro-div
  [x y]
  (if (zero? y)
  0
  (/ x y)))

; target output
(def target-output 191)

; terminals
(def terminals [7 3 1 2 20 100])

; functions and their arities
(def functions (zipmap '(+ - * pro-div)
                   '(2 2 2 2)))

; function that returns random terminal
(defn random-terminal
  []
  (rand-nth terminals))

; function that returns random function
(defn random-function
  []
  (rand-nth (keys functions)))

; function that creates a random expression
(defn random-expression
  [depth]
  (if (or (zero? depth)
          (zero? (rand-int 2)))
    (random-terminal)
    (let [f (random-function)]
      (cons f (repeatedly (get functions f)
                          #(random-expression (dec depth)))))))

; evaluate individual program tree
(defn evalute
  [tree]
  (eval tree))

; calculate error of the individual program tree
(defn error
  [tree]
  (Math/abs (- (evalute tree) target-output)))

; utility function for calculating the size of the code tree
(defn code-size [c]
  (if (seq? c)
    (count (flatten c))
    1))

; returns a subtree from a given tree at index in depth-first traversal
(defn tree-at-index
  [tree index]
  (let [index (mod (Math/abs index) (code-size tree))
        zipper (zip/seq-zip tree)]
    (loop [zip zipper i index]
      (if (zero? i)
        (zip/node zip)
        (if (seq? (zip/node zip))
          (recur (zip/next (zip/next zip)) (dec i))
          (recur (zip/next zip) (dec i)))))))

; returns a copy of a tree with a subtree inserted at index in depth-first traversal
(defn insert-at-index
  [tree index subtree]
  (let [index (mod (Math/abs index) (code-size tree))
        zipper (zip/seq-zip tree)]
    (loop [zip zipper i index]
      (if (zero? i)
        (zip/root (zip/replace zip subtree))
        (if (seq? (zip/node zip))
          (recur (zip/next (zip/next zip)) (dec i))
          (recur (zip/next zip) (dec i)))))))

; mutate the tree
(defn mutate
  [tree]
  (insert-at-index tree
                   (rand-int (code-size tree))
                   (random-expression 2)))

; crossover between to trees
(defn crossover
  [tree1 tree2]
  (insert-at-index tree1
                   (rand-int (code-size tree1))
                   (tree-at-index tree2 (rand-int (code-size tree2)))))

; sort population of programs by error value
(defn sort-by-error
  [population]
  (vec (map second
            (sort (fn [[err1 ind1] [err2 ind2]] (< err1 err2))
                  (map #(vector (error %) %) population)))))

; select the best individual by tournament selection
(defn select
  [population tournament-size]
  (let [pop-size (count population)]
    (nth (sort-by-error population)
         (apply min (repeatedly tournament-size #(rand-int pop-size))))))

; starts the evolution by producing a random population, sorting and evaluating
; individuals and breeding new generations
(defn start-evolution
  [population-size]
  (println "The evolution has started ...")
  (loop [generation 0
         population ()]))
