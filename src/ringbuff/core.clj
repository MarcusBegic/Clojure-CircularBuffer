
(ns ringbuff.core
  (:gen-class)
  (:require [clojure.core
             :as core
             :refer aset])
  (:import java.util.ArrayList)
  )

(defprotocol Buffer
  (take! [buf] "return next item from buffer, called under chan mutex")
  (adda [buf i])
  )

(deftype ringbuffer [size
                     ^{:volatile-mutable true} n
                     ^{:volatile-mutable true} start
                     ^{:volatile-mutable true} end
                     ^{:volatile-mutable true} ^ArrayList buf
                     ^{:volatile-mutable true} capacity]
  Buffer
  (take!
    [this]
      (if (> start n)
          (let [return-val (aget buf 0)]
            (set! start 0)
            (aset buf start nil)
            (set! start (inc start))
            (set! capacity (inc capacity))
            return-val)
          (let [return-val (aget buf start)]
            (set! start (inc start))
            (set! capacity (inc capacity))
            (return-val)
            )
         )
      )

  (adda
    [this e]
    (let [wrapper-index (fn [x] (let [m (mod x n)]
                          (if (< m 0) (+ m n) m)))]
      (if (and (not= capacity 0) (not= start (inc end)))
        (if (> end n)
            (do
              (set! end 0)
              (aset buf end e)
              (set! end (inc end))
              (set! capacity (dec capacity)))
            (do
              (aset buf end e)
              (set! end (inc end))
              (set! capacity (dec capacity)))
              )
        (do
          (let [new-buf (java.util.ArrayList. (range (* 2 n)))]
            (doseq [i (+ start (range n))]
              (aset new-buf (- i start) (aget buf (wrapper-index i))))
            (aset new-buf n e)
            (set! buf new-buf)
            )
          (set! start 0)
          (set! end n)
          (set! capacity (dec n))
          (set! n (* 2 n))
              )
            )
          )
        )
      )

(defn make-ringbuffer [size]
  (let [^{:volatile-mutable true} n size
        ^{:volatile-mutable true} start 0
        ^{:volatile-mutable true} end 0
        ^{:volatile-mutable true} ^ArrayList buf (java.util.ArrayList. (range n))
        ^{:volatile-mutable true} capacity (count buf)]
    (ringbuffer. size n start end buf capacity))
  )

(defn make-arrau [n]
  (java.util.ArrayList. (range n))
  )

(defn -main []
  (let [buf (make-ringbuffer 10)]
    (doseq [i (range 4)]
      (adda buf i)
      )
    (doseq [i (range 4)]
      (println (take! buf))
      )
  )
)
