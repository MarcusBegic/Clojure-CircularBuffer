
(ns ringbuff.core
  (:gen-class)
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
          (let [return-val (.get buf 0)]
            (set! start 0)
            (.add buf start nil)
            (set! start (inc start))
            (set! capacity (inc capacity))
            return-val)
          (let [return-val (.get buf start)]
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
              (.add buf end e)
              (set! end (inc end))
              (set! capacity (dec capacity)))
            (do
              (.add buf end e)
              (set! end (inc end))
              (set! capacity (dec capacity)))
              )
        (do
          (let [new-buf (java.util.ArrayList. (range (* 2 n)))]
            (doseq [i (+ start (range n))]
              (.add new-buf (- i start) (.get buf (wrapper-index i))))
            (.add new-buf n e)
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




;; (defn make-ringbuffer [size]
;;   (let [ n size
;;          start 0
;;          end 0
;;         ^ArrayList buf (java.util.ArrayList. (range n))
;;          capacity n]
;;     (ringbuffer. size n start end buf capacity))
;;   )


(defn make-ringbuffer [size]
  (let [^{:volatile-mutable true} n size
        ^{:volatile-mutable true} start 0
        ^{:volatile-mutable true} end 0
        ^{:volatile-mutable true} buf (java.util.ArrayList. (range n))
        ^{:volatile-mutable true} capacity n]
    (ringbuffer. size n start end buf capacity)
   ) 
  )

(defn king [n]
  (let [ ^{:volatile-mutable true} capacity (atom n)
        ^{:volatile-mutable true} alist (java.util.ArrayList. (range n))]
    (.add alist 3 22222222)
    (swap! capacity inc )
    capacity
    )
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
