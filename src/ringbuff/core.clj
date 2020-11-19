
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
      (if (> @start @n)
          (let [return-val (.get @buf 0)]
            (vreset! start 0)
            (.add @buf @start nil)
            (vswap! start inc)
            (vswap! capacity inc)
            return-val)
          (let [return-val (.get @buf @start)]
            (vswap! start inc)
            (vswap! capacity inc)
            return-val)
            )
         )

  (adda
    [this e]
    (let [wrapper-index (fn [x] (let [m (mod x @n)]
                          (if (< m 0) (+ m @n) m)))]
      (if (and (not= @capacity 0) (not= @start (inc @end)))
        (if (> @end @n)
            (do
              (vreset! end 0)
              (.add @buf @end e)
              (vswap! end inc)
              (vswap! capacity dec)
              )
            (do
              (.add @buf @end e)
              (vswap! end inc )
              (vswap! capacity dec )
              ))
        (do
          (let [new-buf (java.util.ArrayList. (range (* 2 @n)))]
            (doseq [i (range @n)]
              (.add new-buf i (.get @buf (wrapper-index (+ @start i)))))
            (.add new-buf @n e)
            (vreset! buf new-buf)
            )
          (vreset! start 0)
          (vreset! end @n)
          (vswap! capacity dec)
          (vswap! n * 2))
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
  (let [^{:volatile-mutable true} n (volatile! size)
        ^{:volatile-mutable true} start (volatile! 0)
        ^{:volatile-mutable true} end (volatile! 0)
        ^{:volatile-mutable true} buf (volatile! (java.util.ArrayList. (range n)))
        ^{:volatile-mutable true} capacity (volatile! size)]
    (ringbuffer. size n start end buf capacity)
   )
  )


(defn printboi [^{:volatile-mutable true} capacity]
  (vswap! capacity * 2)
  )

(defn king [n]
  (let [ ^{:volatile-mutable true} capacity  (volatile! n)
        ^{:volatile-mutable true} alist (volatile! (java.util.ArrayList. (range n)))]
    (.add @alist 3 22222222)
    ;; (let [newlist (java.util.ArrayList. (range (* 2 n)))]
    ;;   (vreset! alist newlist)
    ;;   alist
    ;; )
    (printboi capacity)
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
