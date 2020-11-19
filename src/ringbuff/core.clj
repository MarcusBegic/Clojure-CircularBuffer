
(ns ringbuff.core
  (:gen-class)
  (:import java.util.ArrayList)
  )

(defprotocol Buffer
  (take! [buf] "return next item from buffer, called under chan mutex")
  (add! [buf i])
  (peep [buf i])
  )

(deftype ringbuffer [size
                     ^{:volatile-mutable true} n
                     ^{:volatile-mutable true} start
                     ^{:volatile-mutable true} end
                     ^{:volatile-mutable true} ^ArrayList buf
                     ^{:volatile-mutable true} capacity
                     wrapper-index]
  Buffer
    (take!
    [this]
      (if (= (inc @start) @n)
          (let [return-val (.get @buf 0)]
            (.set @buf @start nil)
            (vreset! start 0)
            (vswap! capacity inc)
            return-val)
          (let [return-val (.get @buf @start)]
            (.set @buf @start nil)
            (vswap! start inc)
            (vswap! capacity inc)
            return-val)
            )
         )

  (add!
    [this e]
      (if (= @capacity @n)
        (do (.set @buf 0 e)
        (vswap! capacity dec))
        (if (and (not= @capacity 0) (not= @start (inc @end)))
          (if (= (inc @end) @n)
              (do
                (vreset! end 0)
                (.set @buf @end e)
                (vswap! capacity dec)
                )
              (do
                (.set @buf (inc @end) e)
                (vswap! end inc)
                (vswap! capacity dec)
                ))
          (do
            (let [new-buf (java.util.ArrayList. (range (* 2 @n)))]
              (doseq [i (range @n)]
                (.set new-buf i (.get @buf (wrapper-index (+ @start i)))))
              (.set new-buf @n e)
              (vreset! buf new-buf)
              )
            (vreset! start 0)
            (vreset! end @n)
            (vreset! capacity @n)
            (vswap! capacity dec)
            (vswap! n * 2))
                )
              )
            )
    (peep
      [this i]
      (.get @buf (wrapper-index (+ @start i)))
     )
    )

(defn make-ringbuffer [size]
  (let [^{:volatile-mutable true} n (volatile! size)
        ^{:volatile-mutable true} start (volatile! 0)
        ^{:volatile-mutable true} end (volatile! 0)
        ^{:volatile-mutable true} buf (volatile! (java.util.ArrayList. (range size)))
        ^{:volatile-mutable true} capacity (volatile! size)
        wrapper-index (fn [x] (let [m (mod x @n)]
                                     (if (< m 0) (+ m @n) m)))]
    (ringbuffer. size n start end buf capacity wrapper-index)
   )
  )


(defn -main []
  (let [buf (make-ringbuffer 3)]
    (add! buf 111)
    (add! buf 222)
    (add! buf 333)
    (println (peep buf 1))
  )
)
