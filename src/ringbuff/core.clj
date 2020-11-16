(ns ringbuff.core
  (:gen-class)
(:import java.util.ArrayList)
  )

(deftype RingBuffer [n]
  (let [^{:volatile-mutable true} start 0  ^{:volatile-mutable true} end 0
        ^{:volatile-mutable true} capacity (- 1 (.size buf))
        ^ArrayList buf (.ArrayList n)]
  (pop
    (let [result (.get buf start)]
      (set! start (+ 1 start))
      result)
    )

  (insert []
          )

  )
)

(defn -main []
  )
