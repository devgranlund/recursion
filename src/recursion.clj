(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (if (> (count coll) 1)
      (* (first coll) (product (rest coll)))
      (first coll)
      )
  ))

(defn singleton? [coll]
  (== 1 (count coll))
  )

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll))
    )
  )
  )

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))
  )
  )

(defn seq-max [seq-1 seq-2]
  (if (< (count seq-1) (count seq-2))
    seq-2
    (if (< (count seq-2) (count seq-1))
      seq-1
      (if (<= (compare seq-1 seq-2) 0)
        seq-2
        seq-1
        )
  )))


(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (< (count a-seq) 2)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq)))
      )
  )
)

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq))
      )
  ))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (not (= elem (first a-seq)))
      (sequence-contains? elem (rest a-seq))
    :else
      true
    ))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
      a-seq
  (let [firzt (first a-seq)
          rezt (rest a-seq)]
    (if (empty? rezt)
      (if (pred? firzt) (list firzt) '())
    (if (pred? firzt)
      (if (pred? (first rezt))
        (cons firzt (my-take-while pred? rezt))
        (list firzt))
      '())))))

; Drop elements until pred? returns false
(defn my-drop-while [pred? a-seq]
   (let [firzt (first a-seq)
          rezt (rest a-seq)]
     (cond
       (empty? a-seq)
         a-seq
       (not (pred? firzt))
         a-seq
       (empty? rezt)
         rezt
       :else
         (my-drop-while pred? rezt)
       )))

(defn seq= [a-seq b-seq]
  (cond
    (or
      (and (empty? a-seq) (not (empty? b-seq)))
      (and (empty? b-seq) (not (empty? a-seq)))) false
    (and (empty? a-seq) (empty? b-seq)) true
    (== (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    (not (== (first a-seq) (first b-seq)))
      false
    ))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
  ))

(defn power [n k]
  :-)

(defn fib [n]
  :-)

(defn my-repeat [how-many-times what-to-repeat]
  [:-])

(defn my-range [up-to]
  [:-])

(defn tails [a-seq]
  [:-])

(defn inits [a-seq]
  [:-])

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

