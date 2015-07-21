(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [nbr n ret]
                 (if (zero? n)
                   ret
                   (recur nbr (dec n) (* nbr ret))))]
    (if (zero? base)
      0
      (if (< exp 1)
        1
        (helper base (dec exp) base)))))

(defn last-element [a-seq]
  (let [helper (fn [seq-1 len]
                 (if (== len 1)
                   (first seq-1)
                   (recur (rest seq-1) (dec len))))]
    (if (= (count a-seq) 0)
      nil
      (helper a-seq (count a-seq)))))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq-1 seq-2 left]
                 (if (zero? left)
                   true
                   (if (not (= (first seq-1) (first seq-2)))
                     false
                     (recur (rest seq-1) (rest seq-2) (dec left)))))]
    (if (= (count seq1) (count seq2))
      (helper seq1 seq2 (count seq1))
      false)))

(defn find-first-index [pred a-seq]
  (loop [f pred
         seq-1 a-seq
         n 0]
    (if (empty? seq-1)
      nil
      (if (f (first seq-1))
        n
        (recur f (rest seq-1) (inc n))))))

(defn avg [a-seq]
  (loop [seq-1 a-seq
         len (count a-seq)
         n 0]
    (if (empty? seq-1)
      (/ n len)
      (recur (rest seq-1) len (+ n (first seq-1))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [seq-1 a-seq
         set-1 #{}]
    (if (empty? seq-1)
      set-1
      (recur (rest seq-1) (toggle set-1 (first seq-1))))))

(defn fast-fibo [n]
  (loop [x-1 0
         x 1
         cntr n]
    (if (zero? cntr)
      x-1
      (recur x (+ x-1 x) (dec cntr)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn cut-at-repetition [a-seq]
  (loop [seq-1 a-seq
         ret []]
    (if (empty? seq-1)
      ret
      (if (sequence-contains? (first seq-1) ret)
        ret
        (recur (rest seq-1) (conj ret (first seq-1)))))))

