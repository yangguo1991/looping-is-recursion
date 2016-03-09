(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [p n k]
               (cond
                 (zero? n) 0
                 (zero? k) p
                 :else (recur (* p n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [f (first a-seq)
        r (rest a-seq)]
    (cond
      (empty? a-seq) nil
      (empty? r) f
      :else (recur r))))

(defn seq= [seq1 seq2]
  (let [[f1 & r1] seq1
        [f2 & r2] seq2]
    (cond
      (and (empty? seq1) (empty? seq2)) true
      (or (empty? seq1) (empty? seq2)) false
      (not= f1 f2) false
      :else (recur r1 r2))))


(defn find-first-index [pred a-seq]
  (loop [i 0
         s a-seq]
    (cond
      (empty? s) nil
      (pred (first s)) i
      :else (recur (inc i) (rest s)))))

(defn avg [a-seq]
  (loop [n 0
         sum 0
         seqs a-seq]
    (if (empty? seqs)
      (/ sum n)
      (recur (inc n) (+ sum (first seqs)) (rest seqs)))))

(defn parity [a-seq]
  (loop [result #{}
         f (first a-seq)
         r (rest a-seq)]
    (cond
      (nil? f) result
      (contains? result f) (recur (disj result f) (first r) (rest r))
      :else (recur (conj result f) (first r) (rest r)))))

(defn fast-fibo [n]
  (loop [rn n
         fn1 0
         fn  1]
    (cond
      (= rn 0) fn1
      (= rn 1) fn
      :else (recur (dec rn) fn (+ fn1 fn)))))

(defn cut-at-repetition [a-seq]
  (loop [ue #{}
         re []
         rs a-seq]
    (if (or (empty? rs) (contains? ue (first rs)))
      re
      (recur (conj ue (first rs)) (conj re (first rs)) (rest rs)))))

