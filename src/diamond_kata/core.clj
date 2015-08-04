(ns diamond-kata.core
  (:gen-class))

(defonce A-OFFSET 64)

(defn string->index [s]
  (-> s (first) (int) (- A-OFFSET)))

(defn gap-seq [n]
  (take n (conj (iterate (partial + 2) 1) 0)))

(defn off-seq [n]
  (reverse (range 0 n)))

(defn char-seq [n]
  (map #(char (+ 1 A-OFFSET %)) (range 0 n)))

(defn merged-seq [n]
  (map #(vector %1 %2 %3)
       (off-seq n)
       (char-seq n)
       (gap-seq n)))

(defn mirror-seq [l]
  (concat l (->> l (reverse) (drop 1))))

(defn blanks [n]
  (apply str (repeat n " ")))

(defn diamond-line [[off char gap]]
  (str (blanks off)
       char
       (if-not (zero? gap) (str (blanks gap) char))))

(defn print-diamond [s]
  (let [index (string->index s)
        lines (->> (merged-seq index)
                   (map diamond-line)
                   (mirror-seq))]
    (doseq [line lines] (println line))))

(defn -main
  [& args]
  (print-diamond (first args)))
