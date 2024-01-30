(ns random-numbers.core
  (:gen-class))

(defn- lcg-hash-seed [input-str]
  (reduce (fn [checksum char]
            (bit-and (unchecked-add (unchecked-multiply checksum 31) (int char)) 0xFFFFFFFF))
          0
          input-str))

(defn lcg-init [generator seed]
  (assoc generator :state (lcg-hash-seed seed)
         :a 1664525
         :c 1013904223
         :m 1294967296))

(defn lcg-next [generator]
  (let [new-state (mod (+ (* (:a generator) (:state generator)) (:c generator)) (:m generator))]
    (assoc generator :state new-state)))

(defn lcg-random [generator]
  (/ (float (:state (lcg-next generator))) (:m generator)))

(defn get-next-number [generator n]
  (int (Math/floor (* (lcg-random generator) n))))

(defn get-next-number-between [generator n m]
  (let [random-value (lcg-random generator)]
    (int (Math/floor (+ (* random-value (- m n)) n)))))

(defn -main [& args]
  (if (= 1 (count args))
    (let [seed-string (first args)
          generator (lcg-init {} seed-string)]
      (println (clojure.string/join ", " (take 5 (repeatedly #(get-next-number generator 100))))))
    (println "Usage: <seed>")))

