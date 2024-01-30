(ns bankkonto.core)

(defn transfer [accounts k1 k2 amount]
  (if (>= (get @accounts k1 0) amount)
    (do
      (swap! accounts update k1 - amount)
      (swap! accounts update k2 + amount))
    ; else Fall wird ignoriert
    ;(println "Unzureichende Deckung für Transfer")
    ))


(defn create-server [num-accounts seed]
  (let [rand-gen (java.util.Random. seed)
        accounts (atom (vec (map (fn [_] (.nextInt rand-gen 1000)) (range num-accounts))))]
    {:accounts accounts
     :transfer (partial transfer accounts)}))

(defn client-task [server num-transfers]
  (let [transfer-fn (:transfer server)
        num-accounts (count @(:accounts server))
        rand-gen (java.util.Random.)]
    (doseq [_ (range num-transfers)]
      (let [k1 (.nextInt rand-gen num-accounts)
            k2 (.nextInt rand-gen num-accounts)
            amount 6]
        (transfer-fn k1 k2 amount)))))

(defn run-simulation [server num-clients num-transfers]
  (let [client-tasks (vec (map (fn [_] (future (client-task server num-transfers))) (range num-clients)))]
    (dorun (mapv deref client-tasks))))

(defn print-accounts [accounts]
  (doseq [account accounts]
    (println account)))

(defn total-balance [accounts]
  (reduce + @accounts))

(defn measure-time [f & args]
  (let [start-time (System/currentTimeMillis)
        result (apply f args)
        end-time (System/currentTimeMillis)]
    (println "Ausführungszeit: " (- end-time start-time) "ms")
    result))



(defn- lcg-hash-seed [input-str]
  (let [str-input (str input-str)] ; Stellt sicher, dass es ein String ist
    (reduce (fn [checksum char]
              (bit-and (unchecked-add (unchecked-multiply checksum 31) (int char)) 0xFFFFFFFF))
            0
            str-input)))

(defn lcg-init [seed]
  {:state (lcg-hash-seed seed)
   :a 1664525
   :c 1013904223
   :m 1294967296})

(defn lcg-next [generator]
  (let [new-state (mod (+ (* (:a generator) (:state generator)) (:c generator)) (:m generator))]
    (assoc generator :state new-state)))

(defn lcg-random [generator]
  (/ (float (:state generator)) (:m generator)))

(defn lcg-sequence [generator n]
  (map lcg-random (take n (iterate lcg-next generator))))

;(defn -main [& args]
;  (if (= 1 (count args))
;    (let [seed-string (first args)
;          generator (lcg-init seed-string)]
;      (println (clojure.string/join ", " (lcg-sequence generator 5))))
;    (println "Usage: <seed>")))



(defn main [num-accounts-str num-clients-str num-transfers-str seed-str]
  (-main 5323123))
  (let [num-accounts (Integer/parseInt num-accounts-str)
        num-clients (Integer/parseInt num-clients-str)
        num-transfers (Integer/parseInt num-transfers-str)
        seed (Integer/parseInt seed-str)
        server (create-server num-accounts seed)]

    ;; Konten vor der Simulation ausgeben
    (println "Konten vor Transaktionen:")
    (print-accounts @(:accounts server))
    (println "Gesamtwert aller Konten vor Transaktionen:" (total-balance (:accounts server)))

    ;; Simulation starten
    (measure-time run-simulation server num-clients num-transfers)

    ;; Konten nach der Simulation ausgeben
    (println "Konten nach Transaktionen:")
    (print-accounts @(:accounts server))
    (println "Gesamtwert aller Konten nach Transaktionen:" (total-balance (:accounts server)))
    (println "Konsistenz überprüft")

    ;; Beenden der Agenten (falls verwendet)
    (shutdown-agents)))
  

;; Beispielaufruf: (main 10 5 100 12345)





