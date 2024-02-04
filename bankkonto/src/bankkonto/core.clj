(ns bankkonto.core)

; Hier sind einige Funktion, die gebraucht werden, um den Random-Algorithmus zu vereinheitlichen.
(defn- lcg-hash-seed [input-str]
  (let [str-input (str input-str)] ; Stellt sicher, dass Input ein String ist
    (reduce (fn [checksum char]
              (bit-and (unchecked-add (unchecked-multiply checksum 31) (int char)) 0xFFFFFFFF))
            0
            str-input)))
(defn lcg-init [seed]
  (atom (assoc {} :state (lcg-hash-seed seed)
               :a 1664525
               :c 1013904223
               :m 1294967296)))


(defn lcg-next [generator]
  (let [new-state (mod (+ (* (:a @generator) (:state @generator)) (:c @generator)) (:m @generator))]
    (swap! generator assoc :state new-state)))

(defn lcg-random [generator]
  (/ (float (:state (lcg-next generator))) (:m @generator)))

(defn get-next-number [generator n]
  (int (Math/floor (* (lcg-random generator) n))))

(defn get-next-number-between [generator n m]
  (let [random-value (lcg-random generator)]
    (int (Math/floor (+ (* random-value (- m n)) n)))))


; Ab hier beginnen die Methoden für den Banktransfer
; transfer transferient amount von Account k1 zu Account k2, wenn k1 gedeckt ist.
(defn transfer [accounts k1 k2 amount]
  (if (>= (get @accounts k1 0) amount)
    (do
      (swap! accounts update k1 - amount)
      (swap! accounts update k2 + amount))
    ; else Fall wird ignoriert, wurde zu Testzwecken genutzt
    ;(println "Unzureichende Deckung für Transfer")
    ))

; Erstellt eine angegebene Anzahl an Accounts und setzt den initalen Kontostand auf einen Wert zwischen 0 und 1000
(defn create-server [num-accounts generator]
  (let [accounts (atom (vec (repeatedly num-accounts #( get-next-number generator 1000 ))))]
    {:accounts accounts
     :generator generator
     :transfer (partial transfer accounts)}))

; Definiert eine Funktion, die eine Überweisungsaufgabe für einen Client simuliert.
(defn client-task [server num-transfers]
  (let [transfer-fn (:transfer server)
        num-accounts (count @(:accounts server))]
    (doseq [_ (range num-transfers)]
      (let [k1 (get-next-number (:generator server) num-accounts)  
            k2 (get-next-number (:generator server) num-accounts)
            amount (get-next-number (:generator server) 500)]
        (transfer-fn k1 k2 amount)))))

; Definiert eine Funktion zur Ausführung der Simulation mit einer bestimmten Anzahl von Clients und Überweisungen.
(defn run-simulation [server num-clients num-transfers]
  (let [client-tasks (vec (map (fn [_] (future (client-task server num-transfers))) (range num-clients)))]
    (dorun (mapv deref client-tasks))))

(defn print-accounts [accounts]
  (doseq [account accounts]
    (println account)))

(defn total-balance [accounts]
  (reduce + @accounts))


(defn main [num-accounts-str num-clients-str num-transfers-str seed-str]
  (let [num-accounts (Integer/parseInt num-accounts-str)
        num-clients (Integer/parseInt num-clients-str)
        num-transfers (Integer/parseInt num-transfers-str)
        seed seed-str
        generator (lcg-init seed)
        server (create-server num-accounts generator)]

    ;; Konten vor der Simulation ausgeben
    (println "Konten vor Transaktionen:")
    (print-accounts @(:accounts server))
    (println "Gesamtwert aller Konten vor Transaktionen:" (total-balance (:accounts server)))

    ;; Simulation starten
    (run-simulation server num-clients num-transfers)

    ;; Konten nach der Simulation ausgeben
    (println "Konten nach Transaktionen:")
    (print-accounts @(:accounts server))
    (println "Gesamtwert aller Konten nach Transaktionen:" (total-balance (:accounts server)))

    ;; Beenden der Agenten
    (shutdown-agents)))
  

;; Beispielaufruf: (main 10 5 100 ABC123)
