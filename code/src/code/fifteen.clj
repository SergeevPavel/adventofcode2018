(ns code.fifteen
  (:require [lanterna.screen :as s]))

(def input (line-seq (clojure.java.io/reader "/Users/pavel/hobby/adventofcode/code/resources/day15.txt")))

(defn input->state [input]
  (let [screen (into (sorted-map) (mapcat (fn [[y l]]
                                            (map-indexed (fn [x s]
                                                           [[y x] s]) l))
                                          (map-indexed vector input)))]
    {:screen screen
     :units (into (sorted-map) (keep (fn [[p v]] (when (contains? #{\G \E} v)
                                                   [p
                                                    {:race v
                                                     :hp 300}])) screen))}))

(defn show-state [scr {:keys [screen units]}]
  (doseq [[[y x] v] screen]
    (s/put-string scr 0 (+ y 3) (str y))
    (doseq [[idx dx] (map-indexed vector (str x))]
      (s/put-string scr (+ x 3) idx (str dx)))
    (s/put-string scr (+ x 3) (+ y 3) (str v)))
  (s/redraw scr))

(defn neighborhoods [[y x :as p]]
  #{[x (dec y)]
    [(dec x) y]
    [(inc x) y]
    [x (inc y)]})

(defn targets [{:keys [screen units]} p]
  (let [opposite (get {\G \E
                       \E \G}
                      (:race (get units p)))]
    (->> (neighborhoods p)
         (keep (fn [np]
                 (let [n (get units np)]
                   (when (= opposite (:race n))
                     [np n])))))))

(defn attack [{:keys [screen units] :as st} p]
  (let [targets (targets st p)]
    (if (empty? targets)
      st
      (let [[tp t] (apply min-key (comp :hp second) targets)
            t' (update t :hp #(- % 3))]
        (if (pos? (:hp t'))
          {:screen screen
           :units (assoc units tp t')}
          {:screen (assoc screen tp \.)
           :units (dissoc units tp)})))))

(defn bfs [screen start-point]
  (loop [visited #{}
         front #{start-point}
         result []]
    (if (empty? front)
      result
      (recur (clojure.set/union visited front)
             (into #{}
                   (filter (fn [p]
                             (and (= \. (get screen p))
                                  (not (contains? visited p))))
                           (mapcat neighborhoods front)))
             (conj result front)))))

(defn atack-position? [])

(defn move [{:keys [screen units] :as st} p]
  (let [ (first (drop-while (fn [pts]
                              )
                            (bfs screen p)))]))



(defn unit-turn [{:keys [screen units] :as st} p]
  (if (not-empty (targets st p))
    (attack st p)
    (let [[st' p'] (move st p)]
      (attack st' p'))))

(defn round [state]
  )

(defn run! []
  (let [scr (s/get-screen :swing {:cols 80 :rows 50})
        initial-state (input->state input)]
    (s/in-screen scr
                 (show-state scr initial-state)
                 (s/get-key-blocking scr))))



(comment
  (bfs (:screen (input->state input)) [1 1])

  (run!)
  )


