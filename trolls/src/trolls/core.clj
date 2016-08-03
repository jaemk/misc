(ns trolls.core
  (:require [clojure.string :as string]
            [clansi :refer [style]]))

(def state (atom {:maze []
                  :exit [nil nil]
                  :locs {:player [nil nil :up]
                         :troll_1 [nil nil :down]}}))

(defn read-in-maze [filename]
  (as-> (slurp filename) _
      (string/split _ #"\n")
      (map #(string/split % #"") _)
      (into [] _)))

(defn find-exit [maze]
  (->> maze
       (keep-indexed (fn [y row]
                       (if-let [[x] (keep-indexed #(if (= %2 "X") %1) row)]
                         [x y])))
       (filter (fn [x] (every? #(not (nil? %)) x)))
       (first)))

(defn glyph [ptype dir]
  (let [dir (case dir
              :up "^"
              :down "v"
              :left "<"
              :right ">")
        ptype (name ptype)
        color (if (.startsWith ptype "player")
                :green
                :red)]
    (style dir color)))

(defn get-pos [maze [x y]]
  (get-in maze [y x]))

(defn put-char [maze [x y] c]
  (assoc-in maze [y x] c))

(defn put-glyph [maze [ptype [x y dir]]]
  (put-char maze [x y] (glyph ptype dir)))

(defn wall? [x y maze]
  (= "#" (get-pos maze [x y])))

(defn valid? [x y maze]
  (cond
    (= (get-pos maze [x y]) "X") false
    (wall? x y maze) false
    :else true))

(defn- randp [xlim ylim]
  [(rand-int xlim) (rand-int ylim)])

(defn rand-pos [maze]
  (let [width (count (first maze))
        height (count maze)]
    (loop [[x y] (randp width height)]
      (if (valid? x y maze)
        [x y]
        (recur (randp width height))))))

(defn rand-pos-dir [maze]
  (conj (rand-pos maze) (rand-nth [:up :down :left :right])))

(defn init-players! [{:keys [maze trolls]
                      :or {trolls 1}}]
  (let [locs {:player (rand-pos-dir maze)}]
    (reduce #(assoc %1 (keyword (str "troll_" %2)) (rand-pos-dir maze)) locs (range trolls))))

(defn load-maze!
  "Load maze into global state"
  [& {:keys [maze]
      :or {maze (read-in-maze "resources/basic_maze.txt")}}]
  (swap! state assoc :maze maze)
  (swap! state assoc :locs (init-players! {:maze maze :trolls 2}))
  (->> (find-exit maze)
       (swap! state assoc :exit)))

(defn draw! [{:keys [maze locs exit] :as state}]
  (let [maze (reduce #(put-glyph %1 %2) maze locs)
        maze (put-char maze exit (style "X" :bg-green))]
    (doseq [line maze]
      (println (string/join line)))))

(defn inc-by-dir [x y dir]
  (case dir
    :up    [x (dec y)]
    :down  [x (inc y)]
    :left  [(dec x) y]
    :right [(inc x) y]))

(defn can-push? [x y dir maze]
  (not
    (-> (inc-by-dir x y dir)
        (conj maze)
        (#(apply valid? %)))))

(defn push! [x y dir]
  (if (can-push? x y dir (@state :maze))
    (let [[newx newy] (inc-by-dir x y dir)]
      (swap! state assoc :maze
             (-> (put-char (@state :maze) x y " ")
                 (put-char newx newy "#")))
      (println (@state :maze))
      true)
    false))

(defn- move-player-to! [& coords]
  (swap! state update-in [:locs :player] coords))

(defn move-player! []
  (let [[x y dir] (get-in @state [:locs :player])
        [newx newy] (inc-by-dir x y dir)
        maze (@state :maze)]
    (cond
      (not (wall? newx newy maze)) (move-player-to! newx newy dir)
      (push! newx newy dir) (move-player-to! newx newy dir))))

(defn users-move! []
  (let [move (first (read-line))
        curdir (get-in @state [:locs :player 2])
        dir (case (clojure.string/lower-case move)
              "w" :up
              "s" :down
              "a" :left
              "d" :right)]
    (println (= curdir dir))
    (if-not (= curdir dir)
      (swap! state assoc-in [:locs :player 2] dir)
      (move-player!))))

(defn play! []
  (loop []
    (draw! @state)
    (users-move!)
    (recur)))

(defn -main []
  (load-maze!)
  (play!))
