(ns trolls.core
  (:require [clojure.string :as string]
            [clojure.repl :as repl]
            [clojure.tools.logging :as log]
            [clansi :refer [style]]))

(defonce state (atom {:maze []
                  :exit [nil nil]
                  :locs {:player [nil nil :up]
                         :troll_1 [nil nil :down]}
                  :prev [nil nil]}))

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

(defn glyph
  "Create a player/troll glyph based off
  of player-type and direction of movement."
  [ptype dir]
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

(defn out-of-bounds? [x y maze]
  (let [xlim (count (first maze))
        ylim (count maze)]
    (not
      (and (< 0 x xlim)
           (< 0 y ylim)))))

(defn opposites? [dir1 dir2]
  (let [oppos {:up :down
               :down :up
               :left :right
               :right :left}]
    (= dir2 (oppos dir1))))

(defn occupied?
  ([x y locs]
   (first (filter #(= [x y] (take 2 (second %))) locs)))
  ([x y dir locs]
   (first (filter #(and (= [x y] (take 2 (second %)))
                        (opposites? dir (last (second %))))
                  locs))))

(defn valid?
  "valid board position. Not a wall, exit,
  occupied, and is in bounds of the board."
  [x y maze]
  (cond
    (= (get-pos maze [x y]) "X") false
    (wall? x y maze) false
    (out-of-bounds? x y maze) false
    :else true))

(defn- randp
  "random coords within x,y limits"
  [xlim ylim]
  [(rand-int xlim) (rand-int ylim)])

(defn rand-pos
  "Generate a random valid position within board"
  [maze locs]
  (let [width (count (first maze))
        height (count maze)]
    (loop [[x y] (randp width height)]
      (if (and
            (valid? x y maze)
            (not (occupied? x y locs)))
        [x y]
        (recur (randp width height))))))

(defn rand-pos-dir [maze locs]
  (conj (rand-pos maze locs) (rand-nth [:up :down :left :right])))

(defn init-players! [{:keys [maze trolls]
                      :or {trolls 1}}]
  (let [locs {:player (rand-pos-dir maze {})}]
    (reduce #(assoc %1
                    (keyword (str "troll_" %2))
                    (rand-pos-dir maze %1))
            locs
            (range trolls))))

(defn load-maze!
  "Load maze into global state"
  [& {:keys [maze]
      :or {maze (read-in-maze "resources/basic_maze.txt")}}]
  (swap! state assoc :maze maze)
  (swap! state assoc :locs (init-players! {:maze maze :trolls 2}))
  (->> (find-exit maze)
       (swap! state assoc :exit)))

(defn clear-screen! []
  (print (str (char 27) "[2J"))
  (print (str (char 27) "[;H")))

(defn put-header! []
  (let [trolls "TROLLLLLS"
        t-len (+ (count trolls) 2)
        width (count (first (@state :maze)))
        b-len (/ (- width t-len) 2)
        border-left (clojure.string/join (repeat b-len ">"))
        border-right (clojure.string/join (repeat b-len "<"))
        border-bot (clojure.string/join (repeat (dec width) "-"))]
    (println (str border-left " "
                  trolls
                  " " border-right))
    (println border-bot)))

(defn draw! [{:keys [maze locs exit] :as state}]
  (let [maze (reduce #(put-glyph %1 %2) maze locs)
        maze (put-char maze exit (style "X" :bg-green))]
    (clear-screen!)
    (put-header!)
    (doseq [line maze]
      (println (string/join line)))))

(defn inc-by-dir [x y dir]
  (case dir
    :up    [x (dec y)]
    :down  [x (inc y)]
    :left  [(dec x) y]
    :right [(inc x) y]))

(defn can-push? [x y dir maze]
  (-> (inc-by-dir x y dir)
      (conj maze)
      (#(apply valid? %))))

(defn push! [x y dir]
  (if (can-push? x y dir (@state :maze))
    (let [[newx newy] (inc-by-dir x y dir)]
      (swap! state update :maze
             #(-> (put-char % [x y] " ")
                 (put-char [newx newy] "#")))
      (log/trace (@state :maze))
      true)
    false))

(defn- move-to! [character coords & [prev]]
  (when prev
    (swap! state assoc :prev prev))
  (swap! state assoc-in [:locs character] coords))

(defn move-player! []
  (let [[x y dir] (get-in @state [:locs :player])
        [newx newy] (inc-by-dir x y dir)
        maze (@state :maze)]
    (log/trace (str "x: " x ", y: "y ", " dir "; newx: " newx ", newy: " newy))
    (cond
      (not (wall? newx newy maze)) (move-to! :player [newx newy dir] [x y])
      (push! newx newy dir) (move-to! :player [newx newy dir] [x y]))))

(defn get-move []
  (if-let [m (first (read-line))]
    m
    ""))

(defn users-move! []
  (let [move (get-move)
        curdir (get-in @state [:locs :player 2])
        dir (case (clojure.string/lower-case move)
              "w" :up
              "s" :down
              "a" :left
              "d" :right
              curdir)]
    (log/trace (= curdir dir))
    (if-not (= curdir dir)
      (swap! state assoc-in [:locs :player 2] dir)
      (move-player!))))

(defn empty-dir [[x y _] maze]
  (first (filter #(apply valid? (conj (inc-by-dir x y %) maze))
                 (shuffle [:up :down :left :right]))))

(defn move-trolls! []
  (let [{:keys [maze locs]} @state
        trolls (remove #(= :player (first %)) locs)]
    (log/trace trolls)
    (doseq [[troll coords] trolls]
           (log/trace (str "troll: " troll " " coords))
           (if (apply valid? (conj (apply inc-by-dir coords) maze))
             (move-to! troll (conj (apply inc-by-dir coords) (last coords)))
             (swap! state assoc-in [:locs troll 2] (empty-dir coords maze)))
           (log/trace (str "troll: " troll " " coords)))))

(defn win? []
  (let [{:keys [exit locs]} @state
        {:keys [player]} locs]
    (= exit (take 2 player))))

(defn trolled? []
  (let [{:keys [prev locs]} @state
        pcoords (locs :player)
        [x y dir] pcoords
        [lastx lasty] prev
        trolls (remove #(= :player (first %)) locs)]
    (log/trace trolls)
    (or (occupied? x y trolls)
        (occupied? lastx lasty dir trolls))))

(defn sigint [_]
  (println "Exiting...")
  (System/exit 0))

(defn play! []
  (repl/set-break-handler! sigint)
  (loop []
    (draw! @state)
    (users-move!)
    (move-trolls!)
    (cond
      (win?) (println "You're a winner!")
      (trolled?) (println "You're a loser")
      :else (recur))))

(defn -main []
  (load-maze!)
  (play!))

