(ns keyfunc.core)

(defn sum
  "For composing below, need + to act on a coll"
  [coll]
  (apply + coll))

(defn- collect
  "key-collect helper"
  [acc [k v]]
  (if (contains? acc k)
    (update acc k conj v)
    (-> (assoc acc k [v])
        (update :_order conj k))))

(defn- key-collect
  "Act similar to 'frequencies' where coll and coll-keys
  align to make key, value pairs with their matching seq index.
  Also tracks the order that keys were added to the map."
  [coll coll-keys]
  (reduce collect {:_order []} (map vector coll-keys coll)))

(defn keyfn
  "Collects items of coll by coll-keys, applies the
  collection of funcs (afns) to the collected groups.
  Note, the funcs provided should act on collections."
  [coll coll-keys & afns]
  (let [freqs (key-collect coll coll-keys)
        {:keys [_order] :as stuff} freqs]
    (map #((apply comp afns) (stuff %)) _order)))

(defn keyfn-indexed
  "Same as keyfn, but return a coll of
  key, val vector pairs"
  [coll coll-keys & afns]
  (let [freqs (key-collect coll coll-keys)
        {:keys [_order] :as stuff} freqs]
    (map #(vector % ((apply comp afns) (stuff %))) _order)))

(defn separate-keys
  "Transform a coll of key, val vector pairs
  into two separate coll, and coll-keys collections
  for use with keyfn's"
  ([coll]
   (separate-keys coll [[] []]))
  ([coll result]
   (if-let [hd (first coll)]
     (recur (rest coll)
            (-> result
                (update 0 conj (first hd))
                (update 1 conj (last hd))))
     result)))

(defn grouped-keyfn [coll & afns]
  "Takes a coll of key, val vector pairs
  and applies the afns to the grouped colls"
  (let [[coll-keys coll] (separate-keys coll)]
    (apply (partial keyfn-indexed coll coll-keys) afns)))

(defn keyfn-histo [coll]
  (keyfn-indexed (repeat 1) coll sum))

(defn grouped-sum [coll]
  (grouped-keyfn coll sum))

(defn grouped-nub [coll]
  (grouped-keyfn coll first))

