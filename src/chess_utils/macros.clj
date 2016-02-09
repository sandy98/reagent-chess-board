(ns chess-utils.macros)

;(defn r-w-excl [excl] (remove #{excl} (range 64)))

(defmacro attacks-from-sq-simple [fname conds] `(defn ~fname [sq#] (set (filter #(~conds sq# %) 
                                                                      ((fn [excl#] (remove #{excl#} (range 64))) sq#)))))
(defmacro attacks-from-sq-complex [fname h-o-f func juxts excl-fn] `(defn ~fname [sq#] (set (filter #(~h-o-f ~func ((~@juxts) sq# %)) 
                                                                      (~excl-fn sq#)))))

(defmacro moves-map [map-name func-name rang]  `(def ~map-name (reduce conj (map #(hash-map % (~func-name %)) ~rang))))

