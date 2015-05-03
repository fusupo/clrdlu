(ns clrdlu.core
  (:require
   [clrdlu.render :as r]
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]))

(defonce app-state (atom {:text "Hello Chestnut!"}))

(defn main []
  (om/root
   (fn [app owner]
     (reify
       om/IRender
       (render [_]
         (dom/h1 nil (:text app)))))
   app-state
   {:target (. js/document (getElementById "app"))}))


(enable-console-print!)

(defrecord block [name
                  type
                  width
                  height
                  position
                  supported-by
                  support-for])

(defrecord hand [name
                 position
                 grasping])

(def h (-> (make-hierarchy)
          (derive ::movable-block ::basic-block)
          (derive ::load-bearing-block ::basic-block)

          (derive ::brick ::movable-block)
          (derive ::brick ::load-bearing-block)

          (derive ::wedge ::movable-block)

          (derive ::ball ::movable-block)

          (derive ::table ::load-bearing-block)))

(declare block_map)

(defn make-def [k v]
  ;;(eval `(def ~(symbol k) ~v))
  (reset! (block_map k) v))

(defn reset-world []

  (def hh (atom (hand. "hand" '(0 6) nil)))
  
  (def block_map {"tt" (atom (block. "tt" ::table 20 0 '(0 0) nil '()))
                  "b1" (atom (block. "b1" ::brick 2  2 '(0 0) nil nil))
                  "b2" (atom (block. "b2" ::brick 2  2 '(2 0) nil nil))
                  "b3" (atom (block. "b3" ::brick 4  4 '(4 0) nil nil))
                  "b4" (atom (block. "b4" ::brick 2  2 '(8 0) nil nil))
                  "w5" (atom (block. "w5" ::wedge 2  4 '(10 0) nil nil))
                  "b6" (atom (block. "b6" ::brick 4  2 '(12 0) nil nil))
                  "w7" (atom (block. "w7" ::wedge 2  2 '(16 0) nil nil))
                  "lq" (atom (block. "lq" ::ball  2  2 '(18 0) nil nil))})

  (doseq [[k v] block_map
          :let [tt (block_map "tt")]]
    (if (not= v tt)
      (do
        (make-def "tt" (update-in @tt [:support-for] (fn [x y] (cons y x)) v))
        (make-def k (update-in @v [:supported-by] (fn [x y] y) tt))))))

(reset-world)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare top-location)
(declare intersections-p)
(declare find-space)
(declare get-space)
(declare grasp)
(declare ungrasp)
(declare get-ride-of)
(declare make-space)
(declare clear-top)
(declare move)
(declare remove-support)
(declare add-support)
(declare put-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn top-location [object]
  (let [pos (:position @object)
        posx (first pos)
        posy (fnext pos)
        w (:width @object)
        h (:height @object)]
    (list (+ posx  (/ w 2)) (+ posy h))))

(defn intersections-p [object offset base obstacles]
  (let [hit nil]
    (loop [obstacle obstacles
           hit nil]
      (if (or (empty? obstacle) (= true hit))
        hit
        (let [o (first obstacle)
              ls-proposed (+ offset base)
              rs-proposed (+ ls-proposed (:width @object))
              ls-obstacle (first (:position @o))
              rs-obstacle (+ ls-obstacle (:width @o))]
          (if (or (>= ls-proposed rs-obstacle)
                 (<= rs-proposed ls-obstacle))
            (recur (next obstacle) hit)
            (recur (next obstacle) true)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  FIND-SPACE

(defn find-space [object support]
  (println "F-Spc, " (:name @object) " - " (:name @support))
  (loop [curr-offset 0
         max-offset (+ 1 (- (:width @support)
                            (:width @object)))
         hit nil]
    (cond (or (= curr-offset max-offset) (not= nil hit)) hit
          :else (if (not (intersections-p object curr-offset
                                        (first (:position @support))
                                        (:support-for @support)))
                  (recur (+ 1 curr-offset) max-offset (list (+ curr-offset (first (:position @support)))
                                                            (+ (second (:position @support))
                                                               (:height @support))))
                  (recur (+ 1 curr-offset) max-offset hit)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  GET-SPACE

(defmulti get-space
  (fn [b1 b2] [(:type @b1) (:type @b2)])
  :hierarchy #'h)

(defmethod get-space [::movable-block ::basic-block] [object support]
  (println "G-Spc, " (:name @object) " - " (:name @support))
  ;;(println " --- > "(find-space object support))
  (or (find-space object support)
     (make-space object support)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  GRASP

(defmulti grasp
  (fn [b] (:type @b))
  :hierarchy #'h)

(defmethod grasp ::movable-block [object]
  (println "TRY GRASP")
  (when-not (= (:grasping @hh) object)
    (when (:support-for @object) (clear-top object))
    (when (:grasping @hh)
      (get-ride-of (:grasping @hh)))
    (println "Move hand to pick up "
             (:name @object)
             " at location "
             (top-location object))

    (reset! hh (assoc @hh :position (top-location object)))
    (println "Grasp " (:name @object))
    (reset! hh (assoc @hh :grasping object)))
  true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  UNGRASP

(defmulti ungrasp
  (fn [b] (:type @b))
  :hierarchy #'h)

(defmethod ungrasp ::movable-block [object]
  (println "Try Ungrasp " (:name @object))
  (when (:supported-by @object)
    (println "Ungrasp " (:name @object))
    ;;(def hh (assoc hh :grasping nil))
    (reset! hh (assoc @hh :grasping nil))
    true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  GET_RIDE-OF

(defmulti get-ride-of
  (fn [b] (:type @b))
  :hierarchy #'h)

(defmethod get-ride-of ::movable-block [object]
  (println "TRY GET-RID-OF")
  (put-on object (block_map "tt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  MAKE-SPACE

(defmulti make-space
  (fn [b1 b2] [(:type @b1) (:type @b2)])
  :hierarchy #'h)

(defmethod make-space [::movable-block ::basic-block] [object support]
  (println "TRY MAKE-SPACE")
  (loop [obstruction (:support-for support)
         hit nil]
    (cond (or (not= nil hit)(empty? obstruction)) hit 
          :else (do
                  (get-ride-of (first obstruction))
                  (let [space (find-space object support)]
                    (if space
                      (recur (next obstruction) space)
                      (recur (next obstruction) hit)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  CLEAR-TOP

(defmulti clear-top
  (fn [b] (:type @b))
  :hierarchy #'h)

(defmethod clear-top ::load-bearing-block [support]
  (println "TRY CLEAR-TOP" (count (:support-for @support)))
  (doseq [obstacle (:support-for @support)]
    (get-ride-of obstacle))
  true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  MOVE

(defmulti move
  (fn [b1 b2] [(:type @b1) (:type @b2)])
  :hierarchy #'h)

(defmethod move [::movable-block ::basic-block] [object support]
  (println "TRY MOVE" (:name @object))
  (remove-support object)
  (make-def (:name @object) (assoc @object :position '(3000 3000)))
  (let [newplace (get-space object support)]
    (println "Move" (:name @object)
             "to top of" (:name @support)
             "at location" newplace)
    (make-def (:name @object) (assoc @object :position newplace))
    (reset! hh (assoc @hh :position (top-location object))))
  (add-support object support)
  true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  REMOVE-SUPPORT

(defmulti remove-support
  (fn [b] (:type @b))
  :hierarchy #'h)

(defmethod remove-support ::movable-block [object]
  (println "TRY REMOVE SUPPORT" (:name @object))
  (let [support (:supported-by @object)]
    (when support
      (println "remove support" (:name @object) (:name @support))
      (println  (map #(:name @%) (remove #(= % object) (:support-for @support))))
      (make-def (:name @support) (assoc @support :support-for (remove #(= % object) (:support-for @support))))
      (make-def (:name @object) (assoc @object :supported-by nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ADD-SUPPORT

(defmulti add-support
  (fn [b1 b2] [(:type @b1) (:type @b2)])
  :hierarchy #'h)

(defmethod add-support[::movable-block ::basic-block] [object support]
  (println "A-Sup: basic")
  true)

(defmethod add-support [::movable-block ::load-bearing-block] [object support]
  (println "A-Sup: load bearing")
  (println "add support" (:name @object) (:name @support) (map #(:name @%) (:support-for @support)))
  (println (count (:support-for @support) ))
  (make-def (:name @support) (assoc @support :support-for (conj (:support-for @support) object)))
  (println (count (:support-for @support)))
  (make-def (:name @object) (assoc @object :supported-by support))
  true)


(prefer-method add-support [::movable-block ::load-bearing-block][::movable-block ::basic-block])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  PUT-ON

(defmulti put-on
  (fn [b1 b2] [(:type @b1) (:type @b2)])
  :hierarchy #'h)

(defmethod put-on [::movable-block ::basic-block] [object support]
  (print "P-O, " (:name @object) " - " (:name @support))
  (if (get-space object support)
    (and (grasp object)
       (move object support)
       (ungrasp object))
    (println "Sorry, there is no room for" (:name @object)
             "on" (:name @support))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn put [o s]
  (put-on (block_map o) (block_map s))
  (r/draw block_map))

(r/draw block_map)

(put "w5" "b3")
(println "doo")
