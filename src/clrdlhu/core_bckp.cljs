(ns clrdlhu.core
  (:require [clojure.browser.repl :as repl]
            [cljs.core.logic :as m :refer [membero]]))

;; (repl/connect "http://localhost:9000/repl")

(enable-console-print!)

(println  (m/run* [q]
            (membero q [1 2 3])
            (membero q [2 3 4])))

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

;; (defn make-def [k v]
;;   (eval `(def ~(symbol k) ~v)))

(defn reset-world []

  (def *blocks*
    (vector
     (atom (block. "tt" ::table 20 0 '(0 0) nil '()))
     (atom (block. "b1" ::brick 2  2 '(0 0) nil nil))
     (atom (block. "b2" ::brick 2  2 '(2 0) nil nil))
     (atom (block. "b3" ::brick 4  4 '(4 0) nil nil))
     (atom (block. "b4" ::brick 2  2 '(8 0) nil nil))
     (atom (block. "w5" ::wedge 2  4 '(10 0) nil nil))
     (atom (block. "b6" ::table 4  2 '(12 0) nil nil))
     (atom (block. "w7" ::wedge 2  2 '(16 0) nil nil))
     (atom (block. "lq" ::ball  2  2 '(18 0) nil nil))
     ))

  ;; (doseq [l *blocks*]
  ;;   (let [n (:name l)]
  ;;     (make-def n l)))
  
  (doseq [l (rest *blocks*)
          :let [tt (first *blocks*)]]
    (println (:name @tt))
    (println (:name @l))
    ;; (make-def (:name tt))
    (reset! tt (update-in @tt [:support-for] (fn [x y] (cons y x)) l))
    ;; (make-def (:name l))
    (reset! l (update-in @l [:supported-by] (fn [x y] y) tt))
    ))
  
(def hh (atom (hand. "hand" '(0 6) nil)))

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
  (let [pos (:position object)
        posx (first pos)
        posy (fnext pos)
        w (:width object)
        h (:height object)]
    (list (+ posx  (/ w 2)) (+ posy h))))

(defn intersections-p [object offset base obstacles]
  (println "Ip, " (:name object) offset)
  (let [hit nil]
    (loop [obstacle obstacles
           hit nil]
      (cond (or (empty? obstacle) (= true hit)) hit
            :else (let [o (first obstacle)
                        ls-proposed (+ offset base)
                        rs-proposed (+ ls-proposed (:width object))
                        ls-obstacle (first (:position o))
                        rs-obstacle (+ ls-obstacle (:width o))]
                    (if (or (>= ls-proposed rs-obstacle)
                           (<= rs-proposed ls-obstacle))
                      (recur (next obstacle) true)
                      (recur (next obstacle) hit)))))))

;; FIND-SPACE

(defn find-space [object support]
  (println "F-Sp, ")
  (loop [curr-offset 0
         max-offset (+ 1 (- (:width support)
                            (:width object)))
         hit nil]
    (cond (or (= curr-offset max-offset) (not= nil hit)) hit
          :else (if (not (intersections-p object curr-offset
                                        (first (:position support))
                                        (:support-for support)))
                  (recur (+ 1 curr-offset) max-offset (list (+ curr-offset (first (:position support)))
                                                            (+ (second (:position support))
                                                               (:height support))))
                  (recur (+ 1 curr-offset) max-offset hit)))))

;; GET-SPACE

(defmulti get-space
  (fn [b1 b2] [(:type b1) (:type b2)])
  :hierarchy #'h)

(defmethod get-space [::movable-block ::basic-block] [object support]
  (println "G-Sp, ")
  (or (find-space object support)
     (make-space object support)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GRASP

(defmulti grasp
  (fn [b] (:type b))
  :hierarchy #'h)

(defmethod grasp ::movable-block [object]
  (when-not (= (:grasping hh) object)
    (when (:support-for object) (clear-top object))
    (when (:grasping hh)
      (get-ride-of (:grasping hh)))
    (println "Move hand to pick up %s at location "
             (:name object)
             (top-location object))
    (def hh (assoc hh :position (top-location object)))
    (println "Grasp " (:name object))
    (def hh (assoc hh :grasping object)))
  true)

;; UNGRASP

(defmulti ungrasp
  (fn [b] (:type b))
  :hierarchy #'h)

(defmethod ungrasp ::movable-block [object]
  (when (:supported-by object)
    (println "Ungrasp " (:name object))
    (def hh (assoc hh :grasping nil))
    true))

;; GET_RIDE-OF

(defmulti get-ride-of
  (fn [b] (:type b))
  :hierarchy #'h)

(defmethod get-ride-of ::movable-block [object]
  (put-on object tt))

;; MAKE-SPACE

(defmulti make-space
  (fn [b1 b2] [(:type b1) (:type b2)])
  :hierarchy #'h)

(defmethod make-space [::movable-block ::basic-block] [object support]
  (loop [obstruction (:support-for support)
         hit nil]
    (cond (or (not= nil hit)(empty? obstruction)) hit 
          :else (do
                  (get-ride-of (first obstruction))
                  (let [space (find-space object support)]
                    (if space
                      (recur (next obstruction) space)
                      (recur (next obstruction) hit)))))))

;; CLEAR-TOP

(defmulti clear-top
   (fn [b] (:type b))
   :hierarchy #'h)

(defmethod clear-top ::load-bearing-block [support]
  (doseq [obstacle (:support-for support)]
    (get-ride-of obstacle))
  true)

;; MOVE

(defmulti move
  (fn [b1 b2] [(:type b1) (:type b2)])
  :hierarchy #'h)

(defmethod move [::movable-block ::basic-block] [object support]
  (remove-support object)
    (make-def (:name object) (assoc object :position '(3000 3000)))
  (let [newplace (get-space object support)]
    (println "Move %s to top of %s at location "
             (:name object)
             (:name support)
             newplace)
    (make-def (:name object) (assoc object :position newplace))
    (def hh (assoc hh :position (top-location object))))
  (add-support object support)
  true)

;; REMOVE-SUPPORT

(defmulti remove-support
  (fn [b] (:type b))
  :hierarchy #'h)

(defmethod remove-support ::movable-block [object]
  (let [support (:supported-by object)]
    (when support
      (make-def (:name support) (assoc support :support-for (remove object (:support-for support)))))))

;; ADD-SUPPORT

(defmulti add-support
  (fn [b1 b2] [(:type b1) (:type b2)])
  :hierarchy #'h)

(defmethod add-support[::movable-block ::basic-block] [object support]
  true)

(defmethod add-support [::movable-block ::load-bearing-block] [object support]
  (make-def (:name support) (assoc support :support-for (conj object (:support-for support))))
  (make-def (:name object) (assoc object :supported-by support))
  true)

;; PUT-ON

(defmulti put-on
  (fn [b1 b2] [(:type b1) (:type b2)])
  :hierarchy #'h)

(defmethod put-on [::movable-block ::basic-block] [object support]
  (print "P-O, ")
  (if (get-space object support)
    (and (grasp object)
       (move object support)
       (ungrasp object))
    (println "Sorry, there is no room for "
             (:name object)
             " on "
             (:name support))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (do (reset-world)
;;     (println "------------------------->>")
;;     (print (put-on w7 b3))
;;     (println "------------------------->>")
;;     (print (put-on b2 b4))
;;     (println "------------------------->>")
;;     (print (put-on w5 b2)))

;; (pprint w7)
;; (pprint "suckit")
;; (pprint (assoc w7 :position '(999 999)))
;; (print (* 8 8))

(println (top-location @hh))
(println (top-location @(second *blocks*)))
(println (get-space @(second *blocks*)  @(nth *blocks* 3)))
