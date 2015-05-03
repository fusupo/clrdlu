(ns clrdlu.render
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(def black "#000000")
(def yellow "#FFDB7F")
(def red "#E88E7F")
(def purple "#DA98FF")
(def blue "#88BAE8")
(def green "#7FFFA5")

(def canvas (.getElementById js/document "canvas"))
(def context (.getContext canvas "2d"))

(def width (atom 20))
(def height (atom 10))
(def cell-size 25)
(def world (atom {}))

(defn resized []
  (set! (.-width canvas) (* @width cell-size))
  (set! (.-height canvas) (* @height cell-size)))

(defn fill_sq [x y w h];; colour]
  (set! (.-fillStyle context) red)
  (set! (.-strokeStyle context) yellow)
  (.strokeRect context
               (* x cell-size)
               (* y cell-size)
               (* w cell-size)
               (* h cell-size)))

(defn fill_tri [x y w h];; colour]
  (println "DRAW TRI NUKKAH")
  (set! (.-fillStyle context) red)
  (set! (.-strokeStyle context) yellow)
  (let [tx (* cell-size x)
        ty (* cell-size y)
        tw (* cell-size w)
        th (* cell-size h)]
    (.beginPath context)
    (.moveTo context tx ty)
    (.lineTo context (+ tx tw) ty)
    (.lineTo context (+ tx (/ tw 2)) (+ ty th))
    (.lineTo context tx ty)
    (.stroke context)))

(defn fill_circ [x y w];; colour]
  (set! (.-fillStyle context) red)
  (set! (.-strokeStyle context) yellow)
  (let [r (/ (* cell-size w) 2)
        tx (+ r (* cell-size x))
        ty (- (* cell-size y) r)]
    (.beginPath context)
    (.arc context tx ty r 0 (* Math/PI 2) true)
    (.stroke context)))

(defn fill_txt [x y str]
  (set! (.-fillStyle context) yellow)
  (set! (.-font context) "10px sans-serif")
  (.fillText context str (+ 15 (* cell-size x)) (- (* cell-size y) 5)))

(defn deg->rad [d]
  (* Math/PI (/ d 360)))

(set! (.-onresize js/window) resized)

(resized)

(defn blank []
  (set! (.-fillStyle context) black)
  (.fillRect context
             0
             0
             (* cell-size @width)
             (* cell-size @height)))

(defn draw [block_map]
  (blank)
  (doseq [[k v] block_map]
    (let [x (first (:position @v))
          y (- @height (last (:position @v)))
          w (:width @v)
          h (- (:height @v))]
      (println (:type @v))
      (cond
        (= (:type @v) :clrdlu.core/brick)(fill_sq x y w h)
        (= (:type @v) :clrdlu.core/wedge)(fill_tri x y w h)
        (= (:type @v) :clrdlu.core/ball)(fill_circ x y w)
        :else
        (println "draw table")))
    (fill_txt (first (:position @v))
              (- @height (last (:position @v)))
              (:name @v))))




