(ns dodgeball.core
  (:require [goog.dom :as dom]
            [goog.events :as events]
            [cljs.core.async :refer [put! chan <!]]
            [dodgeball.unit :as unit]
            [dodgeball.state :as state]
            [dodgeball.actions :as actions])
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]]))

(def board-offset 80)
(def board-width 9)
(def board-height 5)
(def tile-size 64)

(def mouse-move-chan (chan))
(def mouse-down-chan (chan))


(def ctx (.getContext (dom/getElement "canvas") "2d"))
(set! (.-imageSmoothingEnabled ctx) false)

(defn reset-canvas []
  (.setTransform ctx 1, 0, 0, 1, 0.5, 0.5))

(defn draw-tile! [x y]
  (.beginPath ctx)
  (.rect ctx x y tile-size tile-size)
  (set! (.-fillStyle ctx) "white")
  (.fill ctx)
  (set! (.-lineWidth ctx) 0.5)
  (set! (.-strokeStyle ctx) "black")
  (.stroke ctx))

(defn unit-sprite-path [type]

  (case type
    :red "images/red-default.gif"
    :blue "images/blue-default.gif"
    :ball "images/ball.gif"))

(defn grid-to-pixels [x y]
  [(+ (+ (* x tile-size) board-offset) (/ tile-size 4))
   (* y tile-size)])

(defn draw-tiles! [xOffset yOffset w h]
  (reset-canvas)
  (.translate ctx xOffset yOffset)
  (doall
    (map
      (fn [y]
        (doall
          (map
            (fn [x] (draw-tile! (* tile-size x) (* tile-size y)))
            (range 0 w))))
      (range 0 h))))

(defn draw-unit! [unit]
  (let [player-sprite (new js/Image)]
    (set! (.-src player-sprite) (unit-sprite-path (:type unit)))
    ;(set! (.-onload player-sprite)
    player-sprite))

(defn redraw-unit! [unit]
  (reset-canvas)
  (let [player-sprite (new js/Image)
        [x y] (grid-to-pixels (:x unit) (:y unit))]
    (set! (.-src player-sprite) (unit-sprite-path (:type unit)))
    (set! (.-width player-sprite) "32px")
    (set! (.-width player-sprite) "64px")
    (.drawImage ctx player-sprite x y)
    player-sprite))

(defn draw-highlighted-tile! [color x y]
  (.beginPath ctx)
  (.rect ctx x y tile-size tile-size)
  (set! (.-fillStyle ctx) color)
  (.fill ctx)
  (set! (.-lineWidth ctx) 1)
  (set! (.-strokeStyle ctx) "black")
  (.stroke ctx))

(defn highlight-tile [color x y]
  (let [[pixel-x pixel-y] (grid-to-pixels x y)
        unit (unit/find-one-unit-by x y (:turn @state/game) @state/game)]
    (if (and (< x board-width) (>= x 0) (< y board-height) (>= y 0))
      (if unit
        (do
          (draw-highlighted-tile! color (- pixel-x (/ tile-size 4)) pixel-y)
          (redraw-unit! unit))
        (draw-highlighted-tile! color (- pixel-x (/ tile-size 4)) pixel-y)))))

(defn draw-screen []
  (.clearRect ctx 0 0 (.-width (dom/getElement "canvas")) (.-height (dom/getElement "canvas")))
  (draw-tiles! 0 50 1 3)
  (draw-tiles! board-offset 0 board-width board-height)
  (draw-tiles! 670 50 1 3)

  (if (not (:images-loaded @state/game))
    (do
      (doall (map draw-unit! (:units @state/game)))
      (swap! state/game assoc :images-loaded true))
    (doall (map redraw-unit! (:units @state/game))))
  )

(events/listen (dom/getElement "canvas") events/EventType.MOUSEDOWN #(put! mouse-down-chan %))
(events/listen (dom/getElement "canvas") events/EventType.MOUSEMOVE #(put! mouse-move-chan %))


(defn init! []
  (draw-screen)
  (apply highlight-tile "yellow" (:selected-tile @state/game))
  (go-loop []
           (alt!
             [mouse-down-chan] ([e] (println e))
             [mouse-move-chan] ([ch] (println "hovering")))
           (recur)))