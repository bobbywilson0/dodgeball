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

(def bench-top-offset 50)
(def bench-x-offset 670)

(defn out-of-bounds? [x y]
  (or
    (< x 0)
    (> x 8)
    (< y 0)
    (> y 4)))

(def mouse-move-chan (chan))
(def mouse-down-chan (chan))


(def ctx (.getContext (dom/getElement "canvas") "2d"))
(set! (.-imageSmoothingEnabled ctx) false)

(defn reset-canvas []
  (.setTransform ctx 1, 0, 0, 1, 0.5, 0.5))

(defn clear-screen! []
  (.clearRect ctx 0 0 (.-width (dom/getElement "canvas")) (.-height (dom/getElement "canvas"))))

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

(defn draw-tile! [x y]
;  (reset-canvas)
  (let [tile-sprite (new js/Image)]
    (set! (.-src tile-sprite) "images/tile.gif")
    (.drawImage ctx tile-sprite x y)
    tile-sprite))

(defn pixels-to-grid [x y]
  [(Math/floor (/ (- x board-offset) tile-size))
   (Math/floor (/ y tile-size))])

(defn draw-tiles! [xOffset yOffset w h]
  (reset-canvas)
  (.translate ctx xOffset yOffset)
    (mapv
      (fn [y]
        (mapv
          (fn [x] (draw-tile! (* tile-size x) (* tile-size y)))
          (range 0 w)))
      (range 0 h)))

;(defn draw-unit! [unit]
;  (let [player-sprite (new js/Image)]
;    (set! (.-src player-sprite) (unit-sprite-path (:type unit)))
;    player-sprite))

(defn draw-unit! [unit]
  (reset-canvas)
  (let [player-sprite (new js/Image)
        [x y] (grid-to-pixels (:x unit) (:y unit))]
    (set! (.-src player-sprite) (unit-sprite-path (:type unit)))
    (set! (.-width player-sprite) "32px")
    (set! (.-height player-sprite) "64px")
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
        unit (unit/unit? x y @state/game)]
    (if (not (out-of-bounds? x y))
      (if unit
        (do
          (draw-highlighted-tile! color (- pixel-x (/ tile-size 4)) pixel-y)
          (draw-unit! unit))
        (draw-highlighted-tile! color (- pixel-x (/ tile-size 4)) pixel-y)))))

(defn draw-screen []
  ;draw left bench
  (draw-tiles! 0 bench-top-offset 1 3)
  ;draw blank board
  (draw-tiles! board-offset 0 board-width board-height)
  ;draw right bench
  (draw-tiles! bench-x-offset bench-top-offset 1 3)

  (mapv draw-unit! (:units @state/game)))
  (if (:selected-unit @state/game)
    (highlight-tile "yellow" (:x (:selected-unit @state/game)) (:y (:selected-unit @state/game))))

(defn mouse-move-highlight [e]
  (let [highlight-position (pixels-to-grid (.-offsetX e) (.-offsetY e))]
    (if (not= (:highlighted-unit @state/game) highlight-position)
      (do
        (draw-screen)
        (swap! state/game assoc :highlighted-unit highlight-position)
        (apply highlight-tile "#eee" highlight-position)))))

(defn init! []
  (events/listen (dom/getElement "canvas") events/EventType.MOUSEDOWN #(put! mouse-down-chan %))
  (events/listen (dom/getElement "canvas") events/EventType.MOUSEMOVE #(put! mouse-move-chan %))


  (draw-screen)
  (go-loop []
           (alt!
             [mouse-down-chan] ([event] (actions/determine-action (pixels-to-grid (.-offsetX event) (.-offsetY event))))
             [mouse-move-chan] ([event] (mouse-move-highlight event)))
           (recur)))
