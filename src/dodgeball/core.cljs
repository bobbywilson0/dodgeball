(ns ^:figwheel-always dodgeball.core
    (:require-macros [cljs.core.async.macros :refer [go]])
    (:require [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

(defonce app-state (atom
                    {:cells
                      [{:type :opponent :x 0, :y 0}
                       {:type :opponent :x 2, :y 0}
                       {:type :opponent :x 4, :y 0}
                       {:type :opponent :x 6, :y 0}
                       {:type :opponent :x 8, :y 0}
                       {:type :my-team :x 0, :y 8}
                       {:type :my-team :x 2, :y 8}
                       {:type :my-team :x 4, :y 8}
                       {:type :my-team :x 6, :y 8}
                       {:type :my-team :x 8, :y 8}
                       {:type :loose-ball :x 0, :y 4}
                       {:type :loose-ball :x 2, :y 4}
                       {:type :loose-ball :x 4, :y 4}
                       {:type :loose-ball :x 6, :y 4}
                       {:type :loose-ball :x 8, :y 4}]
                    :selected-unit {}}))

(def board-size (range 0 9))
(def bench-size 4)
(def opponent-image "images/front-noball.gif")
(def player-image "images/back-noball.gif")
(def ball-image "images/ball.gif")

(defn border-top [y]
  (cond
    (or (= y 3) (= y 6)) "middle-top"))

(defn bench []
  (dom/table nil
    (dom/tr nil
      (vec (for [n (range 0 bench-size)]
       (dom/td nil))))))

(defn find-piece [position]
  (first (filter #(and
                 (= (:x position) (:x %))
                 (= (:y position) (:y %)))
               (:cells (om/root-cursor app-state)))))


(defn highlight-tile [e unit]
  (let [cursor (om/ref-cursor (:selected-unit (om/root-cursor app-state)))]
    (om/update! cursor unit))
    (set! (-> e .-target .-parentElement .-style .-border) "2px solid #f00"))

(defn update-piece-position [e state]
  (om/transact! (om/root-cursor app-state) :cells #(conj {:type :my-team :x 1 :y 8})))

(defn ball-view [cursor owner]
  (reify
    om/IRender
    (render [_]
      (dom/img #js {:src ball-image}))))

(defn player-view [cursor owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [move]}]
      (println cursor)
      (dom/img #js {:src player-image :onClick (fn [_]
                                                 (println "hello")
                                                 (put! move @cursor))}))))

(defn opponent-view [cursor owner]
  (reify
    om/IRender
    (render [_]
      (dom/img #js {:src opponent-image}))))

(defn tile-view [cursor owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [x y move] :as position}]
      (dom/td nil
        (let [found (find-piece position)]
          (cond
            (= (:type found) :my-team)
              (om/build player-view found {:state {:move move}})
            (= (:type found) :opponent)
               (om/build opponent-view cursor)
            (= (:type found) :loose-ball)
               (om/build ball-view cursor)))))))


(defn ball-view [cursor owner]
  (reify
    om/IRender
    (render [_]
          (dom/img #js {:src ball-image}))))

(defn board-view [app owner]
  (reify
     om/IInitState
     (init-state [_]
       {:move (chan)})
    om/IWillMount
    (will-mount [_]
      (let [move (om/get-state owner :move)]
        (go (loop []

          (let [player (<! move)]
            (println player)
            (om/transact! app :cells
              (fn [cs] (vec (remove #(= player %) cs))))
            (recur))))))
    om/IRenderState
    (render-state [this {:keys [move]}]
      (dom/div #js {:className "container"}
        (bench)
        (dom/table nil
          (vec (for [y board-size]
            (dom/tr #js {:className (border-top y)}
              (vec (for [x board-size]
                (om/build tile-view (:cells app)
                  {:state {:move move :x x :y y}})))))))
        (bench)))))

(om/root board-view app-state
  {:target (. js/document (getElementById "app"))})
