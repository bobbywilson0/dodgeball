(ns ^:figwheel-always dodgeball.core
    (:require[om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]))

(enable-console-print!)

(defonce app-state (atom
                    {:cells
                      {:team-a
                         [{:player 1 :x 0, :y 0}
                          {:player 2 :x 2, :y 0}
                          {:player 3 :x 4, :y 0}
                          {:player 4 :x 6, :y 0}
                          {:player 5 :x 8, :y 0}]
                       :team-b
                         [{:player 1 :x 0, :y 8}
                          {:player 2 :x 2, :y 8}
                          {:player 3 :x 4, :y 8}
                          {:player 4 :x 6, :y 8}
                          {:player 5 :x 8, :y 8}]
                       :loose-balls
                         [{:ball 1 :x 0, :y 4}
                         {:ball 2 :x 2, :y 4}
                         {:ball 3 :x 4, :y 4}
                         {:ball 4 :x 6, :y 4}
                         {:ball 5 :x 8, :y 4}]}
                       :selected-unit {:x 0 :y 0}}))

(def board-size (range 0 9))
(def bench-size 4)
(def player-image "images/back-noball.gif")
(def ball-image "images/ball.gif")

(defn border-top [y]
  (cond
    (or (= y 3) (= y 6)) "middle-top"))

(defn players [data]
  (concat (:team-a (:cells data)) (:team-b (:cells data))))

(defn bench []
  (dom/table nil
    (dom/tr nil
      (vec (for [n (range 0 bench-size)]
       (dom/td nil))))))

(defn blank-unit [data owner]
  (reify
    om/IInitState
    (init-state [_]
                {:init-state {:x (data :x) :y (data :y)}})
    om/IRender
    (render [_]
      (dom/td nil))))

(defn player-unit [data owner]
  (reify
    om/IInitState
    (init-state [_]
                {:position {:x (data :x) :y (data :y)}})
    om/IRenderState
    (render-state [_ _]
      (if (:player data)
        (dom/td #js {:onClick #(om/update! (om/ref-cursor (:selected-unit (om/root-cursor app-state))) data)}
          (dom/img #js {:src player-image }))
        (om/build blank-unit data)))))


(defn ball-unit [data owner]
  (reify
;;     om/IInitState
;;     (init-state [_]
;;       {:x (data :x) :y (data :y)})
    om/IRender
    (render [_]
      (if (:ball data)
        (dom/td nil
          (dom/img #js {:src ball-image}))
        (om/build blank-unit data)))))

(defn find-unit [x y data]
  (let [found (first
                (filter
                  #(and (= (:x %) x)
                        (= (:y %) y))
                data))]
    (if found
      found
      {:x x :y y})))


(defn board [data]
  (dom/div #js {:className "container"}
  (bench)
  (dom/table nil
    (vec (for [y board-size]
      (dom/tr #js {:className (border-top y)}
        (vec (for [x board-size]
          (if (= y 4)
            (om/build ball-unit
              (find-unit x y (:loose-balls (:cells data))))
            (om/build player-unit
              (find-unit x y (players data)) {:init-state {:x x :y y}}))))))))
  (bench)))


(om/root
  (fn [data owner]
    (reify om/IRender
      (render [_]
        (board data))))
  app-state
  {:target (. js/document (getElementById "app"))})


