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
                         [{:x 0, :y 4}
                         {:x 2, :y 4}
                         {:x 4, :y 4}
                         {:x 6, :y 4}
                         {:x 8, :y 4}]
                     :selected-unit {}}}))

(def board-width 9)
(def board-height 9)
(def bench-size 4)
(def player-image "images/back-noball.gif")
(def ball-image "images/ball.gif")



(defn bench []
  (dom/table nil
    (dom/tr nil
      (vec (for [n (range 0 bench-size)]
       (dom/td nil))))))


(defn player [data owner]
  (do (println data))
  (if data
    (om/component (dom/td #js {:onClick #(om/set-state! owner :selected-unit data) }
                          (dom/img #js {:src player-image })))
    (om/component (dom/td #js {:onClick #(println data) }
                          ))))

(defn ball [data owner]
  (om/component (dom/td nil
                        (if data
                          (dom/img #js {:src ball-image})))))

(defn find-cell [x y data]
  (first
    (filter #(and (= (:x %) x)
                  (= (:y %) y))
            data)))


(defn board [data]
  (dom/div #js {:className "container"}
  (bench)
  (dom/table nil
    (vec (for [y (range 0 board-height)]
      (dom/tr
        #js {:className
          (cond
            (or (= y 3) (= y 6)) "middle-top")}
              (vec (for [x (range 0 board-width)]
                (if (= y 4)
                  (om/build ball
                    (find-cell x y (concat [:x x] [:y y] (:loose-balls (:cells data)))))
                  (om/build player
                    (concat (:team-a (:cells data)) (:team-b (:cells data)))))))))))
  (bench)))


(om/root
  (fn [data owner]
    (reify om/IRender
      (render [_]
        (board data))))
  app-state
  {:target (. js/document (getElementById "app"))})





