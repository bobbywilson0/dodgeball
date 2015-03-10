(ns ^:figwheel-always dodgeball.core
    (:require[om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]))

(enable-console-print!)


(defonce app-state (atom
                    {:cells
                      [{:x 0, :y 0, :team \a, :image "back-noball.gif"}
                       {:x 2, :y 0, :team \a, :image "back-noball.gif"}
                       {:x 4, :y 0, :team \a, :image "back-noball.gif"}
                       {:x 6, :y 0, :team \a, :image "back-noball.gif"}
                       {:x 8, :y 0, :team \a, :image "back-noball.gif"}
                       {:x 0, :y 8, :team \b, :image "back-noball.gif"}
                       {:x 2, :y 8, :team \b, :image "back-noball.gif"}
                       {:x 4, :y 8, :team \b, :image "back-noball.gif"}
                       {:x 6, :y 8, :team \b, :image "back-noball.gif"}
                       {:x 8, :y 8, :team \b, :image "back-noball.gif"}
                       {:x 0, :y 4, :ball true, :image "ball.gif"}
                       {:x 2, :y 4, :ball true, :image "ball.gif"}
                       {:x 4, :y 4, :ball true, :image "ball.gif"}
                       {:x 6, :y 4, :ball true, :image "ball.gif"}
                       {:x 8, :y 4, :ball true, :image "ball.gif"}]}))

(def board-width 9)
(def board-height 9)
(def bench-size 4)

(defn bench []
  (dom/table nil
    (dom/tr nil
      (vec (for [n (range 0 bench-size)]
       (dom/td nil))))))


(defn cell-view [data owner]
  (om/component (dom/td nil
                        (if data
                          (dom/img #js {:src (str "images/" (:image data))})))))

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
          (om/build cell-view
                    (find-cell x y (:cells data))))))))
  (bench))))


(om/root
  (fn [data owner]
    (reify om/IRender
      (render [_]
        (board data))))
  app-state
  {:target (. js/document (getElementById "app"))})





