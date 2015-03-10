(ns ^:figwheel-always dodgeball.core
    (:require[om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]))

(enable-console-print!)


(defonce app-state (atom
                    {:team1 [{:x 0, :y 0, :has-ball false}
                             {:x 2, :y 0, :has-ball false}
                             {:x 4, :y 0, :has-ball false}
                             {:x 6, :y 0, :has-ball false}
                             {:x 8, :y 0, :has-ball false}]
                     :team2 [{:x 0, :y 8, :has-ball false}
                             {:x 2, :y 8, :has-ball false}
                             {:x 4, :y 8, :has-ball false}
                             {:x 6, :y 8, :has-ball false}
                             {:x 8, :y 8, :has-ball false}]
                     :loose-balls [{:x 0, :y 4}
                                   {:x 2, :y 4}
                                   {:x 4, :y 4}
                                   {:x 6, :y 4}
                                   {:x 8, :y 4}]}))

(def board-width 9)
(def board-height 9)
(def bench-size 4)

(defn bench []
  (dom/table nil
    (dom/tr nil
      (vec (for [n (range 0 bench-size)]
       (dom/td nil))))))


(defn cell-view [n owner]
  (om/component (dom/td nil n)))


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
          (om/build cell-view y)))))))
  (bench)))


(om/root
  (fn [data owner]
    (reify om/IRender
      (render [_]
        (board data))))
  app-state
  {:target (. js/document (getElementById "app"))})





