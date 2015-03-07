(ns ^:figwheel-always dodgeball.core
    (:require[om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]))

(enable-console-print!)


(defonce app-state (atom {:balls [1 0 1 0 1 0 1 0 1]}))

(defn bench []
  (dom/table nil
    (dom/tr nil
      (vec (for [n (range 0 4)]
       (dom/td nil))))))


(defn cell-view [n owner]
  (om/component (dom/td nil n)))

(defn row-view [data owner]
  (om/component
    (apply dom/tr nil
      (om/build-all cell-view data))))

(defn board [data]
 (dom/div #js {:className "container"}
 (bench)
 (dom/table nil
    (vec (for [y (range 0 9)]
      (if (= y 5)
        (om/build row-view (:balls data))

        (dom/tr
          #js {:className
            (cond
              (or (= y 3) (= y 6)) "middle-top")}
          (vec (for [x (range 0 9)]
            (dom/td nil y))))))))
  (bench)))


(om/root
  (fn [data owner]
    (reify om/IRender
      (render [_]
        (board data))))
  app-state
  {:target (. js/document (getElementById "app"))})





