(ns ^:figwheel-always dodgeball.core
    (:require[om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]))

(enable-console-print!)


(defonce app-state (atom {:balls [1 3 5 7 9]}))

(defn bench []
  (dom/table nil
    (dom/tr nil
      (vec (for [n (range 0 4)]
       (dom/td nil))))))


(defn board []
 (dom/div #js {:className "container"}
 (bench)
 (dom/table nil
    (vec (for [y (range 0 9)]
      (dom/tr
        #js {:className
          (cond
            (= y 3) "middle-top"
            (= y 5) "middle-bottom")}

        (vec (for [x (range 0 9)]
          (dom/td nil)))))))
  (bench)))

(defn row-view [data owner]
  (reify
    om/IRender
    (render [this]
      (apply dom/tr nil
        (om/build-all square-view (:balls data))))))

(defn square-view [square owner]
  (reify
    (om/IRender
     (render [this]
             (dom/td nil
                     (if "ball")))))

(om/root row-view app-state
         {:target (. js/document (getElementById "container"))})



(om/root
  (fn [data owner]
    (reify om/IRender
      (render [_]
        (board))))
  app-state
  {:target (. js/document (getElementById "app"))})





