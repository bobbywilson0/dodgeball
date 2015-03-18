(ns ^:figwheel-always dodgeball.core
    (:require-macros [cljs.core.async.macros :refer [go]])
    (:require [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

(defonce app-state (atom
                    {:cells
                        {:blue-team
                         [{:coords {:x 0, :y 0} :distance 6}
                          {:coords {:x 2, :y 0} :distance 6}
                          {:coords {:x 4, :y 0} :distance 6}
                          {:coords {:x 6, :y 0} :distance 6}
                          {:coords {:x 8, :y 0} :distance 6}]
                        :red-team
                         [{:coords {:x 0, :y 8} :distance 6}
                          {:coords {:x 2, :y 8} :distance 6}
                          {:coords {:x 4, :y 8} :distance 6}
                          {:coords {:x 6, :y 8} :distance 6}
                          {:coords {:x 8, :y 8} :distance 6}]
                        :balls
                         [{:coords {:x 0, :y 4}}
                          {:coords {:x 2, :y 4}}
                          {:coords {:x 4, :y 4}}
                          {:coords {:x 6, :y 4}}
                          {:coords {:x 8, :y 4}}]}}))

(def board-size (range 0 9))
(def bench-size 4)

(defn border-top [y]
  (cond
    (or (= y 3) (= y 6)) "middle-top"))

(defn bench []
  ( dom/table nil
    (apply dom/tr nil
      (vec (for [n (range 0 bench-size)]
       (dom/td nil))))))

(defn unit? [kind coords]
  (first (filter
          #(= coords (:coords %))
          (kind (:cells (om/root-cursor app-state))))))

(defn highlight-tile [e]
 (set! (.. e -target -style -background) "gray"))

(defn tile-view [cursor owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [coords move] :as position}]
      (dom/td #js {:onClick #(put! move position)}
        (if-let [ball (unit? :balls coords)]
          (dom/div #js {:className "ball"}))
        (if-let [red-player (unit? :red-team coords)]
          (dom/div #js {:onClick #(highlight-tile %) :className "red-team"}))
        (if-let [blue-player (unit? :blue-team coords)]
          (dom/div #js {:className "blue-team"}))))))



(defn board-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:move (chan)})

    om/IWillMount
    (will-mount [_]
     (let [move (om/get-state owner :move)]
       (go (loop []
          (let [selected (:coords (<! move))
                target (:coords (<! move))]
            (if (unit? :red-team target)
              (println "CAN'T MOVE THERE")
              (om/transact! app [:cells :red-team]
                (fn [units]
                  (map
                   #(if (and
                         (= selected (:coords %))
                         (<= (reduce + (map (fn [a b] (Math/abs (- b a))) (vals target) (vals selected))) (:distance %)))
                      (conj % {:coords target})
                      %)
                   units)))))
          (recur)))))

    om/IRenderState
    (render-state [this {:keys [move]}]
      (dom/div #js {:className "container"}
        (bench)
        (apply dom/table nil
          (vec (for [y board-size]
            (apply dom/tr #js {:className (border-top y)}
              (vec (for [x board-size]
                (om/build tile-view app
                  {:state {:move move :coords {:x x :y y}}})))))))
        (bench)))))

(om/root board-view app-state
  {:target (. js/document (getElementById "app"))})
