(ns ^:figwheel-always dodgeball.core
    (:require-macros [cljs.core.async.macros :refer [go]])
    (:require [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

(defonce app-state (atom
                    {:cells
                        {:blue-team
                         [{:coords {:x 1  :y 0}}
                          {:coords {:x 3 :y 0}}
                          {:coords {:x 5 :y 0}}
                          {:coords {:x 7 :y 0}}
                          {:coords {:x 9 :y 0}}]
                        :red-team
                          [{:coords {:x 1, :y 10}}
                          {:coords {:x 3, :y 10}}
                          {:coords {:x 5, :y 10}}
                          {:coords {:x 7, :y 10}}
                          {:coords {:x 9, :y 10}}]
                        :balls
                         [{:coords {:x 1  :y 5}}
                          {:coords {:x 3 :y 5}}
                          {:coords {:x 5 :y 5}}
                          {:coords {:x 7 :y 5}}
                          {:coords {:x 9 :y 5}}]}
                     :turn :blue-team
                     :actions 0}))

(def board-length (range 0 11))
(def board-width (range 0 9))

(def bench-size 4)
(def move-range 6)

(defn border-top [y]
  (cond
    (or (= y 4) (= y 7)) "middle-top"))

(defn bench []
  ( dom/table nil
    (apply dom/tr nil
      (vec (for [n (range 0 bench-size)]
       (dom/td nil))))))

(defn unit? [kind coords]
  (first (filter
          #(= coords (:coords %))
          (kind (:cells (om/root-cursor app-state))))))

(defn select-class [owner unit]
  (if (=
       (:coords unit)
       (:coords (om/get-state owner :selected-unit)))
    "selected"))

(defn tile-view [cursor owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [coords move] :as unit}]
      (dom/td #js {:onClick #(put! move unit)}
        (if-let [ball (unit? :balls coords)]
          (dom/div #js {:className "ball"}))
        (if-let [red-player (unit? :red-team coords)]
          (dom/div
           #js {
           :className (str "red-team" " " (select-class owner unit))}))
        (if-let [blue-player (unit? :blue-team coords)]
          (dom/div
           #js {
           :className (str "blue-team" " " (select-class owner unit))}))))))

(defn distance [a b]
  (Math/abs (- b a)))

(defn in-range? [target selected]
  (<=
   (reduce
    +
    (map distance (vals target) (vals selected)))
   move-range))


(defn turn-view [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/h2 nil (str "Turn: " data)))))

(defn action-view [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/h2 nil (str "Actions: " data)))))

(defn board-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:move (chan)
       :selected-unit nil})

    om/IWillMount
    (will-mount [_]
     (let [move (om/get-state owner :move)]
       (go (loop []
          (let [selected (:coords (<! move))]
            (om/set-state! owner :selected-unit (first (filter #(= selected (:coords %)) ((:turn app) (:cells @app-state)))))
            (let [target (:coords (<! move))]
            (if (or
                 (unit? (:turn app) target)
                 (not (in-range? target selected)))
              (om/set-state! owner :selected-unit nil)
               (om/transact!
               app
               [:cells (:turn app)]
               (fn [units]
                  (map
                   #(if (= (om/get-state owner :selected-unit) %)
                      {:coords target}
                      %)
                   units))))
              (om/transact! app :actions (fn [a] (inc a)))))
          (recur)))))

    om/IRenderState
    (render-state [this {:keys [move]}]
      (dom/div #js {:className "container"}
        (om/build turn-view (:turn app))
        (om/build action-view (:actions app))
        (bench)
        (apply dom/table nil
          (vec (for [y board-length]
            (apply dom/tr #js {:className (border-top y)}
              (vec (for [x board-length]
                (om/build tile-view app
                  {:state {:selected-unit (om/get-state owner :selected-unit) :move move :coords {:x x :y y}}})))))))
        (bench)))))

(om/root board-view app-state
  {:target (. js/document (getElementById "app"))})
