(ns ^:figwheel-always dodgeball.core
    (:require-macros [cljs.core.async.macros :refer [go]])
    (:require [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

(defonce app-state (atom
                    {:cells
                     {:blue-team
                      [{:coords {:x 1 :y 0}}
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
                      [{:coords {:x 1 :y 5}}
                       {:coords {:x 3 :y 5}}
                       {:coords {:x 5 :y 5}}
                       {:coords {:x 7 :y 5}}
                       {:coords {:x 9 :y 5}}]}}))

(def board-length (range 0 11))
(def board-width (range 0 9))

(def bench-size 4)
(def move-range 6)

(defn border-top [y]
  (cond
    (or (= y 4) (= y 7)) "middle-top"))

(defn bench []
  (dom/table nil
    (apply dom/tr nil
      (vec (for [n (range 0 bench-size)]
       (dom/td nil))))))

(defn unit? [kind coords]
  (first
   (filter
    #(= coords (:coords %))
    (kind (:cells @app-state)))))


(defn select-class [owner unit]
  (cond
   (=
    (:coords unit)
    (:coords (om/get-state owner :selected-unit)))
    "selected"))

(defn distance [a b]
  (Math/abs (- b a)))

(defn in-range? [target selected owner]
  (and
   (or
     (and
      (= :blue-team (om/get-state owner :turn))
      (<= (:y target) 7))
     (and
      (= :red-team (om/get-state owner :turn))
      (>= (:y target) 4)))
   (<=
    (reduce
     +
     (map distance (vals target) (vals (:coords selected))))
    move-range)))

(defn action-type [owner target]
  (let [selected (om/get-state owner :selected-unit)]
    (cond
     (and
       (= nil selected)
       (unit? (om/get-state owner :turn) target))
     :select-unit

     (unit? :balls target)
     :pickup-ball

     (in-range? target selected owner)
     :move-unit

     (or
       (unit? (om/get-state owner :turn) target)
       (not (in-range? target selected owner)))
     :deselect-unit)))

(defn tile-view [cursor owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [coords action-queue] :as unit}]
      (dom/td
       #js {:onClick
            #(put!
              action-queue
              {:coords coords
               :type (action-type owner coords)})}
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

(defn flip-team [owner]
  (om/set-state! owner :actions 0)
  (if (= (om/get-state owner :turn) :red-team)
    (om/set-state! owner :turn :blue-team)
    (om/set-state! owner :turn :red-team)))

(defn deselect-unit [owner]
  (om/set-state! owner :selected-unit nil))

(defn ball-in-front-of-unit? [ball-coords unit-coords owner]
  (and
   (=
    (:x ball-coords)
    (:x (:coords unit-coords)))
   (or
    (and
     (= (om/get-state owner :turn) :blue-team)
     (= (:y (:coords unit-coords)) (dec (:y ball-coords))))
    (and
     (= (om/get-state owner :turn) :red-team)
     (= (:y (:coords unit-coords)) (inc (:y ball-coords)))))))

(defn move-unit [app owner target]
  (om/transact!
   app
   [:cells (om/get-state owner :turn)]
   (fn [units]
     (map
      #(if (= (:coords (om/get-state owner :selected-unit)) (:coords %))
         ((fn []
            (om/update-state! owner :actions inc)
            (om/set-state! owner :selected-unit nil)
            (cond (= (om/get-state owner :actions) 2)
                  (flip-team owner))
            {:coords target}))
         %)
      units))))

(defn pickup-ball [app owner ball]
  (let [selected (om/get-state owner :selected-unit)]
  (om/transact!
   app
   [:cells :balls]
   (fn [units]
     (map
      #(if (ball-in-front-of-unit? ball selected owner)
         ((fn []
            (om/update-state! owner :actions inc)
            (om/set-state! owner :selected-unit nil)
            (cond (= (om/get-state owner :actions) 2)
                  (flip-team owner))
            {:coords selected}))
         %)
      units)))))

(defn select-unit [app owner selected]
  (om/set-state!
   owner
   :selected-unit
   (first
    (filter
     #(= selected (:coords %))
     ((om/get-state owner :turn) (:cells @app-state))))))

(defn handle-action [app owner action-queue]
  (go
   (loop []
     (let [event (<! action-queue)]
       (println event)
       (case (:type event)
         :select-unit   (select-unit app owner (:coords event))
         :deselect-unit (deselect-unit owner)
         :move-unit     (move-unit app owner (:coords event))
         :pickup-ball   (pickup-ball app owner (:coords event)))

       (recur)))))

(defn board-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:action-queue (chan)
       :selected-unit nil
       :actions 0
       :turn :blue-team})

    om/IWillMount
    (will-mount [_]
    (handle-action app owner (om/get-state owner :action-queue)))

    om/IRenderState
    (render-state [this {:keys [action-queue actions turn]}]
      (dom/div #js {:className "container"}
        (om/build turn-view (om/get-state owner :turn))
        (om/build action-view (om/get-state owner :actions))
        (bench)
        (apply dom/table nil
          (vec (for [y board-length]
            (apply dom/tr #js {:className (border-top y)}
              (vec (for [x board-length]
                (om/build tile-view app
                  {:state
                   {:selected-unit (om/get-state owner :selected-unit)
                    :action-queue action-queue
                    :actions actions
                    :turn turn
                    :coords {:x x :y y}}})))))))
        (bench)))))

(om/root board-view app-state
  {:target (. js/document (getElementById "app"))})
