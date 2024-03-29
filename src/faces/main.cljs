(ns faces.main
  (:require [reagent.core :as r]
            ["@material-ui/core/Button" :default Button]
            ["@material-ui/core/ButtonGroup" :default ButtonGroup]
            ["@material-ui/core/Typography" :default Typography]
            ["@material-ui/core/LinearProgress" :default LinearProgress]
            ["@material-ui/core/Slider" :default Slider]
            ["face-api.js" :as faceapi]
            ["victory" :refer [VictoryBar VictoryChart VictoryPie] :as V]
            [clojure.core.async :refer [go go-loop alts! chan <! >! promise-chan] :as a]))

;; async helpers

(defn await [p]
  (let [c (promise-chan)]
    (.then p #(go (>! c ::complete)))
    c))

(defn p->c [p]
  (let [c (promise-chan)]
    (.then p #(go (>! c %)))
    c))

;; state

(def video-reference (atom nil))
(def analyzer-chan (atom nil))
(def state (r/atom {:show? false
                    :outlines? false
                    :loaded? false}))

(defn init-face-api!
  [on-complete]
  (go
    (<! (await (.. faceapi -nets -ssdMobilenetv1 (loadFromUri "/"))))
    (<! (await (.. faceapi (loadFaceLandmarkModel "/"))))
    (<! (await (.. faceapi (loadFaceExpressionModel "/"))))
    (on-complete)))

(defn get-webcam-stream
  []
  (p->c (.. js/navigator -mediaDevices
            (getUserMedia (clj->js {:audio false
                                    :video {:facing-mode "user"}})))))

(defn analyze-stream
  [video-ref results-chan overlay-chan]
  (let [min-confidence 0.5]
    (go-loop []
      (let [options (new (.-SsdMobilenetv1Options faceapi) #js {:minConfidence min-confidence})
            timeout-c (a/timeout 1000)
            [result c] (alts! [(p->c (.. faceapi
                                         (detectAllFaces video-ref options)
                                         withFaceExpressions))
                               timeout-c])]
        (if (and result (pos? (count result)))
          (do
            (let [results (->> result
                               (filter #(> (.. % -detection -score) min-confidence))
                               (map (comp first
                                          #(js->clj % :keywordize-keys true)
                                          #(.. % -expressions asSortedArray))))]
              ;; put simplified results onto results chan for others
              ;; (chart, good/bad counts) and the raw results onto the
              ;; overlay chan to draw boxes if desired
              (>! results-chan results)
              (>! overlay-chan result))
            (recur))
          (recur))))))

(defn stop-video!
  []
  (try
    (.stop (aget (.. @video-reference -srcObject getTracks) 0))
    (catch :default _)))

(defn video
  [results-chan overlay-chan]
  [:video {:id "inputVideo"
           :auto-play true
           :muted true
           :plays-inline true
           :ref (fn [ref]
                  (if ref
                    (do
                      (reset! video-reference ref)
                      (go
                        (let [webcam-stream (<! (get-webcam-stream))]
                          (set! (.-srcObject ref) webcam-stream)
                          (reset! analyzer-chan (analyze-stream ref results-chan overlay-chan)))))
                    (when @video-reference
                      (when @analyzer-chan
                        (a/close! @analyzer-chan))
                      (stop-video!)
                      (set! (.-srcObject @video-reference) nil))))}])

(defn chart
  [expression-state]
  (let [data (->> @expression-state (map :expression) frequencies)]
    [:> VictoryPie {:data (clj->js (into []
                                         (map (fn [[k cnt]]
                                                {:x k :y cnt}))
                                         data))
                    :style (clj->js {:labels {:fontSize 20
                                              :fontWeight "bold"}})
                    :colorScale ["#029832", "#62b32b", "#C7EA46", "#fedb00", "#f97a00", "#ff5349", "#d50218"]
                    :animate #js{:duration 200}}]))

(defn summary
  [state]
  (let [{:keys [good bad]} (reduce (fn [s {:keys [expression]}]
                                     (update s
                                             (if (#{"happy" "neutral" "surprised"} expression)
                                               :good
                                               :bad)
                                             inc))
                                   {:good 0 :bad 0}
                                   @state)]
    [:div {:style {:margin-top "20px"}}
     [:h4 (str "Good: " good)]
     [:h4 (str "Bad: " bad)]]))

(defn overlay
  [state overlay-chan]
  (let [overlay-reference (atom nil)
        worker (go-loop []
                 (let [faces (<! overlay-chan)]
                   (when (and @overlay-reference (:outlines? @state))
                     (let [dims (.matchDimensions faceapi @overlay-reference @video-reference true)
                           resizedResult (.resizeResults faceapi faces dims)]
                       (.. faceapi -draw (drawDetections @overlay-reference resizedResult))
                       (.. faceapi -draw (drawFaceExpressions @overlay-reference resizedResult 0.5))))
                   (recur)))]
    (r/create-class
      {:reagent-render
       (fn [state _]
         [:canvas#overlay
          {:style {:position "absolute"
                   :visibility (if (:outlines? @state) "visible" "hidden")
                   :top 0 :left 0 :width "100%" :height "100%"}
           :ref (fn [ref] (reset! overlay-reference ref))}])
       :component-will-unmount
       (fn [_]
         (a/close! worker))})))

(defn app
  []
  (let [expression-state (r/atom nil)
        expressions-chan (chan)
        overlay-chan (chan (a/sliding-buffer 1))
        _ (go-loop []
            (let [analysis (<! expressions-chan)]
              (reset! expression-state analysis)
              (recur)))]
    (init-face-api! #(swap! state assoc :loaded? true))
    (fn []
      [:div.container {:style {:width "75vw"
                               :margin "auto"}}
       (when-not (:loaded? @state)
        [:> LinearProgress])
       [:h1 "Faces"]
       [:div {:style {:display :flex
                      :align-items "center"}}
        [:div {:style {:width 640
                       :height 485
                       :position "relative"}}
         (when (:show? @state)
           [:div
            [video expressions-chan overlay-chan]
            [overlay state overlay-chan]
            [summary expression-state]])]
        [:div {:style {:width 400
                       :height 400}}
         [chart expression-state]]]
       [:> ButtonGroup {:style {:margin-top "85px"}
                        :variant "contained" :color "primary"}
        [:> Button
         {:variant "contained"
          :color "primary"
          :on-click #(do
                       (when (:show? @state)
                         (stop-video!))
                       (swap! state update :show? not))}
         (str "Turn " (if (:show? @state) "off" "on"))]
        [:> Button
         {:variant "contained"
          :color "primary"
          :on-click #(swap! state assoc :outlines? true)}
         "Show outlines"]
        [:> Button
         {:variant "contained"
          :color "primary"
          :on-click #(swap! state assoc :outlines? false)}
         "Hide outlines"]]])))

(defn ^:dev/after-load start
  []
  (r/render [app]
            (.getElementById js/document "app")))

(defn ^:export init
  []
  (start))
