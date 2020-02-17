(ns faces.main
  (:require [reagent.core :as r]
            ["@material-ui/core/Button" :default Button]
            ["@material-ui/core/ButtonGroup" :default ButtonGroup]
            ["@material-ui/core/Typography" :default Typography]
            ["@material-ui/core/Slider" :default Slider]
            ["face-api.js" :as faceapi]
            ["victory" :refer [VictoryBar VictoryChart VictoryPie] :as V]
            [clojure.core.async :refer [go go-loop alts! chan <! >! promise-chan] :as a]))

(defn await [p]
  (let [c (promise-chan)]
    (.then p #(go (>! c ::complete)))
    c))

(defn p->c [p]
  (let [c (promise-chan)]
    (.then p #(go (>! c %)))
    c))

(defn get-webcam-stream
  []
  (let [c (chan)]
    (-> (.. js/navigator -mediaDevices (getUserMedia (clj->js {:audio false
                                                               :video {:facing-mode "user"}})))
        (.then #(go (>! c %))))
    c))

(def video-reference (atom nil))
(def analyzer-chan (atom nil))

(defn init-face-api!
  []
  (go
    (<! (await (.. faceapi -nets -ssdMobilenetv1 (loadFromUri "/"))))
    (<! (await (.. faceapi (loadFaceLandmarkModel "/"))))
    (<! (await (.. faceapi (loadFaceExpressionModel "/"))))))

(defn analyze-stream
  [video-ref results-chan]
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
              (>! results-chan results))
            (recur))
          (recur))))))

(defn stop-video!
  []
  (try
    (.stop (aget (.. @video-reference -srcObject getTracks) 0))
    (catch :default _)))

(defn video
  [results-chan]
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
                          (reset! analyzer-chan (analyze-stream ref results-chan)))))
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
    [:div
     [:h4 (str "Good: " good)]
     [:h4 (str "Bad: " bad)]]))

(defn app
  []
  (init-face-api!)
  (let [show? (r/atom false)
        expression-state (r/atom nil)
        expressions-chan (chan)
        _ (go-loop []
            (let [analysis (<! expressions-chan)]
              (reset! expression-state analysis)
              (recur)))]
    (fn []
      [:div.container {:style {:width "75vw"
                               :margin "auto"}}
       [:h1 "Faces"]
       [:div {:style {:display :flex
                      :align-items "center"}}
        [:div {:style {:width 640
                       :height 585}}
         (when @show?
           [:div
            [video expressions-chan]
            [summary expression-state]])]
        [:div {:style {:width 400
                       :height 400}}
         [chart expression-state]]]
       [:> ButtonGroup {:variant "contained" :color "primary"}
        [:> Button
         {:variant "contained"
          :color "primary"
          :on-click #(do
                       (when @show?
                         (stop-video!))
                       (swap! show? not))}
         (str "Turn " (if @show? "off" "on"))]]])))

(defn ^:dev/after-load start
  []
  (r/render [app]
            (.getElementById js/document "app")))

(defn ^:export init
  []
  (start))
