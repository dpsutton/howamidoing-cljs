(ns faces.main
  (:require [reagent.core :as r]
            ["@material-ui/core/Button" :default Button]
            ["@material-ui/core/ButtonGroup" :default ButtonGroup]
            ["@material-ui/core/Typography" :default Typography]
            ["@material-ui/core/Slider" :default Slider]
            ["face-api.js" :as faceapi]
            [clojure.core.async :refer [go chan <! >!] :as a]))

(defn get-webcam-stream
  []
  (let [c (chan)]
    (-> (.. js/navigator -mediaDevices (getUserMedia (clj->js {:audio false
                                                               :video {:facing-mode "user"}})))
        (.then #(go (>! c %))))
    c))

(def video-reference (atom nil))

(defn init-face-api!
  []
  (let [c (chan)]
    ))


(defn video
  []
  [:video {:id "inputVideo"
           :auto-play true
           :muted true
           :plays-inline true
           :ref (fn [ref]
                  (if ref
                    (do
                      (reset! video-reference ref)
                      (init-face-api!)
                      (go
                        (let [webcam-stream (<! (get-webcam-stream))]
                          (set! (.-srcObject ref) webcam-stream))))
                    (do (.stop (aget (.. @video-reference -srcObject getTracks) 0))
                        (set! (.-srcObject @video-reference) nil))))}])

(defn app
  []
  (let [show? (r/atom false)]
    (fn []
      [:div.container {:style {:width "75vw"
                               :margin "auto"}}
       [:h1 "Faces"]
       [:div
        (when @show?
          [video])]
       [:> ButtonGroup {:variant "contained" :color "primary"}
        [:> Button
         {:variant "contained"
          :color "primary"}
         "Button"]
        [:> Button
         {:variant "contained"
          :color "primary"}
         "Button 2"]
        [:> Button
         {:variant "contained"
          :color "primary"
          :on-click #(swap! show? not)}
         (str "Turn " (if @show? "off" "on"))]]])))

(defn ^:dev/after-load start
  []
  (r/render [app]
            (.getElementById js/document "app")))

(defn ^:export init
  []
  (start))
