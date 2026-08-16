;; The core module provides a nice GUI that allows the user to pick a ROM and start up the emulator.
(ns z80.core
  (:require [z80.emulator :as sms]
            [z80.emulator-gg :as gg]
            [clojure.edn :as edn])
  (:use [seesaw.core :only [dialog frame label menu menubar menu-item action alert show! native!]])
  (:use seesaw.chooser)
  (:gen-class))

(native!)

(def emulator-logo "tv-static.gif")

(defn run-rom-sms [rom-path]
  (sms/start-emulator rom-path))

(defn run-rom-gg [rom-path]
  (gg/start-emulator rom-path))

(def choose-sms-rom-action
  (action :name "Load Master System ROM" :key "menu O"
          :handler 
          (fn [e]
            (if-let [f (choose-file :dir "D:\\Sega\\Master System")]
              (run-rom-sms (str f))))))

(def choose-gg-rom-action
  (action :name "Load Game Gear ROM" :key "menu I"
          :handler 
          (fn [e]
            (if-let [f (choose-file :dir "D:\\Sega\\GameGear")]
              (run-rom-gg (str f))))))

(defn make-manu-bar []
  (menubar :items
           [(menu :text "File"
                  :items [choose-sms-rom-action choose-gg-rom-action (menu-item :text "Exit" :listen [:action (fn [event] (System/exit 0))])])
            (menu :text "Help"
                  :items [(menu-item :text "About" :listen [:action (fn [event] (alert "This program emulates a Sega Master System PAL console."))])])]))

(defn make-logo-label
  [logo-img-path]
  (label :icon (clojure.java.io/file logo-img-path)))

(defn make-main-window [child-widgets]
  (frame :title "DEFN-System launcher"
         :width 510
         :height 420
         :resizable? false
         :content child-widgets
         :menubar (make-manu-bar)
         :on-close :exit))

(defn -main
  [& args]
  (let [main-frame (make-main-window (make-logo-label emulator-logo))]
    (-> main-frame (.setLocationRelativeTo nil)) ;; Center the main window.
    (show! main-frame)
    (println "Done!"))) ;; Signal that we have loaded the program.
