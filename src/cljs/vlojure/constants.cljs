(ns vlojure.constants
  (:require [vlojure.vedn :as vedn]))

(def char-escape-string {\newline "\\n" \tab "\\t" \return "\\r" \" "\\\"" \\ "\\\\" \formfeed "\\f" \backspace "\\b"})

(def color-schemes
  [{:name "Espresso"
    :background 0x221d1d
    :foreground 0x4e3834
    :highlight 0x6b3931
    :text 0xe7b78a}
   {:name "Turtle"
    :background 0x0b1612
    :foreground 0x1e3529
    :highlight 0x2e4136
    :text 0x697f6e}
   {:name "Hemoglobin"
    :background 0x120109
    :foreground 0x61112a
    :highlight 0x803738
    :text 0xca7e74}
   {:name "Egg"
    :background 0xf5f5c9
    :foreground 0xe3c14d
    :highlight 0xbb8521
    :text 0xf5f5c9}
   {:name "Void"
    :background 0x000000
    :foreground 0x222222
    :highlight 0x383838
    :text 0x999999}
   {:name "Blank"
    :background 0xffffff
    :foreground 0xcccccc
    :highlight 0x999999
    :text 0x444444}])

(def font-name "monoid-bold")

(def screen-sides [:bottom :right :top :left])

(def ui-layers [:background :program :drag-forms :formbar :menu :drag :settings-overlay])

(def subform-shrink-factor 0.95)
(def sole-subform-shrink-factor 0.75)

(def bubble-thickness 0.05)

(def vector-size-factor 1.1)

(def map-point-height 0.15)
(def map-point-width 0.075)

(def set-line-length 0.1)
(def set-line-width 0.1)
(def set-line-offset 0.125)

(def text-scale-factor 0.24)
(def text-max-size 0.135)
(def text-x-offset -0.4)
(def text-y-offset 0.65)

(def html-text-size-factor 9.6)
(def html-text-x-offset -0.04)
(def html-text-y-offset -0.0025)
(def html-text-height-factor 1.1)

(def text-input-adjustment-factor 5)

(def min-drag-dist 0.05)
(def drag-cursor-line-width 0.0075)
(def drag-cursor-radius 0.015)

(def drop-form-radius-factor 0.5)
(def drop-form-offset-factor 1.2)
(def drop-form-outline-radius-factor 1.1)

(def formbar-pos 0.2)
(def formbar-spacing 1.1)
(def formbar-outline-thickness 0.15)
(def formbar-form-size 0.85)
(def formbar-form-placement-offset 0.9)
(def formbar-form-placement-size 0.55)

(def formbar-placement-circle-radius 0.075)

(def lower-corner-zone-radius 0.15)
(def upper-corner-zone-radius 0.075)
(def corner-zone-bar-thickness 0.05)

(def discard-zone-form-radius-factor 0.9)
(def discard-zone-icon-radius-factor 0.7)
(def discard-zone-icon-thickness 0.3)

(def eval-zone-max-radius 0.6)
(def eval-zone-speed (/ (Math/pow 10 5)))
(def eval-zone-form-radius-factor 0.9)
(def eval-zone-caret-factor 0.6)
(def eval-zone-caret-offset -0.25)
(def eval-zone-icon-thickness 0.15)
(def eval-zone-icon-angle (* 0.35 Math/PI))
(def eval-zone-underscore-x-offset -0.1)
(def eval-zone-underscore-y-offset 0.075)
(def eval-zone-underscore-size 0.7)

(def settings-zone-icon-radius-factor 0.5)
(def settings-zone-icon-inner-radius-factor 0.5)
(def settings-zone-icon-spokes 8)
(def settings-zone-icon-spoke-length-factor 0.7)
(def settings-zone-icon-spoke-width-factor 0.25)

(def settings-sliders [["Base Zoom" :base-zoom]
                       ["Formbar Size" :formbar-radius]
                       ["Camera Speed" :camera-speed]])

(def settings-top-slider-y -0.44)
(def settings-slider-spacing 0.45)
(def settings-slider-width 0.6)
(def settings-slider-radius 0.1)
(def settings-slider-inner-radius-factor 0.75)
(def settings-slider-text-y -0.18)
(def settings-slider-text-size 0.06)

(def settings-formbar-command-types [:temp :temp :temp :temp :temp :temp :temp :temp :temp :temp :temp :temp])
(def settings-formbar-commands-per-row 4)
(def settings-formbar-command-text-y -0.575)
(def settings-formbar-command-text-size 0.9)
(def settings-formbar-command-y -0.3)
(def settings-formbar-command-radius 0.15)
(def settings-formbar-command-x-spacing 1.25)
(def settings-formbar-command-y-spacing 1.25)

(def settings-saved-formbars-text-y -0.65)
(def settings-saved-formbars-text-size 0.8)
(def settings-saved-formbars-box-x -0.125)
(def settings-saved-formbars-box-y -0.525)
(def settings-saved-formbars-scroll-x 0.685)
(def settings-saved-formbars-box-width 5)
(def settings-saved-formbars-box-height 4)
(def settings-saved-formbars-scroll-radius 0.12)
(def settings-saved-formbar-radius 0.125)

(def settings-project-dropdown-y -0.4)
(def settings-project-dropdown-height 0.3)
(def settings-project-dropdown-width 0.9)
(def settings-project-dropdown-x-shrink-factor 0.45)
(def settings-project-text-y -0.125)
(def settings-project-text-size 0.9)
(def settings-project-name-size 0.08)

(def settings-project-buttons [:new-project :duplicate-project :rename-project :delete-project])

(def settings-project-button-radius 0.11)
(def settings-project-buttons-y-spacing 1.1)
(def settings-project-buttons-x-spacing 0.05)
(def settings-project-button-inner-radius-factor 0.8)

(def settings-bar-scroll-circle-pos 0.95)
(def settings-bar-scroll-circle-radius 0.13)
(def settings-bar-scroll-circle-inner-radius 0.9)
(def settings-bar-scroll-triangle-pos -0.4)
(def settings-bar-scroll-triangle-width 0.5)
(def settings-bar-scroll-triangle-height 0.9)

(def settings-color-header-text-y -0.75)
(def settings-color-header-text-size 0.6)

(def settings-color-text-y -0.5)
(def settings-color-text-size 0.045)
(def settings-color-spacing 0.225)
(def settings-color-width 0.43)
(def settings-color-width-factor 0.1)
(def settings-color-height 0.2)
(def settings-color-height-factor 0.25)

(def settings-pages 5)
(def settings-default-scroll-pos 2)

(def settings-formbar-commands-page 0)
(def settings-saved-formbars-page 1)
(def settings-project-selector-page 2)
(def settings-sliders-page 3)
(def settings-color-scheme-page 4)

(def saved-formbar-zone-corner-radius 0.04)
(def saved-formbar-spacing 0.25)

(def rename-icon-size 0.7)
(def rename-icon-width 0.6)
(def settings-button-line-width 0.1)
(def rename-icon-tip-size 0.75)
(def rename-icon-eraser-size 0.5)
(def rename-icon-tip-factor 1.25)

(def duplicate-icon-width 0.3)
(def duplicate-icon-height 0.4)
(def duplicate-icon-offset 0.1)

(def back-icon-right-length-factor 0.6)
(def back-icon-left-length-factor 0.5)
(def back-icon-tip-length-factor 0.4)
(def back-icon-width-factor 0.2)

(def new-formbar-circle-radius 0.4)

(def new-icon-size 0.7)
(def new-icon-width 0.4)

(def text-page-border 0.07)

(def outer-form-spacing 1.1)

(def quote-divs 32)
(def deref-circles 16)
(def deref-circle-size-factor 0.075)
(def syntax-quote-divs 32)
(def syntax-quote-offset-factor -0.05)
(def comment-divs 16)
(def comment-length-factor 0.15)
(def unquote-divs 2)
(def unquote-splice-circles 2)
(def meta-divs 12)
(def meta-length-factor 0.1)
(def var-quote-divs 12)
(def var-quote-length 0.1)

(def scroll-speed 0 #_0.025)

(def scroll-angle-snap-positions 4)
(def scroll-angle-snap-distance 0.1)