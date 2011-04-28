#!/usr/bin/csi -s

;;; Zanes wmii config, in Chicken Scheme
;;; Modified from the wmii egg example (I guess that goes without saying)
;;;
;;; TODO
;;; Stabilize it a bit. It's not bad but occasionally I get exceptions in places

(use (prefix wmiirc wmii:) srfi-18 posix regex regex-literals srfi-13 numspell)

(wmii:connect)

(include "~/.wmii/chicken-wmii/utilities.scm")          ; Misc functions and Utilities
(include "~/.wmii/chicken-wmii/themes/live-coding.scm") ; Color theme
(include "~/.wmii/chicken-wmii/config.scm")             ; Main configuration
(include "~/.wmii/chicken-wmii/statusbar.scm")          ; Fancy Status Bar
(include "~/.wmii/chicken-wmii/program-list.scm")       ; Program launcher
(include "~/.wmii/chicken-wmii/tag-handling.scm")       ; Tag Handling


;; Set in config.scm (Main configuration)
(wmii:global-settings-set!
 `((font        . ,wmii-font)
   (focuscolors . ,wmii-focuscolors)
   (normcolors  . ,wmii-normcolors)
   (grabmod     . ,modkey)
   (colmode     . ,wmii-colmode)
   (bar         . ,wmii-barposition)
   (border      . ,wmii-floatborder)))


;;; Event handling

(add-key '("space")         (wmii:navigate-to "toggle"))
(add-key '("d")             (wmii:tag-settings-set! '(("colmode" . ("sel" "default")))))
(add-key '("s")             (wmii:tag-settings-set! '(("colmode" . ("sel" "stack")))))
(add-key '("m")             (wmii:tag-settings-set! '(("colmode" . ("sel" "max")))))
(add-key '("Shift" "space") (wmii:send-to "toggle"))
(add-key '("f")             (wmii:change-state "Fullscreen" 'toggle))
(add-key '("Shift" "c")     (wmii:kill))
(add-key '("escape")        (wmii:kill))
(add-key '("Return")        (fork-run wmii-term))

(add-key '("a")             
         (and-let* ((action (wimenu `("reload" "rehash" "exec" "status" "quit"))))
           (cond
            ((string=? action "reload") (wmii:exec (format "wmii -r ~a/.wmii/wmiirc.scm" (getenv "HOME"))))
            ((string=? action "rehash") (update-programs))
            ((string-prefix? "exec " action) (wmii:exec (string-drop action 5)))
            ((string=? action "status") (status))
            ((string=? action "quit") (wmii:quit) (exit)))))

(add-key '("Shift" "colon") ; Eval :D
         (and-let* ((input (wimenu `(""))))
           (wmii:write-tab "lbar" "eval"
                           (format "~s" (eval (with-input-from-string input read)))
                           wmii-evalcolors)))


;;; GO GO GO

(wmii:event-loop)
