;; Custom

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(display-time-mode t)
 '(menu-bar-mode t)
 '(show-paren-mode t)
 '(tabbar-cycle-scope (quote tabs))
 '(tabbar-mode t nil (tabbar))
 '(tabbar-use-images nil))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "#121212" :foreground "#cccccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 92 :width normal :foundry "xos4" :family "Terminus"))))
 '(linum ((t (:stipple nil :background "#121212" :foreground "#333333" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 92 :width normal :foundry "xos4" :family "Terminus"))))
 '(tabbar-button ((t (:inherit tabbar-default :foreground "#000000"))))
 '(tabbar-default ((((class color grayscale) (background dark)) (:inherit variable-pitch :background "black" :foreground "grey70" :height 1.1))))
 '(tabbar-selected ((t (:inherit tabbar-default :bold t :foreground "grey80" :box (:line-width 6 :color "black")))))
 '(tabbar-unselected ((t (:inherit tabbar-default :box (:line-width 6 :color "black")))))
 '(variable-pitch ((t (:height 94 :foundry "microsoft" :family "Calibri")))))

