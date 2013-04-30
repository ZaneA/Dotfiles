;;
;; Zane's .emacs
;; https://github.com/ZaneA/Dotfiles
;;

(require 'cl)
(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))

(setq packages
      '(auto-complete bind-key chicken-scheme edit-server evil
        git-gutter git-gutter-fringe golden-ratio google-this
        inkpot-theme js2-mode kpm-list legalese less-css-mode
        linum-relative magit markdown-mode org popup pos-tip r5rs
        rainbow-delimiters rainbow-mode scratch scss-mode
        simple-mode-line skewer-mode slime starter-kit starter-kit-js
        starter-kit-ruby surround use-package writegood-mode))

(package-initialize)

; Install missing packages
(loop for package in packages
      when (not (package-installed-p package))
      when (y-or-n-p (format "Install %s? " package))
      do (package-install package))

(require 'use-package)

; Color theme
(use-package color-theme
  :init
  (load-theme 'inkpot t))

(setq-default line-spacing 3)

; Font
(set-frame-font "SourceCodePro-13")
(add-to-list 'default-frame-alist
             '(font . "SourceCodePro-13"))

(use-package simple-mode-line
  :init
  (progn
    (setq-default simple-mode-line-small-font-height (face-attribute 'default :height))
    (setq-default simple-mode-line-inactive-line-width simple-mode-line-line-width)
    (activate-simple-mode-line)))

; Evil
(use-package evil
  :init
  (progn
    (setq evil-default-cursor t)
    (evil-mode t)

    ; A nice evil shortcut for indirectly narrowing a region of the
    ; current vi selection
    (evil-define-operator evil-narrow-indirect (beg end type)
      "Indirectly narrow the region from BEG to END."
      (interactive "<R>")
      (evil-normal-state)
      (narrow-to-region-indirect beg end))

    (define-key evil-normal-state-map "m" 'evil-narrow-indirect)
    (define-key evil-visual-state-map "m" 'evil-narrow-indirect)

    (evil-define-operator evil-join-unfill (beg end)
      "Join the selected lines. Uses fill-region so that
adaptive-fill-mode is effective when joining."
      :motion evil-line
      (evil-with-single-undo
        (end-of-line)
        (let* ((fill-column (point-max))
               (lines (count-lines beg end))
               (end (if (> lines 1)
                        end
                      (save-excursion
                        (goto-char end)
                        (forward-line)
                        (point)))))
          (fill-region beg end))))

    (define-key evil-normal-state-map "J" 'evil-join-unfill)
    (define-key evil-visual-state-map "J" 'evil-join-unfill)))

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq mouse-autoselect-window t)
(setq frame-title-format '((buffer-name "%f" ("%b")) " (" mode-name ")"))
(setq-default cursor-in-non-selected-windows nil)

(setq visible-bell nil)
(setq echo-keystrokes 0.1)
(setq max-mini-window-height 0.75)

(setq vc-follow-symlinks t)

; Remove word-wrapping
(setq-default truncate-lines t)
(setq-default indicate-empty-lines nil)
(setq-default fill-column 70)
(setq adaptive-fill-mode t)

; 2 spaces for indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default evil-shift-width 2)
(setq-default css-indent-offset 2)
(setq-default js2-basic-offset 2)

; Remove hooks I don't like
(use-package starter-kit
  :bind ("C-c C-e" . esk-eval-and-replace)
  :init
  (progn
    (remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
    (remove-hook 'prog-mode-hook 'idle-highlight-mode)
    (remove-hook 'text-mode-hook 'turn-on-auto-fill)))

;; Global modes

(use-package kpm-list
  :init
  (defadvice kpm-list (after no-evil-kpm-list activate)
    "Make kpm-list play along with evil-mode."
    (with-current-buffer (get-buffer kpm-list-buffer-name)
      (define-key evil-normal-state-local-map (kbd "<RET>") 'kpm-list-select-buffer)
      (define-key evil-normal-state-local-map "q" 'quit-window)
      (define-key evil-normal-state-local-map "o" 'kpm-list-select-other-window)
      (define-key evil-normal-state-local-map "j" 'kpm-list-next-buffer)
      (define-key evil-normal-state-local-map "k" 'kpm-list-prev-buffer)
      (define-key evil-normal-state-local-map "d" 'kpm-list-kill-buffer))))

(use-package surround
  :init
  (progn
    (define-globalized-minor-mode global-surround-mode
      surround-mode (lambda () (surround-mode t)))
    (global-surround-mode t)))

(use-package google-this
  :bind ("<f2>" . google-this)
  :init
  (google-this-mode t))

(use-package golden-ratio
  :init
  (golden-ratio-mode t))

(use-package rainbow-delimiters
  :init
  (global-rainbow-delimiters-mode t))

(electric-pair-mode t)

(use-package rainbow-mode
  :init
  (add-hook 'css-mode-hook (lambda () (rainbow-mode t))))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'scheme-mode-hook (lambda () (setq adaptive-fill-mode t)))

; Auto-complete
(use-package pos-tip)
(use-package auto-complete-config
  :init
  (progn
    (ac-config-default)
    (setq tab-always-indent 'complete)
    (add-to-list 'completion-styles 'initials t)))

(use-package git-gutter-fringe
  :init
  (progn
    (setq-default left-fringe-width 20)
    (fringe-helper-define 'git-gutter-fr:modified nil
      "........"
      "XXXXXXXX"
      "XXXXXXXX"
      "........"
      "........"
      "XXXXXXXX"
      "XXXXXXXX"
      "........")
    (global-git-gutter-mode t)))

(use-package linum-relative
  :init
  (global-linum-mode t))

(use-package js2-mode
  :mode ("\\.js$" . js2-mode))

(use-package markdown-mode
  :mode ("\\.md$" . markdown-mode))

; Clean up buffers
(use-package midnight
  :init
  (progn
    (add-to-list 'clean-buffer-list-kill-buffer-names
                 '("*Packages*"
                   "*Completions*"
                   "*magit"
                   "*Backtrace*"))

    (setq clean-buffer-list-delay-special 0)

    (run-with-idle-timer 300 t 'clean-buffer-list)))

;; Custom methods

(defun make-windows-with-modes (modes)
  "Open split windows for each mode specified (space separated)."
  (interactive "sModes: ")
  (save-selected-window
    (dolist (mode (split-string modes " "))
      (let ((buffer (generate-new-buffer (format "*%s*" mode))))
        (split-window-vertically)
        (switch-to-buffer-other-window buffer)
        (funcall (intern mode))))))

(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil))
        (mode (guess-mode-of-region start end)))
    (with-current-buffer buf
      (narrow-to-region start end)
      (funcall mode))
    (switch-to-buffer buf)))

(defun intelligent-close ()
  "Intelligently close the frame or quit Emacs."
  (interactive)
  (if (eq (car (visible-frame-list)) (selected-frame))
      (if (> (length (visible-frame-list)) 1)
          (delete-frame (selected-frame))
        (save-buffers-kill-emacs))
    (delete-frame (selected-frame))))

(defun rename-file-and-buffer (new-name)
  "Rename current buffer and file names."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun move-region-to-new-file (file-name beg end)
  "Move the active region to a new file."
  (interactive "FFile name: \nr")
  (kill-region beg end)
  (deactivate-mark)
  (find-file file-name)
  (yank))

(defvar hide-indentation-overlays nil)

(defun hide-unnecessary-indentation (beg end)
  "Hides unnecessary indentation at the beginning of a buffer."
  (interactive "r")
  (unless (local-variable-p 'hide-indentation-overlays)
    (make-local-variable 'hide-indentation-overlays))
  ; Remove old overlays first
  (mapcar 'delete-overlay hide-indentation-overlays)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let* ((point-beginning (progn
                                (beginning-of-buffer)
                                (point)))
             (point-indentation (progn
                                  (or (back-to-indentation)
                                      (forward-to-indentation))
                                  (current-column)))
             (point-end (progn
                          (end-of-buffer)
                          (unless (back-to-indentation)
                            (previous-line))
                          (move-to-column point-indentation t)
                          (point))))
        (apply-on-rectangle
         (lambda (beg end)
           (save-excursion
             (let* ((beg-point (progn (move-to-column beg) (point)))
                    (end-point (progn (move-to-column end) (point)))
                    (overlay (make-overlay beg-point end-point)))
               (setq hide-indentation-overlays
                     (append
                      (list overlay) hide-indentation-overlays))
               (overlay-put overlay 'invisible t))))
         point-beginning
         point-end)
        ))))

(defvar guess-mode-syntax-rules
  '(("html-mode"     . "<.*?>")
    ("js2-mode"      . "var\\|[A-Za-z]+\\.[A-Za-z]+(")
    ("less-css-mode" . "[A-Za-z]+\s?{")
    ("php-mode"      . "$[A-Za-z]+"))
  "Rules for guessing a mode from some text.")

(defun guess-mode-of-region (beg end)
  "Guess the mode of region, in the dumbest possible way."
  (save-restriction
    (narrow-to-region beg end)
    (loop for match = major-mode
          for rule in guess-mode-syntax-rules
          until (save-excursion
                  (when (search-forward-regexp (cdr rule) nil t)
                    (setq match (intern (car rule)))))
          finally return match)))

(defun guess-mode-of-region-and-switch (beg end)
  "Guess the mode of region and switch to it."
  (interactive "r")
  (let ((mode (guess-mode-of-region beg end)))
    (funcall mode)))

;; Advice

; From http://www.emacswiki.org/EmacsNiftyTricks
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent active processes query when quitting Emacs."
  (cl-flet ((process-list ())) ad-do-it))

(defadvice quit-window (before quit-window-and-kill activate)
  "Kill buffer when quitting a window."
  (ad-set-arg 0 t))

; Other keybindings
(global-set-key (kbd "C-x C-c") 'intelligent-close)
(global-set-key (kbd "M-\\") 'align-regexp)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Org-mode customizations
(use-package org
  :init
  (progn
    (evil-define-key 'normal org-mode-map (kbd "RET") 'org-return)
    (setq org-agenda-files (directory-files "~/Documents/org" t ".org$"))
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-return-follows-link t)
    (setq org-startup-folded nil)

                                        ; Set up agenda
    (appt-activate t)
    (add-hook 'org-finalize-agenda-hook (lambda () (org-agenda-to-appt t)))
    (run-with-idle-timer 600 t 'org-agenda-list)
    (org-agenda-list)))

;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("c02fb5690dff0a6e599ba388c2ee167990333a1cd96338cba8abfc4eb0ba1fb9" "66bb5247f8f409414813b3353f15761802f1a996b2d32cbb873ce220c296402b" "a1d9ee31e3ecd85a45c82efe0280f1f7b303e5135c2eb7755ecaba788a2487ad" "bef599043372a954dbf1f30fa372b24736ea62124c4dbdc9aa2b88e99af330c3" "e4f90650031231c035a30cf110dfe40c03291a7b1b8c3826ffc31fc15b787fe4" "90ea1f27b41b936e8cbbb1d194a780e010676413a8e52ceab1cfb846f2f7db76" "a54d42e2c032cc8633feedb0da5445bbcdf1ba3dc550c0d6841fcbe25bfcd95a" "644a9b6d0b382ba0e96ae7df6aa9172c77790ef4c65ece93a6d15177ac32905b" "134d090f2473516f8aec4ab1ec388f567e1032bd1ca4f4aa72945b7dff675219" "8eb5a19ecd21b0305db248f2540bad2f43a253cbb851fe81786caa1533c43601" "06f5145c01ec774a0abb49eeffa3980743ce2f997112b537effeb188b7c51caf" "b6f7795c2fbf75baf3419c60ef7625154c046fc2b10e3fdd188e5757e08ac0ec" "936e5cac238333f251a8d76a2ed96c8191b1e755782c99ea1d7b8c215e66d11e" "446c73cdfb49f1dab4c322e51ac00a536fb0e3cb7e6809b9f4616e0858012e92" "4dacec7215677e4a258e4529fac06e0231f7cdd54e981d013d0d0ae0af63b0c8" "8f6537eb6f9d66b060c736f5f680f5c661e0a6b311b86defa293bc5ba104a030" "3bd9497fb8f39c28ab58a9e957152ba2dc41223c23c5520ef10fc7bd6b222384" "246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "15fa54dffe7ef4c91033739a8d2eba0fb897337dffe1f98b0629978183690c42" "1c27614df08fb5f4457d97d15c50a118d3cb143431e7dbd7eb833e94b880c286" "1cee2deb577b631e22b773120c95a9201a1e994f297802862daa30d311155c8c" "35cdc21c39fce0c36d86dc17a0b68682756511af6ffe165236f59bab47078f73" "d51911fb5c7315afee507a137c763f014ca162cd14736ee945586355229c607c" "fe155bdda1ce7f8cd362b3964f430c7ad0313ee8d32e2dd561319f392d528174" "06ffa5c99dfe0042b6639b28dc9c5fbc2137ca6577cd8b901a664e59f8fab0fb" "e0d089b338f8af95d12e51f73e87c516edf750a9449dec850409f301c3047da9" "48f0724fdd47bdc2be2df019e7ea2a33edc8a08791d03b28b1386aa9730c5d88" "253bd40645913cc95b9f8ef0533082cb9a4cb0810f854c030f3ef833ee5b9731" "059db9c5e55631d154bf3e53233cdc30616020a40f13530d005912ca104a4178" "6fc907278bbe283967372527ad3a98546b4a53cf5a78746cadd6d46bd8d8d05c" "9d9ba63d13aa4401627c7e2f59f3ee1e30dc931b02505c8c54727b696e46b350" "d5e5040c58f860f2b1ad6e652bb00383b7b200125c72d3d99091934afb0567b0" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "5339210234ec915d7d3fd87bfeb506bfc436ff7277a55516ab1781ec85c57224" "281e88e0dfab4980a157331b368fb2e5eba315c38f38099d2d9153980a8047ba" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
