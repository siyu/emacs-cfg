;; https://github.com/technomancy/swank-clojure
;; 
(setenv "PATH" (shell-command-to-string "source $HOME/.bashrc && printf $PATH"))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;(set-face-attribute 'default nil :height 130)
;(load-theme 'zenburn t)

(set-face-attribute 'default nil :font "Consolas-12")

(load-file (concat (file-name-as-directory (concat user-emacs-directory "non-elpa")) "cyberpunk.el"))
(global-hl-line-mode 1)
(set-face-background 'hl-line "#333333")
(color-theme-cyberpunk)
(set-cursor-color "yellow")

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; find-file-in-project
(global-set-key (kbd "C-x f") 'find-file-in-project)

;; auto-complete
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

;; rainbow-delimiter
(require 'rainbow-delimiters)
(dolist (x '(scheme emacs-lisp lisp clojure))
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'rainbow-delimiters-mode))

;; nrepl
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

;; Stop the error buffer from popping up while working in the REPL buffer
(setq nrepl-popup-stacktraces nil)

;; Make C-c C-z switch to the *nrepl* buffer in the current window
(add-to-list 'same-window-buffer-names "*nrepl*")

;;;;;;;;;;;;;;;;;;
;; key bindings ;;
;;;;;;;;;;;;;;;;;;

;; winner undo and redo
(global-set-key (kbd "C-c b") 'winner-undo)
;(define-key nrepl-interaction-mode-map (kbd "C-c b") 'winner-undo)
(global-set-key (kbd "C-c f") 'winner-redo)

;; switch buffer window
(global-set-key [C-tab] 'other-window)

;;use delete-horizontal-space to completely nuke all whitespace
(global-set-key (kbd "M-SPC ")   'live-delete-whitespace-except-one)

;;kill regions
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Ace jump mode
(global-set-key (kbd "C-o") 'ace-jump-mode)
