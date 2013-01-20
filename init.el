(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;(set-face-attribute 'default nil :height 130)
;(load-theme 'zenburn t)

(set-face-attribute 'default nil :font "Consolas-13")

(setq non-elpa-dir (file-name-as-directory (concat user-emacs-directory "non-elpa")))
(load-file (concat non-elpa-dir "cyberpunk.el"))
(color-theme-cyberpunk)
;;(load-file (concat non-elpa-dir "blackbored.el"))
;;(color-theme-blackbored)

(global-hl-line-mode 1)
(set-face-background 'hl-line "#333333")
(set-cursor-color "yellow")

;; highlight-tail
;;(load-file (concat non-elpa-dir "highlight-tail.el"))
;;(require 'highlight-tail)
;;(highlight-tail-reload)

;; paredit
(require 'paredit)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; find-file-in-project
(global-set-key (kbd "C-x f") 'find-file-in-project)

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(ac-flyspell-workaround)
;;(add-to-list 'ac-dictionary-directories (concat (live-pack-lib-dir) "auto-complete/dict"))

(global-auto-complete-mode t)
(setq ac-auto-show-menu t)
(setq ac-dwim t)
(setq ac-use-menu-map t)
(setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)
(setq ac-disable-inline t)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-auto-start 2)
(setq ac-candidate-menu-min 0)

(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic
               ac-source-yasnippet))

(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode markdown-mode tuareg-mode))
  (add-to-list 'ac-modes mode))

(define-key ac-completing-map (kbd "C-M-n") 'ac-next)
(define-key ac-completing-map (kbd "C-M-p") 'ac-previous)

(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))


;; rainbow-delimiter
(require 'rainbow-delimiters)
(dolist (x '(scheme emacs-lisp lisp clojure))
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'enable-paredit-mode)
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'rainbow-delimiters-mode))

;; nrepl
(add-hook 'nrepl-interaction-mode-hook
          (lambda ()
            (nrepl-turn-on-eldoc-mode)
            (enable-paredit-mode)))

(add-hook 'nrepl-mode-hook
          (lambda ()
            (nrepl-turn-on-eldoc-mode)
            (enable-paredit-mode)))

(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

;; Stop the error buffer from popping up while working in the REPL buffer
(setq nrepl-popup-stacktraces nil)

;; Make C-c C-z switch to the *nrepl* buffer in the current window
(add-to-list 'same-window-buffer-names "*nrepl*")




;;;;;;;;;;;;;;;;;;
;; key bindings ;;
;;;;;;;;;;;;;;;;;;

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

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

;; Mac
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'control)


;;;; For OS specific settings
(if (eq system-type 'windows-nt)  
    (progn ;; Windows
      ;; font style and size
      (set-face-attribute 'default nil :font "Consolas-10.5")

      ;; blackbored color theme
      (load-file (concat non-elpa-dir "blackbored.el"))
      (color-theme-blackbored)
      
      ;; cygwin
      (custom-set-variables
       ;; custom-set-variables was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
       '(cygwin-mount-cygwin-bin-directory "C:\\cygwin\\bin"))
      (setq exec-path (cons "C:/cygwin/bin" exec-path))
      (setenv "PATH" (concat "C:/cygwin/bin;" (getenv "PATH")))
      (load-file (concat non-elpa-dir "setup-cygwin.el"))
      (require 'setup-cygwin)

       ;; dos shell with dirtrack-mode
      (defun dos-shell ()
        "Run dos in shell mode."
        (interactive)
        (let ((explicit-shell-file-name "C:/windows/system32/cmd"))
          (call-interactively 'shell)
          (dirtrack-mode 1)))
 
      ;; override dirtrack package's dirtrack-list for list
      (setq-default dirtrack-list '("\\([a-zA-Z]:.*\\)>" 1)))
  (progn ;; *nix & mac
    (setenv "PATH" (shell-command-to-string "source $HOME/.bashrc && printf $PATH"))))


