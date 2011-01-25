;; small settings like key bindings.

(prefer-coding-system 'utf-8)

(global-set-key [(f10)] 'shell)
(global-set-key [(f8)] 'speedbar-get-focus)
(global-set-key [(f5)] 'ywb-create-or-switch-scratch)

(defun ywb-create-or-switch-scratch ()
  (interactive)
  (let ((buf (get-buffer "*scratch*")))
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (when (null buf)
      (lisp-interaction-mode))))

(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(global-set-key (kbd "s-o") 'switch-to-other-buffer)

;; (defun my-split-window-function ()
;;   (interactive)
;;   (split-window-vertically)
;;   (set-window-buffer (next-window) (other-buffer)))

;; (global-set-key "\C-x2" 'my-split-window-function)

(defadvice split-window-vertically
  (after my-window-splitting-advice first () activate)
  (set-window-buffer (next-window) (other-buffer)))

(global-set-key (kbd "M-<SPC>") 'set-mark-command) 

;; unbind C-z from backgrounding emacs.
(global-set-key (kbd "C-z") nil)

;; 对配置文件进行加亮
;; The fvwm-generic-mode provided by the extension below is not good enough. 
;; Use fvwm-mode instead. See to 50fvwm.el. 
;; (require 'generic)
;; (require 'generic-x)
;; (setcdr (assoc "\\.fvwm2rc\\'" auto-mode-alist) 
;; 	'fvwm-mode)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; enable ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; delete-region
(delete-selection-mode 1)

;; wind-move 功能，使用 shift+方向键在各个 window 之间移动
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; browse-kill-ring 功能
;;; (require 'browse-kill-ring)
;;; (global-set-key (kbd "C-c k") 'browse-kill-ring)

(require 'linum)
(dolist (hook (list
               'c-mode-hook
               'text-mode-hook
	       'lisp-mode-hook
	       'lisp-interaction-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'emms-playlist-mode-hook
               'java-mode-hook
	       'javascript-mode-hook
	       'css-mode-hook
               'muse-mode-hook
               'emms-lyrics-mode-hook
	       'perl-mode-hook
	       'pythom-mode-hook
               ))
  (add-hook hook 'linum-mode 1))

(setq linum-format "%6d")

;; http://stackoverflow.com/questions/235254/how-can-i-run-cygwin-bash-shell-from-within-emacs
;; Let's use CYGWIN bash...
;;
;; (setq binary-process-input t) 
;; (setq w32-quote-process-args ?\") 
;; (setq shell-file-name "bash") ;; or sh if you rename your bash executable to sh. 
;; (setenv "SHELL" shell-file-name) 
;; (setq explicit-shell-file-name shell-file-name) 
;; (setq explicit-sh-args '("-login" "-i"))
