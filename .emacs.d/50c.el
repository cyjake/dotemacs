(require 'cc-mode)

;; do not insert any tabs, use spaces instead.
;; (setq-default indent-tabs-mode nil)

;; (setq c-default-style "bsd"
;;       c-basic-offset 4)

;; (setq c-mode-hook
;;     (function (lambda ()
;;                 (setq indent-tabs-mode nil)
;;                 (setq c-indent-level 4))))
;; (setq objc-mode-hook
;;     (function (lambda ()
;;                 (setq indent-tabs-mode nil)
;;                 (setq c-indent-level 4))))
;; (setq c++-mode-hook
;;     (function (lambda ()
;;                 (setq indent-tabs-mode nil)
;;                 (setq c-indent-level 4))))

;; (defun my-indent-or-complete ()
;;   (interactive)
;;   (if (looking-at "\\>")
;;       (hippie-expand nil)
;;     (indent-for-tab-command))
;;   )

(defun vi-backspace-unindent ()
  (interactive)
  (if (looking-back "^[[:blank:]]+")
      (let ((i 4))
        (while (and (not (looking-at "^")) (> i 0))
          (backward-delete-char 1)
          (setq i (- i 1))))
    (backward-delete-char 1))
  )

(defun newline-cindent ()
  (interactive)
  (c-indent-new-comment-line)
  (indent-for-tab-command)
  )

(define-key c-mode-base-map (kbd "DEL") 'vi-backspace-unindent)
(define-key c-mode-base-map (kbd "RET") 'newline-cindent)
