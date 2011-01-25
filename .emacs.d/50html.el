;; nxhtml-mode
;; http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html
;; (load "~/.emacs.d/packages/nxhtml/autostart.el")

;; http://mihai.bazon.net/blog/close-last-xml-tag-emacs
;; define the function
(defun msh-close-tag ()
  "Close the previously defined XML tag"
  (interactive)
  (let ((tag nil))
    (save-excursion
      (do ((skip 1))
          ((= 0 skip))
        (re-search-backward "</?\\([a-zA-Z0-9_-]+\\)")
        (cond ((looking-at "</")
               (setq skip (+ skip 1)))
              ((setq skip (- skip 1)))))
      (when (looking-at "<\\([a-zA-Z0-9_-]+\\)")
        (setq tag (match-string 1))))
    (insert (concat "</" tag ">"))))

;; and bind it to some key, so we can be fast
(define-key global-map [(control c) (/)] 'msh-close-tag)
