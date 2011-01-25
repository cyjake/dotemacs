;;fanfou.el--Simple Emacs-based client for Fanfou

;; Author: ngn999
;; Keywords: fanfou

;; Copyright 2008 Neil Roberts
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; A fanfou client for emacs that can view your friends timeline and
;; publish new statuses.

;;; Your should add the following to your Emacs configuration file:

;; (autoload 'fanfou-get-friends-timeline "fanfou" nil t)
;; (autoload 'fanfou-status-edit "fanfou" nil t)
;; (global-set-key "\C-xt" 'fanfou-get-friends-timeline)
;; (add-hook 'fanfou-status-edit-mode-hook 'longlines-mode)
(require 'url)
(require 'url-http)
(require 'xml)

(defgroup fanfou nil "Fanfou status viewer"
  :group 'applications)

(defgroup fanfou-faces nil "Faces for displaying Fanfou statuses"
  :group 'fanfou)

(defface fanfou-user-name-face
  '((t (:weight bold :background "light gray")))
  "face for user name headers"
  :group 'fanfou-faces)

(defface fanfou-time-stamp-face
  '((t (:slant italic :background "light gray")))
  "face for time stamp headers"
  :group 'fanfou-faces)

(defface fanfou-status-overlong-face
  '((t (:foreground "red")))
  "face used for characters in overly long Fanfou statuses.
The face is also used in the mode line if the character count
remaining drops to negative.")

(defconst fanfou-friends-timeline-url
  "http://api.fanfou.com/statuses/friends_timeline.xml"
  "URL used to receive the friends timeline")

(defconst fanfou-status-update-url
  "http://api.fanfou.com/statuses/update.xml"
  "URL used to update Fanfou status")

(defcustom fanfou-username nil
  "Username to use for connecting to Fanfou.
If nil, you will be prompted."
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'fanfou)

(defcustom fanfou-password nil
  "Password to use for connecting to Fanfou.
If nil, you will be prompted."
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'fanfou)

(defcustom fanfou-maximum-status-length 140
  "Maximum length to allow in a Fanfou status update.一个汉字的长度为1"
  :type 'integer
  :group 'fanfou)

(defvar fanfou-status-edit-remaining-length ""
  "Characters remaining in a Fanfou status update.
This is displayed in the mode line.")

(put 'fanfou-status-edit-remaining-length 'risky-local-variable t)

(defvar fanfou-status-edit-overlay nil
  "Overlay used to highlight overlong status messages.")

(defvar fanfou-status-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-c\C-c" 'fanfou-status-post)
    map)
  "Keymap for `fanfou-status-edit-mode'.")

(defun fanfou-retrieve-url (url cb)
  "Wrapper around url-retrieve.
Optionally sets the username and password if fanfou-username and
fanfou-password are set."
  (when (and fanfou-username fanfou-password)
    (let ((server-cons
	   (or (assoc "api.fanfou.com:80" url-http-real-basic-auth-storage)
	       (car (push (cons "api.fanfou.com:80" nil) url-http-real-basic-auth-storage)))))
      (unless (assoc "Fanfou API" server-cons)
	(setcdr server-cons (cons (cons "Fanfou API"
					(base64-encode-string (concat fanfou-username
								      ":" fanfou-password)))
				  (cdr server-cons))))))
  (url-retrieve url cb))

(defun fanfou-get-friends-timeline ()
  "Fetch and display the friends timeline.
The results are formatted and displayed in a buffer called
*Fanfou friends timeline*"
  (interactive)
  (fanfou-retrieve-url fanfou-friends-timeline-url
			'fanfou-fetched-friends-timeline))

(defun fanfou-fetched-friends-timeline (status &rest cbargs)
  "Callback handler for fetching the Fanfou friends timeline."
  (let ((result-buffer (current-buffer)) doc)
    ;; Make sure the temporary results buffer is killed even if the
    ;; xml parsing raises an error
    (unwind-protect
	(progn
	  ;; Skip the mime headers
	  (goto-char (point-min))
	  (re-search-forward "\n\n")
	  ;; Parse the rest of the document
  	  (mm-enable-multibyte)
      (decode-coding-region (point-min) (point-max) 'utf-8)
	  (setq doc (xml-parse-region (point) (point-max))))
      (kill-buffer result-buffer))
    ;; Get a clean buffer to display the results
    (let ((buf (get-buffer-create "*Fanfou friends timeline*")))
      (with-current-buffer buf
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (kill-all-local-variables)
	  ;; If the GET failed then display an error instead
	  (if (plist-get status :error)
	      (fanfou-show-error doc)
	    ;; Otherwise process each status node
	    (mapcar 'fanfou-format-status-node (xml-get-children (car doc) 'status))))
	(goto-char (point-min)))
      (view-buffer buf))))

(defun fanfou-get-node-text (node)
  "Return the text of XML node NODE.
All of the text elements are concatenated together and returned
as a single string."
  (let (text-parts)
    (dolist (part (xml-node-children node))
      (when (stringp part)
	(push part text-parts)))
    (apply 'concat (nreverse text-parts))))

(defun fanfou-get-attrib-node (node attrib)
  "Get the text of a child attribute node.
If the children of XML node NODE are formatted like
<attrib1>data</attrib1> <attrib2>data</attrib2> ... then this
fuction will return the text of the child node named ATTRIB or
nil if it isn't found."
  (let ((child (xml-get-children node attrib)))
    (if (consp child)
	(fanfou-get-node-text (car child))
      nil)))

(defun fanfou-show-error (doc)
  "Show a Fanfou error message.
DOC should be the XML parsed document returned in the error
message. If any information about the error can be retrieved it
will also be displayed."
  (insert "An error occured while trying to process a Fanfou request.\n\n")
  (let (error-node)
    (if (and (consp doc)
	     (consp (car doc))
	     (eq 'hash (caar doc))
	     (setq error-node (xml-get-children (car doc) 'error)))
	(insert (fanfou-get-node-text (car error-node)))
      (xml-print doc))))	

(defun fanfou-format-status-node (status-node)
  "Insert the contents of a Fanfou status node.
The status is formatted with text properties and insterted into
the current buffer."
  (let ((user-node (xml-get-children status-node 'user)) val)
    (when user-node
      (setq user-node (car user-node))
      (when (setq val (fanfou-get-attrib-node user-node 'name))
	(insert (propertize val 'face 'fanfou-user-name-face))))
    (when (setq val (fanfou-get-attrib-node status-node 'created_at))
      (when (< (+ (current-column) (length val)) fill-column)
	(setq val (concat (make-string (- fill-column
					  (+ (current-column) (length val))) ? )
			  val)))
      (insert (propertize val 'face 'fanfou-time-stamp-face)))
    (insert "\n")
    (when (setq val (fanfou-get-attrib-node status-node 'text))
      (fill-region (prog1 (point) (insert val)) (point)))
    (insert "\n\n")))

(defun fanfou-status-post ()
  "Update your Fanfou status.
The contents of the current buffer are used for the status. The
current buffer is then killed. If there is too much text in the
buffer then you will be asked for confirmation."
  (interactive)
  (when (or (<= (buffer-size) fanfou-maximum-status-length)
			(y-or-n-p (format (concat "The message is %i characters long. "
									  "Are you sure? ") (buffer-size))))
    (message "Sending status...")
    (let ((url-request-method "POST")
		  (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
		  (url-request-data (concat "source=Efan&status="
									(url-hexify-string (buffer-substring
														(point-min) (point-max))))))
      (fanfou-retrieve-url fanfou-status-update-url 'fanfou-status-callback))))

(defun fanfou-status-callback (status)
  "Function called after Fanfou status has been sent."
  (let ((errmsg (plist-get status :error)))
    (when errmsg
      (signal (car errmsg) (cdr errmsg)))
    (kill-buffer "*Fanfou Status*")
    (message "Succesfully updated Fanfou status.")))

(defun fanfou-status-edit ()
  "Edit your fanfou status in a new buffer.
A new buffer is popped up in a special edit mode. Press
\\[fanfou-status-post] when you are finished editing to send the
message."
  (interactive)
  (pop-to-buffer "*Fanfou Status*")
  (fanfou-status-edit-mode))

(defun fanfou-status-edit-update-length ()
  "Updates the character count in Fanfou status buffers.
This should be run after the text in the buffer is changed. Any
characters after the maximum status update length are
hightlighted in the face fanfou-status-overlong-face and the
character count on the mode line is updated."
  ;; Update the remaining characters in the mode line
  (let ((remaining (- fanfou-maximum-status-length
		      (buffer-size))))
    (setq fanfou-status-edit-remaining-length
	  (concat " "
		  (if (>= remaining 0)
		      (number-to-string remaining)
		    (propertize (number-to-string remaining)
				'face 'fanfou-status-overlong-face))
		  " ")))
  (force-mode-line-update)
  ;; Highlight the characters in the buffer that are over the limit
  (if (> (buffer-size) fanfou-maximum-status-length)
      (let ((start (+ (point-min) fanfou-maximum-status-length)))
	(if (null fanfou-status-edit-overlay)
	    (overlay-put (setq fanfou-status-edit-overlay
			       (make-overlay start (point-max)))
			 'face 'fanfou-status-overlong-face)
	  (move-overlay fanfou-status-edit-overlay
			start (point-max))))
    ;; Buffer is not too long so just hide the overlay
    (when fanfou-status-edit-overlay
      (delete-overlay fanfou-status-edit-overlay))))

(defun fanfou-status-edit-after-change (begin end old-size)
  (fanfou-status-edit-update-length))

(define-derived-mode fanfou-status-edit-mode text-mode "Fanfou Status Edit"
  "Major mode for updating your Fanfou status."
  ;; Schedule to update the character count after altering the buffer
  (make-local-variable 'after-change-functions)
  (add-hook 'after-change-functions 'fanfou-status-edit-after-change)
  ;; Add the remaining character count to the mode line
  (make-local-variable 'fanfou-status-edit-remaining-length)
  ;; Copy the mode line format list so we can safely edit it without
  ;; affecting other buffers
  (setq mode-line-format (copy-sequence mode-line-format))
  ;; Add the remaining characters variable after the mode display
  (let ((n mode-line-format))
    (catch 'found
      (while n
	(when (eq 'mode-line-modes (car n))
	  (setcdr n (cons 'fanfou-status-edit-remaining-length
			  (cdr n)))
	  (throw 'found nil))
	(setq n (cdr n)))))
  ;; Make a buffer-local reference to the overlay for overlong
  ;; messages
  (make-local-variable 'fanfou-status-edit-overlay)
  ;; Update the mode line immediatly
  (fanfou-status-edit-update-length))

(provide 'fanfou)

;;; fanfou.el ends here

