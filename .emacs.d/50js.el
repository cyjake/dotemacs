;; for more information plz refer to links below:
;; EmacsWiki: 
;;     http://www.emacswiki.org/cgi-bin/wiki/JavaScriptMode
;; O'Connor's discussion of regex hilighting among those major extensions:
;;     http://edward.oconnor.cx/2005/09/editing-javascript-in-emacs
;; JoostDiepenmaat from the comments on EmacsWiki page:
;;     http://joost.zeekat.nl/2007/10/31/javascript-regex-en-string-literal-highlighting-in-emacs/

(require 'generic-x)

(when (locate-library "javascript")
  (autoload 'javascript-mode "javascript" nil t)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode)))

