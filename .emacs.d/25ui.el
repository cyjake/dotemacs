(set-default-font "-*-Envy Code R-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1")

;; when under Windows NT and Window 7
;; (set-language-environment "Chinese-GBK")
;; (set-locale-environment "zh_CN.GBK")
;; (setq file-name-coding-system 'gbk)

(setq inhibit-startup-message t)	; 关闭启动画面

(tool-bar-mode nil)		        ; 去掉那个大大的工具栏
(set-scroll-bar-mode nil) 	        ; 去掉滚动条
(mouse-avoidance-mode 'animate)	        ; 光标靠近鼠标指针时, 鼠标指针自动让开

(show-paren-mode 1)			; 让括号配对的时候不跳到另外一边
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0.5)

(put 'narrow-to-region 'disabled nil)

(when (eq window-system 'x)
    (setq x-select-enable-clipboard t))	; 支持emacs和外部程序的粘贴

(setq frame-title-format "emacs@%b")	; 在标题栏提示你目前在什么位置

(setq kill-emacs-query-functions
      (lambda ()
        (y-or-n-p "Confirm?")))

(require 'color-theme)			; 颜色主题
(color-theme-initialize)
(if window-system
    (color-theme-charcoal-black)
  (color-theme-charcoal-black))

