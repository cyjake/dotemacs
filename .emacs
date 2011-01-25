(mapc '(lambda (file)
     (load (file-name-sans-extension file)))
       (directory-files "~/.emacs.d/" t "\\.el$"))
