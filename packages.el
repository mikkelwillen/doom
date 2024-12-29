;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; pdf tools package
(elpaca pdf-tools)

;; tablist package
(elpaca tablist)

;; Execution path
(package! exec-path-from-shell)
(package-initialize)

;; copilot package
(package! copilot
   :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))

;; install eyebrowse
(package! eyebrowse)
