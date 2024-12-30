;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; pdf tools package
(package! pdf-tools)

;; tablist package
(package! tablist)

;; Drag-stuff package to move lines and words
(package! drag-stuff)

;; Execution path
(package! exec-path-from-shell)
(package-initialize)

;; copilot package
(package! copilot
   :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))

;; install eyebrowse
(package! eyebrowse)
