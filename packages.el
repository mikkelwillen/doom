;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; pdf tools package
(package! pdf-tools)

;; tablist package
(package! tablist)

;; Execution path
(package! exec-path-from-shell)

;; Magit git client
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)


;; copilot package
(package! copilot
   :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
