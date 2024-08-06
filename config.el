;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; Display line numbers
(global-display-line-numbers-mode 1)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Change the undo limit to 80MB
(setq undo-limit 80000000)

;; Set the font size
(setq doom-font (font-spec :size 18))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))

  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 4))
  (add-to-list 'copilot-indentation-alist '(org-mode 4))
  (add-to-list 'copilot-indentation-alist '(text-mode 4))
  (add-to-list 'copilot-indentation-alist '(closure-mode 4))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 4)))

;; Markdown preview
(defun +markdown-live-preview-window-xwidget-webkit (file)
  "Preview FILE with xwidget-webkit.
To be used with `markdown-live-preview-window-function'."
  (let ((uri (format "file://%s" file)))
      (xwidget-webkit-browse-url uri)
      xwidget-webkit-last-session-buffer))

(set-popup-rule! "^\\*xwidget" :side 'right :size .50 :ttl 0 :quit nil)

(setq markdown-live-preview-window-function
      'markdown-live-preview-window-xwidget-webkit)



;; HELPER FUNCTIONS FOR KEYBINDINGS
;; Opens shell at the bottom with height of 1/3 of the window
;; If it exists, it just switches to the window
;; If it exists but is not focused, it switches to it
;; If it exists but is not visible, it opens it at the bottom
;; If it exists and is focused, it closes it
(defun open-shell-bottom ()
        (interactive)
        (let ((shell-window (get-buffer-window "*shell*")))
        (if shell-window
                (if (eq (selected-window) shell-window)
                (delete-window shell-window)
                (select-window shell-window))
        (split-window-below)
        (other-window 1)
        (evil-window-move-very-bottom)
        (evil-window-set-height (round (* 0.6 (window-total-height))))
        (shell))))

;; Opens new file at the right
;; Opens the dialog to open the file, and then splits the window to the right and opens the file in the new window
;; If no file is selected, it does nothing
;; If there is no window on the right, opens a window and opens the file in it
;; If there already is a window on the right, it switches to it and opens the new file without splitting
(defun open-file-right ()
        (interactive)
        (if (one-window-p)
                (progn
                  (find-file (read-file-name "Open file: "))
                  (split-window-right)
                  (evil-switch-to-windows-last-buffer)
                  (other-window 1))
                 (if (windmove-find-other-window 'right)
                        (progn
                          (other-window 1)
                          (find-file (read-file-name "Open file: ")))
                        (find-file (read-file-name "Open file: ")))))

;; Opens new file at the left
;; If there is no window, opens a window and opens the file in it
;; If there is a window on the left, it switches to it and opens the new file without splitting
(defun open-file-left ()
        (interactive)
        (if (one-window-p)
                (progn
                  (find-file (read-file-name "Open file: "))
                  (split-window-right)
                  (evil-switch-to-windows-last-buffer)
                  (other-window 1)
                  (evil-window-move-far-left))
                (if (windmove-find-other-window 'left)
                        (progn
                          (other-window 1)
                          (find-file (read-file-name "Open file: ")))
                        (find-file (read-file-name "Open file: ")))))

;; KEYBINDINGS
;; Keybindings open shell at bottom and files at right and left
(map! :leader
      (:prefix "f"
       :desc  "open shell bottom" "<down>" #'open-shell-bottom
       :desc  "open file right" "<right>" #'open-file-right
       :desc  "open file left" "<left>" #'open-file-left))
