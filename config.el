;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; Redefine display-line-numbers to not show line numbers in pdf-view-mode
(require 'display-line-numbers)
    (defun display-line-numbers--turn-on ()
    "Turn on `display-line-numbers-mode'."
    (unless (or (minibufferp) (eq major-mode 'pdf-view-mode))
	(display-line-numbers-mode)))

;; Display line numbers
;; (global-display-line-numbers-mode 1)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Change the undo limit to 80MB
(setq undo-limit 80000000)

;; Set the font size
(setq doom-font (font-spec :size 24 ))

;; Set indentation to 4 spaces
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

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


;; TODO FIX!!
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

;; Switches to workspace by index
;; If the workspace does not exist, it creates a new one at n+1
;; 	where n is the index of the current greatest workspace
;; If the workspace exists, it switches to it
;; It also displays the workspace bar
(defun switch-workspace-by-index (index)
  "Switch to workspace by numeric INDEX, showing the workspace bar if necessary."
  (interactive "nWorkspace index: ")
  (let* ((workspaces (+workspace-list-names))
         (target (nth (1- index) workspaces)))
    (if target
        (+workspace-switch target)
      (+workspace/new )))
  (+workspace/display))

;; Customized backward-kill-word
;; If the character behind the cursor is whitespace, it removes all whitespace
;; If the character behind the cursor is a newline, it deletes the newline
;; Otherwise, it removes a word
(defun custom-backward-kill-word ()
  (interactive)
  (let ((char (char-before))) ; Get the character before the cursor
    (cond
     ;; Check for whitespace
     ((and char (member char '(?\s ?\t)))
      (while (and (char-before)
                  (member (char-before) '(?\s ?\t)))
        (delete-char -1)))
     ;; Check for newline
     ((and char (eq char ?\n))
      (delete-char -1))
     ;; Default case: kill the previous word
     (t
      (let ((start (point)))
        (backward-word)
        (kill-region (point) start))))))

;; KEYBINDINGS
;; Keybindings open shell at bottom and files at right and left
(map! :leader
      (:prefix "f"
       :desc  "open shell bottom" "<down>" #'open-shell-bottom
       :desc  "open file right" "<right>" #'open-file-right
       :desc  "open file left" "<left>" #'open-file-left))

;; Set M-1 to M-9 to switch to workspace by index or
;; create a new one if it does not exist by calling
;; my/switch-workspace-by-index
(dotimes (i 10)
  (global-set-key (kbd (format "M-%d" i))
                  (lambda () (interactive) (switch-workspace-by-index i))))

;; Set C-backspace to my custom backward-kill-word
(map! :i "C-<backspace>" #'custom-backward-kill-word) ; In insert mode
(map! :n "C-<backspace>" #'custom-backward-kill-word) ; In normal mode
