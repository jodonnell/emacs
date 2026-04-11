;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VTERM IMPROVEMENTS
;; Make vterm behave more like shell-mode: free cursor movement in scrollback,
;; automatic copy-mode on vertical movement, and a cursor limit that prevents
;; navigating below the prompt.

(require 'vterm)

(setq vterm-max-scrollback 10000)

;;; Make C-g cancel things in the terminal (e.g. bck-i-search)
;; We must remove C-g from exceptions so vterm doesn't hand it to Emacs,
;; then explicitly bind it to send the real C-g (BEL) to the shell.
(setq vterm-keymap-exceptions (delete "C-g" vterm-keymap-exceptions))

;;; Fix spinner jitter — the ⏺ glyph is taller than normal text in most
;;; fonts, so when Claude Code blinks it the line height changes and
;;; everything below shifts.  Swap it for a same-height bullet.
(add-hook 'vterm-mode-hook
          (lambda ()
            (unless buffer-display-table
              (setq buffer-display-table (make-display-table)))
            (aset buffer-display-table ?⏺ [?●])))

;;; Quick-cd shortcuts

(defun jod/vterm-cd (dir)
  "cd to DIR in current vterm buffer, or switch to *vterm* first."
  (unless (derived-mode-p 'vterm-mode)
    (switch-to-buffer "*vterm*"))
  (vterm-send-string (format "cd %s\n" dir)))

(global-set-key (kbd "C-c 1") (lambda () (interactive) (jod/vterm-cd "~/programming/museum/docent-cms")))
(global-set-key (kbd "C-c 2") (lambda () (interactive) (jod/vterm-cd "~/programming/museum/docent-cms-qa")))
(global-set-key (kbd "C-c 3") (lambda () (interactive) (jod/vterm-cd "~/programming/museum/docent-cms-testing")))

;;; Core copy-mode machinery

(defvar-local jod/vterm-cursor-limit-line nil
  "Line number of the input prompt when copy mode was entered.
Movement commands will not go past this line.")

(defun jod/vterm-find-visible-cursor ()
  "Find the buffer position of the visible terminal cursor.
Some terminal apps (like Claude Code) render a status bar below
the input line, pushing point past the visible cursor.  The
visible cursor is drawn with :inverse-video t.  We search the
last 10 lines for it, but only use it if it is above point
\(indicating a status bar gap).  Otherwise just return point."
  (let ((found nil)
        (orig-point (point)))
    (save-excursion
      (goto-char (point-max))
      (let ((lines-checked 0))
        (while (and (not found) (not (bobp)) (< lines-checked 10))
          (let ((pos (line-beginning-position))
                (eol (line-end-position)))
            (while (and (not found) (< pos eol))
              (let ((face (or (get-text-property pos 'font-lock-face)
                              (get-text-property pos 'face))))
                (when (and (listp face) (plist-get face :inverse-video))
                  (setq found pos)))
              (setq pos (1+ pos))))
          (unless found
            (forward-line -1)
            (setq lines-checked (1+ lines-checked))))))
    (if (and found (< found orig-point))
        found
      orig-point)))

(defun jod/vterm-ensure-copy-mode ()
  "Enter vterm-copy-mode if not already in it.
Finds the visible terminal cursor and uses its line as the lower
boundary, then positions point there."
  (unless vterm-copy-mode
    (let* ((cursor-pos (jod/vterm-find-visible-cursor))
           (cursor-line (line-number-at-pos cursor-pos))
           (cursor-col (save-excursion (goto-char cursor-pos) (current-column))))
      (vterm-copy-mode 1)
      (setq jod/vterm-cursor-limit-line cursor-line)
      (goto-char (point-min))
      (forward-line (1- cursor-line))
      (move-to-column cursor-col))))

(defun jod/vterm-clamp-to-cursor-limit ()
  "If point has moved past the saved input line, move it back.
If point is exactly at the cursor limit line, exit copy mode.
Never exit copy mode while a region is active (user is selecting text)."
  (when (and jod/vterm-cursor-limit-line (not (use-region-p)))
    (let ((current-line (line-number-at-pos)))
      (when (>= current-line jod/vterm-cursor-limit-line)
        (vterm-copy-mode -1)))))

(defun jod/vterm-exit-copy-mode-and-send (key)
  "Exit copy mode and send KEY to the terminal."
  (when vterm-copy-mode
    (vterm-copy-mode -1))
  (vterm-send-key key))

;;; Prompt navigation

(defun jod/vterm-return-to-prompt ()
  "Jump to end of buffer, exit copy mode, and send RET."
  (interactive)
  (when vterm-copy-mode
    (goto-char (point-max))
    (vterm-copy-mode -1))
  (vterm-send-return))

(defun jod/vterm-jump-to-prompt ()
  "Jump to end of buffer and exit copy mode."
  (interactive)
  (goto-char (point-max))
  (when vterm-copy-mode
    (vterm-copy-mode -1)))

;;; Terminal-mode movement (keys sent to the shell)

(defun jod/vterm-backward-char ()
  "Send C-b (backward-char) to the terminal."
  (interactive)
  (vterm-send-key "b" nil nil t))

(defun jod/vterm-forward-char ()
  "Send C-f (forward-char) to the terminal."
  (interactive)
  (vterm-send-key "f" nil nil t))

(defun jod/vterm-backward-word ()
  "Send M-b (backward-word) to the terminal."
  (interactive)
  (vterm-send-key "b" nil t))

(defun jod/vterm-forward-word ()
  "Send M-f (forward-word) to the terminal."
  (interactive)
  (vterm-send-key "f" nil t))

(defun jod/vterm-beginning-of-line ()
  "Send C-a (beginning of line) to the terminal."
  (interactive)
  (vterm-send-key "a" nil nil t))

(defun jod/vterm-end-of-line ()
  "Send C-e (end of line) to the terminal."
  (interactive)
  (vterm-send-key "e" nil nil t))

;;; Scrollback movement (enters copy mode, clamped to cursor)

(defun jod/vterm-next-line ()
  "Enter copy mode and move down one line, but not past the cursor."
  (interactive)
  (jod/vterm-ensure-copy-mode)
  (next-line)
  (jod/vterm-clamp-to-cursor-limit))

(defun jod/vterm-previous-line ()
  "Enter copy mode and move up one line."
  (interactive)
  (jod/vterm-ensure-copy-mode)
  (previous-line))

(defun jod/vterm-scroll-up ()
  "Enter copy mode and scroll up (forward in buffer), but not past the cursor."
  (interactive)
  (jod/vterm-ensure-copy-mode)
  (scroll-up)
  (jod/vterm-clamp-to-cursor-limit))

(defun jod/vterm-scroll-down ()
  "Enter copy mode and scroll down (backward in buffer)."
  (interactive)
  (jod/vterm-ensure-copy-mode)
  (scroll-down))

;;; Copy-mode movement (clamped variants for use inside copy mode)

(defun jod/vterm-copy-forward-char ()
  "Move forward one char in copy mode, exit copy mode if at cursor limit."
  (interactive)
  (forward-char)
  (jod/vterm-clamp-to-cursor-limit))

(defun jod/vterm-copy-forward-word ()
  "Move forward one word in copy mode, exit copy mode if at cursor limit."
  (interactive)
  (forward-word)
  (jod/vterm-clamp-to-cursor-limit))

(defun jod/vterm-copy-end-of-line ()
  "Move to end of line in copy mode, exit copy mode if at cursor limit."
  (interactive)
  (end-of-line)
  (jod/vterm-clamp-to-cursor-limit))

;;; Editing commands

(defun jod/vterm-backspace ()
  "Exit copy mode if needed and send backspace to the terminal."
  (interactive)
  (jod/vterm-exit-copy-mode-and-send "<backspace>"))

(defun jod/vterm-backward-kill-word ()
  "In copy mode, kill region if active, otherwise exit and send C-w to bash."
  (interactive)
  (if (and vterm-copy-mode (use-region-p))
      (kill-region (region-beginning) (region-end))
    (when vterm-copy-mode (vterm-copy-mode -1))
    (vterm-send-key "w" nil nil t)))

(defun jod/vterm-kill-ring-save ()
  "In copy mode, copy region to kill ring. Otherwise do nothing."
  (interactive)
  (when (and vterm-copy-mode (use-region-p))
    (kill-ring-save (region-beginning) (region-end))))

;;; Search commands

(defun jod/vterm-isearch-backward ()
  "In copy mode, use Emacs isearch. Otherwise send C-r to bash for reverse search."
  (interactive)
  (if vterm-copy-mode
      (isearch-backward)
    (vterm-send-key "r" nil nil t)))

(defun jod/vterm-isearch-forward ()
  "In copy mode, use Emacs isearch. Otherwise send C-s to bash for forward search."
  (interactive)
  (if vterm-copy-mode
      (isearch-forward)
    (vterm-send-key "s" nil nil t)))

;;; Keybindings — vterm-mode-map (terminal is active)

;; C-g — send to terminal to cancel readline search, etc.
(define-key vterm-mode-map (kbd "C-g") (lambda () (interactive) (vterm-send-key "g" nil nil t)))


;; Horizontal movement — send to terminal so you can edit the current command
(define-key vterm-mode-map (kbd "C-h") #'jod/vterm-backward-char)
(define-key vterm-mode-map (kbd "C-n") #'jod/vterm-forward-char)
(define-key vterm-mode-map (kbd "M-h") #'jod/vterm-backward-word)
(define-key vterm-mode-map (kbd "M-n") #'jod/vterm-forward-word)
(define-key vterm-mode-map (kbd "C-a") #'jod/vterm-beginning-of-line)
(define-key vterm-mode-map (kbd "C-e") #'jod/vterm-end-of-line)

;; Vertical movement — enter copy mode for scrollback
(define-key vterm-mode-map (kbd "C-t") #'jod/vterm-next-line)
(define-key vterm-mode-map (kbd "C-b") #'jod/vterm-previous-line)
(define-key vterm-mode-map (kbd "M-t") #'jod/vterm-scroll-up)
(define-key vterm-mode-map (kbd "M-c") #'jod/vterm-scroll-down)

;; Editing keys — context-aware between copy mode and terminal
(define-key vterm-mode-map (kbd "C-p") #'jod/vterm-backspace)
(define-key vterm-mode-map (kbd "C-w") #'jod/vterm-backward-kill-word)
(defun jod/vterm-set-mark ()
  "Enter copy mode if needed, then set the mark for selecting a region."
  (interactive)
  (unless vterm-copy-mode
    (vterm-copy-mode 1))
  (set-mark-command nil))

(define-key vterm-mode-map (kbd "C-SPC") #'jod/vterm-set-mark)
(define-key vterm-mode-map (kbd "C-.") #'jod/vterm-kill-ring-save)
(define-key vterm-mode-map (kbd "M-w") #'jod/vterm-kill-ring-save)
(define-key vterm-mode-map (kbd "RET") #'jod/vterm-return-to-prompt)

;; Search — isearch in copy mode, readline search in terminal
(define-key vterm-mode-map (kbd "M-s") #'jod/vterm-isearch-forward)
(define-key vterm-mode-map (kbd "M-r") (lambda () (interactive) (vterm-send-key "r" nil nil t)))

;;; Keybindings — vterm-copy-mode-map (browsing scrollback)

(define-key vterm-copy-mode-map (kbd "C-h") #'backward-char)
(define-key vterm-copy-mode-map (kbd "C-n") #'jod/vterm-copy-forward-char)
(define-key vterm-copy-mode-map (kbd "C-t") #'jod/vterm-next-line)
(define-key vterm-copy-mode-map (kbd "C-b") #'previous-line)
(define-key vterm-copy-mode-map (kbd "M-h") #'backward-word)
(define-key vterm-copy-mode-map (kbd "M-n") #'jod/vterm-copy-forward-word)
(define-key vterm-copy-mode-map (kbd "M-t") #'jod/vterm-scroll-up)
(define-key vterm-copy-mode-map (kbd "M-c") #'scroll-down)
(define-key vterm-copy-mode-map (kbd "C-a") #'beginning-of-line)
(define-key vterm-copy-mode-map (kbd "C-e") #'jod/vterm-copy-end-of-line)
(define-key vterm-copy-mode-map (kbd "C-w") #'jod/vterm-backward-kill-word)
(define-key vterm-copy-mode-map (kbd "C-SPC") #'set-mark-command)
(define-key vterm-copy-mode-map (kbd "C-.") #'kill-ring-save)
(define-key vterm-copy-mode-map (kbd "M-w") #'kill-ring-save)
(define-key vterm-copy-mode-map (kbd "M-s") #'isearch-forward-regexp)
(define-key vterm-copy-mode-map (kbd "M->") #'jod/vterm-jump-to-prompt)

;; Project-wide search
(define-key vterm-mode-map (kbd "C-c C-g") #'projectile-ripgrep)
(define-key vterm-copy-mode-map (kbd "C-c C-g") #'projectile-ripgrep)

(provide 'vterm-improvements)
