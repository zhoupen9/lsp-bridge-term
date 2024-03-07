;;; lsp-bridge-term.el --- LSP bridge term -*- lexical-binding: t -*-

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;; lsp-bridge terminal environment support

;;; Code:

(require 'subr-x)
(require 'acm)
(require 'popon)

(defvar lsp-bridge-term-buffer "*lsp-bridge-term*")
(defvar lsp-bridge-term-doc-line-max 75)
(defvar lsp-bridge-term-popup-min-height 5)
(defvar lsp-bridge-term-popup-max-height 25)
(defvar lsp-bridge-term-diagnostics-inline nil)

(defvar-local lsp-bridge-term--frame nil)
(defvar-local lsp-bridge-term--lines nil)
(defvar-local lsp-bridge-term--doc nil)
(defvar-local lsp-bridge-term--frame-popup-point nil)
(defvar-local lsp-bridge-term--menu-index 0)
(defvar-local lsp-bridge-term--menu-max -1)
(defvar-local lsp-bridge-term--doc-index 0)
(defvar-local lsp-bridge-term--doc-max -1)
(defvar-local lsp-bridge-term--completion-point nil)
(defvar-local lsp-bridge-term--code-actions nil)

(defun lsp-bridge-term-diagnostics-inline-toggle ()
  "Toogle display inline diagnostics."
  (interactive)
  (setq lsp-bridge-term-diagnostics-inline (not lsp-bridge-term-diagnostics-inline)))

(defface lsp-bridge-term-callable-face
  '((t :background "grey20" :foreground "brightmagenta"))
  "Default callable face."
  :group 'lsp-bridge-term)

(defface lsp-bridge-term-symbol-face
  '((t :background "grey20" :foreground "royalblue1"))
  "Default symbol face."
  :group 'lsp-bridge-term)

(defface lsp-bridge-term-module-face
  '((t :background "grey20" :foreground "gold3"))
  "Default symbol face."
  :group 'lsp-bridge-term)

(defface lsp-bridge-term-key-face
  '((t :background "grey20" :foreground "royalblue1"))
  "Default key face."
  :group 'lsp-bridge-term)

(defvar lsp-bridge-term-annotation-icons
  '(("Function" . ("󰡱" lsp-bridge-term-callable-face))
    ("Keyword" . ("" lsp-bridge-term-key-face))
    ("Special Form" . ("" lsp-bridge-term-key-face))
    ("Module" . ("" lsp-bridge-term-module-face))
    ("Method" . ("" lsp-bridge-term-callable-face))
    ("Struct" . ("" lsp-bridge-term-module-face))
    ("Snippet" . ("" lsp-bridge-term-symbol-face))
    ("Yas-Snippet" . ("" lsp-bridge-term-symbol-face))
    ("Face" . ("󰙃" lsp-bridge-term-symbol-face))
    ("Text" . ("" lsp-bridge-term-symbol-face))
    ("Variable" . ("󰫧" lsp-bridge-term-symbol-face))
    ("Class" . ("" lsp-bridge-term-module-face))
    ("Custom" . ("" lsp-bridge-term-symbol-face))
    ("Feature" . ("󰯺" lsp-bridge-term-key-face))
    ("Macro" . ("󰰏" lsp-bridge-term-callable-face))
    ("Interface" . ("" lsp-bridge-term-module-face))
    ("Constant" . ("" lsp-bridge-term-symbol-face))
    ("Field" . ("" lsp-bridge-term-symbol-face))
    (nil . ("T" lsp-bridge-term-symbol-face)))
  "Annotation icons.")

(defgroup lsp-bridge-term nil "lsp-bridge terminal group.")

(defcustom lsp-bridge-term-completion-trigger
  '("." "->" "=>")
  "Symbols triggers completion."
  :group 'lsp-bridge-term)

(defface lsp-bridge-term-select-face
  '((t :background "grey10" :foreground "grey85"))
  "Default terminal menu select face."
  :group 'lsp-bridge-term)

(defface lsp-bridge-term-default-face
  '((t :background "grey20" :foreground "grey85"))
  "Default terminal face."
  :group 'lsp-bridge-term)

(defface lsp-bridge-term-diagnostic-symbol-face
  '((t :foreground "grey35" :underline t))
  "Diagnostic symbol face."
  :group 'lsp-bridge-term)

(defface lsp-bridge-term-diagnostic-message-face
  '((t :foreground "dark red" :inherit 'italic))
  "Diagnostic inline message face."
  :group 'lsp-bridge-term)

(defface lsp-bridge-term-diagnostic-indicator-face
  '((t :foreground "red"))
  "Diagnostic indicator face."
  :group 'lsp-bridge-term)

(defun lsp-bridge-term-can-display-p ()
  "Returns whether terminal can display, required by lsp-bridge.el."
  (not (or noninteractive
           emacs-basic-display)))

(defun lsp-bridge-term--menu-item-icon-text (annotation selected)
  "Returns propertized icon text for given annotation."
  (let ((text (cdr (assoc annotation lsp-bridge-term-annotation-icons))))
    (propertize (format " %s " (car text)) 'face
                (if selected
                    'lsp-bridge-term-select-face
                  (cadr text)))))

(defun lsp-bridge-term--get-popup-position (&optional position frame)
  "Returns popup anchor position at POSITION or anchor of existing FRAME."
  (cond
   ((poponp frame) (plist-get (cdr frame) :popup))
   ((numberp position)
    (popon-x-y-at-pos position))
   (t (lsp-bridge-term--get-popup-position (point)))))

(defun lsp-bridge-term-line-number-display-width ()
  "Return width of line number bar."
  (if (bound-and-true-p display-line-numbers-mode)
      (+ (line-number-display-width) 2)
    0))

(cl-defmacro lsp-bridge-term--disable-change-hooks ()
  "Disable lsp-bridge change hooks when modifying buffer without interact with lsp backend."
  `(dolist (hook lsp-bridge--internal-hooks)
     (message "disable lsp-bridge hooks.")
     (remove-hook (nth 0 hook) (nth 1 hook) (nth 3 hook))))

(cl-defmacro lsp-bridge-term--enable-change-hooks ()
  "Enable lsp-bridge change hooks after modifying buffer."
  `(dolist (hook lsp-bridge--internal-hooks)
     (message "enable lsp-bridge hooks.")
     (apply #'add-hook hook)))

(cl-defmacro lsp-bridge-term--without-hooks (&rest body)
  "Execute without lsp-bridge hooks."
  (declare (indent 1))
  `(progn (lsp-bridge-term--disable-change-hooks)
          ,@body
          (lsp-bridge-term--enable-change-hooks)))

(cl-defmacro lsp-bridge-term--create-frame-if-not-exist (type frame pos)
  "Create new popup frame using POS as popup anchor when absent."
  `(unless (popon-live-p ,frame)
     (setq position ,pos)
     (setq ,frame (popon-create (cons "" 0) position))
     (plist-put (cdr ,frame) :type ,type)
     (plist-put (cdr ,frame) :display (cons 0 0))
     (plist-put (cdr ,frame) :popup position)
     (plist-put (cdr ,frame) :direction nil)
     (plist-put (cdr ,frame) :eobp nil)))

(defun lsp-bridge-term--lines-max-length (lines)
  "Returns max length of given lines."
  (let ((max-length 0))
    (dolist (v lines)
      (let ((len (length v)))
        (if (< max-length (length v))
            (setq max-length len))))
    max-length))

(defun lsp-bridge-term--candidates-max-length (candidates)
  "Returns max length of given candidates."
  (let ((lines '()))
    (dolist (v candidates)
      (add-to-list 'lines (plist-get v :displayLabel)))
    (lsp-bridge-term--lines-max-length lines)))

(defun lsp-bridge-term--display-in-other-window ()
  "Display content in other window."
  (unless (get-buffer-window "*lsp-bridge-term*")
    (switch-to-buffer-other-window "*lsp-bridge-term*")))

(defun lsp-bridge-term--frame-render-lines (frame lines &optional index direction action)
  "Render LINES or portion of LINES in the FRAME with preferred DIRECTION."
  (pcase-let* ((`(,edge-left ,edge-top ,edge-right ,edge-bottom) (window-inside-edges))
               (textarea-width
                (- (window-width)
                   (+ (- edge-left (window-left-column))
                      (lsp-bridge-term-line-number-display-width))))
               (textarea-height (- edge-bottom edge-top))
               (`(,cursor-x . ,cursor-y) (plist-get (cdr frame) :popup))
               (top-free-h (- cursor-y edge-top))
               (bottom-free-h (- edge-bottom edge-top cursor-y))
               (display-width (length (car lines)))
               (full-height (length lines))
               (display-height 0)
               (`(,begin . ,end) (plist-get (cdr frame) :display)))
    (if (or (< textarea-width display-width)
            (and (< top-free-h lsp-bridge-term-popup-min-height)
                 (< bottom-free-h lsp-bridge-term-popup-min-height)))
        ;; Insufficient space to display lines in current window, display in other window.
        (lsp-bridge-term--display-in-other-window)
      (setq display-height
            (cond
             ((< lsp-bridge-term-popup-min-height full-height lsp-bridge-term-popup-max-height)
              full-height)
             ((<= full-height lsp-bridge-term-popup-min-height)
              full-height)
             ((< lsp-bridge-term-popup-max-height full-height)
              lsp-bridge-term-popup-max-height)))
      (when (and (= 0 begin) (= 0 end))
        (setq end (1- display-height)))
      (let ((x (if (> textarea-width (+ cursor-x display-width))
                   cursor-x(display-width (length (car lines)))
                   (full-height (length lines))
                   (- cursor-x (- (+ cursor-x display-width) textarea-width) 1))))
        (plist-put (cdr frame) :x x))
      (setq direction
            (cond
             ((eq 'top direction)
              (if (> top-free-h display-height) 'top 'bottom))
             ((eq 'bottom direction)
              (if (> bottom-free-h display-height) 'bottom 'top))
             (t (if (> bottom-free-h top-free-h) 'bottom 'top))))
      (when (numberp index)
        (cond
         ((eq 'prev action)
          (setq begin (if (and (> begin index) (< 0 begin)) (1- begin) begin))
          (setq end (+ begin display-height -1)))
         ((eq 'next action)
          (setq end (if (and (< end index) (< end full-height)) (1+ end) end))
          (setq begin (- end display-height -1)))))
      (plist-put (cdr frame) :display (cons begin end))
      (plist-put (cdr frame) :lines (take display-height (nthcdr begin lines)))
      (plist-put (cdr frame) :direction direction)
      (plist-put (cdr frame) :width display-width)
      (cond
       ((eq 'top direction)
        (plist-put (cdr frame) :y (- cursor-y display-height)))
       ((eq 'bottom direction)
        (plist-put (cdr frame) :y (+ cursor-y 1)))))))

(defun lsp-bridge-term--menu-items-render (candidates index)
  "Render menu items with CANDIDATES, and selected menu item indentified by INDEX."
  (let ((item-index 0)
        (max-length (+ 1 (lsp-bridge-term--candidates-max-length candidates))))
    (dolist (v candidates)
      (let* ((display (plist-get v :displayLabel))
             (icon (plist-get v :icon))
             (annotation (plist-get v :annotation))
             (padding (- max-length (length display)))
             icon candidate)
        (setq icon (lsp-bridge-term--menu-item-icon-text annotation (eq item-index index)))
        (setq candidate
              (concat
               (if (eq 0 padding)
                   display
                 (concat display (make-string padding ?\s)))
               "\n"))
        (add-face-text-property 0 (length candidate)
                                (if (equal item-index index)
                                    'lsp-bridge-term-select-face
                                  'lsp-bridge-term-default-face)
                                'append candidate)
        (insert icon)
        (insert candidate)
        (when (equal item-index (1- (length candidates)))
          (delete-char -1))
        (setq item-index (1+ item-index))))))

(cl-defmacro lsp-bridge-term--popup-display ()
  "Display popup."
  `(progn
      (popon-redisplay)
      (lsp-bridge-term-mode 1)
      (plist-put (cdr lsp-bridge-term--frame) :visible t)
      (add-hook 'pre-command-hook #'lsp-bridge-term--pre-command nil 'local)))

(defun lsp-bridge-term--menu-render (pos candidates index &optional action)
  "Render menu in `lsp-bridge-term-buffer' and then render resulting text
of (portion of resulting text) in `lsp-bridge-term--frame'."
  (let ((len (length candidates))
        lines)
    (setq-local lsp-bridge-term--menu-max len)
    (when (and lsp-bridge-term--frame (< 0 len))
      (with-current-buffer (get-buffer-create lsp-bridge-term-buffer)
        (read-only-mode 0)
        (erase-buffer)
        (lsp-bridge-term--menu-items-render candidates index)
        (goto-char (point-min))
        (setq lines (split-string (buffer-string) "\n"))
        (read-only-mode 1))
      (lsp-bridge-term--frame-render-lines lsp-bridge-term--frame lines index 'bottom action))
    (lsp-bridge-term--popup-display)))

(defun lsp-bridge-term--cancel-if-present ()
  "Cancel when present."
  (when (popon-live-p lsp-bridge-term--frame)
    (lsp-bridge-term-cancel)))

(defun lsp-bridge-term--symbol-end ()
  "Returns true when current point is at end of symbol."
  (save-excursion
    (let ((orig (point)))
      (forward-symbol -1)
      (forward-symbol 1)
      (= (point) orig))))

(defun lsp-bridge-term--trigger-end ()
  "Returns true when current point is at end of completion trigger."
  (save-excursion
    (let ((orig (point)))
      (forward-symbol -1)
      (forward-symbol 1)
      (let ((end (point)))
        (if (= end orig)
            nil
          (let ((prev (buffer-substring-no-properties end orig)))
            (member prev lsp-bridge-term-completion-trigger)))))))

(defun lsp-bridge-term--trigger-completion ()
  "Returns true when current point should trigger completion."
  (cond
   ;; ((and lsp-bridge-term--completion-point lsp-bridge-term--frame-popup-point)
   ;;       (not (= lsp-bridge-term--completion-point lsp-bridge-term--frame-popup-point)))
        ((bounds-of-thing-at-point 'symbol) (lsp-bridge-term--symbol-end))
        ((bounds-of-thing-at-point 'whitespace) (lsp-bridge-term--trigger-end))
        (t nil)))

(defun lsp-bridge-term--menu-update (candidates index &optional pos action)
  "Update terminal menu. Insert \n when at end of current buffer before
rendering menu."
  (if (< 0 (length candidates))
      (setq-local lsp-bridge-term--lines candidates)
    (setq candidates lsp-bridge-term--lines))
  (unless pos
    (setq pos (lsp-bridge-term--get-popup-position (point))))
  (let (end-of-this-buffer)
    (when (eobp)
      (setq end-of-this-buffer t)
      (lsp-bridge-term--without-hooks
       (let ((modified (buffer-modified-p)))
         (save-excursion
           (goto-char (point-max))
           (insert "\n")
           (set-buffer-modified-p modified)))))
    (lsp-bridge-term--create-frame-if-not-exist 'completion lsp-bridge-term--frame pos)
    (when end-of-this-buffer
      (plist-put (cdr lsp-bridge-term--frame) :eobp t))
    (setq-local lsp-bridge-term--menu-index index)
    (lsp-bridge-term--menu-render lsp-bridge-term--frame-popup-point candidates index action)))

(defun lsp-bridge-term-cancel ()
  "Cancel lsp completion, code action, doc and any exiting ui."
  (interactive)
  (lsp-bridge-term-mode 0)
  (with-current-buffer (get-buffer-create lsp-bridge-term-buffer)
    (when (eq major-mode 'gfm-view-mode)
      (gfm-view-mode)
      (read-only-mode 0)))
  (remove-hook 'pre-command-hook #'lsp-bridge-term--pre-command 'local)
  (popon-kill-all)
  ;;(when (bufferp lsp-bridge-term-buffer)
  ;;  (kill-buffer lsp-bridge-term-buffer))
  (when (and (poponp lsp-bridge-term--frame)
             (plist-get (cdr lsp-bridge-term--frame) :eobp))
    (let ((modified (buffer-modified-p)))
      (lsp-bridge-term--without-hooks
       (save-excursion
         (goto-char (point-max))
         (delete-char -1))
       (set-buffer-modified-p modified))))
  (setq-local lsp-bridge-term--frame nil)
  (setq-local lsp-bridge-term--menu-max -1)
  (setq-local lsp-bridge-term--menu-index 0)
  (setq-local lsp-bridge-term--doc-index 0)
  (setq-local lsp-bridge-term--lines nil)
  (setq-local lsp-bridge-term--code-actions nil))

(defun lsp-bridge-term-select-next ()
  "Select next item in menu."
  (interactive)
  (when (popon-live-p lsp-bridge-term--frame)
    (pcase-let ((type (plist-get (cdr lsp-bridge-term--frame) :type))
                (`(,begin . ,end) (plist-get (cdr lsp-bridge-term--frame) :display)))
      (cond
       ((or (eq 'completion type) (eq 'code-action type))
        (unless (or (= -1 lsp-bridge-term--menu-index)
                    (= lsp-bridge-term--menu-index (1- lsp-bridge-term--menu-max)))
          (setq-local lsp-bridge-term--menu-index (1+ lsp-bridge-term--menu-index))
          (lsp-bridge-term--menu-update nil lsp-bridge-term--menu-index nil 'next)))
       ((eq 'doc type)
        (when (< end (1- lsp-bridge-term--doc-max))
          (setq-local lsp-bridge-term--doc-index (1+ end))
          (lsp-bridge-term--frame-render-lines
           lsp-bridge-term--frame
           lsp-bridge-term--lines
           lsp-bridge-term--doc-index
           nil
           'next)
          (lsp-bridge-term--popup-display)))))))

(defun lsp-bridge-term-select-prev ()
  "Select previous item in menu."
  (interactive)
  (when (popon-live-p lsp-bridge-term--frame)
    (pcase-let ((type (plist-get (cdr lsp-bridge-term--frame) :type))
                (`(,begin . ,end) (plist-get (cdr lsp-bridge-term--frame) :display)))
      (cond
       ((or (eq 'completion type) (eq 'code-action type))
        (unless (or (= -1 lsp-bridge-term--menu-index)
                    (= lsp-bridge-term--menu-index 0))
          (setq-local lsp-bridge-term--menu-index (1- lsp-bridge-term--menu-index))
          (lsp-bridge-term--menu-update nil lsp-bridge-term--menu-index nil 'prev)))
       ((eq 'doc type)
        (when (< 0 begin)
          (setq-local lsp-bridge-term--doc-index (1- begin))
          (lsp-bridge-term--frame-render-lines
           lsp-bridge-term--frame
           lsp-bridge-term--lines
           lsp-bridge-term--doc-index
           nil
           'prev)
          (lsp-bridge-term--popup-display)))))))

(defun lsp-bridge-term-complete ()
  "Select candidate in menu."
  (interactive)
  (when (popon-live-p lsp-bridge-term--frame)
    (let ((type (plist-get (cdr lsp-bridge-term--frame) :type)))
      (cond
       ((eq 'completion type)
        (let* ((candidate (nth lsp-bridge-term--menu-index lsp-bridge-term--lines))
               (bound-start lsp-bridge-term--frame-popup-point)
               (backend (plist-get candidate :backend))
               (candidate-expand (intern-soft (format "acm-backend-%s-candidate-expand" backend))))
          (if (fboundp candidate-expand)
              (funcall candidate-expand candidate bound-start)
            (lsp-bridge-term--without-hooks
             (delete-region bound-start (point))
             (insert (plist-get candidate :label))))
          (setq-local lsp-bridge-term--completion-point lsp-bridge-term--frame-popup-point)))
       ((eq 'code-action type)
        (let ((code-action (nth lsp-bridge-term--menu-index lsp-bridge-term--code-actions)))
          (lsp-bridge-code-action--fix-do code-action))))
      (lsp-bridge-term-cancel))))

(defvar lsp-bridge-term-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap next-line] #'lsp-bridge-term-select-next)
    (define-key map [remap previous-line] #'lsp-bridge-term-select-prev)
    (define-key map [down] #'lsp-bridge-term-select-next)
    (define-key map [up] #'lsp-bridge-term-select-prev)
    (define-key map [tab]  #'lsp-bridge-term-complete)
    (define-key map "\C-m" #'lsp-bridge-term-complete)
    (define-key map "\t" #'lsp-bridge-term-complete)
    (define-key map "\n" #'lsp-bridge-term-complete)
    (define-key map "\C-g" #'lsp-bridge-term-cancel)
    map)
  "Keymap used when popup is shown.")

(defvar lsp-bridge-term--commands
  '(lsp-bridge-term-select-next lsp-bridge-term-select-prev lsp-bridge-term-complete))

(defun lsp-bridge-term--pre-command ()
  "Function execute before executing command."
  (unless (acm-match-symbol-p lsp-bridge-term--commands this-command)
    (lsp-bridge-term--cancel-if-present)))

(defun lsp-bridge-term-overriding-key-setup ()
  "Some key define in language mode map will conflict with lsp-bridge-term-mode map.
So we use `minor-mode-overriding-map-alist' to override key, make sure all keys in lsp-bridge-term-mode can response."
  (let ((override-map (make-sparse-keymap)))
    (define-key override-map [?\C-m] 'lsp-bridge-term-complete)))

;;;###autoload
(define-minor-mode lsp-bridge-term-mode
  "LSP Bridge Terminal minor mode."
  :keymap lsp-bridge-term-mode-map
  :init-value nil
  ;; Set override map, avoid some language mode map conflict with lsp-bridge-term-mode map.
  (lsp-bridge-term-overriding-key-setup))

(defun lsp-bridge-term-completion-recv-items (filename filehost candidates position server-name
                                           completion-trigger-characters server-names)
  "Receive lsp-bridge completion."
  (cond
   ((or (= 0 (length candidates))
        (not (lsp-bridge-term--trigger-completion)))
    (lsp-bridge-term--cancel-if-present))
   (t
    (when (popon-live-p lsp-bridge-term--frame)
      (lsp-bridge-term-cancel))
    (lsp-bridge--with-file-buffer
        filename filehost
        ;; `acm-backend-lsp-candidate-expand` needs `acm-backend-lsp-completion-position` to be set
        ;; in `lsp-bridge-term-complete` function when select candidate.
        (setq-local acm-backend-lsp-completion-position position)
        (let ((completion-table (make-hash-table :test 'equal)))
          (dolist (item candidates)
            (plist-put item :annotation (capitalize (plist-get item :icon)))
            (puthash (plist-get item :key) item completion-table))))
    (let* ((bounds (acm-get-input-prefix-bound)))
      (setq-local lsp-bridge-term--frame-popup-point (or (car bounds) (point))))
    (lsp-bridge-term--menu-update
     candidates 0
     (lsp-bridge-term--get-popup-position lsp-bridge-term--frame-popup-point)))))

(defun lsp-bridge-term--code-action-popup-menu (actions action)
  "Popup code action menu."
  (let ((candidates '()))
    (lsp-bridge-term--create-frame-if-not-exist 'code-action lsp-bridge-term--frame (lsp-bridge-term--get-popup-position))
    (dolist (v actions)
      (let* ((title (plist-get v :title))
             (candidate (list :key title :label title :icon "function" :annotation "Function" :displayLabel title)))
        (add-to-list 'candidates candidate 'append)))
    (lsp-bridge-term--menu-update candidates 0)))

(defun lsp-bridge-term-code-action-recv-actions (actions action-kind)
  "Receive lsp-bridge code actions."
  (setq-local lsp-bridge-term--menu-max (length actions))
  (setq-local lsp-bridge-term--code-actions actions)
  (setq-local lsp-bridge-term--frame-popup-point (point))
  (lsp-bridge-term--code-action-popup-menu actions action-kind))

(defun lsp-bridge-term--get-position-at-x-y (x y)
  "Returns position at x:y."
  (save-excursion
    (goto-char (point-min))
    (when (< 0 x)
      (forward-line  x))
    (when (< 0 y)
      (forward-char y))
    (point)))

(defun lsp-bridge-term--get-line-end-pos (pos)
  "Return line end position from pos."
  (save-excursion
    (goto-char pos)
    (cons (line-beginning-position) (line-end-position))))

(defun lsp-bridge-term--render-diagnostic (diagnostic)
  "Render diagnostic."
  (let ((range (plist-get diagnostic :range)))
    (when range
      (let* ((start (plist-get range :start))
             (end (plist-get range :end))
             (startp (lsp-bridge-term--get-position-at-x-y
                      (plist-get start :line)
                      (plist-get start :character)))
             (endp (lsp-bridge-term--get-position-at-x-y
                    (plist-get end :line)
                    (plist-get end :character)))
             (line (lsp-bridge-term--get-line-end-pos endp))
             (code (plist-get diagnostic :code))
             (msg (plist-get diagnostic :message))
             (font-lock-fontify-region-function 'ignore)
             (inhibit-modification-hooks t)
             (modified (buffer-modified-p))
             indicator overlay)
        (unless lsp-bridge-term-diagnostics-inline
          (setq indicator (make-overlay (car line) (car line)))
          (overlay-put indicator 'before-string
                       (propertize "" 'face 'lsp-bridge-term-diagnostic-indicator-face))
          (overlay-put indicator 'type 'diagnostic))
        (cond
         ((= startp endp (line-beginning-position))
          (when (not (= (car line) (cdr line)))
            (put-text-property (car line) (cdr line) 'face 'lsp-bridge-term-diagnostic-symbol-face)
            (put-text-property (car line) (cdr line) 'font-lock-ignore t)))
         (t
          (put-text-property startp endp 'face 'lsp-bridge-term-diagnostic-symbol-face)
          (put-text-property startp endp 'font-lock-ignore t)))
        (when (and lsp-bridge-term-diagnostics-inline msg)
          (setq overlay (make-overlay (cdr line) (cdr line)))
          (overlay-put overlay 'after-string
                       (propertize
                        (format "  %s%s" (if code (format "%s: " code) "") msg)
                        'face 'lsp-bridge-term-diagnostic-message-face))
          (overlay-put indicator 'type 'diagnostic))
        (set-buffer-modified-p modified)))))

(defun lsp-bridge-term-diagnostic-recv-items (filepath filehost diagnostics diagnostic-count)
  "Receive lsp-bridge diagnostic."
  (unless (popon-live-p lsp-bridge-term--frame)
    (dolist (buf (buffer-list))
      (when (string= filepath (buffer-file-name buf))
        (with-current-buffer buf
          (remove-overlays (point-min) (point-max) 'type 'diagnostic)
          (dolist (diag diagnostics)
            (lsp-bridge-term--render-diagnostic diag)))))))

(defun lsp-bridge-term-signature-help-recv (helps index)
  "Receive lsp-bridge signature helps."
  (unless (popon-live-p lsp-bridge-term--frame)
    (lsp-bridge-term--create-frame-if-not-exist 'doc lsp-bridge-term--frame (lsp-bridge-term--get-popup-position))
    (let ((lines '()) max-line-length)
      (with-current-buffer (get-buffer-create lsp-bridge-term-buffer)
        (read-only-mode 0)
        (erase-buffer)
        (insert "\n")
        (let ((i 0) str)
          (dolist (param helps)
            (setq str param)
            (when (= index i)
              (add-face-text-property 0 (length str) '((t :foreground "magenta3")) 'append str))
            (insert str)
            (when (> (length helps) (1+ i))
              (insert ", "))
            (setq i (1+ i))))
        (insert "\n\n")
        (read-only-mode 1)
        (setq max-line-length (+ 2 (lsp-bridge-term--max-line-length)))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (lsp-bridge-term--render-doc-current-line #'buffer-substring max-line-length)))
            (lsp-bridge-term--append-lines lines line))
          (forward-line))
        (dolist (line lines)
          (add-face-text-property 0 (length line) '((t :background "grey20")) 'append line)))
      (lsp-bridge-term--frame-render-lines lsp-bridge-term--frame lines -1 'top))
    (lsp-bridge-term--popup-display)))

(defun lsp-bridge-term--elisp-render (items)
  "Render elisp."
  (let ((candidates '()))
    (dolist (item items)
      (let ((symbol-type (acm-backend-elisp-symbol-type (intern item))))
        (add-to-list 'candidates
                     (list :key item
                           :icon symbol-type
                           :label item
                           :displayLabel item
                           :annotation (capitalize symbol-type)
                           :backend "elisp")
                     'append)))
    (let* ((bounds (acm-get-input-prefix-bound)))
      (setq-local lsp-bridge-term--frame-popup-point (or (car bounds) (point))))
    (lsp-bridge-term--menu-update
     candidates 0
     (lsp-bridge-term--get-popup-position lsp-bridge-term--frame-popup-point))))

(defun lsp-bridge-term-search-backend-recv-items (backend items)
  "Receive lsp-bridge search backend."
  (cond
   ((string= "elisp" backend)
    (lsp-bridge-term--elisp-render items))))

(cl-defmacro lsp-bridge-term--append-lines (to lines)
  "Fill string with whitespace as padding."
  `(setq ,to (append ,to ,lines)))

(cl-defmacro lsp-bridge-term--append-lines-str (to str)
  "Fill string with given string."
  `(lsp-bridge-term--append-lines ,to (list ,str)))

(cl-defmacro lsp-bridge-term--append-empty-line (to max)
  "Fill empty line into lines."
  `(lsp-bridge-term--append-lines-str ,to (make-string ,max ?\s)))

(defun lsp-bridge-term--render-doc-current-line (visible-fn max-len)
  "Render doc line into rendered line or lines."
  (let ((begin (line-beginning-position))
        (end (line-end-position))
        (lines '()))
    (if (= begin end)
        (lsp-bridge-term--append-empty-line lines max-len)
      (while (< begin end)
        (let* ((visible (funcall visible-fn begin end))
               (len (length visible))
               (padding (- max-len len 2)))
          (cond ((= 0 len)
                 ;; no visiable, insert empty line
                 (lsp-bridge-term--append-empty-line lines max-len)
                 (setq begin end))
                ((> 0 padding)
                 ;; line is longer than `max-len', split line into multiple lines,
                 ;; and avoid word wrapping.
                 (let* ((pos (+ end padding))
                        (str (reverse (buffer-substring begin pos)))
                        (blank-index (string-match-p "[[:blank:]]" str)))
                   (if blank-index
                       (setq end (- pos blank-index))
                     (setq end pos))))
                (t
                 (lsp-bridge-term--append-lines-str
                  lines
                  (format " %s%s "
                          (string-replace "\t" " " (buffer-substring begin end))
                          (if (< 0 padding)
                              (make-string padding ?\s)
                            "")))
                 (setq begin end)
                 (setq end (line-end-position)))))))
    lines))

(defun lsp-bridge-term--max-line-length ()
  "Returns max line length of current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((max-len 0))
      (while (not (eobp))
        (let ((len (- (line-end-position) (line-beginning-position))))
          (when (< max-len len)
            (setq max-len len)))
        (forward-line))
      (if (< lsp-bridge-term-doc-line-max max-len)
          lsp-bridge-term-doc-line-max
        max-len))))

(defun lsp-bridge-term-recv-doc (doc)
  "Receive lsp-bridge documentation popup. Render DOC in `lsp-bridge-term-buffer'
and then render resulting text (or portion of resulting text) in `lsp-bridge-term--frame'."
  (lsp-bridge-term--cancel-if-present)
  (lsp-bridge-term--create-frame-if-not-exist 'doc lsp-bridge-term--frame (lsp-bridge-term--get-popup-position))
  (let ((lines '()) max-line-length)
    (with-current-buffer (get-buffer-create lsp-bridge-term-buffer)
      (read-only-mode 0)
      (erase-buffer)
      (insert doc)
      (gfm-view-mode)
      (font-lock-ensure)
      (setq max-line-length (+ 2 (lsp-bridge-term--max-line-length)))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (lsp-bridge-term--render-doc-current-line #'markdown--filter-visible max-line-length)))
            (lsp-bridge-term--append-lines lines line))
          (forward-line))
        ;; add empty line at the end of doc
        (let ((lastline (last lines)))
          (unless (string-match "^\s*$" (car lastline))
            (lsp-bridge-term--append-empty-line lines max-line-length)))
        ;; change doc background face
        (dolist (line lines)
          (add-face-text-property 0 (length line) '((t :background "grey20")) 'append line))))
    (setq-local lsp-bridge-term--lines lines)
    (setq-local lsp-bridge-term--doc-index 0)
    (setq-local lsp-bridge-term--doc-max (length lines))
    (lsp-bridge-term--frame-render-lines lsp-bridge-term--frame lines 0 nil nil))
  (lsp-bridge-term--popup-display))

(defun lsp-bridge-term-post-command ())

(defvar lsp-bridge-term-advices
  '((lsp-bridge-code-action--fix :override lsp-bridge-term-code-action-recv-actions)
    (lsp-bridge-completion--record-items :override lsp-bridge-term-completion-recv-items)
    (lsp-bridge-diagnostic--render :override lsp-bridge-term-diagnostic-recv-items)
    (lsp-bridge-popup-documentation--callback :override lsp-bridge-term-recv-doc)
    (lsp-bridge-signature-help--update :override lsp-bridge-term-signature-help-recv)
    (lsp-bridge-monitor-post-command :override lsp-bridge-term-post-command)
    (lsp-bridge-search-backend--record-items :override lsp-bridge-term-search-backend-recv-items))
    "advices to adapt lsp-bridge.")

(defun lsp-bridge-term-active ()
  "Activate lsp-bridge terminal support."
  (setq-local lsp-bridge-prohibit-completion t)
  (mapc (pcase-lambda (`(,orig-fn ,how ,function))
          (advice-add orig-fn how function))
        lsp-bridge-term-advices))

(defun lsp-bridge-term-deactive ()
  "Deactivate lsp-bridge terminal support."
  (mapc (pcase-lambda (`( ,orig-fn ,_ ,function ))
          (advice-remove orig-fn function))
        lsp-bridge-term-advices))

(unless window-system
  (lsp-bridge-term-active))

(provide 'lsp-bridge-term)

;;; lsp-bridge-term.el ends here
