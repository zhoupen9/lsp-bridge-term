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

(defvar-local lsp-bridge-term-menu nil)
(defvar lsp-bridge-term-buffer "*lsp-bridge-term*")
(defvar lsp-bridge-term-frame nil)
(defvar lsp-bridge-term-candidates nil)
(defvar-local lsp-bridge-term-frame-popup-point nil)

(defvar-local lsp-bridge-term-menu-index 0)
(defvar-local lsp-bridge-term-menu-max -1)

(defvar lsp-bridge-term-annotation-icons
  '(("Function" . " 󰡱 ")
    ("Keyword" . "  ")
    ("Module" . "  ")
    ("Method" . "  ")
    ("Struct" . "  ")
    ("Snippet" . "  ")
    ("Yas-Snippet" . "  ")
    ("Text" . "  ")
    ("Variable" . " 󰫧 ")
    ("Class" . "  ")
    ("Custom" . "  ")
    ("Feature" . " 󰯺 ")
    ("Macro" . " 󰰏 ")
    ("Interface" . "  ")
    ("Constant" . "  ")
    ("Field" . "  ")
    (nil . " T "))
  "Annotation icons.")

(defgroup lsp-bridge-term nil "lsp-bridge terminal group.")

(defface lsp-bridge-term-select-face
  '((t :background "black" :foreground "white"))
  "Default terminal menu select face."
  :group 'lsp-bridge-term)

(defface lsp-bridge-term-default-face
  '((t :background "grey20" :foreground "white"))
  "Default terminal face."
  :group 'lsp-bridge-term)

(defun lsp-bridge-term-can-display-p ()
  (not (or noninteractive
           emacs-basic-display)))

(defun lsp-bridge-term--menu-item-icon-text (annotation)
  "Returns icon text for given annotation."
  (cdr (assoc annotation lsp-bridge-term-annotation-icons)))

(defun lsp-bridge-term--get-popup-position (frame)
  "Return postion of menu."
  (if (and frame (eobp))
      ;; The existing overlay will cause `popon-x-y-at-pos' and `posn-x-y' to
      ;; get the wrong position when point at the and of buffer.
      (let ((pos (popon-position frame))
            (direction (plist-get (cdr frame) :direction))
            (size (popon-size frame)))
        (cons (car pos)
              (if (eq 'top direction)
                  (+ (cdr pos) (cdr size))
                (1- (cdr pos)))))
    (let ((pos (popon-x-y-at-pos lsp-bridge-term-frame-popup-point)))
      (if (eobp)
          (cons (car pos) (1+ (cdr pos)))
        pos))))

(defun lsp-bridge-term-line-number-display-width ()
  "Return width of line number bar."
  (if (bound-and-true-p display-line-numbers-mode)
      (+ (line-number-display-width) 2)
    0))

(cl-defmacro lsp-bridge-term--create-frame-if-not-exist (frame _frame-buffer _frame-name &optional _internal-border)
  `(unless (popon-live-p ,frame)
     (let ((pos (lsp-bridge-term--get-popup-position nil)))
       (setq ,frame (popon-create (cons "" 0) pos)))))

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

(defun lsp-bridge-term--frame-pos (frame lines)
  "Setup terminal frame position."
  (pcase-let* ((`(,edge-left ,edge-top ,edge-right ,edge-bottom) (window-inside-edges))
               (textarea-width (- (window-width)
                                  (+ (- edge-left (window-left-column))
                                     (lsp-bridge-term-line-number-display-width))))
               (textarea-height (- edge-bottom edge-top))
               (`(,cursor-x . ,cursor-y)
                (prog1 (lsp-bridge-term--get-popup-position frame)
                  (when lines
                    (plist-put (cdr frame) :lines lines)
                    (plist-put (cdr frame) :width (length (car lines))))))
               (`(,menu-w . ,menu-h) (popon-size frame))
               (bottom-free-h (- edge-bottom edge-top cursor-y)))
    (let ((x (if (> textarea-width (+ cursor-x menu-w))
                 cursor-x
               (- cursor-x (- (+ cursor-x menu-w) textarea-width) 1))))
      (plist-put (cdr frame) :x x))
    (cond
     ;; top
     ((<= bottom-free-h menu-h)
      (plist-put (cdr frame) :direction 'top)
      (plist-put (cdr frame) :y (- cursor-y menu-h)))
     ;; bottom
     (t
      (plist-put (cdr frame) :direction 'bottom)
      (plist-put (cdr frame) :y (+ cursor-y 1))))))

(defun lsp-bridge-term--menu-items-render (candidates index)
  "Render menu items."
  (let ((item-index 0)
        (max-length (+ 1 (lsp-bridge-term--candidates-max-length candidates))))
    (dolist (v candidates)
      (let* ((display (plist-get v :displayLabel))
             (icon (plist-get v :icon))
             (annotation (plist-get v :annotation))
             (padding (- max-length (length display)))
             candidate)
        (setq candidate
              (concat
               (propertize (format "%s" (lsp-bridge-term--menu-item-icon-text annotation)
                                   'face
                                   (if (equal item-index index)
                                       'lsp-bridge-term-select-face 'font-lock-doc-face)))
               (if (eq 0 padding)
                   display
                 (concat display (make-string padding ?\s)))

               "\n"))

        (add-face-text-property 0 (length candidate)
                                (if (equal item-index index)
                                    'lsp-bridge-term-select-face
                                  'lsp-bridge-term-default-face)
                                'append candidate)

        (insert candidate)

        (when (equal item-index (1- (length candidates)))
          (delete-char -1))

        (setq item-index (1+ item-index))))))

(defun lsp-bridge-term--menu-render (candidates index)
  "Render menu."
  (let ((len (length candidates)))
    (setq-local lsp-bridge-term-menu-max len)
    (when (and lsp-bridge-term-frame (< 0 len))
      (with-current-buffer (get-buffer-create lsp-bridge-term-buffer)
        (erase-buffer)
        (let ((lines (split-string
                      (with-temp-buffer
                        (lsp-bridge-term--menu-items-render candidates index)
                        (buffer-string))
                      "\n")))
          (lsp-bridge-term--frame-pos lsp-bridge-term-frame lines)))
      (popon-redisplay)
      (lsp-bridge-term-mode 1)
      (plist-put (cdr lsp-bridge-term-frame) :visible t))))

(defun lsp-bridge-term--cancel-if-present ()
  "Cancel when present."
  (when (popon-live-p lsp-bridge-term-frame)
    (lsp-bridge-term-cancel)))

(defun lsp-bridge-term--trigger-completion ()
  "Returns true when current point should trigger completion."
  (cond ((bounds-of-thing-at-point 'symbol) t)
        ((bounds-of-thing-at-point 'whitespace) nil)
        (t t)))

(defun lsp-bridge-term--update (candidates index)
  "Update terminal menu."
  (if (< 0 (length candidates))
      (setq lsp-bridge-term-candidates candidates)
    (setq candidates lsp-bridge-term-candidates))

  (let* ((bounds (acm-get-input-prefix-bound)))
    (setq lsp-bridge-term-frame-popup-point (or (car bounds) (point)))

    (lsp-bridge-term--create-frame-if-not-exist lsp-bridge-term-frame lsp-bridge-term-buffer "lsp-bridge-term")
    (unless (plist-get lsp-bridge-term-frame :direction)
      (plist-put (cdr lsp-bridge-term-frame) :direction 'top))
    (lsp-bridge-term--menu-render candidates index)
    ))

(defun lsp-bridge-term-cancel ()
  "Cancel lsp completion, code action, doc and any exiting ui."
  (interactive)
  (lsp-bridge-term-mode 0)
  (popon-kill-all)
  (setq lsp-bridge-term-frame nil)
  (setq-local lsp-bridge-term-menu-max -1)
  (setq lsp-bridge-term-candidates nil)
  )

(defun lsp-bridge-term-select-next()
  "Select next item in menu."
  (interactive)
  (unless (= lsp-bridge-term-menu-index (1- lsp-bridge-term-menu-max))
    (setq lsp-bridge-term-menu-index (1+ lsp-bridge-term-menu-index))
    (lsp-bridge-term--update nil lsp-bridge-term-menu-index))
  )

(defun lsp-bridge-term-select-prev ()
  "Select previous item in menu."
  (interactive)
  (unless (= lsp-bridge-term-menu-index 0)
    (setq lsp-bridge-term-menu-index (1- lsp-bridge-term-menu-index))
    (lsp-bridge-term--update nil lsp-bridge-term-menu-index))
  )

(defun lsp-bridge-term-complete ()
  "Select candidate in menu."
  (interactive)
  (let* ((candidate (nth lsp-bridge-term-menu-index lsp-bridge-term-candidates))
         (bound-start lsp-bridge-term-frame-popup-point)
         (backend (plist-get candidate :backend))
         (candidate-expand (intern-soft (format "acm-backend-%s-candidate-expand" backend))))

    (if (fboundp candidate-expand)
        (funcall candidate-expand candidate bound-start)
      (delete-region bound-start (point))
      (insert (plist-get candidate :label))))
  (lsp-bridge-term-cancel)
  )

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

(defun lsp-bridge-term-overriding-key-setup ()
  "Some key define in language mode map will conflict with lsp-bridge-term-mode map.
So we use `minor-mode-overriding-map-alist' to override key, make sure all keys in lsp-bridge-term-mode can response."
  (let ((override-map (make-sparse-keymap)))
    (define-key override-map [?\C-m] 'lsp-bridge-term-complete)))

(define-minor-mode lsp-bridge-term-mode
  "LSP Bridge Terminal minor mode."
  :keymap lsp-bridge-term-mode-map
  :init-value nil
  ;; Set override map, avoid some language mode map conflict with lsp-bridge-term-mode map.
  (lsp-bridge-term-overriding-key-setup))

(defun lsp-bridge-term-completion-recv-items (filename filehost candidates position server-name
                                                       completion-trigger-characters server-names)
  "Receive lsp-bridge completion."
  (if (or (= 0 (length candidates))
          (not (lsp-bridge-term--trigger-completion)))
      (lsp-bridge-term--cancel-if-present)
    (progn
      (lsp-bridge--with-file-buffer
          filename filehost
          ;; `acm-backend-lsp-candidate-expand` needs `acm-backend-lsp-completion-position` to be set
          ;; in `lsp-bridge-term-complete` function when select candidate.
          (setq-local acm-backend-lsp-completion-position position)
          (let ((completion-table (make-hash-table :test 'equal)))
            (dolist (item candidates)
              (plist-put item :annotation (capitalize (plist-get item :icon)))
              (puthash (plist-get item :key) item completion-table))))
      (lsp-bridge-term--update candidates 0))))

(defun lsp-bridge-term--code-action-popup-menu (actions action)
  "Popup code action menu."
  (let ((candidates '()))
    (lsp-bridge-term--create-frame-if-not-exist lsp-bridge-term-frame lsp-bridge-term-buffer "lsp-bridge-term")
    (unless (plist-get lsp-bridge-term-frame :direction)
      (plist-put (cdr lsp-bridge-term-frame) :direction 'top))

    (dolist (v actions)
      (let* ((title (plist-get v :title))
             (candicate (list :key title :label title :icon "function" :annotation "Function" :displayLabel title)))
        (add-to-list 'candidates candicate)))
    (lsp-bridge-term--update candidates -1)
  ))

(defun lsp-bridge-term-code-action-recv-actions (actions action-kind)
  "Receive lsp-bridge code actions."
  (lsp-bridge-term--code-action-popup-menu actions action-kind)
  )

(defun lsp-bridge-term-diagnostic-recv-items (filepath filehost diagnostics diagnostic-count)
  "Receive lsp-bridge diagnostic.")

(defun lsp-bridge-term-signature-help-recv (helps index)
  "Receive lsp-bridge signature helps."
  (unless (popon-live-p lsp-bridge-term-frame)
    (let ((candidates '()))
      (unless (plist-get lsp-bridge-term-frame :direction)
        (plist-put (cdr lsp-bridge-term-frame) :direction 'bottom))

      (dolist (v helps)
        (add-to-list 'candidates (list :displayLabel v)))

      (lsp-bridge-term--update candidates -1))))

(defun lsp-bridge-term-search-recv-items (backend items)
  "Receive lsp-bridge search backend.")

(defvar lsp-bridge-term-advices
  '((lsp-bridge-code-action--fix :override lsp-bridge-term-code-action-recv-actions)
    (lsp-bridge-completion--record-items :override lsp-bridge-term-completion-recv-items)
    (lsp-bridge-diagnostic--render :override lsp-bridge-term-diagnostic-recv-items)
    (lsp-bridge-signature-help--update :override lsp-bridge-term-signature-help-recv)
    (lsp-bridge-search-backend--record-items :override lsp-bridge-term-search-recv-items))
    "advices to adapt lsp-bridge.")

(defun lsp-bridge-term-active ()
  (mapc (pcase-lambda (`(,orig-fn ,how ,function))
          (advice-add orig-fn how function))
        lsp-bridge-term-advices))

(defun lsp-bridge-term-deactive ()
  (mapc (pcase-lambda (`( ,orig-fn ,_ ,function ))
          (advice-remove orig-fn function))
        lsp-bridge-term-advices))

(unless window-system
  (lsp-bridge-term-active))

(provide 'lsp-bridge-term)

;;; lsp-bridge-term.el ends here