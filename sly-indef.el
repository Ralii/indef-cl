;;; sly-indef.el --- SLY integration for CL indef debugging -*- lexical-binding: t -*-

;; Author: Lari Saukkonen
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (sly "1.0"))
;; Keywords: lisp, debugging, tools
;; URL: https://github.com/ralii/indef

;;; Commentary:

;; This package integrates the Common Lisp indef library with SLY,
;; providing commands to instrument functions for REPL-driven debugging.
;;
;; Main features:
;; - Evaluate a defun as "indef'd" without modifying the source
;; - Toggle indef mode on existing functions
;; - Inspect captured bindings in a dedicated buffer
;; - Quick access to captured values at point
;;
;; Usage:
;;   M-x sly-indef-eval-defun    - Evaluate current defun as indef'd
;;   M-x sly-indef-function      - Indef an existing function by name
;;   M-x sly-indef-unindef       - Remove indef from a function
;;   M-x sly-indef-show          - Show captured bindings
;;   M-x sly-indef-show-at-point - Show bindings for function at point
;;   M-x sly-indef-clear         - Clear all captured state

;;; Code:

(require 'sly)
(require 'cl-lib)

;;; Package directory (for auto-loading indef.lisp)

(defconst sly-indef--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory where sly-indef.el is loaded from.")

(defvar sly-indef--loaded-p nil
  "Whether indef.lisp has been loaded into the current Lisp connection.")

(defun sly-indef--find-lisp-file ()
  "Find indef.lisp, checking multiple possible locations."
  (let ((candidates (list
                     ;; Same directory as sly-indef.el
                     (expand-file-name "indef.lisp" sly-indef--directory)
                     ;; Straight.el repos directory (Doom Emacs)
                     (expand-file-name "~/.config/emacs/.local/straight/repos/indef-cl/indef.lisp")
                     (expand-file-name "~/.emacs.d/.local/straight/repos/indef-cl/indef.lisp")
                     ;; Quelpa/package.el locations
                     (expand-file-name "../indef-cl/indef.lisp" sly-indef--directory))))
    (cl-find-if #'file-exists-p candidates)))

;;; Customization

(defgroup sly-indef nil
  "SLY integration for Common Lisp indef debugging."
  :group 'sly
  :prefix "sly-indef-")

(defcustom sly-indef-auto-show t
  "If non-nil, automatically show bindings after evaluating an indef'd function."
  :type 'boolean
  :group 'sly-indef)

(defcustom sly-indef-show-in-popup t
  "If non-nil, show bindings in a popup. Otherwise use echo area."
  :type 'boolean
  :group 'sly-indef)

(defface sly-indef-binding-name
  '((t :inherit font-lock-variable-name-face :weight bold))
  "Face for binding names in indef output."
  :group 'sly-indef)

(defface sly-indef-binding-value
  '((t :inherit font-lock-string-face))
  "Face for binding values in indef output."
  :group 'sly-indef)

(defface sly-indef-header
  '((t :inherit font-lock-keyword-face :weight bold :height 1.1))
  "Face for headers in indef output."
  :group 'sly-indef)

;;; Core Functions

(defun sly-indef--ensure-loaded ()
  "Ensure the indef package is loaded in the Lisp image.
Auto-loads indef.lisp from the package directory if needed."
  (unless (sly-eval '(cl:not (cl:null (cl:find-package :indef))))
    (let ((lisp-file (sly-indef--find-lisp-file)))
      (if lisp-file
          (progn
            (sly-eval `(cl:load ,lisp-file))
            (setq sly-indef--loaded-p t)
            (message "Loaded indef from %s" lisp-file))
        (error "Cannot find indef.lisp. Searched in: %s, straight repos, etc."
               sly-indef--directory)))))

(defun sly-indef--transform-defun (form-string)
  "Transform a defun form string into an indef'd version."
  (sly-eval `(cl:let ((form (cl:read-from-string ,form-string)))
               (cl:if (cl:and (cl:consp form)
                              (cl:eq (cl:car form) 'cl:defun))
                   (indef:indef-defun-form form)
                   (cl:error "Not a defun form")))))

;;;###autoload
(defun sly-indef-eval-defun ()
  "Evaluate the current defun as an indef'd function.
This instruments the function to capture all bindings without
modifying the source file."
  (interactive)
  (sly-indef--ensure-loaded)
  (let* ((form-string (save-excursion
                        (end-of-defun)
                        (beginning-of-defun)
                        (let ((start (point)))
                          (forward-sexp)
                          (buffer-substring-no-properties start (point)))))
         (name (save-excursion
                 (beginning-of-defun)
                 (down-list)
                 (forward-sexp 2)
                 (sly-symbol-at-point))))
    (if (null form-string)
        (message "No defun at point")
      ;; Transform and evaluate
      (sly-eval-async
          `(cl:let* ((form (cl:read-from-string ,form-string))
                     (instrumented (indef:indef-defun-form form)))
             (cl:eval instrumented)
             (cl:format nil "Indef'd: ~A" (cl:cadr form)))
        (lambda (result)
          (message "%s" result))))))

;;;###autoload
(defun sly-indef-function (function-name)
  "Instrument an existing function for indef debugging.
FUNCTION-NAME is the name of the function to instrument."
  (interactive
   (list (sly-read-symbol-name "Indef function: "
                                (sly-symbol-at-point))))
  (sly-indef--ensure-loaded)
  (when function-name
    (sly-eval-async
        `(indef:indef (cl:read-from-string ,function-name))
      (lambda (result)
        (message "Indef'd: %s" result)))))

;;;###autoload
(defun sly-indef-unindef (function-name)
  "Remove indef instrumentation from a function.
FUNCTION-NAME is the name of the function to restore."
  (interactive
   (list (let ((indef'd (sly-eval '(indef:list-indef))))
           (if indef'd
               (completing-read "Unindef function: "
                                (mapcar #'symbol-name indef'd))
             (sly-read-symbol-name "Unindef function: ")))))
  (sly-indef--ensure-loaded)
  (when function-name
    (sly-eval-async
        `(indef:unindef (cl:read-from-string ,function-name))
      (lambda (result)
        (message "Unindef'd: %s" result)))))

;;;###autoload
(defun sly-indef-show (&optional function-name)
  "Show all captured bindings in a dedicated buffer.
With prefix arg or FUNCTION-NAME, show only bindings for that function."
  (interactive (list (when current-prefix-arg
                       (sly-read-symbol-name "Show bindings for: "))))
  (sly-indef--ensure-loaded)
  (let ((query (if function-name
                   `(indef:format-bindings-for-emacs
                     (cl:read-from-string ,function-name))
                 '(indef:format-bindings-for-emacs))))
    (sly-eval-async query
      (lambda (result)
        (sly-indef--display-bindings result function-name)))))

;;;###autoload
(defun sly-indef-show-at-point ()
  "Show bindings for the function at point."
  (interactive)
  (let ((sym (sly-symbol-at-point)))
    (if sym
        (sly-indef-show sym)
      (message "No symbol at point"))))

(defun sly-indef--display-bindings (text &optional function-name)
  "Display binding TEXT in a buffer or popup."
  (if sly-indef-show-in-popup
      (let ((buf (get-buffer-create "*sly-indef*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert text)
            (goto-char (point-min))
            (sly-indef-mode)))
        (display-buffer buf
                        '((display-buffer-reuse-window
                           display-buffer-pop-up-window)
                          (window-height . 0.3))))
    (message "%s" text)))

;;;###autoload
(defun sly-indef-get-value (var-name)
  "Get the value of a captured variable and insert it.
VAR-NAME is the name of the variable."
  (interactive
   (list (sly-read-from-minibuffer "Get captured var: "
                                    (sly-symbol-at-point))))
  (sly-indef--ensure-loaded)
  (sly-eval-async
      `(indef:@ (cl:read-from-string ,(concat "'" var-name)))
    (lambda (result)
      (message "%s = %s" var-name (sly-prin1-to-string result)))))

;;;###autoload
(defun sly-indef-get-return (function-name)
  "Get the last return value of an indef'd function."
  (interactive
   (list (sly-read-symbol-name "Get return value for: "
                                (sly-symbol-at-point))))
  (sly-indef--ensure-loaded)
  (when function-name
    (sly-eval-async
        `(indef:@@ (cl:read-from-string ,function-name))
      (lambda (result)
        (message "%s< = %s" function-name (sly-prin1-to-string result))))))

;;;###autoload
(defun sly-indef-get-call (function-name)
  "Get the reconstructed call form for an indef'd function."
  (interactive
   (list (sly-read-symbol-name "Get call form for: "
                                (sly-symbol-at-point))))
  (sly-indef--ensure-loaded)
  (when function-name
    (sly-eval-async
        `(indef:@@> (cl:read-from-string ,function-name))
      (lambda (result)
        (let ((call-str (sly-prin1-to-string result)))
          (message "%s> = %s" function-name call-str)
          (kill-new call-str)
          (message "%s> = %s (copied to kill ring)" function-name call-str))))))

;;;###autoload
(defun sly-indef-unindef-all ()
  "Remove instrumentation from all indef'd functions."
  (interactive)
  (sly-indef--ensure-loaded)
  (sly-eval-async '(indef:unindef-all)
    (lambda (_result)
      (message "All functions unindef'd"))))

;;;###autoload
(defun sly-indef-clear ()
  "Clear all captured indef state."
  (interactive)
  (sly-indef--ensure-loaded)
  (sly-eval-async '(indef:clear)
    (lambda (_result)
      (message "Indef state cleared"))))

;;;###autoload
(defun sly-indef-list ()
  "List all currently indef'd functions."
  (interactive)
  (sly-indef--ensure-loaded)
  (sly-eval-async '(indef:list-indef)
    (lambda (result)
      (if result
          (message "Indef'd functions: %s"
                   (mapconcat #'symbol-name result ", "))
        (message "No functions currently indef'd")))))

;;; Minor Mode for Indef Buffer

(defvar sly-indef-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    (define-key map "g" #'sly-indef-show)
    (define-key map "c" #'sly-indef-clear)
    map)
  "Keymap for `sly-indef-mode'.")

(define-derived-mode sly-indef-mode special-mode "Indef"
  "Major mode for viewing indef captured bindings.

\\{sly-indef-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t))

;;; Keybindings

(defvar sly-indef-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'sly-indef-eval-defun)
    (define-key map "S" #'sly-indef-function)
    (define-key map "u" #'sly-indef-unindef)
    (define-key map "U" #'sly-indef-unindef-all)
    (define-key map "v" #'sly-indef-show)
    (define-key map "V" #'sly-indef-show-at-point)
    (define-key map "c" #'sly-indef-clear)
    (define-key map "r" #'sly-indef-get-return)
    (define-key map ">" #'sly-indef-get-call)
    (define-key map "l" #'sly-indef-list)
    (define-key map "?" #'sly-indef-get-value)
    map)
  "Keymap for indef commands, typically bound to a prefix.")

;;;###autoload
(defun sly-indef-setup-keybindings ()
  "Set up indef keybindings under C-c s prefix."
  (define-key sly-mode-map (kbd "C-c s") sly-indef-prefix-map)
  (define-key sly-mode-map (kbd "C-c C-s") #'sly-indef-eval-defun))

;;; Auto-setup

(defun sly-indef--on-connect ()
  "Reset loaded state when connecting to a new Lisp."
  (setq sly-indef--loaded-p nil))

(add-hook 'sly-connected-hook #'sly-indef--on-connect)

;;;###autoload
(defun sly-indef-init ()
  "Initialize indef in the connected Lisp.
Loads indef.lisp from the package directory."
  (interactive)
  (sly-indef--ensure-loaded)
  (message "Indef ready"))

(provide 'sly-indef)

;;; sly-indef.el ends here
