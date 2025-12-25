;;;; indef.lisp - Inline def debugging for Common Lisp
;;;; Inspired by Clojure's indef library (github.com/ralii/indef)
;;;;
;;;; Usage from REPL:
;;;;   (indef:indef 'my-function)     ; Instrument a function
;;;;   (indef:unindef 'my-function)   ; Remove instrumentation
;;;;   (indef:show)                    ; Show all captured bindings
;;;;   (indef:clear)                   ; Clear captured state
;;;;
;;;; After calling an instrumented function, inspect:
;;;;   indef:*bindings*                ; Hash table of all captured bindings
;;;;   (indef:@ 'x)                    ; Get value of captured binding 'x
;;;;   (indef:@@ 'my-function)         ; Get last return value
;;;;   (indef:@@> 'my-function)        ; Get reconstructed call form

(defpackage :indef
  (:use :cl)
  (:export
   ;; Core API
   #:defun*
   #:let*
   #:slet
   #:slet*
   #:slambda

   ;; Instrumentation API (for SLY integration)
   #:indef
   #:unindef
   #:indef-p
   #:list-indef
   #:indef-defun-form
   #:format-bindings-for-emacs

   ;; Inspection API
   #:@                    ; Get binding value
   #:@@                   ; Get return value
   #:@@>                  ; Get reconstructed call
   #:show                 ; Show all bindings
   #:show-function        ; Show bindings for a specific function
   #:clear                ; Clear all state

   ;; Configuration
   #:*set-symbols*        ; Whether to set symbol values directly

   ;; Storage (for direct REPL access)
   #:*bindings*
   #:*returns*
   #:*calls*
   #:*current-function*))

(in-package :indef)

;;; ============================================================================
;;; Configuration
;;; ============================================================================

(defvar *set-symbols* t
  "When non-nil, captured bindings are also set as symbol values.
   This allows direct access like X instead of (indef:@ 'x).
   Symbols are cleaned up when (indef:clear) is called.")

;;; ============================================================================
;;; Storage
;;; ============================================================================

(defvar *bindings* (make-hash-table :test 'equal)
  "Stores captured variable bindings. Keys are (function-name . var-name) cons cells,
   or just var-name symbols for anonymous contexts.")

(defvar *returns* (make-hash-table :test 'eq)
  "Stores last return values of indef'd functions. Keys are function names.")

(defvar *calls* (make-hash-table :test 'eq)
  "Stores reconstructed function calls. Keys are function names.")

(defvar *original-functions* (make-hash-table :test 'eq)
  "Stores original function definitions before indef.")

(defvar *current-function* nil
  "Dynamically bound to the current function being executed.")

(defvar *indef-sources* (make-hash-table :test 'eq)
  "Stores the source forms of indef'd functions for re-instrumentation.")

(defvar *set-symbols-list* (make-hash-table :test 'eq)
  "Tracks symbols that were set by indef (and weren't previously bound).
   Used by clear to makunbound only the symbols we created.")

;;; ============================================================================
;;; Binding Capture
;;; ============================================================================

(defun capture (name value &optional (context *current-function*))
  "Capture a binding. Returns the value for use in macros."
  (let ((key (if context (cons context name) name)))
    (setf (gethash key *bindings*) value)
    ;; Also store under just the name for convenience
    (setf (gethash name *bindings*) value))
  ;; Set symbol value directly for REPL access (like Clojure's inline def)
  (when (and *set-symbols* (symbolp name))
    ;; Track if symbol wasn't already bound (so we can clean it up later)
    (unless (or (gethash name *set-symbols-list*)
                (boundp name))
      (setf (gethash name *set-symbols-list*) t))
    (setf (symbol-value name) value))
  value)

(defun capture-return (function-name value)
  "Capture the return value of a function."
  (setf (gethash function-name *returns*) value)
  value)

(defun capture-call (function-name args)
  "Capture the call form for reconstruction."
  (setf (gethash function-name *calls*) (cons function-name args)))

;;; ============================================================================
;;; Inspection API
;;; ============================================================================

(defun @ (name &optional context)
  "Get a captured binding value."
  (let ((key (if context (cons context name) name)))
    (multiple-value-bind (value found)
        (gethash key *bindings*)
      (if found
          value
          (gethash name *bindings*)))))

(defun @@ (function-name)
  "Get the last return value of an indef'd function."
  (gethash function-name *returns*))

(defun @@> (function-name)
  "Get the reconstructed call form of an indef'd function."
  (gethash function-name *calls*))

(defun show (&optional filter)
  "Display all captured bindings. Optional FILTER is a function name to filter by."
  (format t "~&=== Indef Captured Bindings ===~%")
  (let ((items '()))
    (maphash (lambda (k v)
               (when (or (null filter)
                         (and (consp k) (eq (car k) filter))
                         (eq k filter))
                 (push (cons k v) items)))
             *bindings*)
    (if items
        (dolist (item (sort items #'string<
                            :key (lambda (x)
                                   (format nil "~A" (car x)))))
          (format t "  ~A = ~S~%" (car item) (cdr item)))
        (format t "  (no bindings captured)~%")))

  (format t "~&=== Return Values ===~%")
  (let ((count 0))
    (maphash (lambda (k v)
               (when (or (null filter) (eq k filter))
                 (incf count)
                 (format t "  ~A< = ~S~%" k v)))
             *returns*)
    (when (zerop count)
      (format t "  (no return values captured)~%")))

  (format t "~&=== Reconstructed Calls ===~%")
  (let ((count 0))
    (maphash (lambda (k v)
               (when (or (null filter) (eq k filter))
                 (incf count)
                 (format t "  ~A> = ~S~%" k v)))
             *calls*)
    (when (zerop count)
      (format t "  (no calls captured)~%")))
  (values))

(defun show-function (function-name)
  "Show bindings for a specific function."
  (show function-name))

(defun clear ()
  "Clear all captured state, including symbol bindings created by indef."
  ;; Unbind symbols that we set (only those that weren't already bound)
  (let ((count 0))
    (maphash (lambda (sym was-set-by-us)
               (declare (ignore was-set-by-us))
               (when (boundp sym)
                 (makunbound sym)
                 (incf count)))
             *set-symbols-list*)
    (when (> count 0)
      (format t "~&Unbound ~D symbol~:P.~%" count)))
  (clrhash *set-symbols-list*)
  (clrhash *bindings*)
  (clrhash *returns*)
  (clrhash *calls*)
  (format t "~&Indef state cleared.~%")
  (values))

;;; ============================================================================
;;; Lambda List Parsing
;;; ============================================================================

(defun parse-lambda-list (lambda-list)
  "Parse a lambda list and return all bound parameter names."
  (let ((params '())
        (state :required))
    (dolist (item lambda-list)
      (case item
        (&optional (setf state :optional))
        (&rest (setf state :rest))
        (&key (setf state :key))
        (&allow-other-keys nil)
        (&aux (setf state :aux))
        (otherwise
         (push (extract-param-name item state) params))))
    (remove nil (nreverse params))))

(defun extract-param-name (item state)
  "Extract the parameter name from a lambda list item."
  (case state
    (:required item)
    (:rest item)
    ((:optional :aux)
     (if (listp item) (car item) item))
    (:key
     (if (listp item)
         (let ((name-part (car item)))
           (if (listp name-part)
               (cadr name-part)  ; ((keyword var) default)
               name-part))       ; (var default)
         item))))

;;; ============================================================================
;;; Code Walking / Transformation
;;; ============================================================================

(defun transform-body (body)
  "Walk the body and transform let/let* forms to capture bindings."
  (mapcar #'transform-form body))

(defun transform-form (form)
  "Transform a single form to capture bindings."
  (cond
    ((atom form) form)

    ;; Transform let forms
    ((and (consp form) (eq (car form) 'let))
     (transform-let form))

    ;; Transform let* forms
    ((and (consp form) (eq (car form) 'let*))
     (transform-let* form))

    ;; Transform lambda forms
    ((and (consp form) (eq (car form) 'lambda))
     (transform-lambda form))

    ;; Transform flet/labels
    ((and (consp form) (member (car form) '(flet labels)))
     (transform-flet form))

    ;; Transform destructuring-bind
    ((and (consp form) (eq (car form) 'destructuring-bind))
     (transform-destructuring-bind form))

    ;; Transform multiple-value-bind
    ((and (consp form) (eq (car form) 'multiple-value-bind))
     (transform-multiple-value-bind form))

    ;; Recurse into other forms
    ((consp form)
     (cons (car form) (transform-body (cdr form))))

    (t form)))

(defun transform-let (form)
  "Transform a let form to capture all bindings."
  (destructuring-bind (let bindings &body body) form
    (declare (ignore let))
    (let ((new-bindings
            (mapcar (lambda (binding)
                      (if (listp binding)
                          (destructuring-bind (var val) binding
                            `(,var (capture ',var ,val)))
                          `(,binding (capture ',binding nil))))
                    bindings)))
      `(let ,new-bindings
         ,@(transform-body body)))))

(defun transform-let* (form)
  "Transform a let* form to capture all bindings."
  (destructuring-bind (let* bindings &body body) form
    (declare (ignore let*))
    (let ((new-bindings
            (mapcar (lambda (binding)
                      (if (listp binding)
                          (destructuring-bind (var val) binding
                            `(,var (capture ',var ,val)))
                          `(,binding (capture ',binding nil))))
                    bindings)))
      `(let* ,new-bindings
         ,@(transform-body body)))))

(defun transform-lambda (form)
  "Transform a lambda form to capture parameters."
  (destructuring-bind (lambda lambda-list &body body) form
    (declare (ignore lambda))
    (let ((params (parse-lambda-list lambda-list)))
      `(lambda ,lambda-list
         ,@(loop for p in params collect `(capture ',p ,p))
         ,@(transform-body body)))))

(defun transform-flet (form)
  "Transform flet/labels to capture local function parameters."
  (destructuring-bind (flet-or-labels definitions &body body) form
    (let ((new-definitions
            (mapcar (lambda (def)
                      (destructuring-bind (name lambda-list &body fn-body) def
                        (let ((params (parse-lambda-list lambda-list)))
                          `(,name ,lambda-list
                                  ,@(loop for p in params
                                          collect `(capture ',(intern
                                                               (format nil "~A/~A" name p))
                                                            ,p))
                                  ,@(transform-body fn-body)))))
                    definitions)))
      `(,flet-or-labels ,new-definitions
         ,@(transform-body body)))))

(defun transform-destructuring-bind (form)
  "Transform destructuring-bind to capture all bound variables."
  (destructuring-bind (db pattern expr &body body) form
    (declare (ignore db))
    (let ((vars (extract-destructuring-vars pattern)))
      `(destructuring-bind ,pattern ,expr
         ,@(loop for v in vars collect `(capture ',v ,v))
         ,@(transform-body body)))))

(defun transform-multiple-value-bind (form)
  "Transform multiple-value-bind to capture all bound variables."
  (destructuring-bind (mvb vars expr &body body) form
    (declare (ignore mvb))
    `(multiple-value-bind ,vars ,expr
       ,@(loop for v in vars collect `(capture ',v ,v))
       ,@(transform-body body))))

(defun extract-destructuring-vars (pattern)
  "Extract all variable names from a destructuring pattern."
  (cond
    ((null pattern) nil)
    ((symbolp pattern)
     (unless (member pattern '(&optional &rest &key &allow-other-keys &aux &whole &body))
       (list pattern)))
    ((consp pattern)
     (append (extract-destructuring-vars (car pattern))
             (extract-destructuring-vars (cdr pattern))))
    (t nil)))

;;; ============================================================================
;;; defun* Macro
;;; ============================================================================

(defmacro defun* (name lambda-list &body body)
  "Like DEFUN, but captures all bindings for REPL inspection.
   After calling, inspect:
     (indef:@ 'var-name)      - get a captured binding
     (indef:@@ 'function-name) - get last return value
     (indef:@@> 'function-name) - get reconstructed call"
  (let ((params (parse-lambda-list lambda-list))
        (result (gensym "RESULT"))
        (args (gensym "ARGS")))
    `(defun ,name (&rest ,args)
       (declare (ignorable ,args))
       (destructuring-bind ,lambda-list ,args
         (let ((*current-function* ',name))
           ;; Capture all parameters
           ,@(loop for p in params collect `(capture ',p ,p))

           ;; Capture call reconstruction
           (capture-call ',name (list ,@params))

           ;; Execute transformed body and capture return
           (let ((,result (progn ,@(transform-body body))))
             (capture-return ',name ,result)
             ,result))))))

;;; ============================================================================
;;; slet / slet* Macros
;;; ============================================================================

(defmacro slet (bindings &body body)
  "Like LET, but captures all bindings."
  (let ((new-bindings
          (mapcar (lambda (binding)
                    (if (listp binding)
                        (destructuring-bind (var val) binding
                          `(,var (capture ',var ,val)))
                        `(,binding (capture ',binding nil))))
                  bindings)))
    `(let ,new-bindings
       ,@(transform-body body))))

(defmacro slet* (bindings &body body)
  "Like LET*, but captures all bindings."
  (let ((new-bindings
          (mapcar (lambda (binding)
                    (if (listp binding)
                        (destructuring-bind (var val) binding
                          `(,var (capture ',var ,val)))
                        `(,binding (capture ',binding nil))))
                  bindings)))
    `(let* ,new-bindings
       ,@(transform-body body))))

(defmacro slambda (lambda-list &body body)
  "Like LAMBDA, but captures all bindings."
  (let ((params (parse-lambda-list lambda-list)))
    `(lambda ,lambda-list
       ,@(loop for p in params collect `(capture ',p ,p))
       ,@(transform-body body))))

;;; ============================================================================
;;; Dynamic Instrumentation (for SLY integration)
;;; ============================================================================

(defun get-function-source (function-name)
  "Try to get the source form of a function. Returns NIL if not found."
  ;; First check if we have a cached source
  (or (gethash function-name *indef-sources*)
      ;; function-lambda-expression is standard CL
      (let ((fn (fdefinition function-name)))
        (function-lambda-expression fn))))

(defun indef (function-name)
  "Instrument an existing function for debugging.
   The function will capture all bindings when called."
  (unless (fboundp function-name)
    (error "Function ~A is not defined." function-name))

  ;; Store original if not already indef'd
  (unless (gethash function-name *original-functions*)
    (setf (gethash function-name *original-functions*)
          (fdefinition function-name)))

  (let ((source (get-function-source function-name)))
    (if source
        ;; We have source - create instrumented version
        (let ((instrumented (instrument-function-source function-name source)))
          (setf (gethash function-name *indef-sources*) source)
          (eval instrumented)
          (format t "~&Indef'd ~A (with source)~%" function-name))
        ;; No source - wrap with advice-style instrumentation
        (let ((original (gethash function-name *original-functions*)))
          (setf (fdefinition function-name)
                (lambda (&rest args)
                  (let ((*current-function* function-name))
                    ;; Capture args (we don't know names, use arg0, arg1, etc)
                    (loop for arg in args
                          for i from 0
                          do (capture (intern (format nil "ARG~D" i)) arg))
                    (capture-call function-name args)
                    (let ((result (apply original args)))
                      (capture-return function-name result)
                      result))))
          (format t "~&Indef'd ~A (wrapper only - no source available)~%" function-name))))
  function-name)

(defun instrument-function-source (name source)
  "Create an instrumented version of a function from its source."
  (destructuring-bind (lambda-or-named lambda-list &body body) source
    (declare (ignore lambda-or-named))
    (let ((params (parse-lambda-list lambda-list))
          (result (gensym "RESULT"))
          (args (gensym "ARGS")))
      `(defun ,name (&rest ,args)
         (declare (ignorable ,args))
         (destructuring-bind ,lambda-list ,args
           (let ((*current-function* ',name))
             ,@(loop for p in params collect `(capture ',p ,p))
             (capture-call ',name (list ,@params))
             (let ((,result (progn ,@(transform-body body))))
               (capture-return ',name ,result)
               ,result)))))))

(defun unindef (function-name)
  "Remove instrumentation from a function, restoring the original."
  (let ((original (gethash function-name *original-functions*)))
    (if original
        (progn
          (setf (fdefinition function-name) original)
          (remhash function-name *original-functions*)
          (remhash function-name *indef-sources*)
          (format t "~&Unindef'd ~A~%" function-name))
        (format t "~&~A was not indef'd~%" function-name)))
  function-name)

(defun indef-p (function-name)
  "Check if a function is currently indef'd."
  (not (null (gethash function-name *original-functions*))))

(defun list-indef ()
  "List all currently indef'd functions."
  (let ((result '()))
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k result))
             *original-functions*)
    result))

;;; ============================================================================
;;; SLY/SLIME Integration Helpers
;;; ============================================================================

(defun indef-defun-form (form)
  "Transform a defun form into an indef'd form for evaluation.
   This is called from Emacs to instrument a function on-the-fly."
  (when (and (consp form) (eq (car form) 'defun))
    (let* ((name (cadr form))
           (lambda-list (caddr form))
           (body (cdddr form))
           (params (parse-lambda-list lambda-list))
           (result (gensym "RESULT"))
           (args (gensym "ARGS")))
      ;; Store source for potential re-instrumentation
      (setf (gethash name *indef-sources*)
            `(lambda ,lambda-list ,@body))
      ;; Mark as indef'd (using identity function as placeholder)
      (setf (gethash name *original-functions*) t)
      ;; Return instrumented form
      `(defun ,name (&rest ,args)
         (declare (ignorable ,args))
         (destructuring-bind ,lambda-list ,args
           (let ((*current-function* ',name))
             ,@(loop for p in params collect `(capture ',p ,p))
             (capture-call ',name (list ,@params))
             (let ((,result (progn ,@(transform-body body))))
               (capture-return ',name ,result)
               ,result)))))))

(defun format-bindings-for-emacs (&optional function-name)
  "Format captured bindings in a way suitable for Emacs display."
  (with-output-to-string (s)
    (format s "Indef Bindings")
    (when function-name
      (format s " for ~A" function-name))
    (format s "~%")
    (format s "~V,,,'-A~%" 40 "")

    ;; Bindings
    (let ((items '()))
      (maphash (lambda (k v)
                 (when (or (null function-name)
                           (and (consp k) (eq (car k) function-name))
                           (eq k function-name))
                   (push (cons k v) items)))
               *bindings*)
      (if items
          (dolist (item (sort items #'string<
                              :key (lambda (x) (format nil "~A" (car x)))))
            (format s "~A = ~S~%" (car item) (cdr item)))
          (format s "(no bindings)~%")))

    ;; Return value
    (when function-name
      (multiple-value-bind (val found) (gethash function-name *returns*)
        (when found
          (format s "~%Return: ~S~%" val)))
      (multiple-value-bind (val found) (gethash function-name *calls*)
        (when found
          (format s "Call: ~S~%" val))))))

;;; ============================================================================
;;; Convenience: Create symbol-macros for top-level bindings (optional)
;;; ============================================================================

(defmacro with-indef-symbols (&body body)
  "Execute body with symbol-macros for all captured simple symbol bindings.
   This allows you to reference captured vars directly by name."
  (let ((bindings '()))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (symbolp k)
                 (push k bindings)))
             *bindings*)
    `(symbol-macrolet ,(mapcar (lambda (sym)
                                 `(,sym (@ ',sym)))
                               bindings)
       ,@body)))

;;; ============================================================================
;;; Print greeting on load
;;; ============================================================================

(format t "~&;; Indef loaded. Commands:~%")
(format t ";;   (indef:defun* name args &body body) - define indef'd function~%")
(format t ";;   (indef:indef 'fn)     - instrument existing function~%")
(format t ";;   (indef:unindef 'fn)   - remove instrumentation~%")
(format t ";;   (indef:@ 'var)        - get captured binding~%")
(format t ";;   (indef:@@ 'fn)        - get last return value~%")
(format t ";;   (indef:@@> 'fn)       - get reconstructed call~%")
(format t ";;   (indef:show)          - show all captured state~%")
(format t ";;   (indef:clear)         - clear all state and unbind symbols~%")
(format t ";;~%")
(format t ";;   Captured values are accessible directly: x, sum, etc.~%")
(format t ";;   Set indef:*set-symbols* to nil to disable this.~%")
