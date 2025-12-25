;;;; examples.lisp - Examples of using indef

(in-package :cl-user)

;; Load indef
(asdf:load-system :indef)
(use-package :indef)

;;; ============================================================================
;;; Basic Example
;;; ============================================================================

(defun* calculate-stats (numbers)
  "Calculate basic statistics for a list of numbers."
  (let* ((count (length numbers))
         (sum (reduce #'+ numbers))
         (mean (/ sum count))
         (sorted (sort (copy-list numbers) #'<))
         (median (nth (floor count 2) sorted)))
    (list :count count
          :sum sum
          :mean mean
          :median median)))

#|
;; Try it:
(calculate-stats '(1 5 3 9 2 8 4 7 6))

;; Inspect:
(indef:@ 'numbers)   ; => (1 5 3 9 2 8 4 7 6)
(indef:@ 'count)     ; => 9
(indef:@ 'sum)       ; => 45
(indef:@ 'mean)      ; => 5
(indef:@ 'median)    ; => 5
(indef:@ 'sorted)    ; => (1 2 3 4 5 6 7 8 9)

(indef:@@ 'calculate-stats)   ; => (:COUNT 9 :SUM 45 :MEAN 5 :MEDIAN 5)
(indef:@@> 'calculate-stats)  ; => (CALCULATE-STATS (1 5 3 9 2 8 4 7 6))

(indef:show)
|#

;;; ============================================================================
;;; Destructuring Example
;;; ============================================================================

(defun* process-request (request)
  "Process a request with destructuring."
  (destructuring-bind (&key method path headers body) request
    (let ((content-type (cdr (assoc :content-type headers)))
          (auth-token (cdr (assoc :authorization headers))))
      (list :processed t
            :method method
            :path path
            :has-auth (not (null auth-token))
            :content-type content-type))))

#|
;; Try it:
(process-request '(:method :post 
                   :path "/api/users"
                   :headers ((:content-type . "application/json")
                             (:authorization . "Bearer xyz"))
                   :body "{\"name\": \"test\"}"))

;; Inspect:
(indef:@ 'method)       ; => :POST
(indef:@ 'path)         ; => "/api/users"
(indef:@ 'headers)      ; => ((:CONTENT-TYPE . "application/json") ...)
(indef:@ 'content-type) ; => "application/json"
(indef:@ 'auth-token)   ; => "Bearer xyz"
|#

;;; ============================================================================
;;; Nested Functions Example
;;; ============================================================================

(defun* outer-function (x)
  "Outer function with nested lambdas and flet."
  (flet ((helper (n)
           (let ((doubled (* n 2)))
             doubled)))
    (let ((mapped (mapcar (lambda (item)
                            (let ((processed (+ item 10)))
                              processed))
                          (list x (1+ x) (+ x 2)))))
      (list :input x
            :helper-result (helper x)
            :mapped mapped))))

#|
;; Try it:
(outer-function 5)

;; Inspect:
(indef:@ 'x)           ; => 5
(indef:@ 'helper/n)    ; => 5 (from the flet helper)
(indef:@ 'helper/doubled) ; => 10
(indef:@ 'item)        ; => last item processed (7)
(indef:@ 'processed)   ; => 17
(indef:@ 'mapped)      ; => (15 16 17)
|#

;;; ============================================================================
;;; Using slet in Regular Functions
;;; ============================================================================

(defun regular-function (data)
  "A regular function that uses slet for selective indef capture."
  (let ((not-captured (+ 1 2)))  ; This won't be captured
    (slet ((captured-value (* data 10))
           (another-captured (format nil "Data: ~A" data)))
      (list not-captured captured-value another-captured))))

#|
;; Try it:
(regular-function 42)

;; Inspect:
(indef:@ 'not-captured)      ; => NIL (not captured!)
(indef:@ 'captured-value)    ; => 420
(indef:@ 'another-captured)  ; => "Data: 42"
|#

;;; ============================================================================
;;; Dynamic Instrumentation Example
;;; ============================================================================

;; Define a normal function
(defun fibonacci (n)
  "Calculate nth Fibonacci number."
  (if (<= n 1)
      n
      (let ((a (fibonacci (- n 1)))
            (b (fibonacci (- n 2))))
        (+ a b))))

#|
;; Instrument it:
(indef:indef 'fibonacci)

;; Call it:
(fibonacci 10)

;; Inspect the last call:
(indef:@ 'n)           ; => 2 (last n value)
(indef:@ 'a)           ; => 1
(indef:@ 'b)           ; => 0
(indef:@@ 'fibonacci)  ; => 55

;; Remove instrumentation:
(indef:unindef 'fibonacci)
|#

;;; ============================================================================
;;; Multiple Value Bind Example
;;; ============================================================================

(defun* divide-with-remainder (dividend divisor)
  "Divide and capture both quotient and remainder."
  (multiple-value-bind (quotient remainder) (floor dividend divisor)
    (let ((description (format nil "~A = ~A * ~A + ~A" 
                                dividend quotient divisor remainder)))
      (values quotient remainder description))))

#|
;; Try it:
(divide-with-remainder 17 5)

;; Inspect:
(indef:@ 'dividend)    ; => 17
(indef:@ 'divisor)     ; => 5
(indef:@ 'quotient)    ; => 3
(indef:@ 'remainder)   ; => 2
(indef:@ 'description) ; => "17 = 3 * 5 + 2"
|#

;;; ============================================================================
;;; Debugging Workflow Example
;;; ============================================================================

(defun* process-item (item)
  "Process a single item - but there's a bug!"
  (let* ((value (getf item :value))
         (multiplier (getf item :mult 1))  ; default to 1
         (result (* value multiplier)))
    result))

(defun* process-all (items)
  "Process all items and sum results."
  (let ((results (mapcar #'process-item items)))
    (reduce #'+ results)))

#|
;; This will fail because one item is missing :value
(process-all '((:value 10 :mult 2)
               (:value 20)
               (:mult 3)))  ; <- bug: missing :value!

;; Inspect to find the problem:
(indef:@ 'item)        ; => (:MULT 3)
(indef:@ 'value)       ; => NIL <- aha!
(indef:@ 'multiplier)  ; => 3
(indef:@ 'result)      ; => error or 0

;; Get the call that failed:
(indef:@@> 'process-item)  ; => (PROCESS-ITEM (:MULT 3))

;; Fix the function, re-evaluate, then re-run:
(eval (indef:@@> 'process-item))
|#

;;; ============================================================================
;;; Tips
;;; ============================================================================

#|
TIPS FOR EFFECTIVE INDEF DEBUGGING:

1. Use defun* during development, switch to defun for production

2. Use (indef:clear) between debugging sessions to avoid confusion

3. For recursive functions, remember you only see the LAST call's values

4. Use (indef:show 'function-name) to filter bindings for one function

5. The call reconstruction (@@>) is great for:
   - Re-running after fixing a bug
   - Creating test cases from real calls
   - Understanding what arguments led to a failure

6. In Emacs, use C-c C-s to temporarily indef a function without
   modifying your source code

7. Combine with the inspector (C-c s i) to drill into complex values
|#
