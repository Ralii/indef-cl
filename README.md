# Indef - Debug helper for Common Lisp

REPL-driven debugging by capturing function parameters and let bindings for later inspection.

## Features

- **`defun*`** - Define functions that automatically capture all bindings
- **Dynamic instrumentation** - Indef existing functions without modifying source
- **Let binding capture** - All `let`, `let*`, `destructuring-bind` bindings captured
- **Return value access** - Get the last return value of any indef'd function
- **Call reconstruction** - Reproduce the last call to re-run after fixing bugs
- **SLY integration** - Emacs commands to indef/inspect without leaving your editor

## Installation

### Common Lisp

```lisp
;; Add to your ASDF load path, then:
(asdf:load-system :indef)
```

Or copy `indef.lisp` and load it directly:

```lisp
(load "path/to/indef.lisp")
```

### Emacs/SLY

**With straight.el:**

```elisp
(use-package sly-indef
  :straight (:host github :repo "ralii/indef-cl")
  :hook (sly-mode . sly-indef-setup-keybindings))
```

**With Doom Emacs:**

Add to `packages.el`:

```elisp
(package! sly-indef :recipe (:host github :repo "ralii/indef-cl"))
```

Add to `config.el`:

```elisp
(use-package! sly-indef
  :hook (sly-mode . sly-indef-setup-keybindings))
```

**Manual installation:**

```elisp
(add-to-list 'load-path "path/to/indef/")
(require 'sly-indef)
(add-hook 'sly-mode-hook #'sly-indef-setup-keybindings)
```

## Usage

### Basic Usage (REPL)

```lisp
;; Use defun* instead of defun
(indef:defun* calculate (x y)
  (let ((sum (+ x y))
        (product (* x y)))
    (list :sum sum :product product)))

;; Call the function
(calculate 3 7)
;; => (:SUM 10 :PRODUCT 21)

;; Now inspect captured values:
(indef:@ 'x)       ; => 3
(indef:@ 'y)       ; => 7
(indef:@ 'sum)     ; => 10
(indef:@ 'product) ; => 21

;; Get the return value
(indef:@@ 'calculate)  ; => (:SUM 10 :PRODUCT 21)

;; Get the reconstructed call (to re-run after fixing a bug)
(indef:@@> 'calculate) ; => (CALCULATE 3 7)

;; Show all captured state
(indef:show)
```

### Dynamic Instrumentation

You can indef existing functions without modifying their source:

```lisp
;; Define a normal function
(defun my-complex-function (a b c)
  (let ((intermediate (* a b)))
    (+ intermediate c)))

;; Instrument it
(indef:indef 'my-complex-function)

;; Now when you call it, bindings are captured
(my-complex-function 2 3 4)
;; => 10

(indef:@ 'a)            ; => 2
(indef:@ 'intermediate) ; => 6

;; Remove instrumentation
(indef:unindef 'my-complex-function)
```

### Using slet/slet* in Regular Functions

```lisp
(defun my-function (x)
  (indef:slet* ((doubled (* x 2))
                (squared (* doubled doubled)))
    squared))

(my-function 3)
;; => 36

(indef:@ 'doubled) ; => 6
(indef:@ 'squared) ; => 36
```

## Emacs/SLY Commands

After setting up keybindings with `sly-indef-setup-keybindings`, use `C-c s` as a prefix:

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c C-s` | `sly-indef-eval-defun` | Evaluate current defun as indef'd |
| `C-c s s` | `sly-indef-eval-defun` | Same as above |
| `C-c s S` | `sly-indef-function` | Indef an existing function by name |
| `C-c s u` | `sly-indef-unindef` | Remove indef from a function |
| `C-c s v` | `sly-indef-show` | Show all captured bindings |
| `C-c s V` | `sly-indef-show-at-point` | Show bindings for function at point |
| `C-c s i` | `sly-indef-inspect-var` | Inspect a captured variable |
| `C-c s r` | `sly-indef-get-return` | Get last return value |
| `C-c s >` | `sly-indef-get-call` | Get reconstructed call (copies to kill ring) |
| `C-c s c` | `sly-indef-clear` | Clear all captured state |
| `C-c s l` | `sly-indef-list` | List all indef'd functions |

### Typical Workflow

1. Write your function normally
2. Position cursor inside the function
3. Press `C-c C-s` to evaluate it as indef'd
4. Call the function (from REPL or via tests)
5. Press `C-c s v` to see all captured bindings
6. Press `C-c s i` to inspect specific variables
7. Press `C-c s >` to get the call form, fix bug, re-evaluate

## API Reference

### Macros

- **`(indef:defun* name lambda-list &body body)`** - Like `defun`, captures all bindings
- **`(indef:slet bindings &body body)`** - Like `let`, captures bindings
- **`(indef:slet* bindings &body body)`** - Like `let*`, captures bindings
- **`(indef:slambda lambda-list &body body)`** - Like `lambda`, captures bindings

### Functions

- **`(indef:indef 'function-name)`** - Instrument an existing function
- **`(indef:unindef 'function-name)`** - Remove instrumentation
- **`(indef:indef-p 'function-name)`** - Check if function is indef'd
- **`(indef:list-indef)`** - List all indef'd functions

### Inspection

- **`(indef:@ 'var-name)`** - Get a captured binding value
- **`(indef:@@ 'function-name)`** - Get last return value
- **`(indef:@@> 'function-name)`** - Get reconstructed call form
- **`(indef:show)`** - Display all captured state
- **`(indef:show-function 'fn)`** - Display state for one function
- **`(indef:clear)`** - Clear all captured state

### Storage (Direct Access)

- **`indef:*bindings*`** - Hash table of all captured bindings
- **`indef:*returns*`** - Hash table of return values
- **`indef:*calls*`** - Hash table of call forms

## How It Works

When you define a function with `defun*` or indef it with `(indef:indef 'fn)`, the macro/function:

1. Wraps the function to capture all parameter values
2. Transforms all `let`/`let*` forms to capture their bindings
3. Stores the last return value
4. Stores a reconstructable call form

All values are stored in hash tables keyed by symbol name (and optionally function context).

## Limitations

- Only captures the *last* values (not a history)
- Indef'ing functions without source available only captures numbered args (`ARG0`, `ARG1`, etc.)
- May impact performance on hot paths - use for debugging only
- Thread safety: Uses global state, so concurrent calls may overwrite bindings

## License

MIT
