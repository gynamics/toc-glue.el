;;; toc-glue.el --- Glue functions for outline editting. -*- lexical-binding: t -*-

;; Author: gynamics
;; Maintainer: gynamics
;; Package-Version: 0.2
;; Package-Requires:
;; URL: https://github.com/gynamics/toc-glue.el
;; Keywords: tools


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This file provides a minimal set of functions that interpret
;; various outline formats from or to Lisp.
;;
;; 1. Grammar of DjVuLibre outline
;;
;; START -> "(" "bookmarks" TREE* ")"
;; TREE  -> "(" TITLE PAGE TREE* ")"
;; TITLE -> ESCAPED_STRING
;; PAGE  -> "\"#" DECIMAL_INTEGER "\""
;;
;; This grammar is compatible with Elisp so we can read it in
;; directly. To be simpler, we have:
;;
;; 2. Grammar of simplified outline
;;
;; This grammar is simplified DjVuLibre outline, which is chosen as a
;; generic intermediate representation.
;;
;; PAGE  -> DECIMAL_INTEGER
;;
;; Other rules are the same as DjVuLibre outline format.  This format
;; makes it easier to operate page numbers.
;;
;; This file provides functions that converts between these two
;; formats.
;;
;; There is another common format called ToC format, looks like:
;;
;; Section 1: Introduction ... 1
;;   Chapter 1: History of Lisp ... 2
;;     1.1 Original ideas of McCarthy ... 2
;;     1.2 Early implementations and progressions in 1970s  ... 4
;;     1.3 Industrial implementations and modern Lisp  ... 6
;;   Chapter 2: Features of Emacs Lisp ... 9
;;   ...
;;
;; It will be useful to be able to proceed ToC format because this
;; format is common in printed books. You can get some assistance from
;; doc-toc.el, which provides OCR & ToC text parsing.
;;
;; This file also provides functions that converts between ToC format
;; and simple outline format.
;;
;; Unluckily, currently it can not generate autoloads of commands automatically.
;; If you want to defer loading of this package, you will need to write them
;; manually.

;;; Code:

(defun djvu-outline-map (sexp func)
  "Call FUNC on each (title number) pair in djvulibre outline SEXP."
  (cond
   ;; not a list
   ((not (listp sexp)) sexp)
   ;; it starts from top-level
   ((equal (car sexp) 'bookmarks)
    (cons (car sexp)
          (mapcar (lambda (item) (djvu-outline-map item func))
                  (cdr sexp))))
   ;; if the list has at least two elements
   ((and (>= (length sexp) 2)
         ;; check if elements are valid
         (stringp (cadr sexp))
         (string-match-p "^#[0-9]+$" (cadr sexp)))
    (let* ((first (car sexp))
           (second (cadr sexp))
           (result (funcall func first second)))
      ;; construct a new list
      (cons (car result)
            (cons (cadr result)
                  (mapcar (lambda (item) (djvu-outline-map item func))
                          (cddr sexp))))))
   ;; Otherwise, raise an error
   (t (error "Invalid outline structure, %S" sexp))))

(defun djvu-outline-shift (sexp offset)
  "Add OFFSET to each page number in djvulibre outline SEXP."
  (if (listp sexp)
      (djvu-outline-map
       sexp
       (lambda (first second)
         (list first
               (format "#%d"
                       (+ (string-to-number (substring second 1))
                          offset)))))
    (error "Last sexp is not a list!")))

(defun djvu-outline-simplify (sexp)
  "Simplify djvulibre outline SEXP to a simple outline."
  (djvu-outline-map
   sexp
   (lambda (first second)
     (list first
           (string-to-number (substring second 1))))))

(defun simple-outline-map (sexp func)
  "Map FUNC to each (title number) pair in simple outline SEXP."
  (cond
   ;; not a list
   ((not (listp sexp)) sexp)
   ;; it starts from top-level
   ((equal (car sexp) 'bookmarks)
    (cons (car sexp)
          (mapcar (lambda (item) (simple-outline-map item func))
                  (cdr sexp))))
   ;; if the list has at least two elements
   ((and (>= (length sexp) 2)
         ;; check if elements are valid
         (stringp (car sexp))
         (numberp (cadr sexp)))
    (let* ((first (car sexp))
           (second (cadr sexp))
           (result (funcall func first second)))
      ;; construct a new list
      (cons (car result)
            (cons (cadr result)
                  (mapcar (lambda (item)
                            (simple-outline-map item func))
                          (cddr sexp))))))
   ;; Otherwise, raise an error
   (t (error "Invalid simple outline structure, %S" sexp))))

(defun simple-outline-to-djvu (sexp)
  "Format a simplified SEXP to djvulibre outline format."
  (simple-outline-map
   sexp
   (lambda (first second)
     (list first (format "#%d" second)))))

(defun simple-outline-shift (sexp offset)
  "Shift OFFSET on each page number in simple outline SEXP."
  (simple-outline-map
   sexp
   (lambda (first second)
     (list first (+ offset second)))))

(defun toc-glue--default-lexer (line)
  "Default lexer to analyze one LINE string in a ToC.
It determine level by whitespaces before plus prefix length."
  (let ((prefix "\\(\s*\\)\\([0-9]\\(\\.[0-9]+\\)*\\)?")
        (title "\s*\\([^\s].*\\)")
        (page "\s[\\.\s]*\\([0-9]+\\)"))
    (when (string-match (concat prefix title page) line)
      (list
       ;; decide level by sum of leading spaces and section numbers
       (+ (length (match-string 1 line))
          (length (match-string 2 line)))
       ;; section number ++ title
       (concat (match-string 2 line) " " (match-string 4 line))
       (string-to-number (match-string 5 line))))))

(defun toc-to-simple-outline (s &optional lexer)
  "Convert given ToC string S to simple outline format.
LEXER may be given to replace the lexer function, which accepts one line
string, return a list (level title page) of type (number string number).
If analyze failed, return NIL."
  (let ((lexer (or lexer #'toc-glue--default-lexer))
        (lines (split-string s "\n" t))
        (stack '((-1 bookmarks))))
    (dolist (line lines)
      (if-let ((item (funcall lexer line)))
          (cond
           ;; current line has deeper level, simply shift it in
           ((or (null stack)
                (< (car (car stack)) (car item)))
            (push item stack))
           ;; otherwise reduce on stack until we can shift it in
           (t
            (while (and (>= (length stack) 2)
                        (>= (caar stack) (car item)))
              (let ((child (pop stack)))
                (nconc (cdar stack) (list (cdr child)))))
            ;; then we can push it on stack
            (push item stack)))
        (error "Failed to parse ToC line: %s" line)))
    ;; final reduction
    (while (>= (length stack) 2)
      (let ((child (pop stack)))
        (nconc (cdar stack) (list (cdr child)))))
    ;; return stack top
    (cdar stack)))

(defun toc-to-lisp (begin end)
  "Convert a ToC between BEGIN and END to a simple outline."
  (toc-to-simple-outline
   (buffer-substring-no-properties begin end)))

(defun simple-outline-to-toc (sexp func level)
  "Call FUNC on each (LEVEL title number) trituple in simple outline SEXP.
Different from `simple-outline-map', The output is serialized."
  (cond
   ;; not a list
   ((not (listp sexp)) "")
   ;; it starts from top-level
   ((equal (car sexp) 'bookmarks)
    (mapconcat (lambda (item) (simple-outline-to-toc
                          item func (1+ level)))
               (cdr sexp) ""))
   ((and (>= (length sexp) 2)
           ;; check if elements are valid
           (stringp (car sexp))
           (numberp (cadr sexp)))
      (let* ((first (car sexp))
             (second (cadr sexp))
             (result (funcall func level first second)))
        (concat
         result
         (mapconcat (lambda (item)
                      (simple-outline-to-toc
                       item func (1+ level)))
                    (cddr sexp) ""))))
   ;; Otherwise, raise an error
   (t (error "Invalid simple outline structure, %S" sexp))))

(defun string-repeat (n str &optional separator)
  "Repeat STR for N times, SEPARATOR for padding intervals."
  (mapconcat (lambda (_) str) (number-sequence 0 n) separator))

(defun lisp-to-toc (s &optional top-level formatter)
  "Convert simple outline string S to a ToC.
All sexps in the input string should be closed.
This function returns a whole ToC string, use `insert' to use it.

FORMATTER [default (lambda (level title page) ...)] is a print function,
it should return a string containing one ToC line, including newline.

TOP-LEVEL [default 0] set the top ToC level."
  (let ((top-level (or top-level 0))
        (formatter
         (or formatter
             (lambda (level title page)
               (format "%s \"%s\" %s\n"
                       (string-repeat (1- level) "  " "") title page)))))
    (let ((res)
          (pos 0))
      (while (< pos (length s))
       (let* ((m (read-from-string pos))
              (sexp (car m))
              (next-pos (cadr m)))
         (if (listp sexp)
             (push (simple-outline-to-toc sexp formatter (1- top-level)) res)
           (error "Failed to read sexp at %s:%d!" s pos))
         (setq pos next-pos)))
      (mapconcat 'identity res))))

(defmacro toc-glue:def-interact (f &optional binds body)
  "Define an interactive function based on given function F.

plist BINDS will be passed to `interactive'.

If procedure BODY is given, it will be invoked after calling F,
the output of F will be bound to lexical variable \\[output]."
  (let ((arglist (mapcar (lambda (desc) (car desc)) binds)))
    `(defun ,(intern (concat "toc-glue:" (symbol-name f))) (,@arglist)
       ;; docstring
       ,(concat
         ;; cut the first line of original function
         (let ((str (documentation f)))
           (substring str 0 (or (string-match "\n" str) (length str))))
         "\n\nThis function is produced by `toc-glue:def-interact'\n"
         "With a universal prefix, it prompts for a buffer to output.")
       (interactive (list ,@(mapcar (lambda (desc) (cadr desc)) binds)))
       (let ((output (,f ,@arglist)))
         ,body))))

(defmacro toc-glue:def-interact-s (f &optional binds)
  "An alias of `toc-glue:def-interact' that specialized for sexp.
The first argument of F is always binded to `sexp-at-point'.

By default, the function replaces last sexp with output of F,
With a prefixed argument, it prompts for a buffer to output.

plist BINDS will be passed to `interactive'."
  (macroexpand
   `(toc-glue:def-interact
     ,f
     ((sexp (sexp-at-point)) ,@binds)
     (let ((buf (if current-prefix-arg
                    (read-buffer "Output to buffer: ")
                  (buffer-name))))
       (if current-prefix-arg
           (display-buffer buf)
           (backward-kill-sexp))
       (pp output (get-buffer buf))))))

(toc-glue:def-interact-s
 djvu-outline-shift ((offset (read-number "Offset value: "))))

(toc-glue:def-interact-s
 simple-outline-shift ((offset (read-number "Offset value: "))))

(defmacro toc-glue:def-interact-t (f &optional binds prefix mode)
  "An alias of `toc-glue:def-interact' that specialized for translation.
The output of F is always pretty printed to a buffer.

By default, the function pretty-print to a temporary buffer.
With a prefixed argument, it prompts for a buffer to output.

plist BINDS will be passed to `interactive'.

string PREFIX will be used for constructing temporary buffer name.

Enable MODE in temporary buffer."
  (macroexpand
   `(toc-glue:def-interact
     ,f ,binds
     (pp output
         (or (when current-prefix-arg
               (read-buffer "Output to buffer: "))
             (let ((buf
                    (get-buffer-create
                     (concat ,prefix " - " (format-time-string "%Y%m%d%H%M%S*")))))
               (with-current-buffer buf (,mode))
               (display-buffer buf)
               buf))))))

(toc-glue:def-interact-t
 toc-to-lisp ((begin (region-beginning)) (end (region-end)))
 "Simple Outline" toc-glue-mode)

(toc-glue:def-interact-t
 lisp-to-toc ((sexp (sexp-at-point)))
 "ToC" toc-mode)

(toc-glue:def-interact-t
 djvu-outline-simplify ((sexp (sexp-at-point)))
 "Simple Outline" toc-glue-mode)

(toc-glue:def-interact-t
 simple-outline-to-djvu ((sexp (sexp-at-point)))
 "DjVu Outline" toc-glue-djvu-mode)

;;;###autoload
(define-derived-mode toc-mode fundamental-mode "ToC"
  "Major mode for editing ToC outline."
  (define-key toc-mode-map (kbd "C-c C-c") #'toc-glue:toc-to-lisp))

;;;###autoload
(define-derived-mode toc-glue-mode lisp-data-mode "ToC Glue (Lisp)"
  "Major mode for editing simple outline."
  (define-key toc-glue-mode-map (kbd "C-c >") #'toc-glue:simple-outline-shift)
  (define-key toc-glue-mode-map (kbd "C-c C-t") #'toc-glue:lisp-to-toc)
  (define-key toc-glue-mode-map (kbd "C-c C-c") #'toc-glue:simple-outline-to-djvu))

;;;###autoload
(define-derived-mode toc-glue-djvu-mode lisp-data-mode "ToC Glue (DjVu)"
  "Major mode for editing simple outline."
  (define-key toc-glue-djvu-mode-map (kbd "C-c >") #'toc-glue:djvu-outline-shift)
  (define-key toc-glue-djvu-mode-map (kbd "C-c C-c") #'toc-glue:djvu-outline-simplify))

(provide 'toc-glue)

;;; toc-glue.el ends here
