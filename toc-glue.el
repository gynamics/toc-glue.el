;;; toc-glue.el --- Glue functions for outline editting. -*- lexical-binding: t -*-

;; Author: gynamics
;; Maintainer: gynamics
;; Package-Version: 0.1
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

;;; Code:

(defun djvu-outline-map (lst func)
  "Call FUNC on each (title number) pair in djvulibre outline LST."
  (cond
   ;; not a list
   ((not (listp lst)) lst)
   ;; it starts from top-level
   ((equal (car lst) 'bookmarks)
    (cons (car lst)
          (mapcar (lambda (item) (djvu-outline-map item func))
                  (cdr lst))))
   ;; if the list has at least two elements
   ((and (>= (length lst) 2)
         ;; check if elements are valid
         (stringp (cadr lst))
         (string-match-p "^#[0-9]+$" (cadr lst)))
    (let* ((first (car lst))
           (second (cadr lst))
           (result (funcall func first second)))
      ;; construct a new list
      (cons (car result)
            (cons (cadr result)
                  (mapcar (lambda (item) (djvu-outline-map item func))
                          (cddr lst))))))
   ;; Otherwise, raise an error
   (t (error "Invalid outline structure, %S" lst))))

(defun djvu-outline-shift (lst offset)
  "Add OFFSET to each page number in djvulibre outline LST."
  (interactive (list (sexp-at-point)
                     (read-number "Offset value: ")))
  (if (listp lst)
      (djvu-outline-map
       lst
       (lambda (first second)
         (list first
               (format "#%d"
                       (+ offset
                          (string-to-number
                           (substring second 1)))))))
    (error "Last sexp is not a list!")))

(defun djvu-outline-simplify (lst)
  "Simplify djvulibre outline LST to a simple outline."
  (interactive (list (sexp-at-point)))
  (if (listp lst)
      (djvu-outline-map
       lst
       (lambda (first second)
         (list first
               ((lambda (numstr)
                  (string-to-number
                   (substring numstr 1)))
                second))))
    (error "Last sexp is not a list!")))

(defun simple-outline-map (lst func)
  "Map FUNC to each (title number) pair in simple outline LST."
  (cond
   ;; not a list
   ((not (listp lst)) lst)
   ;; it starts from top-level
   ((equal (car lst) 'bookmarks)
    (cons (car lst)
          (mapcar (lambda (item) (simple-outline-map item func))
                  (cdr lst))))
   ;; if the list has at least two elements
   ((and (>= (length lst) 2)
         ;; check if elements are valid
         (stringp (cadr lst))
         (numberp (cadr lst)))
    (let* ((first (car lst))
           (second (cadr lst))
           (result (funcall func first second)))
      ;; construct a new list
      (cons (car result)
            (cons (cadr result)
                  (mapcar (lambda (item)
                            (simple-outline-map item func))
                          (cddr lst))))))
   ;; Otherwise, raise an error
   (t (error "Invalid simple outline structure, %S" lst))))

(defun lisp-to-djvu-outline (lst)
  "Format a simplified LST to djvulibre outline format."
  (interactive (list (sexp-at-point)))
  (if (listp lst)
      (djvu-outline-map lst
                   (lambda (first second)
                     (list first (format "#%d" second))))
    (error "Last sexp is not a list!")))

(defun simple-outline-shift (lst offset)
  "Shift OFFSET on each page number in simple outline LST."
  (interactive (list (sexp-at-point)
                     (read-number "Offset value: ")))
  (if (listp lst)
      (simple-outline-map
       lst
       (lambda (first second)
         (list first (+ offset second))))
    (error "Last sexp is not a list!")))

(defmacro default-options (options &rest defaults)
  "A more elegant way to set default values for keyword arguments.
This macro can be used to replace the let varlist.

OPTIONS is a plist of keyword arguments, usually given by &rest .
DEFAULTS is an association list consists of (sym . value) pairs."
  (mapcar (lambda (pair)
            `(,(car pair)
              (or (plist-get ,options
                             ,(intern (concat ":" (symbol-name (car pair)))))
                  ,(cadr pair))))
          defaults))

(defmacro let-defaults (options valist &rest body)
  "A let helper for setting default values for keyword arguments.

OPTIONS is a plist of keyword arguments, usually given by &rest .
VALIST is a list of default values.
BODY is the function body, given as it is to `let' ."
  `(let (default-options ,options ,@valist) ,@body))

(defun toc-to-simple-outline (s &rest options)
  "Convert given ToC string S to simple outline format.

Each line of toc string is divided into four parts:

\\(prefix\\) \\(title\\) separator \\(page\\)

where
  prefix gives level of current line; (e. g. indentation depth)
  title gives a string as name tag;
  separator matches the separator sequence between title and page;
  page gives a number as referred position.

Keyword OPTIONS:

- :prefix is a regexp that matches prefix from the start of a
  line, the length of prefix should give correct level of current
  line, be aware that normally we should use passive matching for
  prefix.

- :separator (default \"\\s+\") a regexp that matches separators
  between title and page, it should not include any \\(...\\)
  which breaks match order.

- :regexp (default
  \"^\\\\(\\s+*\\\\)\\\\(.+*\\\\)\\s+\\\\([0-9]+\\\\)$\") allows
  you to overwrite the whole regexp without respecting values of
  :prefix and :separator, please make sure it matches exactly
  three \\(...\\) cells as explained above."
  (let-defaults
   options
   ((prefix "\s+*")
    (separator "\s+"))
   (let-defaults
    options
    ((regexp (format "^\\(%s\\)\\(.+*\\)%s\\([0-9]+\\)$"
                     prefix separator)))
    (let ((lines (split-string s "\n" t))
          (stack '()))
      (dolist (line lines)
        (when (string-match regexp line)
          (let* ((level (match-string 1 line))
                 (title (match-string 2 line))
                 (page (string-to-number (match-string 3 line)))
                 (item (list level title page)))
            (cond
             ;; current line has deeper level, simply shift it in
             ((or (null stack)
                  (< (car (car stack)) level))
              (push item stack))
             ;; otherwise reduce on stack until we can shift it in
             (t
              (while (and (>= (length stack) 2)
                          (>= (car (car stack)) level))
                (let ((child (pop stack)))
                  (nconc (cdar stack) (list (cdr child)))))
              ;; then we can push it on stack
              (push item stack)))
            )))
      ;; final reduction as if a terminal of level 0 pushed in
      (while (>= (length stack) 2)
        (let ((child (pop stack)))
          (nconc (cdar stack) (list (cdr child)))))
      ;; compose result
      (cons 'bookmarks
            (mapcar (lambda (item) (cdr item))
                    stack)))
    )))

(defun toc-to-lisp (start end)
  "Convert a ToC between START and END to a simple outline."
  (interactive "r")
  (toc-to-simple-outline
   (buffer-substring-no-properties start end)))

(defun simple-outline-to-toc (lst func level)
  "Call FUNC on each (LEVEL title number) trituple in simple outline LST.
Different from `simple-outline-map', The output is serialized."
  (cond
   ;; not a list
   ((not (listp lst)) "")
   ;; it starts from top-level
   ((equal (car lst) 'bookmarks)
    (mapconcat (lambda (item) (simple-outline-to-toc
                          item func (1+ level)))
               (cdr lst) ""))
   ((and (>= (length lst) 2)
           ;; check if elements are valid
           (stringp (car lst))
           (numberp (cadr lst)))
      (let* ((first (car lst))
             (second (cadr lst))
             (result (funcall func level first second)))
        (concat
         result
         (mapconcat (lambda (item)
                      (simple-outline-to-toc
                       item func (1+ level)))
                    (cddr lst) ""))))
   ;; Otherwise, raise an error
   (t (error "Invalid simple outline structure, %S" lst))))

(defun string-repeat (n str &optional separator)
  "Repeat STR for N times, SEPARATOR for padding intervals."
  (mapconcat (lambda (_) str) (number-sequence 0 n) separator))

(defun lisp-to-toc (lst &rest options)
  "Convert simple outline LST to a ToC.
This function returns a whole ToC string, use `insert' to use it.

Keyword OPTIONS:

- :formatter (default (lambda (level title page) ...)) is a print
  function, it should return a string containing one ToC line,
  including newline.

- :level (default 0) set the top ToC level."
  (interactive (list (sexp-at-point)))
  (let-defaults
   options
   ((level 0)
    (formatter
     (lambda (level title page)
           (format "%s \"%s\" %s\n"
                   (string-repeat (1- level) "  " "") title page))))
   (if (listp lst)
       (simple-outline-to-toc lst formatter (1- level))
     (error "Last sexp is not a list!"))))

(provide 'toc-glue)

;;; toc-glue.el ends here
