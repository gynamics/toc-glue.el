# toc-glue.el
## Ideas

DjVuLibre use lisp as an intermediate outline format, and Emacs is the all-mighty lisp editor. So it is nice to edit such outlines in Emacs. Moreover, is it possible to edit other outlines (e. g. PDF outlines) in lisp format?

There is already a good package ![doc-toc.el](https://github.com/dalanicolai/doc-tools-toc) which can use OCR and parse document text to generate a ToC, which can then be used by backend ![HandyOutliner](https://sourceforge.net/projects/handyoutlinerfo/). However, HandyOutliner is too integrated and does not work on ARM platforms, so I am looking for a looser solution. I created a pypdf-based backend ![pdf-outline](https://github.com/gynamics/pdf-outline) as the last piece of this puzzle.

## Architecture

![architecture](https://exiled-images.pages.dev/file/AgACAgUAAyEGAASL6SCLAAMEZ4fGKahjCSGdUGaTBBKitdWnUzsAArDBMRvMAUBUExeU78T9CegBAAMCAAN4AAM2BA.png)

If you want to edit ToC in Emacs, the first step will be convert it to a lisp structure, and map your operations to Lisp transformations. ToC is just a UI layer of lisp editing, and I think the core representation should be in Lisp format.

`toc-glue.el` provides a minimal set of interpreting functions to connect different representations and backends.

## Usage

``` emacs-lisp
;; there is only one single file, simply load it by `require', or something else.
(require 'toc-glue)
```

Currently I only implemented a minimal set of elisp functions for format converting as described above. These functions are just enough for one to use, but not that smoothly. It does not own a safe buffer or mode, but directly prints to `*Messages*` buffer. You can create a wrapper to yank the output to kill-ring to avoid copying across buffers.

I have defined a macro `toc-glue:def-interactive` which converts these functions into interactive function definitions. You can access these functions with prefix `toc-glue:`. The default behavior of these interactive functions are replace in place, and a `C-u` prefix you will be able to select a buffer for output.

- `toc-glue:simple-outline-to-djvu`
- `toc-glue:simple-outline-shift`
- `toc-glue:toc-to-lisp`
- `toc-glue:lisp-to-toc`
- `toc-glue:djvu-outline-shift`
- `toc-glue:djvu-outline-simplify`

I don't have good ideas about UI, if you have a better idea, I will be happy to know it!

## Example

Suppose we have a pdf file `sicp.pdf` with wrong page numbers in outline.

``` shell
    ./pdf-get-outline.py sicp.pdf > sicp-outline.lisp
```

(Hint: You can also run shell commands in Emacs with `M-!`.)

In Emacs, let's edit `sicp-outline.lisp` in a buffer, you may found like a subtree of it looks like this:

``` emacs-lisp
        ("The Elements of Programming" "#35"
            ("Expressions" "#36")
            ("Naming and the Environment" "#39")
            ("Evaluating Combinations" "#41")
            ("Compound Procedures" "#44")
            ("The Substitution Model for Procedure Application" "#47")
            ("Conditional Expressions and Predicates" "#51")
            ("Example: Square Roots by Newton's Method" "#57")
            ("Procedures as Black-Box Abstractions" "#62"))
```

This subtree has wrong page numbers, which shifted forward by 1 from the correct page numbers. Let's correct it by:

- Move cursor to the start/end of this list, `lisp.el` provides related moving functions binded in `esc-map` by default.
- `M-x toc-glue:djvu-outline-shift RET`, it will prompt for a number as offset, input `-1 RET`.

Then you will find it becomes:

``` emacs-lisp
        ("The Elements of Programming" "#34"
            ("Expressions" "#35")
            ("Naming and the Environment" "#38")
            ("Evaluating Combinations" "#40")
            ("Compound Procedures" "#43")
            ("The Substitution Model for Procedure Application" "#46")
            ("Conditional Expressions and Predicates" "#50")
            ("Example: Square Roots by Newton's Method" "#56")
            ("Procedures as Black-Box Abstractions" "#61")
        )
```

Finally, write the outline back to `sicp.pdf`, save it in `sicp-corrected.pdf`:

``` shell
    ./pdf-set-outline.py sicp-corrected.pdf sicp-outline.lisp sicp.pdf
```

You can also create your own interactive wrappers to catch the output, e. g.

``` emacs-lisp
    (defun pp-value-to-kill-ring (cmd)
      (interactive "aCommand: ")
      (kill-new (pp (call-interactively cmd))))
```

Run `M-x pp-value-to-kill-ring RET djvu-outline-shift RET -1 RET` you will get this on your kill-ring:
