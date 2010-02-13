;;; rebol.el --- EMACS REBOL Editing Mode

;;-- History -------------------------------------------------------------
;;
;;   Original: jrm <bitdiddle@hotmail.com> 1998 from Scheme mode.
;;   Adapted-by: Marcus Petersson <d4marcus@dtek.chalmers.se>
;;   Modified-by: Jeff Kreis <jeff@rebol.com> 1999
;;   Updated-by: Sterling Newton <sterling@rebol.com> 2001
;;
;;   Archive: http://www.rebol.com/tools/rebol.el
;;   Keywords: languages, REBOL, lisp
;;
;;-------------------------------------------------------------------------

;;; Code:

(defvar rebol nil
  "Support for the REBOL programming language, <http://www.rebol.com/>")
;  :group 'languages)

(defvar rebol-rebol-command "rebol"
  "*Shell command used to start REBOL interpreter.")
;  :type 'string
;  :group 'rebol)

(defvar rebol-indent-offset 4
  "*Amount of offset per level of indentation.")
;  :type 'integer
;  :group 'rebol)

(defvar rebol-backspace-function 'backward-delete-char-untabify
  "*Function called by `rebol-electric-backspace' when deleting backwards.")
;  :type 'function
;  :group 'rebol)

(defvar rebol-delete-function 'delete-char
  "*Function called by `rebol-electric-delete' when deleting forwards.")
;  :type 'function
;  :group 'rebol)

;;;###autoload
(defun rebol-mode ()
  "Major mode for editing REBOL code.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{rebol-mode-map}
Entry to this mode calls the value of rebol-mode-hook
if that value is non-nil."
  (interactive)
  (column-number-mode t)
  (kill-all-local-variables)
  (rebol-mode-initialize)
  (rebol-mode-variables)
  (run-hooks 'rebol-mode-hook))

(defun rebol-mode-initialize ()
  (use-local-map rebol-mode-map)
  (setq mode-name "REBOL" major-mode 'rebol-mode)
  (setq tab-width 4) ; Added these two. -jeff
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))

(defun beginning-of-rebol-definition ()
  "Moves point to the beginning of the current REBOL definition"
  (interactive)
  (re-search-backward "^[a-zA-Z][a-zA-Z0-9---_]*:" nil 'move)
  )

(defun rebol-comment-indent (&optional pos)
  (save-excursion
    (if pos (goto-char pos))
    (cond ((looking-at ";;;") (current-column))
          ((looking-at ";;")
           (let ((tem (guess-rebol-indent)))
             (if (listp tem) (car tem) tem)))
          (t
           (skip-chars-backward " \t")
           (max (if (bolp) 0 (1+ (current-column)))
                comment-column)))))

(defvar rebol-indent-function 'rebol-indent-function "")

(defun rebol-indent-line (&optional whole-exp)
  "Indent current line as REBOL code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (interactive "P")
  (let ((indent (guess-rebol-indent)) shift-amt beg end
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (looking-at "[ \t]*;;;")
	;; Don't alter indentation of a ;;; comment line.
	nil
      (if (listp indent) (setq indent (car indent)))
	  (if (looking-at "[ \t]*[])]") (setq indent (- indent 4)))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
	  nil
	(delete-region beg (point))
	(indent-to indent))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos)))
      ;; If desired, shift remaining lines of expression the same amount.
      (and whole-exp (not (zerop shift-amt))
	   (save-excursion
	     (goto-char beg)
	     (forward-sexp 1)
	     (setq end (point))
	     (goto-char beg)
	     (forward-line 1)
	     (setq beg (point))
	     (> end beg))
	   (indent-code-rigidly beg end shift-amt)))))


(defun guess-rebol-indent (&optional parse-start)
  "Return appropriate indentation for current line as rebol code.
In usual case returns an integer: the column to indent to.
Can instead return a list, whose car is the column to indent to.
This means that following lines at the same level of indentation
should not necessarily be indented the same way.
The second element of the list is the buffer position
of the start of the containing expression."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point)) 
          indenting-block-p
          state
          block-depth
          desired-indent
          (retry t)
	  last-expr
          containing-expr
          first-expr-list-p)
      (setq indenting-block-p (looking-at "^[ \t]*\\s("))
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-rebol-definition))
      ;; Find outermost containing expr
      (while (< (point) indent-point)
	(setq state (parse-partial-sexp (point) indent-point 0)))
      ;; Find innermost containing sexp
      (while (and retry (setq block-depth (car state)) (> block-depth 0))
	(setq retry nil)
	(setq last-expr (nth 2 state))
	(setq containing-expr (car (cdr state)))
	;; Position following last unclosed open.
	(goto-char (1+ containing-expr))
	;; Is there a complete sexp since then?
	(if (and last-expr (> last-expr (point)))
	    ;; Yes, but is there a containing expr after that?
	    (let ((peek (parse-partial-sexp last-expr indent-point 0)))
	      (if (setq retry (car (cdr peek))) (setq state peek))))
	(if (not retry)
	    ;; Innermost containing sexp found
	    (progn
	      (goto-char (1+ containing-expr))
	      (if (not last-expr)
		  (setq desired-indent (* block-depth rebol-indent-offset))
		(setq desired-indent (* block-depth rebol-indent-offset))
;;;-----------------------------------------------------------------------------
;;; Seems to work the same with or without the commented-out lines below -Marcus
;;;
; 		;; Move to first expr after containing open paren
; 		(parse-partial-sexp (point) last-expr 0 t)
; 		(setq first-expr-list-p (looking-at "\\s("))
; 		(cond
; 		 ((> (save-excursion (forward-line 1) (point))
; 		     last-expr)
; 		  ;; Last expr is on same line as containing expr.
; 		  ;; It's almost certainly a function call.
; 		  (parse-partial-sexp (point) last-expr 0 t)
; 		  (if (/= (point) last-expr)
; 		      ;; Indent beneath first argument or, if only one expr
; 		      ;; on line, indent beneath that.
; 		      (progn (if indenting-block-p (forward-sexp 1))
; 			     (parse-partial-sexp (point) last-expr 0 t)))
; 		  (backward-prefix-chars))
; 		 (t
; 		  ;; Indent beneath first expr on same line as last-expr.
; 		  ;; Again, it's almost certainly a function call.
; 		  (goto-char last-expr)
; 		  (beginning-of-line)
; 		  (parse-partial-sexp (point) last-expr 0 t)
; 		  (backward-prefix-chars)))
;;;------------------------------------------------------------------------------
                ))))
      (cond ((car (nthcdr 3 state))
	     ;; Inside a string, don't change indentation.
	     (goto-char indent-point)
	     (skip-chars-forward " \t")
	     (setq desired-indent (current-column)))
	    ((not (or desired-indent
		      (and (boundp 'rebol-indent-function)
			   rebol-indent-function
			   (not retry)
			   (setq desired-indent
				 (funcall rebol-indent-function
					  indent-point state)))))
	     ;; Use default indentation if not computed yet
	     (setq desired-indent (current-column))))
      desired-indent
      )))

(defun rebol-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (save-excursion
      (goto-char (1+ (car (cdr state))))
      (re-search-forward "\\sw\\|\\s_")
      (if (/= (point) (car (cdr state)))
	  (let ((function (buffer-substring (progn (forward-char -1) (point))
					    (progn (forward-sexp 1) (point))))
		method)
	    (setq function (downcase function))
	    (setq method (get (intern-soft function) 'rebol-indent-function))
	    (cond ((integerp method)
		   (rebol-indent-specform method state indent-point))
		  (method
		   (funcall method state indent-point))
                  ))))))

(defvar rebol-body-indent 2 "")

(defun rebol-indent-specform (count state indent-point)
  (let ((containing-form-start (car (cdr state))) (i count)
	body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  rebol-indent-function guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ rebol-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
		(condition-case nil
		    (progn
		      (setq count (1- count))
		      (forward-sexp 1)
		      (parse-partial-sexp (point) indent-point 1 t))
		  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (cond ((> count 0)
	   ;; A distinguished form.  Use double rebol-body-indent.
	   (list (+ containing-form-column (* 2 rebol-body-indent))
		 containing-form-start))
	  ;; A non-distinguished form. Use body-indent if there are no
	  ;; distinguished forms and this is the first undistinguished
	  ;; form, or if this is the first undistinguished form and
	  ;; the preceding distinguished form has indentation at least
	  ;; as great as body-indent.
	  ((and (= count 0)
		(or (= i 0)
		    (<= body-indent normal-indent)))
	   body-indent)
	  (t
	   normal-indent))))

(defun rebol-indent-defform (state indent-point)
  (goto-char (car (cdr state)))
  (forward-line 1)
  (if (> (point) (car (cdr (cdr state))))
      (progn
	(goto-char (car (cdr state)))
	(+ rebol-body-indent (current-column)))))

(defun would-be-symbol (string)
  (not (string-equal (substring string 0 1) "(")))

(defun next-sexp-as-string ()
  ;; Assumes that protected by a save-excursion
  (forward-sexp 1)
  (let ((the-end (point)))
    (backward-sexp 1)
    (buffer-substring (point) the-end)))

(defun rebol-let-indent (state indent-point)
  (skip-chars-forward " \t")
  (if (looking-at "[-a-zA-Z0-9+*/?!@$%^&_:~]")
      (rebol-indent-specform 2 state indent-point)
      (rebol-indent-specform 1 state indent-point)))

(defun rebol-indent-expr ()
  "Indent each line of the list starting just after point."
  (interactive)
  (let ((indent-stack (list nil)) (next-depth 0) bol
	outer-loop-done inner-loop-done state this-indent)
    (save-excursion (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (not outer-loop-done)
	(setq last-depth next-depth
	      innerloop-done nil)
	(while (and (not innerloop-done)
		    (not (setq outer-loop-done (eobp))))
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq next-depth (car state))
	  (if (car (nthcdr 4 state))
	      (progn (indent-for-comment)
		     (end-of-line)
		     (setcar (nthcdr 4 state) nil)))
	  (if (car (nthcdr 3 state))
	      (progn
		(forward-line 1)
		(setcar (nthcdr 5 state) nil))
	    (setq innerloop-done t)))
	(if (setq outer-loop-done (<= next-depth 0))
	    nil
	  (while (> last-depth next-depth)
	    (setq indent-stack (cdr indent-stack)
		  last-depth (1- last-depth)))
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  last-depth (1+ last-depth)))
	  (forward-line 1)
	  (setq bol (point))
	  (skip-chars-forward " \t")
	  (if (or (eobp) (looking-at "[;\n]"))
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		(setq this-indent (car indent-stack))
	      (let ((val (guess-rebol-indent
			  (if (car indent-stack) (- (car indent-stack))))))
		(if (integerp val)
		    (setcar indent-stack
			    (setq this-indent val))
		  (if (cdr val)
		      (setcar indent-stack (- (car (cdr val)))))
		  (setq this-indent (car val)))))
	    (if (/= (current-column) this-indent)
		(progn (delete-region bol (point))
		       (indent-to this-indent)))))))))

(provide 'rebol)


(defconst rebol-natives (regexp-opt '("alias" "all" "any" "arccosine" "arcsine" "arctangent" "bind" "break" "browse" "caret-to-offset" "catch" "checksum" "close" "comment" "compose" "compress" "connected?" "cosine" "debase" "decompress" "dehex" "detab" "difference" "disarm" "do" "either" "else" "enbase" "entab" "exclude" "exit" "exp" "foreach" "form" "free" "get" "halt" "hide" "if" "in" "input?" "intersect" "launch" "load" "log-10" "log-2" "log-e" "loop" "lowercase" "mold" "not" "now" "offset-to-caret" "open" "parse" "prin" "print" "protect" "q" "query" "quit" "read" "read-io" "recycle" "reduce" "repeat" "return" "reverse" "save" "script?" "secure" "set" "show" "sine" "size-text" "square-root" "tangent" "textinfo" "throw" "to-hex" "trace" "try" "type?" "union" "unprotect" "unset" "until" "update" "uppercase" "use" "value?" "wait" "while" "write" "write-io")))

(defconst rebol-functions (regexp-opt '("abort-launch" "about" "alter" "append" "array" "ask" "build-tag" "center-face" "change-dir" "charset" "choose" "clean-path" "clear-fields" "confine" "confirm" "context" "cvs-date" "cvs-version" "decode-cgi" "deflag-face" "delete" "demo" "dir?" "dirize" "dispatch" "do-boot" "do-events" "do-face" "do-face-alt" "does" "dump-face" "dump-pane" "echo" "edit-text" "exists-via?" "exists?" "feedback" "find-key-face" "find-window" "flag-face" "flag-face?" "focus" "for" "forall" "forever" "form-local-file" "forskip" "found?" "func" "function" "get-net-info" "get-style" "help" "hide-popup" "import-email" "info?" "inform" "input" "insert-event-func" "join" "launch-safe" "layout" "license" "list-dir" "load-image" "load-thru" "make-dir" "make-face" "modified?" "net-error" "offset?" "parse-email-addrs" "parse-header" "parse-header-date" "parse-xml" "probe" "protect-system" "read-net" "read-thru" "read-via" "reform" "rejoin" "remold" "remove-event-func" "rename" "repend" "replace" "request" "request-color" "request-date" "request-download" "request-file" "request-list" "request-pass" "resend" "save-user" "screen-offset?" "scroll-para" "send" "send-text" "set-font" "set-net" "set-para" "set-style" "set-user-name" "show-popup" "size?" "source" "span?" "split-path" "start-view" "styliz" "stylize" "switch" "throw-on-error" "unfocus" "unique" "unview" "upgrade" "Usage" "view" "what" "what-dir" "win-offset?" "within?" "write-user")))

(defconst rebol-ops (regexp-opt '("and" "or" "xor")))

(defconst rebol-actions (regexp-opt '("abs" "absolute" "action?" "add" "and~" "any-block?" "any-function?" "any-string?" "any-type?" "any-word?" "at" "back" "change" "clear" "complement" "copy" "cp" "divide" "empty?" "equal?" "error?" "even?" "fifth" "find" "first" "fourth" "function?" "greater-or-equal?" "greater?" "head" "head?" "index?" "insert" "last" "length?" "lesser-or-equal?" "lesser?" "library?" "make" "max" "maximum" "min" "minimum" "multiply" "native?" "negate" "negative?" "next" "not-equal?" "number?" "object?" "odd?" "op?" "or~" "pick" "poke" "port?" "positive?" "power" "random" "remainder" "remove" "routine?" "same?" "second" "select" "series?" "skip" "sort" "strict-equal?" "strict-not-equal?" "struct?" "subtract" "tail" "tail?" "third" "to" "trim" "unset?" "xor~" "zero?")))

(defconst rebol-types1 (regexp-opt '("binary" "bitset" "block" "char" "date" "decimal" "email" "event" "file" "get-word" "hash" "image" "integer" "issue" "list" "lit-path" "lit-word" "logic" "money" "none" "pair" "paren" "path" "refinement" "set-path" "set-word" "string" "tag" "time" "tuple" "url" "word")))

(defconst rebol-types2 (regexp-opt '("action" "any-block" "any-function" "any-string" "any-type" "any-word" "datatype" "error" "function" "library" "native" "number" "object" "op" "port" "routine" "series" "struct" "symbol" "unset")))

(defconst rebol-refinement-end "\\)\\(/[0-9a-zA-Z]+\\)*\\)[^-_/0-9a-zA-Z]")

(defvar rebol-font-lock-keywords
  (list
   (list (concat "[^-_/]\\<\\(\\(" rebol-natives rebol-refinement-end) '1 'font-lock-keyword-face) ; native
   (list (concat "[^-_/]\\<\\(\\(" rebol-functions rebol-refinement-end) '1 'font-lock-keyword-face) ; function
   (list (concat "[^-_/]\\<\\(\\(" rebol-ops rebol-refinement-end) '1 'font-lock-doc-string-face) ; op
   (list (concat "[^-_/]\\<\\(\\(" rebol-actions rebol-refinement-end) '1 'font-lock-type-face) ; action
   (list (concat "\\<\\(to-\\(" rebol-types1 "\\)\\)") '1 'font-lock-keyword-face) ; to-type
   (list (concat "\\(\\(" rebol-types1 "\\|" rebol-types2 "\\)\\(!\\|\\?\\)\\)") '1 'font-lock-type-face) ; type? or type! 
   '("\\([^][ \t\r\n{}()]+\\):"  1 font-lock-function-name-face) ; define variable
   '("\\([^][ \t\r\n{}()]+\\):[ ]*\\(does\\|func\\(tion\\)?\\)" (1 'underline prepend) (2 font-lock-keyword-face)) ; define function
   '("\\(:\\|'\\)\\([^][ \t\r\n{}()]+\\)"  2 font-lock-variable-name-face) ; value or quoted
   '("\\(:?[0-9---]+:[:.,0-9]+\\)" 1 font-lock-preprocessor-face t) ; time
   '("\\([0-9]+\\(-\\|/\\)[0-9a-zA-Z]+\\2[0-9]+\\)" 1 font-lock-preprocessor-face t) ; date
   '("\\($[0-9]+\\(\\.\\|,\\)[0-9][0-9]\\)" 1 font-lock-preprocessor-face t) ; money
   '("\\([0-9]+\\.[0-9]+\\.\\([0-9]+\\(\\.[0-9]+?\\)?\\)?\\)" 1 font-lock-preprocessor-face t) ; tuple
   '("\\([0-9a-z]+@\\([0-9a-z]+\\.\\)+[a-z]+\\)" 1 font-lock-preprocessor-face t) ; email
   '("\\(http\\|ftp\\|mailto\\|file\\):[^ \n\r]+" 1 font-lock-preprocessor-face t) ; URL
   '("\\(%[^ \n\r]+\\)" 1 font-lock-preprocessor-face) ; file name
   '("\\(#\\([0-9a-zA-Z]+\\-\\)*[0-9a-zA-Z]+\\)" 1 font-lock-preprocessor-face t) ; issue

   ;; comment out these two (long string, binary) if you think it runs too slow
   '("[^#]\\({[^{}]*}\\)" 1 font-lock-string-face t) ; long string
   '("\\(\\(2\\|64\\)?#{[0-9a-zA-Z]+}\\)" 1 font-lock-preprocessor-face t) ; binary
   )
  "Additional expressions to highlight in REBOL mode.")


(defvar rebol-mode-syntax-table nil 
  "Syntax table for REBOL buffers.")

(if (not rebol-mode-syntax-table)
    (let ((i 0))
      (setq rebol-mode-syntax-table (make-syntax-table))
      (set-syntax-table rebol-mode-syntax-table)

      ;; Default is `word' constituent.
      (while (< i 256)
        (modify-syntax-entry i "_   ")
        (setq i (1+ i)))

      ;; Digits are word components.
      (setq i ?0)
      (while (<= i ?9)
        (modify-syntax-entry i "w   ")
        (setq i (1+ i)))

      ;; As are upper and lower case.
      (setq i ?A)
      (while (<= i ?Z)
        (modify-syntax-entry i "w   ")
        (setq i (1+ i)))
      (setq i ?a)
      (while (<= i ?z)
        (modify-syntax-entry i "w   ")
        (setq i (1+ i)))

      ;; Whitespace
      (modify-syntax-entry ?\t "    ")
      (modify-syntax-entry ?\n ">   ")
      (modify-syntax-entry ?\f "    ")
      (modify-syntax-entry ?\r "    ")
      (modify-syntax-entry ?  "    ")

      ;; Delimiters
      (modify-syntax-entry ?[ "(]  ")
      (modify-syntax-entry ?] ")[  ")
      (modify-syntax-entry ?\( "()  ")
      (modify-syntax-entry ?\) ")(  ")

      ;; comments
      (modify-syntax-entry ?\; "<   ")
      (modify-syntax-entry ?\" "\"    ")
      (modify-syntax-entry ?{ "    ")
      (modify-syntax-entry ?} "    ")
      (modify-syntax-entry ?' "  p")
      (modify-syntax-entry ?` "  p")

      (modify-syntax-entry ?^ "\\   ")))

(defvar rebol-mode-abbrev-table nil 
  "*Abbrev table for rebol-mode buffers")

(define-abbrev-table 'rebol-mode-abbrev-table ())

(defun rebol-mode-variables ()
  (set-syntax-table rebol-mode-syntax-table)
  (setq local-abbrev-table rebol-mode-abbrev-table)

  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'rebol-indent-line)
  (make-local-variable 'parse-expr-ignore-comments)
  (setq parse-expr-ignore-comments t)
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'rebol-comment-indent)
  (make-local-variable 'parse-expr-ignore-comments)
  (setq parse-expr-ignore-comments t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(rebol-font-lock-keywords nil nil))
  (make-local-variable 'mode-line-process)
  (setq mode-line-process '("" rebol-mode-line-process)))

(defvar rebol-mode-line-process "")

(defun rebol-mode-commands (map)
  (define-key map "\e\C-a" 'beginning-of-rebol-definition)
  (define-key map "\t" 'rebol-indent-line)
  (define-key map "\e\C-q" 'rebol-indent-expr))

(defvar rebol-mode-map nil)
(if (not rebol-mode-map)
    (progn
      (setq rebol-mode-map (make-sparse-keymap))
      (rebol-mode-commands rebol-mode-map)))


