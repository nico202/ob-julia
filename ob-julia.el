;;; ob-julia --- Org Mode babel support for Julia

;; Copyright © 2020, 2021 Nicolò Balzarotti
;; SPDX-License-Identifier: GPL-3.0+

;; Author: Nicolò Balzarotti
;; Version: 1.0.0
;; Keywords: languages
;; URL: https://github.com/nico202/ob-julia
;; Package-Requires: ((julia-mode "0.4"))

;; This file is *not* part of GNU Emacs.

;;; Commentary:
;; This package adds Julia support to Org Mode src block evaluation
;;; Code:

;; Required packages:
(require 'ob)
;; For external eval, we do not rely on ESS:
(defcustom org-babel-julia-external-command "julia"
  "Command to use for executing Julia code."
  :group 'org-babel
  :package-version '(ob-julia . "1.0.0")
  :version "24.1"
  :type 'string)

(defcustom ob-julia-startup-script
  (concat (file-name-directory (or load-file-name (buffer-file-name)))
          "init.jl")
  "Julia file path to run at startup.  Must be absolute."
  :group 'org-babel
  :package-version '(ob-julia . "1.0.0")
  :version "24.1"
  :type 'string)

(defconst org-babel-header-args:julia
  '((width		 . :any)
    (height		 . :any)
    (size		 . :any)
    (let		 . :any)
    ;; (import		 . :any)
    ;; (using		 . :any)
    (async		 . :any)
    (results		 . ((file matrix table list)
			    (raw html latex org)
			    (replace silent none append prepend)
			    (output value))))
  "Julia-specific header arguments.")

;; Set default extension to tangle Julia code:
(add-to-list 'org-babel-tangle-lang-exts '("julia" . "jl"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Real code starts here ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-babel-julia-params->named-tuple (params)
  "Takes the arguments in `params' that needs to be processed by
Julia, and put them in a NamedTuple() that will be passed to Julia."
  (defun param->julia (param &optional julia-name)
    "Takes the org parm named `param' from params and return an
equivalent Julia assignment.  The value will be assigned to
`julia-name' when not nil.  Elisp types are converted to Julia
equivalents."
    (let* ((name (symbol-name param))
           (name (or julia-name
                     (substring name 1 (length name))))
           (val (alist-get param params))
           (val (if val
                    (format "%S" val)
                  "nothing")))
      (format "%s=%s" name val)))
  ;; Create a named tuple (the comma is required to make it a tuple
  ;; if only one element is present)
  (format "(%s,)"
          (mapconcat 'concat
                     (list
                      (param->julia :dir)
                      (param->julia :results))
                     ", ")))

(defun org-babel-julia-prepare-format-call (src-file out-file params)
  "Format a call to OrgBabelEval

OrgBabelEval is the entry point of the Julia code defined in
the startup script."
  (format
   "OrgBabelEval(%S,%S,%S);"
   src-file out-file (org-babel-julia-params->named-tuple params)))

(defun make-trace-buffer (&optional do-not-pop)
  (let ((buf (get-buffer-create "*ob-julia-stacktrace*")))
    (with-current-buffer buf
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (unless do-not-pop
      (pop-to-buffer buf))
    buf))

(defun org-babel-julia-evaluate-external-process (org-babel-eval-call)
  "Evaluate ORG_BABEL_EVAL_CALL in an external Julia process.
If the shell-command returns an error, show it in a stacktrace buffer."
  ;; We write shell-command output to a trace-buffer so that we are
  ;; able to capture internal ob-julia errors
  (let ((buf (make-trace-buffer -1)))
    (with-current-buffer buf
      (read-only-mode -1)
      (insert "If you can read me, there might be a bug in ob-julia!
Please submit a bug report!")
      (let ((ret (shell-command
                  (format "%s --load %s --eval '%s'" org-babel-julia-command
                          ob-julia-startup-script org-babel-eval-call)
                  nil buf)))
        ;; Display the stack trace buffer only if we need to
        (if (= ret 1)
            (pop-to-buffer buf)
          (erase-buffer))
        (read-only-mode 1)))))

(defun org-babel-expand-body:julia (body params)
  "Expand BODY according to PARAMS.  Return the expanded body, a
  string containing the julia we need to evaluate, possibly
  wrapped in a let block with variable assignmenetns."
  (let ((block (and (alist-get :let params) "let"))
        (vars (mapconcat
               'concat (org-babel-variable-assignments:julia params) ";")))
    (concat
     ;; no newline between vars and body
     ;; so that the stacktrace line is aligned
     block " " vars "; " body
     ";\n"
     (if block "end\n" ""))))

(defun org-babel-julia-output-file (file &optional extension)
  "Return the a path where Julia should store its results.
  The output file is either a temporary file, or the file
  name passed to the :file argument.  It might contain a
  non-existing path (when :output-dir is a non-existing
  directory).
  If `extension' is not nil, use it as file extension."
  (or file
      (org-babel-process-file-name
       (org-babel-temp-file
        "julia-" (if extension (concat "." extension) nil)))))


(defun org-babel-julia-process-value-result (results type)
  "Insert hline if needed (combining info from RESULT and TYPE."
  ;; add an hline if the result seems to be a table
  ;; always obay explicit type
  (if (or (eq type 'table)
	    (and (eq type 'auto)
	         (listp results)       ; a table must be a list
	         (listp (car results)) ; of lists
	         (stringp (caar results)))) ; with strings as first line
        (cons (car results) (cons 'hline (cdr results)))
      results))

(defun org-babel-julia-parse-result-type (params)
  "Decide how to parse results. Default is \"auto\"
(results can be anything. If \"table\", force parsing as a
table. To force a matrix, use matrix"
  (let* ((results (alist-get :results params))
	 (results (if (stringp results) (split-string results) nil)))
    (cond
     ((member "table" results) 'table)
     ((member "matrix" results) 'matrix)
     ((member "list" results) 'list)
     ((member "raw" results) 'raw)
     (t 'auto))))

(defun org-babel-julia-parse-result-format (params)
  (let* ((results (alist-get :results params))
	 (results (if (stringp results) (split-string results) nil)))
    (cond
     ((member "html" results) "html")
     ((member "latex" results) "tex")
     ((member "org" results) "org")
     ;; ((member "graphcis" results) "")
     (t "org"))))

(defun org-babel-julia-process-results (params output-file)
  "Decides what to insert as result.  If trace is true, add a drawer."
  (let ((result-type (org-babel-julia-parse-result-type params))
	(file (alist-get :file params))
	(res (alist-get :results params)))
    (unless file			; do not process files
      (org-babel-result-cond (if res (split-string res) nil)
        (with-temp-buffer
          (insert-file-contents output-file)
          (buffer-string))
        (org-babel-julia-process-value-result
         (org-babel-import-elisp-from-file output-file '(4))
         result-type)))))

(defun ob-julia-create-stacktrace-buffer (stacktrace-file)
  "Display the stacktrace in a new buffer"
  (let ((buf (make-trace-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (insert-file-contents stacktrace-file)))))

(defun ob-julia-dispatch-output-type (params output-file)
  (defun trace-file (output-file)
    (concat output-file ".trace"))
  (defun has-stacktrace (output-file)
    (file-exists-p (trace-file output-file)))
  ;; First, we have the special case in which the output is a
  ;; stacktrace.  If there's one, open it in a buffer, then continue
  ;; showing the results.
  (when (has-stacktrace output-file)
    ;; TODO: jump to the corresponding src line?
    (ob-julia-create-stacktrace-buffer (trace-file output-file)))
  (org-babel-julia-process-results params output-file))

(defun org-babel-variable-assignments:julia (params)
  "Return list of julia statements assigning the block's variables."
  (let ((vars (org-babel--get-vars params))
	(colnames (alist-get :colname-names params)))
    (mapcar
     (lambda (i)
       (let* ((var (nth i vars))
	      (column-names
	       (car (seq-filter
		     (lambda (cols)
		       (eq (car cols) (car var)))
		     colnames))))
	 (if column-names
	     (if t ;; org-babel-julia-table-as-dict
		 (org-babel-julia-assign-to-dict
		  (car var) (cdr column-names) (cdr var))
	       (org-babel-julia-assign-to-named-tuple
		(car var) (cdr column-names) (cdr var)))
	   (org-babel-julia-assign-to-var-or-array var))))
     (number-sequence 0 (1- (length vars))))))

;; Main entry point when code is evaluated in an Org Mode buffer
(defun org-babel-execute:julia (body params)
  "Execute a block of julia code.
This function is called by `org-babel-execute-src-block'.
BODY is the content of the src block
PARAMS are the parameter passed to the block"
  ;; Save excursion as we might open new buffers (e.g. stacktrace)
  (save-excursion
    (let* ((body (org-babel-expand-body:julia body params))
           (src-file (make-temp-file "ob-julia-" nil ".jl" body))
           (out-format (or (org-babel-julia-parse-result-format params)
                           (bound-and-true-p org-export-current-backend)))
           (output-file (org-babel-julia-output-file (alist-get :file params) out-format))
           (OrgBabelEval-call
            (org-babel-julia-prepare-format-call
             src-file output-file params)))
      (org-babel-julia-evaluate-external-process OrgBabelEval-call)
      (ob-julia-dispatch-output-type params output-file))))

(provide 'ob-julia)
;;; ob-julia.el ends here
