;;; ob-julia --- Org Mode babel support for Julia, using ESS

;; Copyright © 2020, 2021 Nicolò Balzarotti
;; SPDX-License-Identifier: GPL-3.0+

;; Author: Nicolò Balzarotti
;; Version: 1.0.0
;; Keywords: languages
;; URL: https://github.com/nico202/ob-julia
;; Package-Requires: ((ess "20201004.1522") (julia-mode "0.4"))

;; This file is *not* part of GNU Emacs.

;;; Commentary:
;; This package adds Julia support to Org Mode src block evaluation
;;; Code:

;; Required packages:
(require 'ob)
(require 'ess)
(require 'ess-julia)

;; For external eval, we do not rely on ESS:
(defcustom org-babel-julia-external-command "julia"
  "Command to use for executing Julia code."
  :group 'org-babel
  :package-version '(ob-julia . "1.0.0")
  :version "24.1"
  :type 'string)

(defcustom org-babel-julia-command-arguments nil
  "Arguments to apply to `org-babel-julia-external-command'."
  :group 'org-babel
  :package-version '(ob-julia . "1.0.0")
  :version "24.1"
  :type 'string)

(defcustom ob-julia-startup-script
  (expand-file-name "julia/init.jl" (file-name-directory (or load-file-name buffer-file-name)))
  "Julia file path to run at startup.  Must be absolute."
  :group 'org-babel
  :package-version '(ob-julia . "1.0.0")
  :version "24.1"
  :type 'string)

(defcustom ob-julia-default-session-name "julia"
  "Default name given to ob-julia sessions.  Will be earmuffed
automatically."
  :group 'org-babel
  :package-version '(ob-julia . "1.0.0")
  :version "24.1"
  :type 'string)

(defcustom ob-julia-insert-latex-environment-advice t
  "When non-nil, add advice on loading to make latex environment results raw.
`org-babel-insert-result' will be advised by `ob-julia-latexify-make-raw'.")

(defconst org-babel-header-args:julia
  '((width		 . :any)
    (height		 . :any)
    (size		 . :any)
    (let		 . :any)
    (async		 . :any)
    (results		 . ((file matrix table list verbatim)
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
  "Takes the arguments in PARAMS that needs to be processed by
Julia, and put them in a NamedTuple() that will be passed to Julia."
  (defun param->julia (param &optional julia-name)
    "Takes the org parm named PARAM from params and return an
equivalent Julia assignment.  The value will be assigned to
JULIA_NAME when not nil.  Elisp types are converted to Julia
equivalents."
    (let* ((name (symbol-name param))
           (name (or julia-name
                     (substring name 1 (length name))))
           (val (alist-get param params))
           (val (if val
                    (format "%S" val)
                  "nothing")))
      (format "%s=%s" (subst-char-in-string ?- ?_ name) val)))
  ;; Create a named tuple (the comma is required to make it a tuple
  ;; if only one element is present)
  (format "(%s,)"
          (mapconcat 'concat
                     (list
                      (param->julia :dir)
                      (param->julia :results)
                      ;; Optional arguments.  We pass them all and let
                      ;; julia decide what to do
                      (param->julia :latexify)
                      (param->julia :size)
                      (param->julia :width)
                      (param->julia :height)
                      (param->julia :output-dir)
                      (param->julia :file-ext))
                     ", ")))

(defvar org-babel-julia--async-map '()
  "Association list between async block uuids and its requried info (evaluation params, buffer).")

(defun org-babel-julia-prepare-format-call (src-file out-file params &optional uuid)
  "Format a call to OrgBabelEval

OrgBabelEval is the entry point of the Julia code defined in
the startup script."
  (format
   "ObJulia.OrgBabelEval(%S,%S,%S,%s);"
   src-file out-file (org-babel-julia-params->named-tuple params)
   (or (when uuid (format "%S" uuid)) "nothing")))

(defun ob-julia--get-create-trace-buffer ()
  (get-buffer-create "*ob-julia-stacktrace*"))

(defun ob-julia--make-trace-buffer (&optional do-not-pop)
  (let ((buf (ob-julia--get-create-trace-buffer)))
    (with-current-buffer buf
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (unless do-not-pop
      (pop-to-buffer buf))
    buf))

(defun org-babel-julia--async-get-remove (uuid)
  "Get UUID from the list of async processes, remove it from
  the list and return its value."
  (let ((el (assoc uuid org-babel-julia--async-map)))
    (setq org-babel-julia--async-map (delq el org-babel-julia--async-map))
    el))

(defun org-babel-julia--async-add (uuid properties)
  "Register the async background block, identified by UUID with
properties PROPERTIES."
  (setq org-babel-julia--async-map
        (cons `(,uuid . ,properties) org-babel-julia--async-map)))

(defun ob-julia--trace-file (output-file)
  (concat output-file ".trace"))
(defun org-julia-async-process-filter-ess (process output)
  "A function that is called when new output is available on the
  Julia buffer, which waits until the async execution is
  completed.  Replace julia-async: tags with async results.
  This version is specific to ESS."
  (org-julia-async-process-filter process output t))

(defun ob-julia--has-stacktrace (output-file)
  (file-exists-p (ob-julia--trace-file output-file)))

(defun ob-julia-create-stacktrace-buffer (stacktrace-file &optional do-not-pop)
  "Display the stacktrace in a new buffer"
  (let ((buf (ob-julia--make-trace-buffer do-not-pop)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (insert-file-contents stacktrace-file)))))

(defun ob-julia-dispatch-output-type (params output-file &optional async)
  ;; First, we have the special case in which the output is a
  ;; stacktrace.  If there's one, open it in a buffer, then continue
  ;; showing the results.
  (when (ob-julia--has-stacktrace output-file)
    ;; TODO: jump to the corresponding src line?
    (ob-julia-create-stacktrace-buffer
     (ob-julia--trace-file output-file) (when async -1)))
  (org-babel-julia-process-results params output-file))

(defun org-julia-async-process-filter (process output &optional fallback)
  "A function that is called when new output is available on the
  Julia buffer, which waits until the async execution is
  completed.  Replace julia-async: tags with async results."
  ;; Wait until ob_julia_async_UUID is printed
  (if (string-match ".*ob_julia_async_\\([0-9a-z\\-]+\\).*" output)
      ;; Recover the uuid from the julia output
      (let* ((uuid (match-string-no-properties 1 output))
             ;; get properties about the block that started the async process
             (properties (org-babel-julia--async-get-remove uuid))
             (vals (cdr properties))
             (params (elt vals 0))
             (output-file (elt vals 1))
             (org-buffer (elt vals 2)))
        ;; In the meanwhile, the user can change point and buffer, we
        ;; need to jump to where we started without disturbing.
        (save-window-excursion
          (switch-to-buffer org-buffer)
          (save-excursion
            (save-restriction
      	      ;; If it's narrowed, substitution would fail
      	      (widen)
      	      ;; search the matching src block
      	      (goto-char (point-max))
      	      (when (search-backward (concat "julia-async:" uuid) nil t)
      	        ;; remove results
      	        (search-backward "#+end_src")
      	        ;; insert new one
      	        (org-babel-insert-result
                 (ob-julia-dispatch-output-type params output-file t)
                 (alist-get :result-params params) nil nil "julia")
                ;; Pop up the stacktrace buffer if needed
                (when (ob-julia--has-stacktrace output-file)
                  (pop-to-buffer (ob-julia--get-create-trace-buffer)))))))
        (when fallback
          (inferior-ess-output-filter process "\n")))
    (when fallback
      (inferior-ess-output-filter process output))))

(defun org-babel-julia-evaluate-external-process:async (cmd uuid properties)
  "Run CMD in a separate process.  The output buffer will be
*ob-julia-async-process*, with an async filter registered on it.
The block PROPERTIES will be stored with uuid UUID."
  (make-process :name "*ob-julia-async-process*"
        	:filter #'org-julia-async-process-filter
        	:command cmd)
  (org-babel-julia--async-add uuid properties))

(defun org-babel-julia-evaluate-external-process:sync (cmd buf)
  "Evaluate CMD synchronously, storing stderr in BUF."
  ;; We use the same cmd for both make-process and shell-command, here
  ;; we escape the parts.
  (shell-command (mapconcat (lambda (c) (format "%S" c)) cmd " ") nil buf))

(defun org-babel-julia-evaluate-external-process
    (org-babel-eval-call async params output-file org-buffer)
  "Evaluate ORG_BABEL_EVAL_CALL in an external Julia process.
If the shell-command returns an error, show it in a stacktrace buffer.
Depending on ASYNC the appropriate evaluation is choosen.
ORG_BUFFER stores the provenance of the execution (required for
async evaluation)."
  ;; We write shell-command output to a trace-buffer so that we are
  ;; able to capture internal ob-julia errors
  (let ((buf (ob-julia--make-trace-buffer -1))
        (cmd
         `(,org-babel-julia-external-command
           ,@org-babel-julia-command-arguments
           "--load" ,ob-julia-startup-script
           "--eval" ,org-babel-eval-call)))
    (if async
        (org-babel-julia-evaluate-external-process:async
         cmd async (list params output-file org-buffer))
      (with-current-buffer buf
        (read-only-mode -1)
        (insert "If you can read me, there might be a bug in ob-julia!
Please submit a bug report!")
        (let ((ret (org-babel-julia-evaluate-external-process:sync cmd buf)))
          ;; Display the stack trace buffer only if we need to
          (if (= ret 1)
              (pop-to-buffer buf)
            (erase-buffer))
          (read-only-mode 1))))
    (if async
        (concat "julia-async:" async)
      (ob-julia-dispatch-output-type params output-file))))

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
  If EXTENSION is not nil, use it as file extension."
  (or file
      (org-babel-process-file-name
       (org-babel-temp-file
        "julia-" (if extension (concat "." extension) nil)))))

(defun org-babel-julia-process-value-result (results type)
  "Insert hline if needed (combining info from RESULT and TYPE."
  ;; add an hline if the result seems to be a table
  ;; always obay explicit type
  (if (eq type 'table)
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
     ((member "verbatim" results) 'verbatim)
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
    (unless file
      (with-temp-buffer
        (condition-case err
            (progn
              (insert-file-contents output-file)
              (delete-file output-file)
              (let* ((content (split-string
                               (buffer-substring-no-properties
                                (point-min) (point-max)) "\n"))
                     (suggested-type (intern (car content)))
                     (result (mapconcat 'concat (cdr content) "\n"))
                     ;; Either enforce the result-type requested by the
                     ;; user, or use the one provided by julia if 'auto
                     (result-type (if (eq result-type 'auto)
                                      suggested-type
                                    result-type)))
                ;; Dispatch processing of result based on result-type
                (pcase result-type
                  ;; ('table
                  ;;  ;; Add hline
                  ;;  (let ((res (car (read-from-string result))))
                  ;;    `(,(car res) hline ,(cdr res))))
                  ('table (car (read-from-string result)))
                  ('matrix (car (read-from-string result)))
                  ('list (car (read-from-string result)))
                  ('verbatim result)
                  ('raw result)
                  (_ result))))
          (error
           (display-warning 'org-babel
        		    (format "Error reading results: %S" err)
        		    :error)
           nil))))))

(defcustom ob-julia-latexify-star-environments t
  "Insert an asterisk in the environment name of generated LaTeX.
By default, latexify's outermost environment is usually unstarred, e.g.
\\begin{environment}. When set, this will insert an asterisk at the end of
the environment name, i.e. \\begin{environment*}."
  :type 'boolean
  :group 'org-babel)

(defun ob-julia-latexify-make-raw (orig-fn result &optional result-params info hash lang)
  (unless (and (stringp result)
               (string= lang "julia")
               (let ((latex-env (string-match-p "\\`\\\\begin" result))
                     (latex-disp (string-match-p "\\`\\$\\$[^\u0000]+\\$\\$\\'" result))
                     (latex-inline (string-match-p "\\`\\$[^\u0000]+\\$\\'" result)))
                 (when (or latex-env latex-disp latex-inline)
                   (if (or latex-disp latex-inline)
                       (setf result (replace-regexp-in-string "\\`\\$\\$?\\([^\u0000]+\\)\\$\\$?\\'"
                                                              "\\\\begin{equation*}\n\\1\n\\\\end{equation*}"
                                                              result))
                     (when ob-julia-latexify-star-environments
                       (setf result (replace-regexp-in-string
                                     "\\`\\\\begin{\\([a-z]+\\)}\\([^\u0000]+\\)\\\\end{[a-z]+}\n\\'"
                                     "\\\\begin{\\1*}\\2\\\\end{\\1*}\n"
                                     result))))
                   (funcall orig-fn result (append result-params '("raw")) info hash lang)
                   (when (bound-and-true-p org-fragtog-mode)
                     (save-excursion
                       (goto-char (org-babel-where-is-src-block-result nil info))
                       (forward-line 1)
                       (org-latex-preview)))
                   t)))
    (funcall orig-fn result result-params info hash lang)))

(when ob-julia-insert-latex-environment-advice
  (advice-add 'org-babel-insert-result :around #'ob-julia-latexify-make-raw))

(defun org-babel-julia-assign-to-var (name value)
  "Assign VALUE to a variable called NAME."
  (format "%s = %S" name value))

(defun org-babel-julia-assign-to-array (name matrix)
  "Create a Julia Matrix (Vector{Any ,2}) from MATRIX and assign
it to NAME."
  (format "%s = [%s]" name
	  (mapconcat (lambda (line)
                       (mapconcat (lambda (e)
				    (format "%S" e))
				  line " "))
                     matrix ";")))

(defun org-babel-julia-assign-to-var-or-array (var)
  "Assign an org variable as a Julia variable or array."
  (if (listp (cdr var))
      (org-babel-julia-assign-to-array (car var) (cdr var))
    (org-babel-julia-assign-to-var (car var) (cdr var))))

(defun org-babel-julia-assign-to-dict (name column-names values)
  "Create a Dict with lists as values.
Create a Dict where keys are Symbol from COLUMN-NAMES,
values are Array taken from VALUES, and assign it to NAME"
  (format "%s = Dict(%s)" name
	  (mapconcat
	   (lambda (i)
	     (format "Symbol(\"%s\") => [%s]" (nth i column-names)
		     (mapconcat
		      (lambda (line) (format "%S" (nth i line)))
		      values
		      ",")))
	   (number-sequence 0 (1- (length column-names)))
	   ",")))

(defun org-babel-julia-assign-to-named-tuple (name column-names values)
  "Create a NamedTuple using (; zip([], [])...)"
  (format "%s = [%s]" name
	  (mapconcat
	   (lambda (i)
	     (concat
	      "(; zip(["
	      (mapconcat
	       (lambda (col) (format "Symbol(\"%s\")" col))
	       column-names ", ")
	      "],["
	      (mapconcat
	       (lambda (cell) (format "\"%s\"" cell))
	       (nth i values)
	       ",")
	      "])...)"))
	   (number-sequence 0 (1- (length values))) ", ")))

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

(defun org-babel-julia-async-p (params)
  "Return t if :async is in params and its value is not \"no\"."
  (let ((async (assoc :async params)))
    (and async (not (string-equal (cdr async) "no")))))

(defun org-babel-julia-get-session-name (params)
  "Extract the session name from the PARAMS.

If session should not be used, return nil.

 session can be:
 - (:session) :: param passed, empty, use default
 - (:session name) :: param passed, with a name, use it
 - (:session none) :: param not passed, do not use the session"
  (defun maybe-earmuff-session-name (name &optional id)
    (concat (if (string= "*" (substring name 0 1)) "" "*")
            ;; FIXME
            name (if id (format ":%s" id) "")
            (if (string= "*" (substring name (- (length name) 1))) "" "*")))
  (let* ((session (alist-get :session params))
         (name (cond
                ((null session) ob-julia-default-session-name)
                ((string-equal session "none") nil)
                (t session))))
    (when name
      (maybe-earmuff-session-name name))))

(defun org-babel-prep-session:julia (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let ((dir (or (alist-get :dir params)
		 (inferior-ess--maybe-prompt-startup-directory session "julia")))
        ;; We manyally ask for starting directory, don't ask twice
        (ess-ask-for-ess-directory nil))
    (save-window-excursion
      (let* ((start-script-arg
              (concat
               (mapconcat #'identity org-babel-julia-command-arguments " ")
               (format " --load=%s" ob-julia-startup-script)))
             (inferior-julia-args (if inferior-julia-args
                                      (concat inferior-julia-args " " start-script-arg)
                                    start-script-arg)))
        (switch-to-buffer (run-ess-julia)))
      (rename-buffer
       (if (bufferp session)
           (buffer-name session)
         (if (stringp session)
             session
           (buffer-name))))
      ;; Register the async callback. Important to do this before
      ;; running any command
      (set-process-filter
       (get-buffer-process
        (org-babel-comint-buffer-livep session))
       'org-julia-async-process-filter-ess))))

(defun org-babel-julia-evaluate-in-session:sync (session body block output)
  "Run FILE, in session SESSION, synchronously."
  (org-babel-comint-eval-invisibly-and-wait-for-file
   session output body 0.1)
  (with-current-buffer session
    (comint-add-to-input-history block))
  output)

(defun org-babel-julia-evaluate-in-session:async
    (session uuid body block output properties)
  "Run FILE, in session SESSION, synchronously."
  (process-send-string session (concat body "\n"))
  (with-current-buffer session
    (comint-add-to-input-history block))
  (org-babel-julia--async-add uuid properties)
  (concat "julia-async:" uuid))

(defun org-babel-julia-evaluate-in-session
    (session block OrgBabelEval-call uuid params output-file org-buffer)
  "Evaluate BLOCK in session SESSION, starting it if necessary.
If UUID is provided, run the block asynchronously."
  ;; If the session does not exists, start it
  (when (not (org-babel-comint-buffer-livep session))
    (org-babel-prep-session:julia session params))
  (if uuid
      (org-babel-julia-evaluate-in-session:async
       session uuid OrgBabelEval-call block output-file
       (list params output-file org-buffer))
    (ob-julia-dispatch-output-type
     params
     (org-babel-julia-evaluate-in-session:sync
      session OrgBabelEval-call block output-file))))

;; Main entry point when code is evaluated in an Org Mode buffer
(defun org-babel-execute:julia (block params)
  "Execute a block of julia code.
This function is called by `org-babel-execute-src-block'.
BLOCK is the content of the src block
PARAMS are the parameter passed to the block"
  ;; Save excursion as we might open new buffers (e.g. stacktrace)
  ;; TODO: if the block already has a julia-async link, it would be
  ;; nice to interrupt it and start the new one.
  (save-excursion
    (let* ((org-buffer (current-buffer))
           (session (org-babel-julia-get-session-name params))
           (body (org-babel-expand-body:julia block params))
           (src-file (make-temp-file "ob-julia-" nil ".jl" body))
           (out-format (or (org-babel-julia-parse-result-format params)
                           (bound-and-true-p org-export-current-backend)))
           (uuid (and (org-babel-julia-async-p params) (org-id-uuid)))
           (output-file
            (org-babel-julia-output-file (alist-get :file params) out-format))
           (OrgBabelEval-call
            (org-babel-julia-prepare-format-call
             src-file output-file params uuid)))
      (if session
          (org-babel-julia-evaluate-in-session
           session block
           OrgBabelEval-call uuid params output-file org-buffer)
        (org-babel-julia-evaluate-external-process
         OrgBabelEval-call uuid params output-file org-buffer)))))

(provide 'ob-julia)
;;; ob-julia.el ends here
