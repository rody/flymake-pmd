;;; flymake-pmd.el --- A PMD checker for Flymake -*- lexical-binding: t; -*-


;; Author: Rodolphe Blancho <rodolphe.blancho@gmail.com>
;; Version: 0.0.1
;; Keywords: flymake, pmd, tools

;; Copyright (C) 2022 Rodolphe Blancho
;; License: MIT

;;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Provides a PMD checker for Flymake.

;;; Code:

;;;; Requirements

(require 'flymake)

;;; Customization

(defgroup flymake-pmd nil
  "PMD support for flymake."
  :prefix "flymake-pmd-"
  :group 'flymake)

(defcustom flymake-pmd-ruleset-filename-list '("pmd.xml" "pmd-ruleset.xml" "ruleset.xml")
  "Names of PMD ruleset files to search for."
  :type '(repeat string)
  :group 'flymake-pmd)

(defcustom flymake-pmd-executable-name "pmd"
  "The PMD executable name.
If the command is not present in variable `exec-path', the full
path to the executable is required."
  :type 'string
  :group 'flymake-pmd)

(defcustom flymake-pmd-use-eglot nil
  "Use PMD checker alongside eglot.
When Eglot integrates with Flymake, it replaces all
the checkers with the eglot checker. Setting this variable
to `t` adds a hook to eglot-managed-hook to ensure that the
PMD checker is set in Flymake."
  :type 'boolean
  :group 'flymake-pmd)

(defcustom flymake-pmd-use-pmd-6 nil
  "Use pmd 6 cli format."
  :type 'boolean
  :group 'flymake-pmd)

(defcustom flymake-pmd-pmd-6-app-name nil
  "App name when using run.sh.
When using PMD 6 run.sh script, an app name needs to
be specified (usually \"pmd\"). If this variable is not nil
it will be used as the app name. This is usually not needed
on windows as pmd provides a .bat script for each app."
  :type 'string
  :group 'flymake-pmd)

;;;; Private

(defvar-local flymake-pmd--process nil)

(defun flymake-pmd--find-ruleset-file ()
  "Search for a PMD ruleset file and return it.

The function is searching for any file mathing a name
in `flymake-pmd-ruleset-filename-list' in the current
buffer directory or its parents."

  (let ((filenames flymake-pmd-ruleset-filename-list)
	(location (buffer-file-name (current-buffer)))
	(found nil)
	(current-filename nil)
	(result nil))
    (while filenames
      (setq current-filename (car filenames))
      (setq found (locate-dominating-file location current-filename))
      (if found
	  (setq result (expand-file-name (concat (file-name-as-directory found) current-filename)))
	)
      (setq filenames (cdr filenames))
      )
    result))


(defun flymake-pmd--create-process (source-buffer callback)
  "Create linter for SOURCE-BUFFER.
CALLBACK is invoked once linter has finished the execution.
CALLBACK accepts a buffer containing stdout from linter as its
argument."

  ;; stop if we can't find a ruleset file
  (unless (flymake-pmd--find-ruleset-file) (error "Cannot find a ruleset file"))

  ;; if a live process launched in a earlier check was found, kill that process
  (when (process-live-p flymake-pmd--process)
    (kill-process flymake-pmd--process))

  ;; Save current buffer, remove any narrowing r
  (let* (;; PMD relies on file extension to automatically select the language
	 ;; so we make sure that the temporary buffer has the same extension
	 ;; as the original file.
         (extension (file-name-extension (buffer-file-name source-buffer)))
	 (tmp-file (make-temp-file "pmd-input" nil (format ".%s" extension)))
	 (ruleset-file (flymake-pmd--find-ruleset-file)))

    ;; save current buffer to temp file
    (write-region nil nil tmp-file nil -1)
    (save-restriction
      (widen)
      ;; reset the `pmd--flymake-proc' process to a new process
      (flymake-log :warning "starting process")
      (setq flymake-pmd--process
	    (make-process :name "flymake-pmd"
			  :noquery t
			  :connection-type 'pipe
			  ;; make output go to a temporary buffer
			  :buffer (generate-new-buffer "*flymake-pmd*")
			  :stderr (get-buffer-create "*flymake-pmd stderr*")
                          :command (flymake-pmd--command ruleset-file tmp-file)
			  :sentinel
			  (lambda (proc _ignore)
			    ;; Check that the process has indeed exited, as it might
			    ;; be simply suspended.
			    ;;
			    (when (memq (process-status proc) '(exit signal))
				  ;; Only proceed if `proc' is the same as
				  ;; `pmd--flymake-proc', which indicates that
				  ;; `proc' is not an obsolete process.
				  ;;
                                  (when (eq proc flymake-pmd--process)
                                    (let ((proc-buffer (process-buffer proc)))
                                      (funcall callback proc-buffer)
                                      (kill-buffer proc-buffer)
			              (delete-file tmp-file)))
			         )))))))

(defun flymake-pmd--command (ruleset-file tmp-file)
  "Return the command line as a list to execute PMD.
RULESET-FILE is used as the ruleset,
TMP-FILE is the temporary file containing the code to analyze."
  (if flymake-pmd-use-pmd-6
      `(,flymake-pmd-executable-name
        ,flymake-pmd-pmd-6-app-name
	"--format" "json"
	"--fail-on-violation" "false"
	"--no-cache"
	"--rulesets" ,ruleset-file
        "-d" ,tmp-file)
    `(,flymake-pmd-executable-name
      "check"
      "--format" "json"
      "--no-fail-on-violation"
      "--no-cache"
      "--no-progress"
      "--rulesets" ,ruleset-file
      "-d" ,tmp-file)))

(defun flymake-pmd--check-and-report (source-buffer report-fn)
  "Run PMD against SOURCE-BUFFER.
Use REPORT-FN to report results."

  (flymake-pmd--create-process
   source-buffer
   (lambda (pmd-stdout)
     (funcall report-fn (flymake-pmd--report pmd-stdout source-buffer)))))


(defun flymake-pmd--report (pmd-stdout-buffer source-buffer)
  "Create Flymake diag messages from the content of PMD-STDOUT-BUFFER.
They are reported against SOURCE-BUFFER. Returns a list of results."

  (with-current-buffer pmd-stdout-buffer
    ;; start at the top
    (goto-char (point-min))
    (flymake-pmd--create-diagnostics (json-parse-buffer) source-buffer)))






(defun flymake-pmd--processing-errors-diags (errors source-buffer)
  "FIXME."
  (if (eq errors nil)
      nil
    (mapcar (lambda (perror)
              (let ((description (gethash "message" perror))
                    (position (flymake-diag-region source-buffer 1)))
                (flymake-make-diagnostic source-buffer
                                         (car position)
                                         (cdr position)
                                         :error
                                         description)))
            errors)))

(defun flymake-pmd--rule-violations-diags (files source-buffer)
  "FIXME"
  (if (eq files nil)
      '()
    (let ((diags '()))
      (mapc (lambda (file)
              (mapc (lambda (violation)
                      (let* ((description (gethash "description" violation))
                            (begin-line (gethash "beginline" violation))
                            (begin-column (gethash "begincolumn" violation))
                            ;; (end-line (gethash "endkine" violation))
                            ;; (end-column (gethash "endcolumn" violation))
                            (priority (gethash "priority" violation))
                            (rule (gethash "rule" violation))
                            (position (flymake-diag-region source-buffer begin-line begin-column)))
                       (push (flymake-make-diagnostic source-buffer
                                                 (car position)
                                                 (cdr position)
                                                 :error ; FIXME set some threshold
                                                 (format "%s [%s] - %s" priority rule description))
                             diags)))
                    (gethash "violations" file)))
            files)
      diags
      )))
                

                    


(defun flymake-pmd--create-diagnostics (json source-buffer)
  "TODO"
  (append
   (flymake-pmd--processing-errors-diags (gethash "processingErrors" json) source-buffer)
   (flymake-pmd--rule-violations-diags (gethash "files" json) source-buffer))
  )
   
(defun flymake-pmd--checker (report-fn &rest _ignored)
  "Backend function for flymake.
REPORT-FN callback function to notify flymake of the errors.
See `flymake-diagnostic-functions' for more details."
  (flymake-pmd--check-and-report (current-buffer) report-fn))


(defun flymake-pmd-setup-backend ()
  "Setup function for `flymake-pmd'."
  (if flymake-pmd-use-eglot
      (add-hook 'eglot-managed-mode-hook
                (lambda () (add-hook 'flymake-diagnostic-functions 'flymake-pmd--checker nil t)))
    (add-hook 'flymake-diagnostic-functions 'flymake-pmd--checker nil t)))

;;;###autoload
(defun flymake-pmd-enable ()
  "Enable `flymake' and `flymake-pmd'.
Add this function to some apex/java/... major mode hook."
  (interactive)
  (flymake-mode t)
  (flymake-pmd-setup-backend))

(provide 'flymake-pmd)
;;; flymake-pmd.el ends here
