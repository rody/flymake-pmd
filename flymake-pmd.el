;;; flymake-pmd.el --- A PMD backend for Flymake -*- lexical-binding: t; -*-


;; Author: Rodolphe Blancho <rodolphe.blancho@gmail.com>
;; Version: 0.0.1
;; Keywords: flymake, pmd, tools

;; Copyright (C) 2022 Rodolphe Blancho

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Provides a Flymake backend using PMD.

;;; Code:

;;;; Requirements

(require 'flymake)

;;; Customization

(defgroup flymake-pmd nil
  "PMD support for flymake."
  :prefix "flymake-pmd-"
  :group 'flymake)

(defcustom flymake-pmd-ruleset-filename-list
  '("pmd.xml" "pmd-ruleset.xml" "ruleset.xml")

  "Names of PMD ruleset files to search for."
  :type '(repeat string)
  :group 'pmd-flymake)

(defcustom flymake-pmd-java-executable
  "java"

  "The java executable used to run PMD.

If the java command is not present in variable `exec-path', the full
path to the executable is required."
  :type 'string
  :group 'pmd-flymake)

(defcustom flymake-pmd-pmd-home-dir
  (getenv "PMD_HOME")

  "Directory where PMD was installed.

This value defaults to the PMD_HOME environment variable.
It is used to locate the jar files that PMD needs at runtime."
  :type 'string
  :group 'pmd-flymake)


;;;; Private

(defvar-local pmd--flymake-proc nil)

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

(defun flymake-pmd--ensure-java-exists ()
  "Check that the configure java executable exists.

Throw an error if no java executable is found."
  (unless (executable-find flymake-pmd-java-executable)
    (error "Cannot find Java. Check the value of `flymake-pmd-java-executable'"))
  )

(defun flymake-pmd--ensure-pmd-exists ()
  "Check that the PMD installation dir exists."
  (if (string-blank-p flymake-pmd-pmd-home-dir)
      (error "`flymake-pmd-pmd-home-dir is not set"))
  (unless (file-exists-p flymake-pmd-pmd-home-dir)
    (error "`flymake-pmd-pmd-home-dir is not set"))
  (unless (file-exists-p (flymake-pmd--pmd-lib-dir))
    (error "Could not find PMD lib dir in '%s'" flymake-pmd-pmd-home-dir)))

(defun flymake-pmd--pmd-lib-dir ()
  "Return PMD lib dir (derived from PMD home dir)."
  (file-name-as-directory
   (concat
    (file-name-as-directory
     (expand-file-name flymake-pmd-pmd-home-dir))
    "lib")))
  
(defun flymake-pmd--checker (report-fn &rest _args)
  "Backend function for flymake.

REPORT-FN callback function to notify flymake of the errors.

See `flymake-diagnostic-functions' for more details."
  ;; stop if we can't find Java
  (flymake-pmd--ensure-java-exists)

  ;; stop if we can't find PMD
  (flymake-pmd--ensure-pmd-exists)

  ;; stop if we can't find a ruleset file
  (unless (flymake-pmd--find-ruleset-file) (error "Cannot find a ruleset file"))

  ;; if a live process launched in a earlier check was found, kill that process
  (when (process-live-p pmd--flymake-proc)
    (kill-process pmd--flymake-proc))

  ;; Save current buffer, remove any narrowing restriction
  (let* ((source (current-buffer))
	 (extension (file-name-extension (buffer-file-name (current-buffer))))
	 ;; PMD relies on file extension to automatically select the language
	 ;; so we make sure that the temporary buffer has the same extension
	 ;; as the original file.
	 (tmp-file (make-temp-file "pmd-input" nil (format ".%s" extension)))
	 ;; For PMD to work on individual files, we need to provide a file
	 ;; containing the name of the files that we want to analyse.
	 (tmp-file-list (make-temp-file "pmd-file-list"))
	 (classpath (concat (flymake-pmd--pmd-lib-dir) "*"))
	 (ruleset-file (flymake-pmd--find-ruleset-file)))

    ;; save current buffer to temp file
    (write-region nil nil tmp-file nil -1)
    ;; save tmp-file name to tmp file list
    (write-region tmp-file nil tmp-file-list nil -1)
    (save-restriction
      (widen)
      ;; reset the `pmd--flymake-proc' process to a new process
      (setq pmd--flymake-proc
	    (make-process :name "pmd-flycheck"
			  :noquery t
			  :connection-type 'pipe
			  ;; make output go to a temporary buffer
			  :buffer (generate-new-buffer "*pmd-flymake*")
			  :stderr (get-buffer-create "*pmd-flymake-errors*")
			  :command `(,flymake-pmd-java-executable
				     "-classpath" ,classpath
				     "net.sourceforge.pmd.PMD"
				     "--format" "csv"
				     "--fail-on-violation" "false"
				     "--no-cache"
				     "--rulesets" ,ruleset-file
				     "--file-list" ,tmp-file-list)
			  :sentinel
			  (lambda (proc _event)
			    ;; Check that the process has indeed exited, as it might
			    ;; be simply suspended.
			    ;;
			    (when (memq (process-status proc) '(exit signal))
			      (unwind-protect
				  ;; Only proceed if `proc' is the same as
				  ;; `pmd--flymake-proc', whihc indicates that
				  ;; `proc' is not an obsolete process.
				  ;;
				  (if (with-current-buffer source (eq proc pmd--flymake-proc))
				      (with-current-buffer (process-buffer proc)
					(goto-char (point-min))
					(forward-line 1) ;; skip the csv header line

					;; Parse the output buffer for diagnostic'
					;; messages and locations, collect them in a list
					;; of objects, and call `report-fn'.
					;;
					(let ((diags '()))
					  (while (not(eobp))
					    (let* ((fields
						    ;; split the csv line
						    (split-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)) ","))
						   (description (string-trim (nth 5 fields) "\"" "\""))
						   (linenum (string-to-number (string-trim (nth 4 fields) "\"" "\"")))
						   (priority (string-to-number (string-trim (nth 3 fields) "\"" "\"")))
						   (ruleset (string-trim (nth 6 fields) "\"" "\""))
						   (type (if (<= priority 2)
							     :error
							   (if (<= priority 4)
							       :warning
							     :note)))
						   (position (flymake-diag-region source linenum)))
					      (setq diags (cons (flymake-make-diagnostic source
											 (car position)
											 (cdr position)
											 type
											 (format "%s (%s) [%d]" description ruleset priority))
								diags)))
					    (forward-line 1))
					  (funcall report-fn diags)
					  ))
				    (flymake-log :warning "Cancelling obsolete check %s" proc))
				;; cleanup the temporary buffer to hold the checks output
				;;
				(kill-buffer (process-buffer proc))
				(delete-file tmp-file)
				(delete-file tmp-file-list)
				))))))))

(defun flymake-pmd-setup-backend ()
  "Setup function for `flymake-pmd'."
  (add-hook 'flymake-diagnostic-functions 'flymake-pmd--checker nil t))

;;;###autoload
(defun flymake-pmd-enable ()
  "Enable `flymake' and `flymake-pmd'.

Add this function to some apex/java/... major mode hook."
  (interactive)
  (flymake-pmd--ensure-java-exists)
  (flymake-pmd--ensure-pmd-exists)
  (flymake-mode t)
  (add-hook 'flymake-diagnostic-functions 'pmd-flymake--checker nil t))

(provide 'flymake-pmd)
;;; flymake-pmd.el ends here
