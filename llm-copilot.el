;;; llm-copilot.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Hamza Hamud
;;
;; Author: Hamza Hamud
;; Maintainer: Hamza Hamud
;; Created: May 18, 2023
;; Modified: May 18, 2023
;; Version: 0.0.1
;; Keywords: llm copilot ai rust rustformers transformers
;; Homepage: https://github.com/hhamud/llm-copilot
;; Package-Requires: ((emacs "24.3") (spinner "1.7.4") )
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; llm-copilot is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; llm-copilot is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Notmuch.  If not, see <http://www.gnu.org/licenses/>.
;;
;; This is an emacs-based interface to the llm-copilot system.
;;
;; You will first need to have the llm-copilot program installed.
;; See README.md for further details.
;;
;; To install this software, copy it to a directory that is on the
;; `load-path' variable within Emacs (a good candidate is
;; /usr/local/share/emacs/site-lisp).
;; Then, to actually run it, add:
;;
;;	(require 'llm-copilot)
;;
;; to your ~/.emacs file, and then run "M-x llm-copilot-start-server"
;; from within Emacs,
;;
;; Have fun, and let us know if you have any comment, questions, or
;;
;;; Code:
;

(require 'url)
(require 'json)
(require 'org)
(require 'spinner)


(defvar llm-copilot--server-address "http://localhost:3000"
  "Default address.")

(defvar llm-copilot--languages
  '("awk" "C" "C++" "clojure" "css" "csv" "emacs-lisp" "fortran" "gnuplot"
    "go" "haskell" "java" "javascript" "julia" "latex" "ledger" "lisp"
    "lua" "matlab" "ocaml" "octave" "org" "perl" "python" "R" "ruby" "rust"
    "scala" "scheme" "shell" "sql" "typescript" "markdown")
  "List of languages that can be highlighted in an org code block.")

(defvar llm-copilot--prompt-type
  '("GENERATE" "FIX" "EMACS")
  "Prompt type field for the post request.")


(defun llm-copilot--send-post-request (url payload callback)
  "Send a POST request to the specified URL with the given payload.
   Calls the CALLBACK function with the response data."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data payload))
    (url-retrieve url (lambda (status)
                        (if status
                            (funcall callback nil)
                          (let ((response-buffer (current-buffer)))
                            (with-current-buffer response-buffer
                              (goto-char (point-min))
                              (if (search-forward-regexp "\n\n" nil t)
                                  (let ((json-string (buffer-substring-no-properties (point) (point-max))))
                                    (kill-buffer response-buffer)
                                    (condition-case nil
                                        (let ((json-object-type 'plist))
                                          (funcall callback (plist-get (json-read-from-string json-string) :data)))
                                      (error (funcall callback nil))))
                                (kill-buffer response-buffer)
                                (funcall callback nil)))))))))

(defvar llm-copilot--spinner (make-hash-table :test 'equal)
  "A hashtable for storing spinner objects associated with buffer names.")

(defun llm-copilot--start-spinner (buffer-name)
  "Start the spinner animation."
  (puthash buffer-name (spinner-start 'minibox) llm-copilot--spinner))

(defun llm-copilot--stop-spinner (buffer-name)
  "Stop the spinner animation."
  (let ((spinner (gethash buffer-name llm-copilot--spinner)))
    (when spinner
      (spinner-stop spinner)
      (remhash buffer-name llm-copilot--spinner))))

(defun llm-copilot--insert-into-org-mode (text lang prompt)
  "Insert response into an Org Mode buffer with the given
        TEXT and LANG as the syntax highlighter and PROMPT as prompt-type."
  (let* ((url llm-copilot--server-address)
         (payload (json-encode `(("data" . ,text)
                                 ("prompt_type" . ,prompt))))
         (buffer-name "*llm-copilot*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (org-mode)
      (insert "* Copilot\n")
      (insert "  - Prompt: " text "\n")
      (llm-copilot--start-spinner buffer-name)
      (switch-to-buffer buffer-name)
      (llm-copilot--send-post-request url payload
                                      (lambda (response)
                                        (with-current-buffer buffer-name
                                          (goto-char (point-max))
                                          (if response
                                              (insert "    #+BEGIN_SRC\s" lang "\n" response "\n    #+END_SRC\n\n")
                                            (insert "  - Failed to fetch response\n\n")))
                                        (llm-copilot--stop-spinner buffer-name))))))



(defun llm-copilot--process-sentinel (proc _event)
  "Process sentinel for llm-copilot start server process.
PROC is the process for the command, and EVENT describes the changes to it."
  (when (eq (process-status proc) 'exit)
    (if (= (process-exit-status proc) 0)
        (message "llm-copilot server started successfully.")
      (message "llm-copilot server failed to start."))))


(defun llm-copilot-start-server (model &optional address)
  "Start the server supplying the MODEL path and ADDRESS for the server."
  (interactive "fChoose Model: \nsEnter Address: ")
  (if (not (string= address ""))
      (setq llm-copilot--server-address address))
  (let* ((command (format "cargo run --release -- llama --model %s --address %s" model llm-copilot--server-address)))
    (message "Starting server with command: %s" command)
    (let ((process (start-process-shell-command "llm-copilot-server" "*llm-copilot-server*" command)))
      (set-process-query-on-exit-flag process nil)
      (set-process-sentinel process (lambda (process event)
                                      (message "Server process %s" (substring event 0 -1)))))))

(defun llm-copilot-stop-server ()
  "Shutdown the server."
  (interactive)
  (let ((process (get-buffer-process "*llm-copilot-server*")))
    (when process
      (interrupt-process process)
      (message "Sent SIGINT to server process"))))


(defun llm-copilot-generate (text)
  "Interactively ask for input and insert code in an Org Mode buffer."
  (interactive "sEnter Prompt: ")
  (let* ((selected-lang (ivy-read "Select Language: " llm-copilot--languages))
         (selected-prompt-type (ivy-read "Select Prompt Type: " llm-copilot--prompt-type)))
    (message selected-prompt-type)
    (llm-copilot--insert-into-org-mode text selected-lang selected-prompt-type)))

(provide 'llm-copilot)
;;; llm-copilot.el ends here
