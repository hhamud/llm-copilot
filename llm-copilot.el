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
;;
;;  Description
;;
;;; Code:
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





(defun llm-copilot--generate (text)
  "Interactively ask for input and insert code in an Org Mode buffer."
  (interactive "sEnter Prompt: ")
  (let* ((selected-lang (ivy-read "Select Language: " llm-copilot--languages))
         (selected-prompt-type (ivy-read "Select Prompt Type: " llm-copilot--prompt-type)))
    (message selected-prompt-type)
    (llm-copilot--insert-into-org-mode text selected-lang selected-prompt-type)))


(defun llm-copilot-start-server (model &optional address)
  "Start the server supplying the MODEL path and server ADDRESS."
  (interactive "fSelect model: \nsSelect address (press Enter to use default localhost:3000): ")
  (let ((address-arg (if (not (string-empty-p address))
                         (progn
                         (setq llm-copilot--server-address (format "http://%s" address))
                         (format "--address %s" (shell-quote-argument address)))
                       "")))
    (shell-command (format "llm-copilot llama --model %s %s"
                           (shell-quote-argument model)
                           address-arg))))




(provide 'llm-copilot)
;;; llm-copilot.el ends here
