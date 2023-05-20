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
;; Package-Requires: ((emacs "24.3") (spinner "1.7.4"))
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

(defvar llm-copilot--spinner nil
  "Variable to store the spinner object.")

(defvar llm-copilot--languages
  '("awk" "C" "C++" "clojure" "css" "csv" "emacs-lisp" "fortran" "gnuplot"
    "go" "haskell" "java" "javascript" "julia" "latex" "ledger" "lisp"
    "lua" "matlab" "ocaml" "octave" "org" "perl" "python" "R" "ruby" "rust"
    "scala" "scheme" "shell" "sql" "typescript" "markdown")
  "List of languages that can be highlighted in an org code block.")

(defun llm-copilot--start-spinner ()
  "Start the spinner animation."
  (setq llm-copilot--spinner (spinner-start 'minibox)))

(defun llm-copilot--stop-spinner ()
  "Stop the spinner animation."
  (when llm-copilot--spinner
    (spinner-stop llm-copilot--spinner)
    (setq llm-copilot--spinner nil)))

(defun llm-copilot--send-post-request (url payload callback)
  "Send a POST request to the specified URL with the given payload.
   Calls the CALLBACK function with the response data."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data payload))
    (url-retrieve url (lambda (status)
                        (let ((response-buffer (current-buffer)))
                          (with-current-buffer response-buffer
                            (goto-char (point-min))
                            (search-forward-regexp "\n\n")
                            (let ((json-string (buffer-substring-no-properties (point) (point-max))))
                              (kill-buffer response-buffer)
                              (condition-case nil
                                  (let ((json-object-type 'plist))
                                    (funcall callback (plist-get (json-read-from-string json-string) :data)))
                                (error (funcall callback nil))))))))))

(defun llm-copilot--insert-into-org-mode (text lang)
  "Insert response into an Org Mode buffer with the given
        TEXT and LANG as the syntax highlighter."
  (let* ((url "http://localhost:3000")
         (payload (json-encode `((data . ,text))))
         (buffer-name "*llm-copilot*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (org-mode)
      (insert "* Copilot\n")
      (insert "  - Prompt: " text "\n")
      (llm-copilot--start-spinner) ;
      (switch-to-buffer buffer-name)
      (llm-copilot--send-post-request url payload
                                         (lambda (response)
                                           (llm-copilot--stop-spinner)
                                           (if response
                                               (progn
                                                 (with-current-buffer buffer-name
                                                   (goto-char (point-max))
                                                   (insert "    #+BEGIN_SRC\s" lang "\n" response "\n    #+END_SRC\n\n")))
                                             (with-current-buffer buffer-name
                                               (goto-char (point-max))
                                               (insert "  - Failed to fetch response\n\n"))))))))


(defun llm-copilot--generate (text)
  "Interactively ask for input and insert code in an Org Mode buffer."
  (interactive "sEnter Prompt: ")
  (let* ((selected-lang (ivy-read "Select Language: " llm-copilot--languages)))
    (llm-copilot--insert-into-org-mode text selected-lang)))






(provide 'llm-copilot)
;;; llm-copilot.el ends here
