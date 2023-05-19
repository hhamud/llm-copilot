;;; llm-copilot.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Hamza Hamud
;;
;; Author: Hamza Hamud
;; Maintainer: Hamza Hamud
;; Created: May 18, 2023
;; Modified: May 18, 2023
;; Version: 0.0.1
;; Keywords: llm-copilot
;; Homepage: https://github.com/hhamud/llm-copilot
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'json)


(defvar llm-copilot-sse-buffer-name "*Server Side Events*"
  "Name of the buffer to store SSE events.")



(defun llm-copilot-process-sse-event (event-data)
  "Process a single SSE EVENT-DATA and append it to a new buffer."
  (let ((buffer (generate-new-buffer llm-copilot-sse-buffer-name)))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "%s\n" event-data)))
    (switch-to-buffer buffer)))


(defun llm-copilot-retrieve-sse-events ()
  "Prompt for user input and retrieve SSE events from the server."
  (interactive)
  (let* ((data (read-string "Enter the data: "))
         (url "http://localhost:3000")
         (json-data (json-encode `(("data" . ,data))))
         (curl-args `("-X" "POST"
                      "-H" "Content-Type: application/json"
                      "-d" ,json-data
                      ,url))
         (process (apply #'start-process "curl" nil "curl" curl-args)))
    (set-process-filter process #'llm-copilot-process-output)
    (set-process-sentinel process #'llm-copilot-process-sentinel)))

(defun llm-copilot-process-output (process output)
  "Process the OUTPUT from the PROCESS and handle SSE events."
  (message output)
  (when (string-match "^data:\\s-*\\(.*\\)" output)
    (let ((event-data (match-string 1 output)))
      (llm-copilot-process-sse-event event-data))))





(defun llm-copilot-process-sentinel (process event)
  "Process the EVENT from the PROCESS sentinel."
  (when (string= event "finished\n")
    (message "cURL process finished successfully")))



(provide 'llm-copilot)
;;; llm-copilot.el ends here
