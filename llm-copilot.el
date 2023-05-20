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
(require 'url)
(require 'json)
(require 'org)

(defun llm-copilot--my-send-post-request (url payload callback)
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

(defun llm-copilot--my-insert-hello-world (text)
  "Insert 'Hello, World!' into an Org Mode buffer with the given TEXT."
  (let* ((url "http://localhost:3000")
         (payload (json-encode `((data . ,text))))
         (buffer-name "*llm-copilot*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (org-mode)
      (insert "* Hello, World!\n")
      (insert "  - Input: " text "\n")
      (insert "  - Waiting for response...\n\n")
      (switch-to-buffer buffer-name)
      (llm-copilot--my-send-post-request url payload
                                         (lambda (response)
                                           (if response
                                               (progn
                                                 (with-current-buffer buffer-name
                                                   (goto-char (point-max))
                                                   (insert "  - Response:\n")
                                                   (insert "    #+BEGIN_SRC\n" response "\n    #+END_SRC\n\n")))
                                             (with-current-buffer buffer-name
                                               (goto-char (point-max))
                                               (insert "  - Failed to fetch response\n\n"))))))))

(defun llm-copilot--my-interact-insert-hello-world ()
  "Interactively ask for input and insert 'Hello, World!' in an Org Mode buffer."
  (interactive)
  (let* ((text (read-string "Enter text: ")))
    (llm-copilot--my-insert-hello-world text)))


(provide 'llm-copilot)
;;; llm-copilot.el ends here
