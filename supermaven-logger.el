;;; supermaven-logger.el --- Supermaven logging -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains logging functions for Supermaven.

;;; Code:

(require 'supermaven-config)

(defconst supermaven-log-buffer-name "*Supermaven Log*"
  "Name of the Supermaven log buffer.")

(defun supermaven-log (level message)
  "Log MESSAGE at LEVEL."
  (when (>= (supermaven--log-level-to-number supermaven-log-level)
            (supermaven--log-level-to-number level))
    ;; make the logging atomic
    (save-current-buffer
      (with-current-buffer (get-buffer-create supermaven-log-buffer-name)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
          (insert (format "[%s] %s\n" (upcase (symbol-name level)) message)))))))

(defun supermaven-log-trace (message)
  "Log MESSAGE at TRACE level."
  (supermaven-log 'trace message))

(defun supermaven-log-debug (message)
  "Log MESSAGE at DEBUG level."
  (supermaven-log 'debug message))

(defun supermaven-log-info (message)
  "Log MESSAGE at INFO level."
  (supermaven-log 'info message))

(defun supermaven-log-warn (message)
  "Log MESSAGE at WARN level."
  (supermaven-log 'warn message))

(defun supermaven-log-error (message)
  "Log MESSAGE at ERROR level."
  (supermaven-log 'error message))

(defun supermaven--log-level-to-number (level)
  "Convert log LEVEL to a number for comparison."
  (pcase level
    ('off 0)
    ('error 1)
    ('warn 2)
    ('info 3)
    ('debug 4)
    ('trace 5)
    (_ 3)))  ; Default to INFO level

(defun supermaven-show-log ()
  "Show the Supermaven log buffer."
  (interactive)
  (switch-to-buffer-other-window supermaven-log-buffer-name))

(defun supermaven-clear-log ()
  "Clear the Supermaven log buffer."
  (interactive)
  (with-current-buffer (get-buffer-create supermaven-log-buffer-name)
    (erase-buffer)))

(provide 'supermaven-logger)

;;; supermaven-logger.el ends here
