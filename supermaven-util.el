;;; supermaven-util.el --- Supermaven utility functions -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains utility functions for Supermaven.

;;; Code:

(defun supermaven--trim-string (string)
  "Remove whitespace at the beginning and end of STRING."
  (replace-regexp-in-string "\\`[ \t\n]*" ""
                            (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun supermaven--first-line (string)
  "Get the first line of STRING."
  (car (split-string string "\n")))

(defun supermaven--contains-p (string substring)
  "Check if STRING contains SUBSTRING."
  (string-match-p (regexp-quote substring) string))

(defun supermaven--starts-with-p (string prefix)
  "Check if STRING starts with PREFIX."
  (string-prefix-p prefix string))

(defun supermaven--ends-with-p (string suffix)
  "Check if STRING ends with SUFFIX."
  (string-suffix-p suffix string))

(defun supermaven--line-count (string)
  "Count the number of lines in STRING."
  (1+ (cl-count ?\n string)))

(defun supermaven--get-last-line (string)
  "Get the last line of STRING."
  (car (last (split-string string "\n"))))

(defun supermaven--to-next-word (string)
  "Get STRING up to the next word boundary."
  (if (string-match "\\([[:alnum:]_]+\\)" string)
      (match-string 0 string)
    string))

(defun supermaven--is-whitespace-p (char)
  "Check if CHAR is whitespace."
  (memq char '(?\s ?\t ?\n ?\r ?\v ?\f)))

(provide 'supermaven-util)

;;; supermaven-util.el ends here
