;;; supermaven-completion.el --- Improved completion handling -*- lexical-binding: t; -*-

;;; Commentary:

;; Core completion functionality for Supermaven.

;;; Code:

(require 'cl-lib)
(require 'supermaven-state)
(require 'supermaven-logger)
(require 'supermaven-config)

;; Variables
(defvar-local supermaven--current-overlay nil
  "Current completion overlay.")

(defconst supermaven--completion-chunk-size 1000
  "Maximum size of text to process in one chunk.")

(defvar-local supermaven--polling-timer nil
  "Timer for polling completion updates.")

(defvar-local supermaven--last-prefix nil
  "Last completion prefix.")

(defvar-local supermaven--completion-cache (make-hash-table :test 'equal)
  "Cache for completions.")

(defclass supermaven-completion-context ()
  ((buffer :initarg :buffer
           :type buffer
           :documentation "Buffer being completed.")
   (prefix :initarg :prefix
           :type string
           :documentation "Completion prefix.")
   (point :initarg :point
          :type number
          :documentation "Point position.")
   (line-before :initarg :line-before
                :type string
                :documentation "Text before point.")
   (line-after :initarg :line-after
               :type string
               :documentation "Text after point.")
   (bounds :initarg :bounds
           :type cons
           :documentation "Completion bounds."))
  "Context for completion operation.")

(defclass supermaven-completion-item ()
  ((kind :initarg :kind
         :type string)
   (text :initarg :text
         :type string))
  "A single completion item.")

;; Core functions
(defun supermaven--initialize-completion ()
  "Initialize completion system."
  (add-hook 'completion-at-point-functions #'supermaven-completion-at-point nil t)
  (when (featurep 'company)
    (supermaven--setup-company)))

(defun supermaven--setup-company ()
  "Set up company-mode integration."
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-supermaven)))

(defun supermaven--create-completion-context ()
  "Create completion context at point."
  (let* ((buf (current-buffer))
         (pos (point))
         (bounds (supermaven--completion-bounds))
         (line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
         (prefix-start (car bounds))
         (line-before (substring line 0 (- pos (line-beginning-position))))
         (line-after (substring line (- pos (line-beginning-position)))))
    (make-instance 'supermaven-completion-context
                  :buffer buf
                  :prefix (buffer-substring-no-properties prefix-start pos)
                  :point pos
                  :line-before line-before
                  :line-after line-after
                  :bounds bounds)))

(defun supermaven--completion-bounds ()
  "Get bounds of thing to complete."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (cons (or (car bounds) (point))
          (or (cdr bounds) (point)))))

(defun supermaven--display-completion (text &optional after)
  "Display completion TEXT with optional AFTER text."
  (supermaven--clear-completion)
  (when (and text (not (string-empty-p text)))
    (let* ((pos (point))
           (ov (make-overlay pos pos nil t nil)))
      (overlay-put ov 'priority 100)
      (overlay-put ov 'supermaven t)
      (overlay-put ov 'after-string
                   (propertize (if after
                                  (concat text after)
                                text)
                              'face 'shadow))
      (setq supermaven--current-overlay ov))))

(defun supermaven--clear-completion ()
  "Clear current completion."
  (when supermaven--current-overlay
    (delete-overlay supermaven--current-overlay)
    (setq supermaven--current-overlay nil)))

(defun supermaven--cache-completion (key completion)
  "Cache COMPLETION for KEY."
  (puthash key completion supermaven--completion-cache))

(defun supermaven--get-cached-completion (key)
  "Get cached completion for KEY."
  (gethash key supermaven--completion-cache))

(defun supermaven--clear-completion-cache ()
  "Clear completion cache."
  (clrhash supermaven--completion-cache))

;; Interactive commands
(defun supermaven-accept-completion ()
  "Accept current completion."
  (interactive)
  (when-let* ((ov supermaven--current-overlay)
              (text (overlay-get ov 'after-string)))
    (supermaven--clear-completion)
    (insert text)))

(defun supermaven-accept-word ()
  "Accept next word of completion."
  (interactive)
  (when-let* ((ov supermaven--current-overlay)
              (text (overlay-get ov 'after-string))
              (word (supermaven--to-next-word text)))
    (supermaven--clear-completion)
    (insert word)))

(defun supermaven-clear-completion ()
  "Clear current completion."
  (interactive)
  (supermaven--clear-completion))

;; Company backend
(defun company-supermaven (command &optional arg &rest _ignored)
  "Company backend for Supermaven."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-supermaven))
    (prefix (and supermaven-mode
                 (not (company-in-string-or-comment))
                 (not (supermaven--should-ignore-buffer))
                 (let ((bounds (supermaven--completion-bounds)))
                   (buffer-substring-no-properties (car bounds) (point)))))
    (candidates
     (when-let ((completion (supermaven--get-completion-text)))
       (list completion)))
    (sorted t)
    (kind 'text)
    (annotation "<Supermaven>")
    (no-cache t)
    (require-match 'never)
    (duplicates nil)
    (meta "AI-powered completion")))

(defun supermaven--poll-completions ()
  "Poll for completion updates."
  (when supermaven--polling-timer
    (cancel-timer supermaven--polling-timer))
  (setq supermaven--polling-timer
        (run-with-timer 0 0.025 #'supermaven--poll-once
                       (current-buffer))))

(defun supermaven--stop-polling ()
  "Stop polling for completions."
  (when supermaven--polling-timer
    (cancel-timer supermaven--polling-timer)
    (setq supermaven--polling-timer nil)))

(defun supermaven--poll-once (buffer)
  "Poll once for completions in BUFFER."
  (when (and (buffer-live-p buffer)
             (eq buffer (current-buffer)))
    (supermaven--update-completion)))


;; Cleanup
(defun supermaven--cleanup-completion ()
  "Clean up completion system."
  (supermaven--clear-completion)
  (supermaven--clear-completion-cache)
  (setq supermaven--last-prefix nil))

(provide 'supermaven-completion)

;;; supermaven-completion.el ends here