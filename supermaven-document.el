;;; supermaven-document.el --- Document handling -*- lexical-binding: t; -*-

;;; Commentary:

;; Handles document changes and updates.

;;; Code:

(require 'cl-lib)
(require 'supermaven-state)
(require 'supermaven-process)
(require 'supermaven-completion)

;; Variables
(defvar-local supermaven--change-timer nil
  "Timer for debouncing changes.")

(defvar-local supermaven--last-change nil
  "Information about last change.")

(defclass supermaven-document-change ()
  ((begin :initarg :begin
          :type number
          :documentation "Change start position.")
   (end :initarg :end
        :type number
        :documentation "Change end position.")
   (length :initarg :length
           :type number
           :documentation "Length of change.")
   (text :initarg :text
         :type string
         :documentation "Changed text.")
   (time :initarg :time
         :type number
         :documentation "When change occurred."))
  "Represents a document change.")

;; Core functions
(defun supermaven--schedule-update (change)
  "Schedule an update based on CHANGE."
  (when supermaven--change-timer
    (cancel-timer supermaven--change-timer))
  (setq supermaven--last-change change
        supermaven--change-timer
        (run-with-idle-timer 0.5 nil #'supermaven--process-update (current-buffer))))

(defun supermaven--process-update (buffer)
  "Process pending update in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq supermaven--change-timer nil)
      (when supermaven--last-change
        (supermaven--send-update)))))

(defun supermaven--send-update ()
  "Send document update to Supermaven."
  (when-let* ((file-name (buffer-file-name))
              (content (buffer-substring-no-properties
                       (point-min) (point-max))))
    (when (< (length content) supermaven--hard-size-limit)
      (when supermaven--state-manager
        (supermaven-state-record-buffer-change
         supermaven--state-manager
         file-name
         content)
        (supermaven--submit-state-update)
        (setq supermaven--last-change nil)))))

(defun supermaven--track-change (beg end len)
  "Track change between BEG and END with length LEN."
  (when (and supermaven-mode
             (supermaven--process-running-p)
             (not (supermaven--should-ignore-buffer)))
    (let ((change (list :begin beg
                        :end end
                        :length len
                        :text (buffer-substring-no-properties beg end)
                        :time (float-time))))
      (supermaven--schedule-update change))))

(defun supermaven--should-ignore-buffer ()
  "Check if current buffer should be ignored."
  (or (not (buffer-file-name))
      (member major-mode supermaven-ignore-filetypes)
      (and supermaven-condition
           (funcall supermaven-condition))))

;; Hooks
(defun supermaven--setup-document-hooks ()
  "Set up document change hooks."
  (add-hook 'after-change-functions #'supermaven--track-change nil t))

(defun supermaven--cleanup-document-hooks ()
  "Remove document change hooks."
  (remove-hook 'after-change-functions #'supermaven--track-change t))

(provide 'supermaven-document)

;;; supermaven-document.el ends here
