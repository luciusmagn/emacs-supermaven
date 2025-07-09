;;; supermaven-process.el --- Supermaven process management -*- lexical-binding: t; -*-

;;; Commentary:

;; This file handles the Supermaven binary process and communication.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'supermaven-binary)
(require 'supermaven-completion)
(require 'supermaven-logger)
(require 'supermaven-config)
(require 'supermaven-state)

;; Process variables
(defvar supermaven--process nil
  "The Supermaven process.")

(defvar supermaven--process-buffer " *supermaven*"
  "Buffer for Supermaven process output.")

(defvar supermaven--message-queue nil
  "Queue of messages to be processed.")

(defvar supermaven--changed-documents (make-hash-table :test 'equal)
  "Hash table of changed documents.")

(defvar supermaven--retry-count 0
  "Number of process restart attempts.")

(defvar supermaven--last-message-time 0
  "Time of last message sent.")

(defconst supermaven--message-delay 0.05
  "Minimum delay between messages in seconds.")

(defconst supermaven--max-retries 3
  "Maximum number of restart attempts.")

(defconst supermaven--hard-size-limit 10e6
  "Maximum size of buffer text to process.")

;; Core process functions
(defun supermaven--process-running-p ()
  "Check if Supermaven process is running."
  (and supermaven--process
       (process-live-p supermaven--process)))

(defun supermaven--start-process ()
  "Start the Supermaven process."
  (when (supermaven--process-running-p)
    (supermaven--stop-process))

  (condition-case err
      (progn
        (supermaven--ensure-binary)
        (let ((process-buffer (get-buffer-create supermaven--process-buffer)))
          (with-current-buffer process-buffer
            (erase-buffer)
            (setq buffer-read-only t))

          (setq supermaven--process
                (make-process
                 :name "supermaven"
                 :buffer process-buffer
                 :command (list supermaven-binary-path "stdio")
                 :filter #'supermaven--process-filter
                 :sentinel #'supermaven--process-sentinel
                 :noquery t))

          (setq supermaven--retry-count 0)
          (supermaven--send-greeting)
          (supermaven-log-info "Supermaven process started successfully"))
      (error
       (supermaven-log-error (format "Failed to start Supermaven: %s" err))
       (when (< supermaven--retry-count supermaven--max-retries)
         (cl-incf supermaven--retry-count)
         (run-with-timer 2 nil #'supermaven--start-process))))))

(defun supermaven--stop-process ()
  "Stop the Supermaven process."
  (when (supermaven--process-running-p)
    (supermaven-log-info "Stopping Supermaven process...")
    (delete-process supermaven--process)
    (setq supermaven--process nil
          supermaven--message-queue nil)
    (when-let* ((buf (get-buffer supermaven--process-buffer)))
      (kill-buffer buf))))

(defun supermaven--send-message (message)
  "Send MESSAGE to the Supermaven process."
  (when (supermaven--process-running-p)
    (let ((now (float-time)))
      (when (> (- now supermaven--last-message-time) supermaven--message-delay)
        (setq supermaven--last-message-time now)
        (supermaven-log-info (format "Sending: %s" message))
        (condition-case err
            (process-send-string
             supermaven--process
             (concat (json-encode message) "\n"))
          (error
           (supermaven-log-error (format "Failed to send message: %s" err))))))))

(defun supermaven--process-filter (proc output)
  "Process filter for OUTPUT from PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert output)
          (set-marker (process-mark proc) (point)))
        (when moving
          (goto-char (process-mark proc))))
      (supermaven--process-pending-messages))))

(defun supermaven--process-pending-messages ()
  "Process any complete messages in the process buffer."
  (with-current-buffer supermaven--process-buffer
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (re-search-forward "^SM-MESSAGE \\(.+\\)\n" nil t)
        (let ((msg-text (match-string 1)))
          (delete-region (match-beginning 0) (match-end 0))
          (condition-case err
              (supermaven--handle-message msg-text)
            (error
             (supermaven-log-error
              (format "Error processing message: %s\nMessage: %s"
                      err msg-text)))))))))

(defun supermaven--process-sentinel (proc event)
  "Handle process state changes for PROC with EVENT."
  (supermaven-log-info (format "Supermaven process %s" event))
  (when (memq (process-status proc) '(exit signal))
    (supermaven-log-info "The supermaven process exit, and will need to be started again")
    (setq supermaven--process nil)
    ;; Attempt restart if appropriate
    (when (and (< supermaven--retry-count supermaven--max-retries)
               supermaven-auto-start)
      (run-with-timer 2 nil #'supermaven--start-process))))

;; Message handling
(defun supermaven--handle-message (message-text)
  "Handle a message MESSAGE-TEXT from the Supermaven process."
  (supermaven-log-info (format "Reiceive message: %s" message-text))
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (message (json-read-from-string message-text)))
    (pcase (gethash "kind" message)
      ("response" (supermaven--handle-response message))
      ("metadata" (supermaven--handle-metadata message))
      ("activation_request" (supermaven--handle-activation-request message))
      ("activation_success" (supermaven--handle-activation-success))
      ("service_tier" (supermaven--handle-service-tier message))
      ("error" (supermaven--handle-error message))
      (_ (supermaven-log-debug
          (format "Unknown message kind: %s" (gethash "kind" message)))))))

(defun supermaven--handle-response (message)
  "Handle response MESSAGE from Supermaven."
  (let ((state-id (gethash "stateId" message))
        (items (gethash "items" message)))
    (supermaven--update-completion-state state-id items)))

(defun supermaven--handle-metadata (message)
  "Handle metadata MESSAGE from Supermaven."
  (when-let* ((dust-strings (gethash "dustStrings" message)))
    (setq supermaven-dust-strings dust-strings)))

(defun supermaven--handle-activation-request (message)
  "Handle activation request MESSAGE from Supermaven."
  (let ((url (gethash "url" message)))
    (setq supermaven-activate-url url)
    (supermaven-log-info "Activation required. Use M-x supermaven-use-pro to activate.")
    (when (y-or-n-p "Supermaven Pro activation required. Open activation page now? ")
      (browse-url url))))

(defun supermaven--handle-activation-success ()
  "Handle successful activation."
  (supermaven-log-info "Supermaven Pro activated successfully!")
  (message "Supermaven Pro activated successfully!"))

(defun supermaven--handle-service-tier (message)
  "Handle service tier MESSAGE."
  (let ((tier (gethash "tier" message)))
    (supermaven-log-info (format "Supermaven service tier: %s" tier))
    (message "Supermaven service tier: %s" tier)))

(defun supermaven--handle-error (message)
  "Handle error MESSAGE from Supermaven."
  (let ((error-msg (gethash "error" message)))
    (supermaven-log-error (format "Supermaven error: %s" error-msg))
    (message "Supermaven error: %s" error-msg)))

;; Document handling
(defun supermaven--document-changed (path content)
  "Notify that document at PATH has changed to CONTENT."
  (supermaven--send-message
   (list :kind "inform_file_changed"
         :path path))
  (puthash path
           (list :path path
                 :content content
                 :timestamp (float-time))
           supermaven--changed-documents))

(defun supermaven--send-greeting ()
  "Send initial greeting to Supermaven."
  (supermaven--send-message
   (list :kind "greeting"
         :allowGitignore :json-false)))

(defun supermaven--submit-state-update ()
  "Submit pending document updates to Supermaven."
  (when supermaven--state-manager
    (let ((updates (supermaven-state-get-changes supermaven--state-manager)))
      (when updates
        (supermaven--send-message
         (list :kind "state_update"
               :newId (number-to-string supermaven--current-state-id)
               :updates updates))))))

(defun supermaven--update-completion-state (state-id items)
  "Update completion state with STATE-ID and ITEMS."
  (when (and supermaven--state-manager items)
    (supermaven-state-update supermaven--state-manager state-id items)
    (when-let ((completion-text (gethash "text" (car items))))
      (supermaven-state-update-completion
       supermaven--state-manager state-id completion-text))
    (supermaven--process-completion items)))

(defun supermaven--process-completion (items)
  "Process completion ITEMS and update UI."
  (when items
    (let ((completion-text (gethash "text" (car items))))
      (when (and completion-text (not (string-empty-p completion-text)))
        (run-with-idle-timer 0 nil #'supermaven--update-completion)))))

(defun supermaven--initialize-process-manager ()
  "Initialize the Supermaven process manager."
  (setq supermaven--message-queue nil
        supermaven--changed-documents (make-hash-table :test 'equal)
        supermaven--retry-count 0
        supermaven--last-message-time 0))

(defun supermaven--cleanup-process ()
  "Clean up process resources."
  (supermaven--stop-process)
  (setq supermaven--message-queue nil
        supermaven--changed-documents (make-hash-table :test 'equal)
        supermaven--retry-count 0))

(provide 'supermaven-process)

;;; supermaven-process.el ends here
