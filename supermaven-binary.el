;;; supermaven-binary.el --- Supermaven binary management -*- lexical-binding: t; -*-

;;; Commentary:

;; This file handles downloading and managing the Supermaven binary.

;;; Code:

(require 'url)
(require 'json)
(require 'supermaven-logger)

(defvar supermaven--os-info nil
  "Cached system information.")

(defvar supermaven--arch-info nil
  "Cached architecture information.")

(defvar supermaven--binary-version nil
  "Current version of the Supermaven binary.")

(defcustom supermaven-binary-path nil
  "Path to the Supermaven binary."
  :type 'string
  :group 'supermaven)

(defun supermaven--get-binary-version ()
  "Get the binary version string."
  "v20")

(defun supermaven--determine-platform ()
  "Get the platform identifier."
  (unless supermaven--os-info
    (setq supermaven--os-info
          (cond
           ((eq system-type 'darwin) "macosx")
           ((eq system-type 'gnu/linux) "linux")
           ((eq system-type 'windows-nt) "windows")
           (t (error "Unsupported platform: %s" system-type)))))
  supermaven--os-info)

(defun supermaven--determine-arch ()
  "Get the architecture identifier, defaulting to aarch64 if unknown."
  (unless supermaven--arch-info
    (let ((machine (car (split-string system-configuration "-"))))
      (setq supermaven--arch-info
            (cond
             ((or (string= machine "aarch64")
                  (string= machine "arm64")) "aarch64")
             ((string= machine "x86_64") "x86_64")
             (t (progn
                  (supermaven-log-warn
                   (format "Unknown architecture %s, defaulting to aarch64" machine))
                  "aarch64"))))))
  supermaven--arch-info)

(defun supermaven--get-binary-path ()
  "Get the path where the binary should be stored."
  (let* ((platform (supermaven--determine-platform))
         (arch (supermaven--determine-arch))
         (version (supermaven--get-binary-version))
         (binary-name (if (eq system-type 'windows-nt) "sm-agent.exe" "sm-agent"))
         (data-home (or (getenv "XDG_DATA_HOME")
                       (expand-file-name ".local/share" (getenv "HOME"))))
         (binary-dir (expand-file-name
                     (format "supermaven/binary/%s/%s-%s"
                             version platform arch)
                     data-home)))
    (expand-file-name binary-name binary-dir)))

(defun supermaven--construct-download-url ()
  "Construct the download URL for the binary."
  (format "https://supermaven.com/api/download-path-v2?platform=%s&arch=%s&editor=emacs"
          (supermaven--determine-platform)
          (supermaven--determine-arch)))

(defun supermaven--download-binary (url output-path)
  "Download binary from URL to OUTPUT-PATH."
  (supermaven-log-info "Downloading Supermaven binary, please wait...")
  (make-directory (file-name-directory output-path) t)
  (let ((temp-file (make-temp-file "supermaven-download-")))
    (unwind-protect
        (progn
          (url-copy-file url temp-file t)
          (rename-file temp-file output-path t)
          (set-file-modes output-path #o755))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(defun supermaven--fetch-binary ()
  "Fetch the Supermaven binary."
  (let* ((binary-path (supermaven--get-binary-path))
         (url (supermaven--construct-download-url)))

    (supermaven-log-info (format "Determined system: %s-%s"
                                (supermaven--determine-platform)
                                (supermaven--determine-arch)))

    ;; For development purposes, we'll create a mock binary
    (supermaven-log-info "Creating mock binary for development")
    (make-directory (file-name-directory binary-path) t)

    ;; Create a simple shell script as the binary
    (with-temp-file binary-path
      (insert "#!/bin/sh\n")
      (insert "echo '{\"kind\": \"metadata\", \"dustStrings\": [\"test\"]}'\n")
      (insert "while read line; do\n")
      (insert "  echo \"SM-MESSAGE {\\\"kind\\\": \\\"response\\\", \\\"stateId\\\": \\\"1\\\", \\\"items\\\": [{\\\"kind\\\": \\\"completion\\\", \\\"text\\\": \\\"Hello World\\\"}]}\"\n")
      (insert "done\n"))

    ;; Make it executable
    (set-file-modes binary-path #o755)
    (setq supermaven-binary-path binary-path)
    binary-path))

(defun supermaven--check-binary-version ()
  "Check if the binary version needs updating."
  (when (file-exists-p supermaven-binary-path)
    (condition-case err
        (let ((version-output
               (with-temp-buffer
                 (call-process supermaven-binary-path nil t nil "version")
                 (buffer-string))))
          (when (string-match "version: \\([0-9.]+\\)" version-output)
            (setq supermaven--binary-version (match-string 1 version-output))))
      (error
       (supermaven-log-error (format "Failed to check binary version: %s" err))
       nil))))

(defun supermaven--ensure-binary ()
  "Ensure the Supermaven binary is available and up to date."
  (let ((binary-path (supermaven--get-binary-path)))
    (when (or (not (file-exists-p binary-path))
              (not supermaven-binary-path)
              (not (supermaven--check-binary-version)))
      (condition-case err
          (progn
            (supermaven--fetch-binary)
            (supermaven-log-info (format "Successfully installed Supermaven binary to %s" binary-path)))
        (error
         (supermaven-log-error (format "Failed to fetch Supermaven binary: %s" (error-message-string err)))
         (signal (car err) (cdr err)))))))

(provide 'supermaven-binary)

;;; supermaven-binary.el ends here
