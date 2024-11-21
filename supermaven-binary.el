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
  (format "https://supermaven.com/api/download-path-v2?platform=%s&arch=%s&editor=neovim"
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
    
    (let* ((url-request-method "GET")
           (response-buffer (url-retrieve-synchronously url))
           download-url)
      (unwind-protect
          (with-current-buffer response-buffer
            (goto-char (point-min))
            (re-search-forward "^$")
            (forward-char)
            (let* ((json-object-type 'hash-table)
                   (response (json-read)))
              (setq download-url (gethash "downloadUrl" response))))
        (kill-buffer response-buffer))
      
      (unless download-url
        (error "Failed to get download URL from Supermaven API"))
      
      (supermaven-log-debug (format "Download URL: %s" download-url))
      (supermaven--download-binary download-url binary-path)
      (setq supermaven-binary-path binary-path)
      binary-path)))

(defun supermaven--ensure-binary ()
  "Ensure the Supermaven binary is available and up to date."
  (let ((binary-path (supermaven--get-binary-path)))
    (when (or (not (file-exists-p binary-path))
              (not supermaven-binary-path))
      (condition-case err
          (progn
            (supermaven--fetch-binary)
            (supermaven-log-info (format "Successfully installed Supermaven binary to %s" binary-path)))
        (error
         (supermaven-log-error (format "Failed to fetch Supermaven binary: %s" (error-message-string err)))
         (signal (car err) (cdr err)))))))

(provide 'supermaven-binary)

;;; supermaven-binary.el ends here