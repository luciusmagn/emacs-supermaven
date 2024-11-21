;;; supermaven.el --- Supermaven for Emacs -*- lexical-binding: t; -*-

;; Author: Brayden Moon<crazywolf132@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (json "1.5"))
;; Keywords: convenience, completion, tools
;; URL: https://github.com/crazywolf132/supermaven.el

;;; Commentary:

;; Supermaven is an AI-powered code completion plugin for Emacs.

;;; Code:

(require 'cl-lib)
(require 'json)

;; Load dependencies
(require 'supermaven-config)
(require 'supermaven-logger)
(require 'supermaven-util)
(require 'supermaven-binary)
(require 'supermaven-state)
(require 'supermaven-process)
(require 'supermaven-completion)
(require 'supermaven-document)

;; Core variables
(defvar supermaven--initialized nil
  "Whether Supermaven has been initialized.")

(defvar supermaven-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd (cdr (assq 'accept-suggestion supermaven-keymaps)))
                #'supermaven-accept-completion)
    (define-key map (kbd (cdr (assq 'clear-suggestion supermaven-keymaps)))
                #'supermaven-clear-completion)
    map)
  "Keymap for Supermaven mode.")

;; Core functions
(defun supermaven--initialize ()
  "Initialize Supermaven if not already initialized."
  (unless supermaven--initialized
    (supermaven--ensure-binary)
    (supermaven--initialize-process-manager)
    (supermaven-state-initialize)
    (supermaven--initialize-completion)
    (setq supermaven--initialized t)))

(defun supermaven--cleanup ()
  "Clean up Supermaven resources."
  (supermaven-stop)
  (supermaven--cleanup-completion)
  (supermaven-state-cleanup)
  (setq supermaven--initialized nil))

;; Interactive commands
;;;###autoload
(defun supermaven-start ()
  "Start Supermaven."
  (interactive)
  (supermaven--initialize)
  (supermaven--start-process))

;;;###autoload
(defun supermaven-stop ()
  "Stop Supermaven."
  (interactive)
  (supermaven--stop-process))

;;;###autoload
(defun supermaven-restart ()
  "Restart Supermaven."
  (interactive)
  (supermaven-stop)
  (supermaven-start))

;;;###autoload
(defun supermaven-toggle ()
  "Toggle Supermaven on/off."
  (interactive)
  (if (supermaven--process-running-p)
      (supermaven-stop)
    (supermaven-start)))

;;;###autoload
(defun supermaven-status ()
  "Show Supermaven status."
  (interactive)
  (message "Supermaven is %s"
           (if (supermaven--process-running-p)
               "running" "not running")))

;;;###autoload
(defun supermaven-use-free ()
  "Switch to Supermaven Free version."
  (interactive)
  (supermaven--send-message '(:kind "use_free_version")))

;;;###autoload
(defun supermaven-use-pro ()
  "Switch to Supermaven Pro version."
  (interactive)
  (if-let ((url supermaven-activate-url))
      (browse-url url)
    (supermaven-log-error "No activation URL available.")))

;;;###autoload
(defun supermaven-logout ()
  "Log out of Supermaven."
  (interactive)
  (supermaven--send-message '(:kind "logout")))

;;;###autoload
(defun supermaven-show-log ()
  "Show Supermaven log buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Supermaven Log*")))
    (with-current-buffer buffer
      (view-mode 1)
      (goto-char (point-max)))
    (display-buffer buffer)))

;;;###autoload
(defun supermaven-clear-log ()
  "Clear Supermaven log buffer."
  (interactive)
  (when-let ((buffer (get-buffer "*Supermaven Log*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))))

;; Mode definitions
;;;###autoload
(define-minor-mode supermaven-mode
  "Toggle Supermaven mode."
  :init-value nil
  :lighter " Supermaven"
  :keymap supermaven-mode-map
  :group 'supermaven
  (if supermaven-mode
      (progn
        (supermaven--initialize)
        (supermaven--setup-document-hooks)
        (when supermaven-auto-start
          (supermaven-start)))
    (supermaven--cleanup-document-hooks)
    (supermaven--clear-overlays)
    (supermaven-stop)))

;;;###autoload
(define-globalized-minor-mode global-supermaven-mode
  supermaven-mode
  (lambda ()
    (unless (or (minibufferp)
                (not (buffer-file-name))
                (member major-mode supermaven-ignore-filetypes))
      (supermaven-mode 1)))
  :group 'supermaven)

;; Setup function
;;;###autoload
(defun supermaven-setup ()
  "Set up Supermaven."
  (interactive)
  ;; Initialize
  (supermaven--initialize)
  
  ;; Set up completion
  (with-eval-after-load 'company
    (supermaven--setup-company))
  
  ;; Set up hooks
  (add-hook 'completion-at-point-functions
            #'supermaven-completion-at-point nil 'local)
  
  ;; Start if auto-start enabled
  (when supermaven-auto-start
    (supermaven-start)))

;; Register all commands
(mapc (lambda (cmd)
        (put cmd 'completion-predicate #'ignore))
      '(supermaven-start
        supermaven-stop
        supermaven-restart
        supermaven-toggle
        supermaven-status
        supermaven-use-free
        supermaven-use-pro
        supermaven-logout
        supermaven-show-log
        supermaven-clear-log))

(provide 'supermaven)

;;; supermaven.el ends here