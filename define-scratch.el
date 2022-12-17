;;; define-scratch.el --- Define new commands to make scratch buffers -*- lexical-binding: t -*-

;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/emacs-define-scratch
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: languages util
;; SPDX-License-Identifier: ISC

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Define new commands to make scratch buffers. Examples:

;;     (define-scratch scheme-scratch scheme-mode)
;;     (define-scratch text-scratch text-mode auto-fill-mode)

;;; Code:

(defun define-scratch--buffer-base-name (mode)
  "Internal function to make up a name for a MODE buffer."
  (let ((name (symbol-name mode)))
    (format "*%s-scratch*"
            (save-match-data (replace-regexp-in-string "-mode$" "" name t)))))

(defun define-scratch--get-or-create-blank-buffer (name)
  "Internal function to get a (possibly numbered) scratch buffer for NAME."
  (let ((result nil)
        (number 1)
        (max-number 100)
        (name-and-number name))
    (while (and (not result) (<= number max-number))
      (let ((buffer (get-buffer-create name-and-number)))
        (if (and buffer (zerop (buffer-size buffer)))
            (setq result buffer)
          (setq number (1+ number)
                name-and-number (format "%s<%d>" name number)))))
    (or result (error "Too many scratch buffers"))))

(defun define-scratch--switch-to-buffer (name mode minor-modes)
  "Internal function to get, create, and switch to a scratch buffer.

NAME is a template for the buffer name.

MODE and MINOR-MODES are the modes to use in the buffer."
  (let ((buffer
         (let* ((home-directory
                 (expand-file-name
                  (file-name-as-directory "~")))
                (default-directory
                  home-directory))
           (if name
               (get-buffer-create name)
             (define-scratch--get-or-create-blank-buffer
               (define-scratch--buffer-base-name mode))))))
    (switch-to-buffer buffer)
    (unless (eq major-mode mode)
      (funcall mode)
      (dolist (minor-mode minor-modes)
        (funcall minor-mode)))
    buffer))

(defun define-scratch--interactive (mode)
  "Internal function to get interactive arg for scratch buffer command.

MODE is the major mode for the buffer."
  (list (and current-prefix-arg
             (read-from-minibuffer
              "Name of new scratch buffer: "
              (define-scratch--buffer-base-name mode)))))

(defmacro define-scratch (scratch-command mode &rest minor-modes)
  "Define SCRATCH-COMMAND as a command to create a scratch buffer.

MODE is the major mode that buffers created with this command will use.

MINOR-MODES axre extra minor modes (if any) to enable."
  `(defun ,scratch-command (&optional name)
     ,(format "Create a new scratch buffer in %s.

With a prefix argument, prompt for buffer NAME."
              mode)
     (interactive (define-scratch--interactive ',mode))
     (define-scratch--switch-to-buffer name ',mode ',minor-modes)))

(provide 'define-scratch)

;;; define-scratch.el ends here
