;;; docker-compose.el --- Emacs interface to docker-compose

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'docker-process)
(require 'docker-utils)
(require 'tablist)

(require 'magit-popup)

(defgroup docker-compose nil
  "docker-compose customization group."
  :group 'docker)

(defcustom docker-compose-keymap-prefix "C-c C"
  "Prefix keymap for `docker-compose-mode'."
  :group 'docker-compose)

(defun docker-compose (action &rest args)
  "Execute docker ACTION passing arguments ARGS."
  (let ((command (format "docker-compose %s %s" action (s-join " " (-non-nil args)))))
    (message command)
    (shell-command-to-string command)))

(defun docker-compose-entries ()
  "Returns the docker compose data for `tabulated-list-entries'."
  (let* ((data (docker-compose "ps"))
         (lines (-slice (s-split "\n" data t) 3)))
    (-map #'docker-compose-parse lines)))

(defun docker-compose-parse (line)
  "Convert a LINE from \"docker-compose ps\" to a `tabulated-list-entries' entry."
  (s-split " \\{3,\\}" line))

(defun docker-compose-services ()
  "Return the list of services."
  (append '("(all)") (s-split "\n" (docker-compose "config" "--services") t)))

(defun docker-compose-read-service-name (prompt)
  "Read an service name using PROMPT."
  (let ((service (completing-read prompt (docker-compose-services))))
    (when (string-equal service "(all)")
      (setq service nil))
    service))

;;;###autoload
(defun docker-compose-build (service &optional pull no-cache force-rm)
  "docker-compose build."
  (interactive (list (docker-compose-read-service-name "Build service: ") current-prefix-arg))
  (docker-compose "build" (when pull "--pull ") (when no-cache "--no-cache ") (when pull "--pull ") (when service service)))

;;;###autoload
(defun docker-compose-up (service &optional do-not-daemonize)
  "docker-compose up."
  (interactive (list (docker-compose-read-service-name "Up service: ") current-prefix-arg))
  (docker-compose "up" (unless do-not-daemonize "-d ") (when service service)))

;;;###autoload
(defun docker-compose-start (service)
  "docker-compose start."
  (interactive (list (docker-compose-read-service-name "Start service: ")))
  (docker-compose "start" (when service service)))

;;;###autoload
(defun docker-compose-stop (service)
  "docker-compose stop."
  (interactive (list (docker-compose-read-service-name "Stop service: ")))
  (docker-compose "stop" (when service service)))

;;;###autoload
(defun docker-compose-restart (service)
  "docker-compose restart."
  (interactive (list (docker-compose-read-service-name "Restart service: ")))
  (docker-compose "restart" (when service service)))

;;;###autoload
(defun docker-compose-rm (service)
  "docker-compose rm."
  (interactive (list (docker-compose-read-service-name "Delete service: ")))
  (docker-compose "rm" "-f" (when service service)))

(defvar docker-compose-map
  (let ((map (make-sparse-keymap)))
    (define-key map "b" 'docker-compose-build)
    (define-key map "u" 'docker-compose-up)
    (define-key map "s" 'docker-compose-start)
    (define-key map "o" 'docker-compose-stop)
    (define-key map "d" 'docker-compose-rm)
    (define-key map "r" 'docker-compose-restart)
    map)
  "Keymap for `docker-compose-mode' after `docker-compose-keymap-prefix' was pressed.")

(defvar docker-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd docker-compose-keymap-prefix) docker-compose-map)
    map)
  "Keymap for `docker-compose-mode'.")

;;;###autoload
(define-minor-mode docker-compose-mode
  "Minor mode to manage docker-compose."
  nil
  " docker-compose"
  docker-compose-mode-map
  :group 'docker-compose)

;;;###autoload
(define-globalized-minor-mode docker-compose-global-mode
  docker-compose-mode
  docker-compose-mode)

(provide 'docker-compose)

;;; docker-compose.el ends here
