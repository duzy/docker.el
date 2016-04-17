;;; docker-containers.el --- Emacs interface to docker-containers

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

(require 'docker-api-containers)
(require 'docker-process)
(require 'docker-utils)
(require 'tablist)

(require 'magit-popup)

(defun docker-container-name (data)
  "Return the container name from DATA."
  (s-chop-prefix "/" (car (assoc-default 'Names data))))

(defun docker-container-created-at (data)
  "Return the container creation date from DATA."
  (prin1-to-string (assoc-default 'Created data)))

(defun docker-container-port (data)
  "Return the container creation date from DATA."
  (prin1-to-string (assoc-default 'Ports data)))

(defun docker-container-to-tabulated-list-entry (data)
  "Convert a docker container to tabulated list entry."
  (vconcat
   (--map-when (null it) ""
               (list
                (assoc-default 'Id data)
                (assoc-default 'Image data)
                (assoc-default 'Command data)
                (docker-container-created-at data)
                (assoc-default 'Status data)
                (docker-container-port data)
                (docker-container-name data)))))

(defun docker-container-to-tabulated-list (data)
  "Convert a docker container to tabulated list."
  (list (assoc-default 'Id data)
        (docker-container-to-tabulated-list-entry data)))

(defun docker-container-names ()
  "Return the list of container names."
  (-map 'docker-container-name (docker-api-containers t)))

(defun docker-read-container-name (prompt)
  "Read an container name."
  (completing-read prompt (docker-container-names)))

(defun docker-start (name)
  "Start a container."
  (interactive (list (docker-read-container-name "Start container: ")))
  (docker "start" name))

(defun docker-stop (name &optional timeout)
  "Stop a container."
  (interactive (list (docker-read-container-name "Stop container: ") current-prefix-arg))
  (docker "stop" (when timeout (format "-t %d" timeout)) name))

(defun docker-restart (name &optional timeout)
  "Restart a container."
  (interactive (list (docker-read-container-name "Restart container: ") current-prefix-arg))
  (docker "restart" (when timeout (format "-t %d" timeout)) name))

(defun docker-pause (name)
  "Pause a container."
  (interactive (list (docker-read-container-name "Pause container: ")))
  (docker "pause" name))

(defun docker-unpause (name)
  "Unpause a container."
  (interactive (list (docker-read-container-name "Unpause container: ")))
  (docker "unpause" name))

(defun docker-rm (name &optional force)
  "Destroy or uncommand an container."
  (interactive (list (docker-read-container-name "Delete container: ") current-prefix-arg))
  (docker "rm" (when force "-f") name))

(defun docker-inspect (name)
  "Inspect a container."
  (interactive (list (docker-read-container-name "Inspect container: ")))
  (docker "inspect" name))

(defun docker-containers-selection ()
  "Get the containers selection as a list of ids."
  (-map 'car (docker-utils-get-marked-items)))

(defun docker-containers-run-command-on-selection (command arguments)
  "Run a docker COMMAND on the containers selection with ARGUMENTS."
  (interactive "sCommand: \nsArguments: ")
  (--each (docker-containers-selection)
    (docker command arguments it))
  (tabulated-list-revert))

(defun docker-containers-run-command-on-selection-print (command arguments)
  "Run a docker COMMAND on the containers selection with ARGUMENTS and print"
  (interactive "sCommand: \nsArguments: ")
  (let ((buffer (get-buffer-create "*docker result*")))
    (with-current-buffer buffer
      (erase-buffer))
    (--each (docker-containers-selection)
      (let ((result (docker command arguments it)))
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert result))))
    (display-buffer buffer)))

(defmacro docker-containers-create-selection-functions (&rest functions)
  `(progn ,@(--map
             `(defun ,(intern (format "docker-containers-%s-selection" it)) ()
                ,(format "Run `docker-%s' on the containers selection." it)
                (interactive)
                (docker-containers-run-command-on-selection ,(symbol-name it)
                                                            (s-join " " ,(list (intern (format "docker-containers-%s-arguments" it))))))
             functions)))

(defmacro docker-containers-create-selection-print-functions (&rest functions)
  `(progn ,@(--map
             `(defun ,(intern (format "docker-containers-%s-selection" it)) ()
                ,(format "Run `docker-%s' on the containers selection." it)
                (interactive)
                (docker-containers-run-command-on-selection-print ,(symbol-name it)
                                                                  (s-join " " ,(list (intern (format "docker-containers-%s-arguments" it))))))
             functions)))

(docker-containers-create-selection-functions start stop restart pause unpause rm)

(docker-containers-create-selection-print-functions inspect logs)

(docker-utils-define-popup docker-containers-inspect-popup
  "Popup for inspecting containers."
  'docker-containers-popups
  :man-page "docker-inspect"
  :actions  '((?I "Inspect" docker-containers-inspect-selection)))

(docker-utils-define-popup docker-containers-logs-popup
  "Popup for showing containers logs."
  'docker-containers-popups
  :man-page "docker-logs"
  :actions  '((?L "Logs" docker-containers-logs-selection)))

(docker-utils-define-popup docker-containers-start-popup
  "Popup for starting containers."
  'docker-containers-popups
  :man-page "docker-start"
  :actions  '((?S "Start" docker-containers-start-selection)))

(docker-utils-define-popup docker-containers-stop-popup
  "Popup for stoping containers."
  'docker-containers-popups
  :man-page "docker-stop"
  :options '((?t "Timeout" "-t "))
  :actions '((?O "Stop" docker-containers-stop-selection)))

(docker-utils-define-popup docker-containers-restart-popup
  "Popup for restarting containers."
  'docker-containers-popups
  :man-page "docker-restart"
  :options '((?t "Timeout" "-t "))
  :actions '((?R "Restart" docker-containers-restart-selection)))

(docker-utils-define-popup docker-containers-pause-popup
  "Popup for pauseing containers."
  'docker-containers-popups
  :man-page "docker-pause"
  :actions  '((?P "Pause" docker-containers-pause-selection)
              (?U "Unpause" docker-containers-unpause-selection)))

(docker-utils-define-popup docker-containers-rm-popup
  "Popup for removing containers."
  'docker-containers-popups
  :man-page "docker-rm"
  :switches '((?f "Force" "-f")
              (?v "Volumes" "-v"))
  :actions  '((?D "Remove" docker-containers-rm-selection)))

(defun docker-containers-refresh ()
  "Refresh the containers list."
  (setq tabulated-list-entries (-map 'docker-container-to-tabulated-list (docker-api-containers t))))

(defvar docker-containers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "I" 'docker-containers-inspect-popup)
    (define-key map "L" 'docker-containers-logs-popup)
    (define-key map "S" 'docker-containers-start-popup)
    (define-key map "O" 'docker-containers-stop-popup)
    (define-key map "R" 'docker-containers-restart-popup)
    (define-key map "P" 'docker-containers-pause-popup)
    (define-key map "D" 'docker-containers-rm-popup)
    map)
  "Keymap for `docker-containers-mode'.")

;;;###autoload
(defun docker-containers ()
  "List docker containers."
  (interactive)
  (pop-to-buffer "*docker-containers*")
  (docker-containers-mode)
  (docker-containers-refresh)
  (tabulated-list-revert))

(defalias 'docker-ps 'docker-containers)

(define-derived-mode docker-containers-mode tabulated-list-mode "Containers Menu"
  "Major mode for handling a list of docker containers."
  (setq tabulated-list-format [("Id" 16 t)("Image" 15 t)("Command" 30 t)("Created" 15 t)("Status" 20 t)("Ports" 10 t)("Names" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Image" nil))
  (add-hook 'tabulated-list-revert-hook 'docker-containers-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-containers)

;;; docker-containers.el ends here
