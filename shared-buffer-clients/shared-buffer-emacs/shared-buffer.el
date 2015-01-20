;;; shared-buffer.el --- Collaberative editing in Emacs. -*- lexical-binding: t -*-

;; Copyright (C) 2013 - 2014 Lars Tveito.

;; Author: Lars Tveito <larstvei@ifi.uio.no>
;; Version: 0.2.1
;; Package: shared-buffer
;; Package-Requires: ((cl-lib "1.0") (websocket "1.3"))

;; Contains code from GNU Emacs <https://www.gnu.org/software/emacs/>,
;; released under the GNU General Public License version 3 or later.

;; Shared buffer is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Shared buffer is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Shared buffer.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;; Requirements

(require 'websocket)
(require 'cl-lib)
(require 'json)

;;; Variables

(defvar-local sb-room-key nil
  "A buffer that is shared belongs to a room. This variable keeps an
  identifier to the room this buffer belongs to.")

(defvar-local sb-host nil
  "A buffer that is shared is connected to a server. This
  variable keeps the address of that server.")

(defvar-local sb-socket nil
  "A buffer that is shared is connected to a server. This
  variable keeps the connected socket.")

;; Changing major-mode should not affect Shared Buffer.
(dolist (var '(sb-room-key sb-host sb-socket))
  (put var 'permanent-local t))

;;; Socket communication

(defun sb-connect-to-server (host &optional room)
  (setq sb-host "ws://localhost:8080")
  (setq sb-socket (websocket-open sb-host
                                  :on-message 'sb-receive
                                  :on-open    'sb-on-open
                                  :on-close   'sb-on-close))
  (when sb-socket
    (setf (process-buffer (websocket-conn sb-socket))
          (current-buffer))))

(defun sb-on-open (websocket)
  (with-current-buffer (process-buffer (websocket-conn websocket))
    (sb-send (json-encode (list :type 'room :room sb-room-key)))))

(defun sb-on-close (websocket)
  (with-current-buffer (process-buffer (websocket-conn websocket))
    (message "Shared Buffer: Connection closed!")
    (sb-quit)))

;;; Send

(defun sb-send (data)
  (websocket-send-text sb-socket data))

(defun sb-send-addition (point string)
  (sb-send (json-encode (list :type'change
                              :key sb-room-key
                              :current-point (point)
                              :change-point point
                              :addition string))))

(defun sb-send-deletion (point del)
  (sb-send (json-encode (list :type'change
                              :key sb-room-key
                              :current-point (point)
                              :change-point point
                              :deletion del))))

;;; Receive

(defun sb-receive (websocket frame)
  (with-current-buffer (process-buffer (websocket-conn websocket))
    (let* ((payload (websocket-frame-payload frame))
           (json-object-type 'plist)
           (data (json-read-from-string payload))
           (type (plist-get data :type)))
      (cond ((string= type "room")
             (sb-set-room (plist-get data :room)))
            ((string= type "change")
             (sb-apply-change data))))))

(defun sb-set-room (room)
  (setq sb-room-key room)
  (sb-add-key-to-kill-ring)
  (message "Shared Buffer: \
Connected to room: %s, the key was added to the kill ring." room))

;;; Buffer modification

(defun sb-after-change (beg end del)
  "A function that is run after each change when
`shared-buffer-mode' is enabled. Each change is sent to `sb-host'
in order to keep the buffer synchronized."
  (if (zerop del)
      (sb-send-addition
       beg (buffer-substring-no-properties beg end))
    (sb-send-deletion beg del)))

(defun sb-apply-change (data)
  (cond ((plist-get data :addition) (sb-apply-addition data))
        ((plist-get data :deletion) (sb-apply-deletion data))
        (t 'error)))

(defun sb-apply-addition (data)
  (save-excursion
    (let ((inhibit-modification-hooks t)
          (point (plist-get data :change-point))
          (addition (plist-get data :addition)))
      (when (and point addition)
        (goto-char point)
        (insert addition)))))

(defun sb-apply-deletion (data)
  (save-excursion
    (let ((inhibit-modification-hooks t)
          (point (plist-get data :change-point))
          (deletion (plist-get data :deletion)))
      (when (and point deletion)
        (goto-char point)
        (delete-char deletion)))))

;;; Interactive functions

(defun sb-share-buffer (host &optional buffer)
  (interactive "sHost: ")
  (with-current-buffer (or buffer (current-buffer))
    (sb-connect-to-server host)
    (if (not sb-socket)
        (message "Shared Buffer: Connection failed.")
      (shared-buffer-mode 1))))

(defun sb-join-room (host room &optional buffer)
  (interactive "sHost: \nsRoom: ")
  (setq sb-room-key room)
  (sb-share-buffer host buffer))

(defun sb-disconnect (&optional buffer)
  (interactive)
  (when shared-buffer-mode
    (with-current-buffer (or buffer (current-buffer))
      (websocket-close sb-socket))
    (sb-quit)))

(defun sb-add-key-to-kill-ring ()
  (interactive)
  (kill-new sb-room-key))

;;; Minor mode

(defun sb-quit ()
  (dolist (var '(sb-room-key sb-host sb-socket))
    (kill-local-variable var))
  (shared-buffer-mode 0)
  (remove-hook 'after-change-functions 'sb-after-change))

(define-minor-mode shared-buffer-mode
  "A minor mode for Shared Buffer."
  nil " SB" nil
  ;; fixme
  (add-hook 'after-change-functions 'sb-after-change t t))

;; (sb-send (json-encode '(:type "update")))
;; (sb-disconnect)
;; (sb-share-buffer 'asdf)
;; (delete-process (car (process-list)))
;; sb-room-key



;;; shared-buffer.el ends here.
