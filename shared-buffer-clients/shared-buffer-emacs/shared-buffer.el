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

(require 'websocket)
(require 'cl-lib)
(require 'json)

(defvar sb-room nil
  "A buffer that is shared belongs to a room. This variable keeps
  an identifier to the room this buffer belongs to.")

(defvar sb-host nil
  "A buffer that is shared is connected to a server. This
  variable keeps the address of that server.")

(defvar sb-socket nil
  "A buffer that is shared is connected to a server. This
  variable keeps the connected socket.")

(defun sb-connect-to-server (host &optional room)
  (setq sb-host "ws://localhost:8080")
  (setq sb-socket
        (websocket-open sb-host
                        :on-message 'sb-receive
                        :on-open
                        (lambda (w) (sb-send (json-encode
                                         (list :type 'room :room sb-room))))
                        :on-close
                        (lambda (w) (message "SB: Connection closed!")))))

(defun sb-share-buffer (host &optional buffer)
  (interactive "sHost: ")
  (with-current-buffer (or buffer (current-buffer))
    (setf sb-socket (sb-connect-to-server host))
    (if sb-socket
        (message "Connected")
      (message "Connection failed."))))

(defun sb-join-room (host room &optional buffer)
  (interactive "sHost: \nsRoom: ")
  (setq sb-room room)
  (sb-share-buffer host buffer))

(defun sb-disconnect (&optional buffer)
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (websocket-close sb-socket)))

(defun sb-set-room (room)
  (setq sb-room room)
  (kill-new room)
  (message
   "Connected to room: %s, the key was added to the kill ring." room))

(defun sb-receive (websocket frame)
  (let* ((payload (websocket-frame-payload frame))
         (json-object-type 'plist)
         (data (json-read-from-string payload))
         (type (plist-get data :type)))
    (cond ((string= type "room")
           (sb-set-room (plist-get data :room))))))

(defun sb-send (data)
  (websocket-send-text sb-socket data))

;; (sb-send (json-encode '(:type "update")))
;; (sb-disconnect)
;; (sb-share-buffer 'asdf)
;; (process-list)
;; sb-room



;;; shared-buffer.el ends here.
