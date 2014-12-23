;;; shared-buffer.el --- Collaberative editing in Emacs.

;; Copyright (C) 2013 - 2014 Lars Tveito.

;; Author: Lars Tveito <larstvei@ifi.uio.no>
;; Version: 0.2.1
;; Package: shared-buffer
;; Package-Requires: ((websocket "1.3"))

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

(defvar sb-socket nil
  "A buffer-local reference to the connection.")

(defun sb-connect-to-server (host buffer)
  (websocket-open "ws://localhost:8080" :on-message 'sb-receive))

(defun sb-share-buffer (host &optional buffer)
  (interactive "sHost: ")
  (setq sb-socket (sb-connect-to-server host (or buffer (current-buffer))))
  (if sb-socket
      (message "Connected!")
    (message "Connection failed.")))

(defun sb-disconnect (&optional buffer)
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (websocket-close sb-socket)))

(defun sb-receive (websocket frame)
  (print (websocket-frame-payload frame)))

(defun sb-send (data)
  (websocket-send-text sb-socket data))

;; (sb-send "asdfasdf")
;; (sb-disconnect)


;;; shared-buffer.el ends here.
