;;; shared-buffer.el --- Collaberative editing in Emacs. -*- lexical-binding: t -*-

;; Copyright (C) 2013 - 2016 Lars Tveito.

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

(defvar-local sb-key nil
  "A buffer that is shared belongs to a room. This variable keeps an
  identifier to the room this buffer belongs to.")

(defvar-local sb-socket nil
  "A buffer that is shared is connected to a server. This
  variable keeps the connected socket.")

(defvar-local sb-seqno 0
  "A sequence number that is incremented with every interaction
  with the server.")

(defvar-local sb-token 0
  "A number that represents the last consistent state. It is
  updated when messages are received from the server.")

;; Changing major-mode should not affect Shared Buffer.
(dolist (var '(sb-key
               sb-socket
               sb-seqno
               sb-token
               shared-buffer-mode
               after-change-functions))
  (put var 'permanent-local t))

(defalias 'sb-substr 'buffer-substring-no-properties)

;;; Socket communication

(defun sb-connect-to-server (host &optional room)
  (let ((host (concat "ws://" (or host "localhost") ":3705")))
    (setq sb-socket (websocket-open host
                                    :on-message #'sb-receive
                                    :on-open    #'sb-on-open
                                    :on-close   #'sb-on-close))
    (when sb-socket
      (setf (process-buffer (websocket-conn sb-socket))
            (current-buffer)))))

(defun sb-on-open (websocket)
  (with-current-buffer (process-buffer (websocket-conn websocket))
    (sb-send (list :type 'room :room sb-key))))

(defun sb-on-close (websocket)
  (with-current-buffer (process-buffer (websocket-conn websocket))
    (message "Shared Buffer: Connection closed!")
    (sb-quit)))

;;; Send

(defun sb-send (data)
  (websocket-send-text sb-socket (json-encode data)))

(defun sb-send-entire-buffer ()
  (sb-send (list :type 'entire-buffer
                 :room sb-key
                 :pos (point-min)
                 :ins (sb-substr (point-min) (point-max))
                 :token sb-token
                 :seqno sb-seqno)))

(defun sb-send-insertion (beg end len)
  (and (zerop len)
       (sb-send (list :type 'operation
                      :room sb-key
                      :current-pos (point)
                      :pos beg
                      :ins (sb-substr beg end)
                      :token sb-token
                      :seqno (cl-incf sb-seqno)))))

(defun sb-send-deletion (beg end)
  (and (not (= beg end))
       (sb-send (list :type 'operation
                      :room sb-key
                      :current-pos (point)
                      :pos beg
                      :del (sb-substr beg end)
                      :token sb-token
                      :seqno (cl-incf sb-seqno)))))

;;; Receive

(defun sb-receive (websocket frame)
  (with-current-buffer (process-buffer (websocket-conn websocket))
    (let* ((payload (websocket-frame-payload frame))
           (json-object-type 'plist)
           (data (json-read-from-string payload))
           (type (plist-get data :type)))
      (cond ((string= type "room")
             (sb-set-room (plist-get data :room)))
            ((string= type "entire-buffer")
             (sb-entire-buffer data))
            ((string= type "operations")
             (sb-apply-operations data))
            (t (error "Shared Buffer: Error in protocol."))))))

(defun sb-entire-buffer (data)
  (if (and (plist-get data :pos)
           (plist-get data :ins))
      (sb-apply-operations data)
    (sb-send-entire-buffer)))

(defun sb-set-room (room)
  (setq sb-key room)
  (sb-add-key-to-kill-ring)
  (message "Shared Buffer: \
Connected to room: %s, the key was added to the kill ring." room))

;;; Buffer modification

(defun sb-apply-operations (data)
  (when (= sb-seqno (plist-get data :seqno))
    (cl-mapc 'sb-apply-operation (plist-get data :operations))
    (setq sb-token (plist-get data :token)))
  (cl-incf sb-seqno))

(defun sb-apply-operation (operation)
  (save-excursion
    (let ((inhibit-modification-hooks t)
          (point     (plist-get operation :pos))
          (insertion (plist-get operation :ins))
          (deletion  (plist-get operation :del)))
      (when (and insertion bytes-deleted)
        (error "Shared Buffer: Error in protocol."))
      (goto-char point)
      (when insertion (insert insertion))
      ;; use length, or rather just send number of chars to delete
      (when deletion  (delete-char (length deletion))))))

;;; Interactive functions

(defun sb-share-buffer (host &optional buffer)
  (interactive "sHost: ")
  (with-current-buffer (or buffer (current-buffer))
    (sb-connect-to-server (if (string= "" host) nil host))
    (if (not sb-socket)
        (message "Shared Buffer: Connection failed.")
      (shared-buffer-mode 1))))

(defun sb-join-room (host room)
  (interactive "sHost: \nsRoom: ")
  (let ((buffer (generate-new-buffer room)))
    (switch-to-buffer buffer)
    (setq sb-key room)
    (sb-share-buffer host buffer)))

(defun sb-disconnect (&optional buffer)
  (interactive)
  (when shared-buffer-mode
    (with-current-buffer (or buffer (current-buffer))
      (websocket-close sb-socket))
    (sb-quit)))

(defun sb-add-key-to-kill-ring ()
  (interactive)
  (kill-new sb-key))

;;; Minor mode

(defun sb-quit ()
  (mapc #'kill-local-variable
        '(sb-key sb-host sb-socket sb-seqno sb-token))
  (shared-buffer-mode 0)
  (remove-hook 'before-change-functions 'sb-send-deletion)
  (remove-hook 'after-change-functions 'sb-send-insertion))

(define-minor-mode shared-buffer-mode
  "A minor mode for Shared Buffer."
  nil " SB" nil
  ;; fixme
  (add-hook 'before-change-functions 'sb-send-deletion t t)
  (add-hook 'after-change-functions 'sb-send-insertion t t))

;;; shared-buffer.el ends here.
