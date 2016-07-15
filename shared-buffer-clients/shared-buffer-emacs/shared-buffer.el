;;; shared-buffer.el --- Collaberative editing in Emacs. -*- lexical-binding: t -*-

;; Copyright (C) 2016 Lars Tveito.

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
  "A key that identifies the editing session.

For convenience `sb-add-key-to-kill-ring' copies the key to the
kill ring, so that you may share it.")

(defvar-local sb-socket nil
  "The socket used for communicating with the server.

The variable is set by `sb-connect-to-server'. ")

(defvar-local sb-seqno 0
  "A sequence number that is incremented at every interaction with the server.")

(defvar-local sb-token 0
  "A number that represents the last consistent state.

It is only updated when an operation is received.")

;; Changing major-mode should not affect Shared Buffer.
(dolist (var '(sb-key
               sb-socket
               sb-seqno
               sb-token
               shared-buffer-mode
               after-change-functions
               before-change-functions))
  (put var 'permanent-local t))

(defalias 'sb-substr 'buffer-substring-no-properties)

;;; Socket communication

(defun sb-connect-to-server (host)
  "Establishes connection with HOST, and binds `sb-socket'."
  (let ((host (concat "ws://" (or host "localhost") ":3705")))
    (setq sb-socket (websocket-open host
                                    :on-message #'sb-receive
                                    :on-open    #'sb-on-open
                                    :on-close   #'sb-on-close))
    (when sb-socket
      (setf (process-buffer (websocket-conn sb-socket))
            (current-buffer)))))

(defun sb-on-open (websocket)
  "This function is called when a connection to WEBSOCKET is established.

It sends an a request to connect to a session, specified by
`sb-key', and waits for a reply."
  (with-current-buffer (process-buffer (websocket-conn websocket))
    (sb-send (list :type 'connect :session sb-key))
    (with-local-quit
      ;; Block until first message is received
      (accept-process-output (websocket-conn websocket)))))

(defun sb-on-close (websocket)
  "Called when a connection to WEBSOCKET is closed."
  (with-current-buffer (process-buffer (websocket-conn websocket))
    (message "Shared Buffer: Connection closed!")
    (sb-quit)))

;;; Send

(defun sb-send (data)
  "Encode DATA to a JSON string and send it over `sb-socket'."
  (websocket-send-text sb-socket (json-encode data)))

(defun sb-send-buffer ()
  "Send an insertion that includes the entire buffer."
  (sb-send (list :type 'buffer
                 :session sb-key
                 :pos (1- (point-min))
                 :ins (sb-substr (point-min) (point-max))
                 :token sb-token)))

(defun sb-send-operation (type beg end)
  "Send an operation.

TYPE specifies if it is a deletion or insertion, whilst BEG and
END specifies what part of the buffer is sent."
  (sb-send (list :type 'operation
                 :session sb-key
                 type (sb-substr beg end)
                 :pos (1- beg)
                 :current-pos (1- (point))
                 :token sb-token
                 :seqno (1- (cl-incf sb-seqno)))))

(defun sb-send-insertion (beg end len)
  "Send an insertion.

Called after every change, where BEG and END specifies the part
of the buffer which is changed. If LEN is zero then the change
was a deletion, and nothing is sent."
  (let ((inhibit-modification-hooks nil))
    (when (zerop len) (sb-send-operation :ins beg end))))

(defun sb-send-deletion (beg end)
  "Send an insertion.

Called before every change, where BEG and END specifies the part
of the buffer which is about to be changed. If BEG is equal to
END, then the change was a deletion, and nothing is sent."
  (let ((inhibit-modification-hooks nil))
    (when (not (= beg end)) (sb-send-operation :del beg end))))

;;; Receive

(defun sb-receive (websocket frame)
  "Receive a message on WEBSOCKET.

The message is inside the FRAME. Every message has a type, which
is either \"connect\", \"send-buffer\" or \"operations\"."
  (with-current-buffer (process-buffer (websocket-conn websocket))
    (let* ((payload (websocket-frame-payload frame))
           (json-object-type 'plist)
           (json-array-type 'list)
           (data (json-read-from-string payload))
           (type (plist-get data :type)))
      (cond ((string= type "connect")
             (sb-set-session (plist-get data :session)))
            ((string= type "send-buffer")
             (sb-send-buffer))
            ((string= type "operations")
             (sb-apply-operations data))
            (t (error "Shared Buffer: Error in protocol"))))))

(defun sb-set-session (session)
  "Change the editing SESSION.

The editing session is only set if the connection was established
using `sb-share-buffer'."
  (setq sb-key session)
  (sb-add-key-to-kill-ring)
  (message "Shared Buffer: \
Connected to session: %s, the key was added to the kill ring." session))

;;; Buffer modification

(defun sb-apply-operations (data)
  "Apply operations received from the server.

`sb-token' is set, and `sb-seqno' is incremented.
Argument DATA is a plist with fields :seqno, :operations
and :token."
  (when (= sb-seqno (plist-get data :seqno))
    (cl-mapc 'sb-apply-operation (plist-get data :operations))
    (setq sb-token (plist-get data :token)))
  (cl-incf sb-seqno))

(defun sb-apply-operation (operation)
  "Apply a given OPERATION.

The operation is either an insertion or a deletion at a given
position."
  (save-excursion
    (let ((inhibit-modification-hooks t)
          (point     (1+ (plist-get operation :pos)))
          (insertion (plist-get operation :ins))
          (deletion  (plist-get operation :del)))
      (when (and insertion deletion)
        (error "Shared Buffer: Error in protocol"))
      (goto-char point)
      (when insertion (insert insertion))
      (when deletion  (delete-char (length deletion))))))

;;; Interactive functions

(defun sb-share-buffer (host &optional buffer)
  "Share the current buffer to enable real-time collaboration.

HOST specifies the host address of the server. A key will be
generated by the server, and added to the kill ring when
received. This key can be shared so that other can join your
editing session. Use `sb-join-session' to connect to an existing
editing session.

If called non-interactively, a BUFFER may be specified."
  (interactive "sHost: ")
  (with-current-buffer (or buffer (current-buffer))
    (sb-connect-to-server (if (string= "" host) nil host))
    (if (not sb-socket)
        (message "Shared Buffer: Connection failed.")
      (shared-buffer-mode 1))))

(defun sb-join-session (host session)
  "Connect to HOST to join SESSION.

Like `sb-share-buffer', except that a new buffer with the name
SESSION is created with the contents of the shared buffer."
  (interactive "sHost: \nsSession: ")
  (let ((buffer (generate-new-buffer session)))
    (switch-to-buffer buffer)
    (setq sb-key session)
    (sb-share-buffer host buffer)))

(defun sb-disconnect (&optional buffer)
  "Disconnect the current shared buffer session.

If called non-interactively a BUFFER may be specified."
  (interactive)
  (when shared-buffer-mode
    (with-current-buffer (or buffer (current-buffer))
      (websocket-close sb-socket))
    (sb-quit)))

(defun sb-add-key-to-kill-ring ()
  "Add the session key to the kill ring, so you may share it."
  (interactive)
  (kill-new sb-key))

;;; Minor mode

(defun sb-quit ()
  "Disable `shared-buffer-mode' and clean up."
  (mapc #'kill-local-variable
        '(sb-key sb-host sb-socket sb-seqno sb-token))
  (shared-buffer-mode 0)
  (remove-hook 'before-change-functions 'sb-send-deletion)
  (remove-hook 'after-change-functions 'sb-send-insertion))

(define-minor-mode shared-buffer-mode
  "A minor mode for Shared Buffer."
  nil " SB" nil
  (add-hook 'before-change-functions 'sb-send-deletion t t)
  (add-hook 'after-change-functions  'sb-send-insertion t t))

(provide 'shared-buffer)

;;; shared-buffer.el ends here
