;;; websocket-bridge-grammarly.el --- Grammarly check using websocket-bridge  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'websocket-bridge)

(defgroup websocket-bridge-grammarly()
  "check grammar in buffers by grammarly."
  :group 'applications)

(defvar websocket-bridge-grammarly-py-path
  (concat
   (file-name-directory load-file-name)
   "websocket-bridge-grammarly.py"))

(defcustom websocket-bridge-grammarly-need-login t
  "If t, login grammarly using chrome cookie
If nil, don't login."
  :group 'websocket-bridge-grammarly
  :type 'boolean)


(defun websocket-bridge-grammarly-start ()
  "Start websocket bridge grammarly."
  (interactive)
  (websocket-bridge-app-start "grammarly" "python3" websocket-bridge-grammarly-py-path))


(defun websocket-bridge-grammarly-restart ()
  "Restart websocket bridge grammarly and show process."
  (interactive)
  (websocket-bridge-app-exit "grammarly")
  (websocket-bridge-grammarly-start)
  (websocket-bridge-app-open-buffer "grammarly"))

(defun websocket-bridge-grammarly-analyze-current-line()
  (interactive)
  (websocket-bridge-call-grammarly-on-current-line "analyze"))


(defun websocket-bridge-grammarly-analyze-buffer()
  (interactive)
  (websocket-bridge-call-grammarly-on-buffer "analyze"))

(defun websocket-bridge-grammarly-get-analyze-info()
  (interactive)
  (websocket-bridge-call-grammarly-on-buffer "getInfo"))


(defun websocket-bridge-grammarly-get-list-infos()
  (interactive)
  (websocket-bridge-call-grammarly-on-buffer "listInfos"))

(defun websocket-bridge-grammarly-analyze-current-line()
  (interactive)
  (websocket-bridge-call-grammarly-on-current-line "analyze"))

(defun websocket-bridge-grammarly-overlay-from(category begin end)
  (websocket-bridge-grammarly-from begin end))

(defun websocket-bridge-call-grammarly-on-current-line(func-name)
  "Call grammarly function on current line by FUNC-NAME."
  (websocket-bridge-call "grammarly" func-name
                         (thing-at-point 'line nil)
                         (- (point) (line-beginning-position))))

(defun websocket-bridge-call-grammarly-on-buffer (func-name)
  "Call grammarly function on current line by FUNC-NAME."
  (websocket-bridge-call "grammarly" func-name
                         (buffer-string)
                         (point)))


(defun websocket-bridge-grammarly-from (begin end)
  (let ((ov (make-overlay begin end)))
    (overlay-put ov 'face 'bold)))

(defun websocket-bridge-grammarly-line-from (begin end)
  (put-text-property begin end 'font-lock-face '(:foreground "red")))

(defun websocket-bridge-grammarly-render (html)
  (with-temp-buffer
    (insert html)
    (shr-render-buffer (current-buffer))))

(websocket-bridge-grammarly-start)
(provide 'websocket-bridge-grammarly)
;;; websocket-bridge-grammarly.el ends here
