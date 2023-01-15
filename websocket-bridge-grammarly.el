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

(defgroup
  websocket-bridge-grammarly()
  "Check grammar in buffers by grammarly."
  :group 'applications)

(defvar websocket-bridge-grammarly-py-path
  (concat
   (file-name-directory load-file-name)
   "websocket_bridge_grammarly.py"))

(defun random-color ()
  "Generate a random color."
  (let* ((colors (ns-list-colors))
         (random-num (random (length colors))))
    (nth random-num colors)))

(defvar websocket-bridge-grammarly-faces
  #s(hash-table
     test equal
     data ("PassiveVoice" '(:underline (:color "gray" :style "wave"))
           "SentenceVariety" '(:underline (:color "gray" :style "wave")))))

(defvar grammarly-overlay-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") #'websocket-bridge-grammarly-get-details)
    map)
  "Keymap automatically activated inside overlays.
You can re-bind the commands to any keys you prefer.")

(defcustom websocket-bridge-grammarly-need-login nil
  "If t, login grammarly using chrome cookie.
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
  "Grammarly analyze current line."
  (interactive)
  (websocket-bridge-call-grammarly-on-current-line "analyze"))


(defun websocket-bridge-grammarly-analyze-buffer()
  "Analyze current buffer by Grammarly."
  (interactive)
  (remove-overlays)
  (websocket-bridge-call-grammarly-on-buffer "analyze"))

(defun websocket-bridge-grammarly-get-details()
  "Get Grammarly analyzed details."
  (interactive)
  (websocket-bridge-call-grammarly-on-buffer "get_details"))

(defun websocket-bridge-grammarly-refine()
  "Get Grammarly analyzed details."
  (interactive)
  (websocket-bridge-call-grammarly-on-buffer "refine"))

(defun websocket-bridge-grammarly-list-all()
  "List all Grammarly resutl."
  (interactive)
  (websocket-bridge-call-grammarly-on-buffer "list_all"))

(defun websocket-bridge-grammarly-analyze-current-line()
  "Grammarly analyze current line."
  (interactive)
  (websocket-bridge-call-grammarly-on-current-line "analyze"))

(defun websocket-bridge-grammarly-overlay-from(category begin end &optional transform)
  "Add overlay from BEGIN to END, different CATEGORY diferent face."
  (print category)
  (websocket-bridge-grammarly-from category begin end transform))

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

(defun grammarly-overlay-modify-hook(overlay after begin end)
  (delete-overlay overlay))


(defun websocket-bridge-grammarly-from (category begin end &optional transform)
  "Add overlay from BEGIN to END, different CATEGORY diferent face."
  (when (not (gethash category websocket-bridge-grammarly-faces))
    (puthash category
             (list :strike-through "red")
             websocket-bridge-grammarly-faces))
  (let ((ov (make-overlay begin end)))
    (overlay-put ov 'modification-hooks
                 '(grammarly-overlay-modify-hook))
    (overlay-put ov 'keymap grammarly-overlay-keymap)
    (overlay-put ov 'face
                 (gethash category websocket-bridge-grammarly-faces))
    (when transform
      (overlay-put ov 'after-string (format "(%s)" transform)))))

(defun websocket-bridge-grammarly-render (html)
  "Called by python, to render HTML.
HTML is the Grammarly resutl."
  (pop-to-buffer "*Grammaly Details*")
  (setq buffer-read-only nil)
  (erase-buffer)
  (shr-insert-document
   (with-temp-buffer
     (insert html)
     (libxml-parse-html-region (point-min) (point-max))))
  (goto-char (point-min))
  (special-mode))

(provide 'websocket-bridge-grammarly)
;;; websocket-bridge-grammarly.el ends here
