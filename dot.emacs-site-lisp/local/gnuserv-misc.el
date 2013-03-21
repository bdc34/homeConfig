
(defvar dv-initial-frame (car (frame-list)) "Holds initial frame.")

(defun dv-focus-frame (frame) "pop to top and give focus"
;; Code 'borrowed' from frame.el.
;; (Meaning I don't understand it. But it beats all I tried. :)
(make-frame-visible frame)
(raise-frame frame)
(select-frame frame)
(w32-focus-frame frame))

(defun dv-focus-initial-frame () "Make the initial frame visible"
(dv-focus-frame dv-initial-frame))

(defvar dv-mail-frames () "Frames created by dv-do-mailto")

(defun dv-do-mailto (arg) "For handling mailto URLs via gnudoit"
(dv-focus-frame (make-frame))
(message-mail (substring arg 7))
(delete-other-windows)
(setq dv-mail-frames (cons (selected-frame) dv-mail-frames)))

(defun dv-close-client-frame () "Close frame, kill client buffer."
(interactive)
(if (or (not (member (selected-frame) dv-mail-frames))
(and (> (length (buffer-name)) 4)
(equal (substring (buffer-name) 0 5) "*mail")
(not (buffer-modified-p))))
(kill-buffer (current-buffer)))
(setq dv-mail-frames (delete (selected-frame) dv-mail-frames))
(if (equal (selected-frame) dv-initial-frame)
(iconify-frame)
(delete-frame)))
(global-set-key [\M-f4] 'dv-close-client-frame)

(defun dv-paste-to-temp () "Load clipboard in a temp buffer"
(dv-focus-frame (make-frame))
(switch-to-buffer (generate-new-buffer "temp"))
(clipboard-yank))
;;end gnuserv stuff
