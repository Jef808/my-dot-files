(require :stumpwm)
(in-package :stumpwm)

;; Enable modeline
(defun enable-mode-line-on-all-screens ()
  "Enable the mode line on all screens."
  (dolist (screen *screen-list*)
    (dolist (head (screen-heads screen))
      (enable-mode-line screen head t))))

(defun external-monitor-enabled-p ()
  "Check if an external monitor is enabled."
  (> (string-to-number
      (string-right-trim (run-shell-command "xrandr | grep '*' | wc -l" t)) 1)))

;; Screen layouts
(defcommand hdmi-on () ()
 (run-shell-command "/home/jfa/.screenlayout/hdmi-on.sh"))

(defcommand hdmi-off () ()
  (run-shell-command "/home/jfa/.screenlayout/hdmi-off.sh"))

(defcommand toggle-external-monitor () ()
  "Toggle the external monitor."
  (if (external-monitor-enabled-p)
      (run-shell-command "/home/jfa/.screenlayout/hdmi-off.sh")
      (progn
        (run-shell-command "/home/jfa/.screenlayout/hdmi-on.sh")
        (enable-mode-line-on-all-screens))))

(defun take-screenshot ()
  "Take a screenshot and save it as a PNG file."
  (let ((xwd-file (merge-pathnames "ss.xwd" (user-homedir-pathname)))
        (png-file (merge-pathnames "ss.png" (user-homedir-pathname))))
    (run-shell-command (format nil "xwd -out %f" (namestring xwd-file)))
    (loop until (probe-file xwd-file)
          do (sleep 1))
    (uiop:run-program (list "magick" (namestring xwd-file) (namestring png-file)))
    (delete-file xwd-file)
    (namestring png-file)))

(defcommand screenshot (window)
  ((:string "Enter the window name: "))
  (run-shell-command (concat "screenshot " window)))

(defcommand set-keyboard-settings () ()
  (run-shell-command "setxkbmap 'us,ca' -option ctrl:swapcaps -option grp:win_space_toggle"))

;(defcommand ellm (prompt &optional context id) ()
 ; (let* ((selection (run-shell-command "xclip -o t"))
  ;       (prompt (or prompt (emacs-everywhere:with-fallback 'stumpwm:emacs-everywhere-grab)))))(run-shell-command "prompt"))
