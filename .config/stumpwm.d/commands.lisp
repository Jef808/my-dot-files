(require :stumpwm)
(in-package :stumpwm)

(defvar *screenlayout-dir* "~/.screenlayout/")

;; Enable modeline
(defun enable-mode-line-on-all-screens ()
  "Enable the mode line on all screens."
  (dolist (screen *screen-list*)
    (dolist (head (screen-heads screen))
      (enable-mode-line screen head nil))))

(defun external-monitor-enabled-p ()
  "Check if an external monitor is enabled."
  (let ((other-screen
         (string-right-trim (run-shell-command "xrandr | grep '*' | wc -l" t))))
    (> (string-to-number other-screen) 1)))

;; Screen layouts
(defcommand hdmi-on () ()
 (run-shell-command (concat *screenlayout-dir* "hdmi-on.sh"))
  (enable-mode-line-on-all-screens))

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

(defvar *browser-names* '("Firefox" "Chromium" "Google-chrome" "Brave-browser"))

(defun browser-window-p (window)
  (member (window-class window) *browser-names* :test #'string-equal))

(defcommand active-browser-url () ()
  (let ((window (current-window)))
    (when (and window (browser-window-p window))
      (run-shell-command "active-browser-url" t))))

(defcommand my/active-browser-url () ()
  (let ((windowname (window-class (current-window))))
    (message windowname)
    (when (member windowname *browser-names*)
      (run-shell-command "xdotool key F6")
      (get-x-selection 0.2 '(:primary)))))


;(defcommand ellm (prompt &optional context id) ()
 ; (let* ((selection (run-shell-command "xclip -o t"))
  ;       (prompt (or prompt (emacs-everywhere:with-fallback 'stumpwm:emacs-everywhere-grab)))))(run-shell-command "prompt"))
