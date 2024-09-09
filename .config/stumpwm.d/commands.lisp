(require :stumpwm)
(in-package :stumpwm)

;;;;;;;;;;;;;;;;;;;;;
;; Enable modeline ;;
;;;;;;;;;;;;;;;;;;;;;
(defcommand enable-mode-line-on-all-screens () ()
  (dolist (screen *screen-list*)
    (dolist (head (screen-heads screen))
      (enable-mode-line screen head t))))

(defun external-monitor-enabled-p ()
  "Check if an external monitor is enabled."
  (let ((num-screens
          (parse-integer
            (run-prog-collect-output
             *shell-program* "-c" "xrandr | grep '*' | wc -l"))))
    (> num-screens 1)))

;;;;;;;;;;;;;;;;;;;;
;; Screen layouts ;;
;;;;;;;;;;;;;;;;;;;;
(defvar *screenlayout-dir* "~/.screenlayout/")

(defun hdmi-on () ()
  "Run the `hdmi-on.sh' script found in *screenlayout-dir*."
 (run-shell-command (concat *screenlayout-dir* "hdmi-on.sh"))
  (enable-mode-line-on-all-screens))

(defun hdmi-off () ()
  "Run the `hdmi-off.sh' script found in *screenlayout-dir*."
  (run-shell-command (concat *screenlayout-dir* "hdmi-off.sh")))

(defcommand toggle-external-monitor () ()
  (if (external-monitor-enabled-p)
      (hdmi-off) (hdmi-on)))

;;;;;;;;;;;;;;;;
;; Screenshot ;;
;;;;;;;;;;;;;;;;
(defcommand screenshot () ()
  "Run `screenshot' in a shell, if such a program exists."
  (if (member "screenshot" (programs-in-path) :test #'string-equal)
    (let ((output (run-shell-command "screenshot" t)))
      (format nil "Screenshot saved:~%~A" output))
    (err "No executable named `screenshot' found in path~%")))

;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard settings ;;
;;;;;;;;;;;;;;;;;;;;;;;
(defcommand set-keyboard-settings () ()
  "Setup my keyboard settings
This enables toggling the french canadian keyboard layout
with @key{s-SPC} and swap the @key{CAPS_LOCK} with the @key{TAB} keys."
  (run-shell-command
   "setxkbmap 'us,ca' -option ctrl:swapcaps -option grp:win_space_toggle"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy the URL from active browser window ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *browser-names* '("Firefox" "Chromium" "Google-chrome" "Brave-browser"))

(defun browser-window-p (window)
  (member (window-class window) *browser-names* :test #'string-equal))

(defcommand active-browser-url () ()
  "Copy a browser's current URL to the clipboard.
This does nothing if the current window's WM_CLASS property
is not a browser class, as defined by the `*browser-names*' list."
  (let ((window (current-window)))
    (when (and window (browser-window-p window))
      (run-shell-command
       (format nil "~{~A~^ && ~}"
               '("active_window_id=$(xdotool getactivewindow)"
                 "xdotool key F6"
                 "xdotool key alt+w"
                 "xdotool key Escape"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make LLM prompt with screenshot of active window ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcommand ss-prompt (prompt) ((:string "Prompt: "))
  "Makes a prompt to anthropic with a screenshot of active window."
  (run-shell-command (format nil "ss-prompt ~a" prompt) t))
