;; -*-lisp-*-/
;;
;;; stumpwm config

(in-package :stumpwm)

;; The 'leader-key'
(set-prefix-key (kbd "M-TAB"))

;;(stumpwm:unbind (stumpwm:ksb "c"))

;; Application launch map
(defvar *launch-map*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "f") "exec firefox")
    (stumpwm:define-key m (stumpwm:kbd "e") "exec emacs")
    (stumpwm:define-key m (stumpwm:kbd "c") "exec alacritty")
    m
  ))
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "SPC") '*launch-map*)

;; Redefine keys to behave emacs-like when focused window
;; has class firefox or Chrome (redirects firefox's keys)
(define-remapped-keys
  '(("(firefox|Chrome)"
     ("C-n"   . "Down")
     ("C-p"   . "Up")
     ("C-f"   . "Right")
     ("C-b"   . "Left")
     ("C-v"   . "Next")
     ("M-v"   . "Prior")
     ("M-w"   . "C-c")
     ("C-w"   . "C-x")
     ("C-y"   . "C-v")
     ("M-<"   . "Home")
     ("M->"   . "End")
     ("C-s"   . "C-f")
     ("C-g"   . "ESC")
     ("C-M-b" . "M-Left")
     ("C-M-f" . "M-Right")
     ("M-f"   . "C-Right")
     ("M-b"   . "C-Left")
     ("C-M-k" . "C-w")
     ("C-k"   . ("C-S-End" "C-x")))))

;; If circumventing the above remapping is needed
;; e.g. when setting up keybindings on the browser
(defcommand toggle-remapped-keys () ()
  ((lambda (var) (setf var (not var)))
   *REMAPPED-KEYS-ENABLED-P*))

;; Window navigation
(define-key *root-map* (kbd "k")   "move-focus up")
(define-key *root-map* (kbd "j")   "move-focus down")
(define-key *root-map* (kbd "h")   "move-focus left")
(define-key *root-map* (kbd "l")   "move-focus right")
(define-key *root-map* (kbd "M-k") "move-window up")
(define-key *root-map* (kbd "M-j") "move-window down")
(define-key *root-map* (kbd "M-h") "move-window left")
(define-key *root-map* (kbd "M-l") "move-window right")

;; Modeline formatting
(setf *mode-line-pad-x* 5
      *mode-line-pad-y* 8)
(setf *window-format* "%m%n%s%c")
(setf *screen-mode-line-format*
      "[^B%n^b] %v^>%d   ")

;; NOTE: Doesn't use fontconfig, need to make sure Xorg knows the font
;; can do that with mkfontdir <fontpath> && xset +fp <fontpath>, then
;; get the available XLFD (X Logical Font Description) string with xlsfonts
;; or xfontsel
(set-font "-xos4-terminus-bold-r-normal-*-18-*-*-*-*-*-*-*")
;; Enable modeline
(stumpwm:enable-mode-line (stumpwm:current-screen) (stumpwm:current-head) t)

;; Commands for custom screenlayout
(defcommand hdmi-on () ()
  (run-shell-command "hdmi-on.sh"))
(defcommand hdmi-off () ()
  (run-shell-command "hdmi-off.sh"))
