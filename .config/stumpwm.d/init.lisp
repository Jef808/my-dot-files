;; -*-lisp-*-/
;;
;;; stumpwm config

(in-package :stumpwm)

;; The 'leader-key'
(set-prefix-key (kbd "M-TAB"))

;; (stumpwm:unbind (stumpwm:kbd "c"))

;; Application launch map
(defvar *launch-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "b") "exec qutebrowser")
    (define-key m (kbd "f") "exec firefox")
    (define-key m (kbd "e") "exec emacs")
    (define-key m (kbd "c") "exec alacritty")
    (define-key m (kbd "d") "exec dmenu_run -fn 'JetBrains Sans Mono-18'")
    m
  ))
(define-key *root-map* (kbd "SPC") '*launch-map*)

;; Redefine keys to behave emacs-like when focused window
;; has class firefox or Chrome (redirects firefox's keys)
;; (define-remapped-keys
;;   (("(firefox|Chrome)"
;;      ("C-n"   . "Down")
;;      ("C-p"   . "Up")
;;      ("C-f"   . "Right")
;;      ("C-b"   . "Left")
;;      ("C-v"   . "Next")
;;      ("M-v"   . "Prior")
;;      ("M-w"   . "C-c")
;;      ("C-w"   . "C-x")
;;      ("C-y"   . "C-v")
;;      ("M-<"   . "Home")
;;      ("M->"   . "End")
;;      ("C-s"   . "C-f")
;;      ("C-g"   . "ESC")
;;      ("C-M-b" . "M-Left")
;;      ("C-M-f" . "M-Right")
;;      ("M-f"   . "C-Right")
;;      ("M-b"   . "C-Left")
;;      ("C-M-k" . "C-w")
;;      ("C-k"   . "C-q"))))

;; If circumventing the above remapping is needed
;; e.g. when setting up keybindings on the browser
;; (defcommand toggle-remapped-keys () ()
;;   ((lambda (var) (setf var (not var)))
;;    *REMAPPED-KEYS-ENABLED-P*))

;; Window navigation
(define-key *root-map* (kbd "k")   "move-focus up")
(define-key *root-map* (kbd "j")   "move-focus down")
(define-key *root-map* (kbd "h")   "move-focus left")
(define-key *root-map* (kbd "l")   "move-focus right")
(define-key *root-map* (kbd "M-k") "move-window up")
(define-key *root-map* (kbd "M-j") "move-window down")
(define-key *root-map* (kbd "M-h") "move-window left")
(define-key *root-map* (kbd "M-l") "move-window right")

;; Help
(define-key *root-map* (kbd "d")  '*help-map*)

;; Message window
(setf *data-dir* "/home/jfa/.local/share/stumpwm")

;; Modeline formatting
(setf *mode-line-pad-x* 5
      *mode-line-pad-y* 8)
(setf *window-format* "%m%n%s%c")
(setf *screen-mode-line-format*
      "[^B%n^b] %v^>%d   ")

;; Message window font
;; NOTE: Doesn't use fontconfig, need to make sure Xorg knows the font
;; can do that with mkfontdir <fontpath> && xset +fp <fontpath>, then
;; get the available XLFD (X Logical Font Description) string with xlsfonts
;; or xfontsel
(set-font "-xos4-terminus-bold-r-normal-*-18-*-*-*-*-*-*-*")

;; Input and Message window placements
(setf *message-window-gravity* :top)
(setf *message-window-input-gravity* :top)
(setf *input-window-gravity* :top)


;; Enable modeline
(enable-mode-line (stumpwm:current-screen) (stumpwm:current-head) t)

;; Commands for custom screenlayout
(defcommand hdmi-on () ()
  (run-shell-command "hdmi-on.sh"))
(defcommand hdmi-off () ()
  (run-shell-command "hdmi-off.sh"))
(defcommand mishascreen-on () ()
  (run-shell-command "mishascreen-on.sh"))
(defcommand mishascreen-off () ()
  (run-shell-command "mishascreen-off.sh"))
(defcommand screenshot (window)
  ((:string "Enter the window name: "))
  (run-shell-command (concat "screenshot.sh " window)))

;; emacs everywhere
(defcommand emacs-everywhere () ()
  (run-shell-command "emacsclient --eval '(emacs-everywhere)'"))
(define-key *root-map* (kbd "E") "emacs-everywhere")

;; Custom commands
(defun kill-windows-by-title-containing (substr)
  (labels ((match (win)
             (search substr (window-title win))))
    (mapcar #'delete-window
            (remove-if-not #'match (group-windows (current-group))))))
(defcommand kill-qutebrowser-windows () ()
  (kill-windows-by-title-containing "qutebrowser"))
(define-key *root-map* (kbd "P") "kill-all-qutebrowser-windows")

;; swank server
(require :swank)
(swank-loader:init)
(swank:create-server :port 4004
                     :style swank:*communication-style*
                     :dont-close t)
