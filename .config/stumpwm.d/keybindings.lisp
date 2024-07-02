(in-package :stumpwm)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top keybinds        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Everywhere
;(defcommand emacs-everywhere () ()
;  (run-shell-command "emacsclient --eval '(emacs-everywhere)'"))
;(define-key *root-map* (kbd "E") "emacs-everywhere")

;; Execute shell commands using rofi
(define-key *root-map* (kbd "!") "exec rofi -show run -font 'Fira Code -18'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Leader Map keybindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-prefix-key (kbd "M-TAB"))

 ;; Global keybindings
(define-key *root-map* (kbd "TAB") "abort")
(define-key *root-map* (kbd "C-g") "abort")
 ;; Window navigation
(define-key *root-map* (kbd "k")   "move-focus  up")
(define-key *root-map* (kbd "j")   "move-focus  down")
(define-key *root-map* (kbd "h")   "move-focus  left")
(define-key *root-map* (kbd "l")   "move-focus  right")
;; Window displacement
(define-key *root-map* (kbd "M-k") "move-window up")
(define-key *root-map* (kbd "M-j") "move-window down")
(define-key *root-map* (kbd "M-h") "move-window left")
(define-key *root-map* (kbd "M-l") "move-window right")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Launch map             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *chrome-window-count* 0
  "Counter for Chrome windows launched.")

(defun make-chrome-cmd (new-window)
  "Return a command to launch Chrome with a new window or not."
  (let ((cmd (format nil "exec systemd-run --user --unit=chrome~A /usr/bin/google-chrome-stable ~A --remote-debugging-port=9222 --no-first-run --user-data-dir=/home/jfa/.config/google-chrome --profile-directory=Default"
                     *chrome-window-count*
                     (if new-window "--new-window"))))
    (incf *chrome-window-count*)
    cmd))

(defvar *exec-emacs*              "exec emacs --init-directory=/home/jfa/.config/emacs")
(defvar *exec-chrome*             #.(make-chrome-cmd nil))
(defvar *exec-chrome-nw*          #.(make-chrome-cmd t))
(defvar *exec-firefox*            "exec systemd-run --user --unit=firefox /usr/bin/firefox")
(defvar *exec-rofi*               "exec rofi -show drun -font 'Fira Code -18'")
(defvar *exec-rofipass*           "exec rofi-pass -show drun -font 'Fira Code -18'")
(defvar *exec-rofi-window*        "exec rofi -show window -font 'Fira Code -18'")
(defvar *exec-emacs-everywhere*   "exec emacsclient --eval '(emacs-everywhere)'")

(defvar *launch-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "b") "exec qutebrowser")
    (define-key m (kbd "g") *exec-chrome*)
    (define-key m (kbd "G") *exec-chrome-nw*)
    (define-key m (kbd "f") *exec-firefox*)
    (define-key m (kbd "e") *exec-emacs*)
    (define-key m (kbd "c") *exec-emacs-everywhere*)
    (define-key m (kbd "k") "exec kitty")
    (define-key m (kbd "r") *exec-rofi*)
    (define-key m (kbd "w") *exec-rofi-window*)
    (define-key m (kbd "p") *exec-rofipass*)
    m
  ))
(define-key *root-map* (kbd "SPC") '*launch-map*)

(defun show-launch-menu ()
  "Show the launch menu."
  (let* ((options (mapcar (lambda (binding)
                            (cons (key-to-string (first binding))
                                  (second binding)))
                          (kmap-alist *launch-map*)))
         (selection (select-from-menu (current-screen)
                                      options
                                      "Launch: ")))
    (when selection
      (eval-command ))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help (describe) map ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *describe-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "k") "describe-key")
    (define-key m (kbd "f") "describe-function")
    (define-key m (kbd "c") "describe-command")
    (define-key m (kbd "v") "describe-variable")
    m
    ))
(define-key *root-map* (kbd "M-d") '*describe-map*)

;; Redefine keys to behave emacs-like when focused window
;; has class firefox or Chrome (redirects firefox's keys)
(define-remapped-keys
    '(("firefox|Google-chrome|Chromium"
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
     ("C-l"   . "F6")
     ("C-k"   . "C-w"))))

;; If circumventing the above remapping is needed
;; e.g. when setting up keybindings on the browser
(defcommand toggle-remapped-keys () ()
 ((lambda (var) (setf var (not var)))
  *REMAPPED-KEYS-ENABLED-P*))
