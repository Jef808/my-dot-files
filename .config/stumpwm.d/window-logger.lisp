(defpackage :stumpwm-window-logger
  (:use :cl :stumpwm))
(in-package :stumpwm-window-logger)

(defvar *window-focus-log* (make-pathname :name "window-focus-log"
                                          :type "txt"
                                          :defaults (user-homedir-pathname))
  "Path to the log file for window focus data.")

(defvar *last-window* nil "The last window that was focused.")
(defvar *last-time* (get-universal-time) "The last time the window was focused.")

(defun log-window-focus (window)
  "Log the focused window and the time spent on it."
    (let* ((current-time (get-universal-time))
         (time-spent (- current-time *last-time*))
         (window-title (if window (window-name window) "nil")))
    (when *last-window*
      (append-to-file (format nil "~A - ~A for ~A seconds~%"
                              *last-time* (window-name *last-window*) time-spent)
                      nil
                      *window-focus-log*))
    (setf *last-window* window
          *last-time* current-time)))

(defun append-to-file (string &optional (start 0) (end nil) (path *window-focus-log*))
  "Append STRING to the file specified by PATH."
  (with-open-file (stream path
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :append)
    (write-string string stream :start start :end end)))

(defun window-focus-change-hook ()
  "Hook function to be called on window focus change."
  (log-window-focus (current-window)))

;; Setup the hook
(add-hook *window-focus-changed-hook* 'window-focus-change-hook)

;; Ensure logging starts with the current window
(log-window-focus (current-window))
