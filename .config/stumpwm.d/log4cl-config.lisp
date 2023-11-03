(in-package :cl-user)

(log4cl:clear-all-configurations)

(logcl:configure
 '((:file-appender
    :file "/home/steve/.local/share/stumpwm/stumpwm.log"
    :rolling-kind :size
    :max-file-size "10MB"
    :max-backup-index 3)
   (:logger :root :all (:file-appender))))
