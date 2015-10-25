(defvar *sneps-directory*
    ;; Installer:  Change the path on the following line
    (if (sys:getenv "Sneps3Home")
        (concatenate 'string (sys:getenv "Sneps3Home") "\\")
        "C:\\Program Files\\Sneps3\\")
  "Root of the SNePS 3 directory tree")

(defvar *sneps-load-file*
    (concatenate 'string *sneps-directory* "sneps3.cl")
  "Location of the SNePS load file.")

(unless (find-package :sneps)
  (load *sneps-load-file*))

(in-package snuser)

(startGUI)
