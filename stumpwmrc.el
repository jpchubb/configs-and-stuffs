;; ============================
;; custom .stumpwmrc file
;; Joshua Chubb
;; ============================

;; tell stumpwm what lisp implementation to use
;debian=sbcl

;; ===============
;;; initial config
;; ===============
(in-package :stumpwm) ;; declare the package name

(setf *default-package* :stumpwm ;; set default package to be stumpwm
      *startup-message* "Welcome to stumpwm, happy hacking!"
      *startup-message* nil ;; suppress the message stumpwm displays when it starts
      ;; *debug-level* 10 ;; turn on stumpwm debugging (WARNING: creates massive text dumps)
      *shell-program* (getenv "SHELL") ;; set the default shell
      *mouse-focus-policy* :sloppy) ;; set the mouse policy so focus follows mouse (alternatives are: :click, :ignore, :sloppy)

(redirect-all-output (data-dir-file "debug-output" "txt")) ;; send debug information to ~/.stumpwm.d/debug-output.txt
(set-prefix-key (kbd "s-x")) ;; set the stumpwm prefix key to super+z

(defparameter *foreground-colour* "darkseagreen4" "Set the foreground colour.") ;; zenburn foreground colour
(defparameter *background-colour* "grey25" "Set the background colour.") ;; zenburn background colour
(defparameter *border-colour* "grey25" "Set the border colour.")
(defparameter *focus-colour* "darkseagreen1" "Set the focus colour.")
(defparameter *unfocus-colour* "grey25" "Set the unfocus colour.")


;;; Battery
(defcommand show-battery () ()
"Show current battery status."
(echo-string (current-screen) (run-shell-command "acpi" t)))

;;; Keys
(define-key stumpwm:*root-map* (kbd "B") "show-battery")
;(define-key sutmpwm:*root-map* (kbd "E") "emacsclient -c -e \"(zenburn)\"")