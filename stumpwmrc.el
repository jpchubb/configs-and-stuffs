;; ==================== stumpwmrc.el
;;; Author: 
;; Joshua Chubb <chubb.jp@gmail.com>
;;; Forked from:
;; Matthew Ball <matt.ball@gmail.com>
;;
;;; DATE: Friday, August 26 2011
;; ----------------------------------------
;; A config file for the tiling window
;; manager stumpwm
;; ----------------------------------------
;;; Licence
;; GPL
;; ========================================


;; tell stumpwm what lisp implementation to use
;debian=sbcl

;;=================;;
;;; initial config ;;
;;=================;;
(in-package :stumpwm) ;; declare the package name

(setf *default-package* :stumpwm ;; set default package to be stumpwm
      *startup-message* "Welcome to stumpwm, happy hacking!"
      *startup-message* nil ;; suppress the message stumpwm displays when it starts
      ;; *debug-level* 10 ;; turn on stumpwm debugging (WARNING: creates massive text dumps)
      *shell-program* (getenv "SHELL") ;; set the default shell
      *mouse-focus-policy* :sloppy) ;; set the mouse policy so focus follows mouse (alternatives are: :click, :ignore, :sloppy)

(redirect-all-output (data-dir-file "debug-output" "txt")) ;; send debug information to ~/.stumpwm.d/debug-output.txt
(set-prefix-key (kbd "s-x")) ;; set the stumpwm prefix key to super+z

;;==================;;
;;; Zenburn Colours ;;
;;==================;;
(defparameter *foreground-colour* "darkseagreen4" "Set the foreground colour.") ;; zenburn foreground colour
(defparameter *background-colour* "grey25" "Set the background colour.") ;; zenburn background colour
(defparameter *border-colour* "grey26" "Set the border colour.")
(defparameter *focus-colour* "darkseagreen1" "Set the focus colour.")
(defparameter *unfocus-colour* "grey25" "Set the unfocus colour.")

;; ======================
;;; message and input box
;; ======================
(set-bg-color *background-colour*)
(set-fg-color *foreground-colour*)
(set-border-color *foreground-colour*)
(set-focus-color *focus-colour*)
(set-unfocus-color *unfocus-colour*)
(set-msg-border-width 1)

(setf *message-window-gravity* :top-right ;; set the message-box to the top right
      *input-window-gravity* :top-right ;; set the input-box to the top right
      ;; *window-name-source* :title ;; windows get their name from their title property
      *timeout-wait* 5) ;; how long a message will appear for (in seconds)


;;==========;;
;;; Battery ;;
;;==========;;
(defcommand show-battery () ()
  "Show current battery status."
  (echo-string (current-screen) (run-shell-command "acpi" t)))

(defvar *Battery-timer* nil "runs the battery status checker")

(defcommand battery-popup () () "displays a battery warning message"
  (echo-string (current-screen)  "< 15% battery remaining"))

(defcommand battery-check () () "displays the battery pop-up if necessary"
  (if (equal (run-shell-command "acpi | grep -o [[:digit]]\*%") "92%")
      (battery-popup)))


(defcommand toggle-touchpad () ()
  "Toggle the laptop touchpad on/off.
   Need to have set 'Option SHMConfig' for Synaptics Touchpad
   device in xorg.conf."
  (let ((state (run-shell-command
		"synclient -l | grep TouchpadOff | awk '{ print $3 }'" t)))
    (case (string= (subseq state 0 1) "1")
      (t (run-shell-command "synclient TouchpadOff=0"))
      (otherwise (run-shell-command "synclient TouchpadOff=1")
		 (banish-pointer)))))

(define-key *root-map* (kbd "T") "toggle-touchpad")

;;=======;;
;;; Keys ;;
;;=======;;
;(defmacro defkey-top (key cmd)
;  '(define-key *top-map* (kbd key) cmd))

;(defmacro defkeys-top (& rest keys)
;  (let ((ks (mapcar #'(lambda (k) (cons 'defkey-top k)) keys)))
;    '(progn ,@ks)))

;(defkeys-top
; ("s-b" "show-battery")
; ("M-<F1>" "switch-to-tty1"))
;(define-key sutmpwm:*root-map* (kbd "E") "emacsclient -c -e \"(zenburn)\"")
(define-key stumpwm:*root-map* (kbd "B") "show-battery")
(define-key stumpwm:*top-map* (kbd "M-F1") "switch-to-tty1")
(define-key stumpwm:*top-map* (kbd "M-F2") "switch-to-tty2")
(define-key stumpwm:*root-map* (kbd "i") "chrome")

;;=============;;
;;; Emacs Keys ;;
;;=============;;

(define-key stumpwm:*root-map* (kbd "g") "gnus")
(define-key stumpwm:*root-map* (kbd "C-r") "capture") 

(define-key stumpwm:*root-map* (kbd "t") "org-todo-message")
(define-key stumpwm:*root-map* (kbd "C-a") "org-agenda-message")
(define-key stumpwm:*root-map* (kbd "a") "my-time")

;;===================;;
;;; Program Commands ;;
;;===================;;
(defcommand emacs () ()
  "Start emacs unless it is already running in which case focus it."
  (run-or-raise "emacsclient -c" '(:class "Emacs")))

(defcommand chrome () ()
	   (run-or-raise "chromium" '(:class "Chromium")))

(defcommand switch-to-tty1 () ()
  "Switch to tty1"
  (run-shell-command "chvt 1" t))
(defcommand switch-to-tty2 () ()
  "Switch to tty2"
  (run-shell-command "chvt 2" t))

(defcommand my-time () ()
  (echo-string (current-screen) (run-shell-command "date \"+%a %d %b, %R\"" t)))

;;========;;
;;; Swank ;;
;;========;;
(load "/usr/share/common-lisp/source/slime/swank-loader.lisp")
(swank-loader:init)
(defcommand swank () ()
  (setf stumpwm:*top-level-error-action* :break)
  (swank:create-server :port 4005
		       :style swank:*communication-style*
		       :dont-close t)
  (echo-string (current-screen)
	       "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)."))

;;=================;;
;;; Emacs Commands ;;
;;=================;;

;; Org Agenda
(defcommand Agenda () ()
  (emacs)
  (send-meta-key (current-screen) (kbd "F10"))
  (window-send-string " ")
  )

;; Capture
(defcommand capture () ()
  (emacs)
  (send-meta-key (current-screen) (kbd "C-M-r"))
  (focus-em-window))

;; gnus
(defcommand gnus () ()
  (emacs)
  (send-meta-key (current-screen) (kbd "M-x"))
  (window-send-string "gnus")
  (send-meta-key (current-screen) (kbd "RET")))

;; ERC
(defcommand erc () ()
  (emacs)
  (send-meta-key (current-screen) (kbd "M-x"))
  (window-send-string "erc-start-or-switch")
  (send-meta-key (current-screen) (kbd "RET")))

;; jabber
(defcommand jabber () ()
  (emacs)
  (send-meta-key (current-screen) (kbd "C-x"))
  (window-send-string "b")
  (send-meta-key (current-screen) (kbd "RET"))
  (window-send-string "roster")
  (send-meta-key (current-screen) (kbd "RET")))


;;; Epic but ultimately useless set of commands
;;	   (setq old-screen (window-number (current-window)))
;;	   (new-em-frame)
;;	   (send-meta-key (current-screen) (kbd "F10"))
;;	   (window-send-string "a")
;;	   (send-meta-key (current-screen) (kbd "C-x"))
;;	   (send-meta-key (current-screen) (kbd "C-w"))
;;	   (window-send-string "~/.ag.txt")
;;	   (send-meta-key (current-screen) (kbd "RET"))
;;	   (send-meta-key (current-screen) (kbd "C-x"))
;;	   (window-send-string "5")
;;	   (window-send-string "0")
;;	   (pull-window-by-number old-screen)

;; display current days agenda
(defcommand org-agenda-message () ()
  (run-shell-command "emacsclient -e \"(org-store-agenda-views)\"")
  (stumpwm:message (run-shell-command "cat ~/.ag.txt" t)))

(defcommand org-todo-message () ()
  (run-shell-command "emacsclient -e \"(org-store-agenda-views)\"")
  (echo-string (current-string)  (run-shell-command "cat ~/.td.txt" t)))

(defcommand new-em-frame () ()
  (run-shell-command "emacsclient -e \"(new-frame)\""))

(defcommand focus-em-window () ()
  (send-meta-key (current-screen) (kbd "C-x"))
  (window-send-string "1"))

(defvar kblayout 0)
(defcommand switch-kbd 
	    () () (if (eq kblayout 0)
		    (progn (run-shell-command "setxkbmap il") (incf kblayout))
		    (progn (run-shell-command "setxkbmap us") (decf kblayout))))

;;; init
(swank)
