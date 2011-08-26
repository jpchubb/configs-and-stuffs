;; ==================== org-config.el
;;; Author: 
;; Joshua Chubb <chubb.jp@gmail.com>
;;
;;; DATE: Friday, August 26 2011
;; ----------------------------------------
;; A config file for org-mode, which
;; is a organisational extension to emacs
;; ----------------------------------------
;;; Licence
;; GPL
;; ========================================

(require 'org-entities)

;;==================;;
;;;; Key-bindings ;;;;
;;==================;;

;; I use C-M-r to start capture mode
(global-set-key (kbd "C-M-r") 'org-capture)
(global-set-key (kbd "C-c s") 'insert-fsa)

;;=============;;
;;;; Capture ;;;;
;;=============;;

(setq org-default-notes-file  "~/Documents/agenda/refile.org")

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Documents/agenda/refile.org")
               "* TODO %?\n%U\n%a\n  %i" :clock-in t :clock-resume t)
              ("n" "note" entry (file "~/Documents/agenda/refile.org")
               "* %? :NOTE:\n%U\n%a\n  %i" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/Documents/agenda/diary.org")
               "* %?\n%U\n  %i" :clock-in t :clock-resume t)
              ("l" "Library book" entry (file+headline "~/Documents/agenda/Uni.org" "Library")
               "*** %^{Title}-%^{Library} \n DEADLINE: %^T\n\n" :immediate-finish t)
              ("p" "Phone call" entry (file "~/Documents/agenda/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/Documents/agenda/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n  %i")
	      ("a" "Assignment" plain (file+function "~/Documents/agenda/Uni.org" course-code)
	       "*** TODO %^{Title} %?%^g\n DEADLINE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
	      ("u" "University" entry (file+headline "~/Documents/agenda/Uni.org" "University")
	       "%(add-course) LECTURES:\n + \n TUTORIAL/LAB:\n + " :empty-lines 1)
	      ("w" "" entry (file "~/Documents/agenda/refile.org")
	       "* TODO [[%u][%^{Title}]]\n%c\n%i"))))

;;==========;;
;;;; Tags ;;;;
;;==========;;

(setq org-agenda-files '("~/Documents/agenda"))
(setq org-tag-alist (quote ((:startgroup . nil)
			   ("@UNI" . ?u)
			   ("@HOME" . ?h)
			   ("@CHURCH" . ?c)
			   (:endgroup . nil)
			   ("STUDY" . ?s)
			   ("READING" . ?r)
			   ("NOTES" . ?n)
			   ("ASSIGNMENT" . ?a)
			   ("CONTACT" . ?@)
			   ("PROJECT" . ?p))))
(setq org-fast-tag-selection-single-key (quote expert))

;;===================;;
;;;; TODO keywords ;;;;
;;===================;;

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)")
	      (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "|" "CANCELLED(c@/!)" "PHONE")
	      (sequence "OPEN(O!)" "|" "CLOSED(C!)"))))
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
	      ("NEXT" :foreground "blue" :weight bold)
	      ("STARTED" :foreground "blue" :weight bold)
	      ("DONE" :foreground "green" :weight bold)
	      ("WAITING" :foreground "orange" :weight bold)
     	      ("SOMEDAY" :foreground "magenta" :weight bold)
	      ("CANCELLED" :foreground "green" :weight bold)
	      ("OPEN" :foreground "blue" :weight bold)
	      ("CLOSED" :foreground "green" :weight bold)
	      ("PHONE" :foreground "green" :weight bold))))
(setq org-use-fast-todo-selection t)
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("SOMEDAY" ("WAITING" . t))
              (done ("WAITING"))
              ("TODO" ("WAITING") ("CANCELLED"))
              ("NEXT" ("WAITING"))
              ("STARTED" ("WAITING"))
              ("DONE" ("WAITING") ("CANCELLED")))))
(setq org-default-notes-file "~/git/org/refile.org")

;;============;;
;;;; Agenda ;;;;
;;============;;

;; Files
'(org-agenda-files (quote ("~/Documents/agenda/Uni.org"
			   "~/Documents/agenda/todo.org"
			   "~/Documents/agenda/refile.org")))

;; Don't dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-agenda-ndays 1)

;; Custom agenda definition
(setq org-agenda-custom-commands
      (quote ((" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Notes and Tasks to Refile")
                       (org-agenda-overriding-header "Tasks to Refile")))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
                (tags-todo "-WAITING-CANCELLED/!NEXT|STARTED"
                           ((org-agenda-overriding-header "Next Tasks")
                            (org-agenda-skip-function 'bh/skip-projects)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED/!-NEXT-STARTED-WAITING"
                           ((org-agenda-overriding-header "Relevant Tasks")
                            (org-agenda-skip-function 'bh/skip-non-relevant-tasks)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-todo-ignore-scheduled 'future)
                            (org-agenda-todo-ignore-deadlines 'future)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (todo "WAITING|SOMEDAY"
                      ((org-agenda-overriding-header "Waiting and Postponed tasks")
                       (org-agenda-skip-function 'bh/skip-projects)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks))))
               nil)
              ("r" "Tasks to Refile" tags "REFILE"
               ((org-agenda-overriding-header "Notes and Tasks to Refile")
                (org-agenda-overriding-header "Tasks to Refile"))))))
(setq org-tags-match-list-sublevels nil)

;;==============;;
;;;; Refiling ;;;;
;;==============;;

; Targets include this file and any file contributing to the agenda - up to 2 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 3)
                                 (org-agenda-files :maxlevel . 3))))

; Stop using paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path nil)
; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;;==============;;
;;;; Entities ;;;;
;;==============;;

(add-to-list 'org-entities '("neg" "\\neg" t "&neg;" "[negation]" "[negation]" "¬"))
(add-to-list 'org-entities '("vdash" "\\vdash" t "|-" "[vdash]" "[vdash]" "⊢"))
(add-to-list 'org-entities '("iff" "\\iff" t "&iff;" "[if and only if]" "[if and only if]" ""))
(add-to-list 'org-entities '("top" "\\top" t "&top;" "[Top, true]" "[Top, true]" "⊤"))
(add-to-list 'org-entities '("bot" "\\bot" t "&bot;" "[Bot, false]" "[Bot, false]" "⊥"))
(add-to-list 'org-entities '("langle" "\\langle" t "&blah;" "[angle bracket left]" "[angle bracket left]" "⟨"))
(add-to-list 'org-entities '("rangle" "\\rangle" t "&blah;" "[angle bracket right]" "[angle bracket right]" "⟩"))

;;===========================;;
;;;; Org-Related Functions ;;;;
;;===========================;;
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (let ((has-subtask)
        (subtree-end (save-excursion (org-end-of-subtree t)))
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (forward-line 1)
      (while (and (not has-subtask)
                  (< (point) subtree-end)
                  (re-search-forward "^\*+ " subtree-end t))
        (when (member (org-get-todo-state) org-todo-keywords-1)
          (setq has-subtask t))))
    (and is-a-task has-subtask)))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
         (subtree-end (save-excursion (org-end-of-subtree t)))
         (has-next (save-excursion
                     (forward-line 1)
                     (and (< (point) subtree-end)
                          (re-search-forward "^\\*+ \\(NEXT\\|STARTED\\) " subtree-end t)))))
    (if (and (bh/is-project-p) (not has-next))
        nil ; a stuck project, has subtasks but no next task
      next-headline)))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (bh/is-project-p)
        nil
      subtree-end)))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (cond
     ((bh/is-project-p)
      subtree-end)
     ((org-is-habit-p)
      subtree-end)
     (t
      nil))))

(defun bh/skip-projects ()
  "Skip trees that are projects"
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
    (cond
     ((bh/is-project-p)
      next-headline)
     (t
      nil))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (cond
     ((bh/is-project-p)
      subtree-end)
     ((org-is-habit-p)
      subtree-end)
     (t
      nil))))

(defun add-course (&rest junk)
  "Capture a course via org-mode's `org-capture'."
  (let ((course-details ""))
    ;; fix the concats with (setq ...)'s
    (setq course-details (concat course-details "** " (read-from-minibuffer "Course Code: ") "\n"
" TITLE: " (read-from-minibuffer "Course Title: ") "\n"
" LECTURER: " (read-from-minibuffer "Course Lecturer: ") "\n"
;" LECTURES: \n + <" (read-from-minibuffer "Lecture Time: ") " +1w> : " (read-from-minibuffer "Room Location: ") "\n"))
    ;(while (string= (read-from-minibuffer "Add Lecture? (y/n): ") "y") ;; this technically lies, y goes into the loop, anything else jumps to tutorial/seminar
     ; (setq course-details (concat course-details " + <" (read-from-minibuffer "Time: ") " +1w> : " (read-from-minibuffer "Room Location: ") "\n")))
    ;(concat course-details " " (if (string= (read-from-minibuffer "Tutorial or Seminar? (t/s): ") "t") ;; this technically lies, t for "tutorial", any other input means "seminar"
; "TUTORIAL: "
; "SEMINAR: ") "\n + <" (read-from-minibuffer "Time: ") " +1w> : " (read-from-minibuffer "Room Location: ")"\n")))
))))