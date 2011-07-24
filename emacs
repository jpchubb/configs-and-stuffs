(add-to-list 'load-path "~/.emacs.d/") ;; user load path
(add-to-list 'load-path "/usr/share/emacs/site-lisp/haskell-mode/")
(require 'org-entities)
;;(add-to-list
;;;; Prettify emacs ;;;;
(require 'zenburn)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)

(menu-bar-mode -1)
  ;(setq frame-title-format "%b" icon title format "%b")

  (fringe-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
(when window-system
  (zenburn)
  (set-frame-height (selected-frame) 38)
  (set-frame-width (selected-frame) 143)
  (add-hook 'org-mode-hook 'org-toggle-pretty-entities))


(setq inhibit-startup-message t
      column-number-mode 1
      echo-keystrokes 0.1
      show-trailing-whitespace 1)

(set-face-attribute 'default nil :height 92)

;;pretty lambdas
(font-lock-add-keywords 'emacs-lisp-mode '(("(\\(lambda\\)\\>" (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) ?λ))))))

(add-to-list 'org-entities '("neg" "\\neg" t "&neg;" "[negation]" "[negation]" "¬"))
(add-to-list 'org-entities '("vdash" "\\vdash" t "&vdash;" "[vdash]" "[vdash]" "⊢"))
(add-to-list 'org-entities '("iff" "\\iff" t "&iff;" "[if and only if]" "[if and only if]" ""))
(add-to-list 'org-entities '("top" "\\top" t "&top;" "[Top, true]" "[Top, true]" "⊤"))
(add-to-list 'org-entities '("bot" "\\bot" t "&bot;" "[Bot, false]" "[Bot, false]" "⊥"))
(add-to-list 'org-entities '("langle" "\\langle" t "&blah;" "[angle bracket left]" "[angle bracket left]" "⟨"))
(add-to-list 'org-entities '("rangle" "\\rangle" t "&blah;" "[angle bracket right]" "[angle bracket right]" "⟩"))


;;;; extra keybindings ;;;;
(global-set-key (kbd "C-c 1") 'dired)
(global-set-key (kbd "C-c 4") 'switch-to-screenrc)
(global-set-key (kbd "C-c 0") 'switch-to-dot-emacs)
(global-set-key (kbd "C-c 3") 'switch-to-xmonadhs)
(global-set-key (kbd "C-c 2") 'switch-to-gtd)
(global-set-key (kbd "C-c 9") 'ielm)
(global-set-key (kbd "C-c 8") 'magit-status)

;;;; IDE useful stuffs ;;;;

;;;; Major Modes ;;;;
(add-hook 'message-mode-hook 'orgstruct++-mode 'append)
(add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
(add-hook 'message-mode-hook 'bbdb-define-all-aliases 'append)
(add-hook 'message-mode-hook 'orgtbl-mode 'append)
(add-hook 'message-mode-hook 'turn-on-flyspell 'append)
(add-hook 'message-mode-hook '(lambda () (setq fill-column 72)) 'append)
(add-hook 'message-mode-hook '(lambda () (local-set-key (kbd "C-c M-o") 'org-mime-htmlize)) 'append)
;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; Make TAB the yas trigger key in the org-mode-hook and turn on flyspell mode
(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet
            ;(make-variable-buffer-local 'yas/trigger-key)
            ;(org-set-local 'yas/trigger-key [tab])
            ;(define-key yas/keymap [tab] 'yas/next-field-group)
            ;; Undefine C-c [ and C-c ] since this breaks my org-agenda files when directories are include
            ;; It expands the files in the directories individually
            (org-defkey org-mode-map "\C-c["    'undefined)
            (org-defkey org-mode-map "\C-c]"    'undefined)
            (local-set-key (kbd "C-c M-o") 'bh/mail-subtree)) 'append)

(defun bh/mail-subtree ()
  (interactive)
  (org-mark-subtree)
  (org-mime-subtree))

;; Org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-files '("~/Documents/agenda/"))
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
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-agenda-ndays 1)
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

;; Org-Agenda
;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
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
                (org-agenda-overriding-header "Tasks to Refile")))
              ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
               ((org-agenda-overriding-header "Stuck Projects")
                (org-tags-match-list-sublevels 'indented)
                (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
              ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT|STARTED"
               ((org-agenda-overriding-header "Next Tasks")
                (org-agenda-skip-function 'bh/skip-projects)
                (org-tags-match-list-sublevels t)
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("R" "Relevant Tasks" tags-todo "-REFILE-CANCELLED/!-NEXT-STARTED-WAITING"
               ((org-agenda-overriding-header "Relevant Tasks")
                (org-agenda-skip-function 'bh/skip-non-relevant-tasks)
                (org-tags-match-list-sublevels 'indented)
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("p" "Projects" tags-todo "-CANCELLED/!"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-skip-function 'bh/skip-non-projects)
                (org-tags-match-list-sublevels 'indented)
                (org-agenda-todo-ignore-scheduled 'future)
                (org-agenda-todo-ignore-deadlines 'future)
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("w" "Waiting Tasks" todo "WAITING|SOMEDAY"
               ((org-agenda-overriding-header "Waiting and Postponed tasks"))
               (org-agenda-skip-function 'bh/skip-projects))
              ("A" "Tasks to Archive" tags "-REFILE/"
               ((org-agenda-overriding-header "Tasks to Archive")
                (org-agenda-skip-function 'bh/skip-non-archivable-tasks))))))
(setq org-tags-match-list-sublevels nil)

;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Small windows on my Eee PC displays only the end of long lists which isn't very useful
(setq org-clock-history-length 10)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change task to STARTED when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-started)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist (quote history))
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)

(defun bh/clock-in-to-started (kw)
  "Switch task from TODO or NEXT to STARTED when clocking in.
Skips capture tasks."
  (if (and (member (org-get-todo-state) (list "TODO" "NEXT"))
           (not (and (boundp 'org-capture-mode) org-capture-mode)))
      "STARTED"))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (let ((parent-task (save-excursion (org-back-to-heading) (point))))
    (while (org-up-heading-safe)
      (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
        (setq parent-task (point))))
    (goto-char parent-task)
    parent-task))

(defun bh/clock-in-and-set-project-as-default (pom)
  "Clock in the current task and set the parent project (if any) as the
default clocking task.  Agenda filter tags are set from the default task"
  ;; Find the parent project task if any and set that as the default
  (save-excursion
    (save-excursion
      (org-with-point-at pom
        (bh/find-project-task)
        (org-clock-in '(16))))
    (save-excursion
      (org-with-point-at pom
        (org-clock-in nil)))))

(defun bh/set-agenda-restriction-lock ()
  "Set filter to tags of POM, current task, or current project and refresh"
  (interactive)
  ;;
  ;; We're in the agenda
  ;;
  (let* ((pom (org-get-at-bol 'org-hd-marker))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (if (equal major-mode 'org-agenda-mode)
        (if tags
            (org-with-point-at pom
              (bh/find-project-task)
              (org-agenda-set-restriction-lock))
          (org-agenda-remove-restriction-lock))
      (if (equal org-clock-default-task (org-id-find "eb155a82-92b2-4f25-a3c6-0304591af2f9" 'marker))
          (org-agenda-remove-restriction-lock)
        (org-with-point-at pom
          (bh/find-project-task)
          (org-agenda-set-restriction-lock))))))

(defun bh/punch-in ()
  "Start continuous clocking and set the default task to the project task
of the selected task.  If no task is selected set the Organization task as
the default task."
  (interactive)
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if tags
            (bh/clock-in-and-set-project-as-default marker)
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)))
          (bh/clock-in-and-set-project-as-default nil)
        (bh/clock-in-organization-task-as-default))))
  (bh/set-agenda-restriction-lock))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (save-restriction
    (widen)
    (org-with-point-at (org-id-find "eb155a82-92b2-4f25-a3c6-0304591af2f9" 'marker)
      (org-clock-in '(16)))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-default-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)
(require 'org-id)  
(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (save-restriction
    (widen)
    (org-with-point-at (org-id-find id 'marker)
      (org-clock-in nil))))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

;; Custom Key Bindings
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f5>") 'bh/org-todo)
(global-set-key (kbd "<S-f5>") 'bh/widen)
(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> g") 'gnus)
(global-set-key (kbd "<f9> h") 'bh/hide-other)

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (org-shifttab)
    (org-reveal)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(global-set-key (kbd "<f9> i") 'info)

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> u") 'bh/untabify)

(defun bh/untabify ()
  (interactive)
  (untabify (point-min) (point-max)))

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
(global-set-key (kbd "C-M-r") 'org-capture)
(global-set-key (kbd "C-c r") 'org-capture)
(global-set-key (kbd "M-<f9>") (lambda ()
                                 (interactive)
                                 (unless (buffer-modified-p)
                                   (kill-buffer (current-buffer)))
                                 (delete-frame)))
(setq org-time-stamp-rounding-minutes (quote (1 1)))
(setq org-agenda-clock-consistency-checks 
      (quote (:max-duration "4:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00"))))
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t)))
;; Agenda log mode items to display (clock time only by default)
(setq org-agenda-log-mode-items (quote (closed state)))
; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
; global Effort estimate values
(setq org-global-properties (quote (("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00"))))


;;; Org-Capture
;; I use C-M-r to start capture mode
(global-set-key (kbd "C-M-r") 'org-capture)
;; I use C-c r to start capture mode when using SSH from my Android phone
(global-set-key (kbd "C-c r") 'org-capture)

(setq org-default-notes-file  "~/Documents/agenda/refile.org")

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Documents/agenda/refile.org")
               "* TODO %?\n%U\n%a\n  %i" :clock-in t :clock-resume t)
              ("n" "note" entry (file "~/Documents/agenda/refile.org")
               "* %? :NOTE:\n%U\n%a\n  %i" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/Documents/agenda/diary.org")
               "* %?\n%U\n  %i" :clock-in t :clock-resume t)
	      ("a" "Assignment" plain (file+function "~/Documents/agenda/Uni.org" course-code)
	       "*** TODO %^{Title} %?%^g\n DEADLINE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
	      ("u" "University" entry (file+headline "~/Documents/agenda/Uni.org" "University")
	       "%(add-course) LECTURES:\n + \n TUTORIAL/LAB:\n + " :empty-lines 1)
	      ("w" "" entry (file "~/Documents/agenda/refile.org")
	       "* TODO [[%u][%^{Title}]]\n%c\n%i")
)))

(setq org-stuck-projects (quote ("" nil nil "")))
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


;; Haskell
(autoload 'haskell-mode "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file" "Major mode for editing haskell" t)
(setq haskell-font-lock-symbols t) ;; enable comments in haskell
(let ( (fn-list '(turn on haskell-dot-mode turn-on-haskell-indent)) ) 
  (mapc (lambda (fn) (add-hook 'haskell-mode-hook fn)) fn-list))
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode) )
;(require 'haskell)

;;;; Minor Modes ;;;;
;(require 'scratch-mode) ;; funky mode sensitive scratch buffers

(autoload 'tramp "Remote file manipulation in TRAMP." t)

(require 'ido) ;; interactively do stuff

;;; Use "%" to jump to the matching parenthesis.
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert
the character typed."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
    ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
    (t                    (self-insert-command (or arg 1))) ))
(global-set-key (kbd "C-%") `goto-match-paren)

;;;; formatting ;;;;

;; Linus' kernel formatting
(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))
(setq make-backup-files nil)
(line-number-mode 1)

;;;; Custom functions ;;;;
(defun switch-to-dot-emacs (&rest junk)
  "Switch to .emacs file"
  (interactive)
  (if (equal (buffer-name) ".emacs")
      (eval-buffer)
    (find-file "~/.emacs")))

(defun switch-to-xmonadhs (&rest junk)
  "Switch to xmonad.hs file"
  (interactive)
    (find-file "~/.xmonad/xmonad.hs"))

(defun switch-to-screenrc (&rest junk)
  "Switch to .screenrc file"
  (interactive)
    (find-file "~/.screenrc"))

(defun switch-to-gtd (&rest junk)
  "Switch to gtd.org file"
  (interactive)
    (find-file "~/Documents/gtd.org"))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido"
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(setq org-export-exclude-category (list "google" "private"))

(defun org-mycal-export-limit ()
  "Limit the export to items that have a date, time and range. Also exclude certain categories."
  (setq org-tst-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ... [0-9]\\{2\\}:[0-9]\\{2\\}[^\r\n>]*?\\)>")
  (setq org-tstr-regexp (concat org-tst-regexp "--?-?" org-tst-regexp))
  (save-excursion
    ; get categories
    (setq mycategory (org-get-category))
    ; get start and end of tree
    (org-back-to-heading t)
    (setq mystart    (point))
    (org-end-of-subtree)
    (setq myend      (point))
    (goto-char mystart)
    ; search for timerange
    (setq myresult (re-search-forward org-tstr-regexp myend t))
    ; search for categories to exclude
    (setq mycatp (member mycategory org-export-exclude-category))
    ; return t if ok, nil when not ok
    (if (and myresult (not mycatp)) t nil)))

;;; activate filter and call export function
(defun org-mycal-export () 
  (let ((org-icalendar-verify-function 'org-mycal-export-limit))
    (org-export-icalendar-combine-agenda-files)))

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
;				  "TUTORIAL: "
;				"SEMINAR: ") "\n + <" (read-from-minibuffer "Time: ") " +1w> : " (read-from-minibuffer "Room Location: ")"\n")))
))))

(defun course-code (&rest junk)
  "Search for a COURSE-CODE appearing in 'Uni.org' and if found move the point to that location."
  (interactive)
  (switch-to-buffer "Uni.org")
  (goto-char (point-min))
  (let ((str (read-from-minibuffer "Enter course code: ")))
    (when (search-forward (concat "** " str "\t") nil nil)
      (forward-line 9))))

(server-start)
(require 'org-protocol)
(desktop-load-default)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Documents/agenda/uni-2600.org" "~/Documents/agenda/Uni.org"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
