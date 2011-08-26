;; ==================== babel.el
;;; Author: 
;; Joshua Chubb <chubb.jp@gmail.com>
;;
;;; DATE: Friday, August 26 2011
;; ----------------------------------------
;; A configuration file for the org-mode
;; extension babel
;; ----------------------------------------
;;; Licence
;; GPL
;; ========================================

;;=========;;
;;; LaTeX ;;;
;;=========;;

;;(load "auctex.el" nil t t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-default-bibliography
      (quote
       ("default.bib")))

;;; Languages ;;;
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (ditaa . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (haskell . t)
   (latex . t)
   (ocaml . nil)
   (perl . t)
   (ruby . t)
   (screen . nil)
   (sh . t)
   (sql . nil)
   (sqlite . nil)))

