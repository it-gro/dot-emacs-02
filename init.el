;;* gnu-emacs config Grossniklaus Bruno -*-emacs-lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;$Copyright: Bruno Grossniklaus $
;;$License:   GPL $
;;$Customer:  none $
;;$Project:   none $
;;$Version:   26.1 $
;;$Code:      emacs elisp $
;;$Writer:    Bruno Grossniklaus $
;;$Remark:    emacs for GNU-Emacs version >= 26.1 $
;;$Creation:  1992-08-27 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;$Id:        $
;;$Date:      2018-02-06 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;; my Emacs config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
 
;;(debug-on-entry 'display-warning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* new version inspired by
;; http://cestlaz.github.io/stories/emacs/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* how to use in a sandbox:
;; $ \emacs -Q -l $HOME/.emacs.d/it-gro/init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(defvar user-home-directory
;; 	(concat (getenv "HOME") "/")
;; 	) ; must end with /

;;(setq user-emacs-directory
;; 			(concat user-home-directory ".emacs.d/it-gro/")
;; 			) ; must end with /

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* initialize an package install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
						 '("melpa" . "https://melpa.org/packages/")
						 )

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)
;; M-x package-refresh-contents
;; M-x package-list-packages
;; M-x list-packages

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (org-babel-load-file (expand-file-name "~./.emacs.d/myinit.org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t)
;;(tool-bar-mode -1)
(load-theme 'manoj-dark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; most important packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package try
  :ensure t
	)

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
	)

(use-package editorconfig
	:ensure t
	:config
	(editorconfig-mode 1)
	)

(use-package auto-complete
	:ensure t
	:init
	(progn
		(ac-config-default)
		(global-auto-complete-mode t)
		)
	)

(use-package hungry-delete
  :ensure t
  :config
  ;;(global-hungry-delete-mode)
)

(use-package expand-region
	:ensure t
	:config
	(global-set-key (kbd "C-=") 'er/expand-region)
	)

(use-package counsel
	:ensure t
  :bind
  (
	 ("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)
	 )
	)

(use-package smex
	:ensure t
  :bind
	:config
	(global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
)

(use-package neotree
	:ensure t
	)

;;(use-package aggressive-indent
;; 	:ensure t
;; 	:config
;; 	(global-aggressive-indent-mode 1)
;; 	)

;;(use-package iedit
;; 	:ensure t
;; 	:config
;; 	(global-set-key (kbd "M-;") 'iedit-mode)
;; 	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* visual upgrade packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package theme-looper
	:ensure t
	:config
	(global-set-key (kbd "C-t") 'theme-looper-enable-next-theme)
	)

;;(use-package gruber-darker-theme
;; 	:ensure t
;; 	)

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  )

;;(use-package smart-mode-line
;;  :ensure t
;;  :config
;;	(smart-mode-line-enable)
;;  )

;;(use-package rainbow-mode
;;  :ensure t
;;  )
;;
;;(use-package rainbow-identifiers
;;  :ensure t
;;  )
;;
;;(use-package rainbow-delimiters
;;  :ensure t
;;  )
;;
;;(use-package rainbow-blocks
;;  :ensure t
;;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
	:ensure t
	)

(use-package org-bullets
	:ensure t
	:config
	(add-hook 'org-mode-hook
						(lambda () (org-bullets-mode 1))
						)
	)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-window
	:ensure t
	:init
	(progn
		(global-set-key [remap other-window] 'ace-window)
		(custom-set-faces
		 '(aw-leading-char-face
			 ((t (:inherit ace-jump-face-foreground :height 3.0)))))
		)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pool check out
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package shell-pop
	:ensure t
  :bind
	(
	 ("C-t" . shell-pop)
	 )
	:config
	(setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
	)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* useful packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;;* e.g. 4hugo
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lorem-ipsum
  :ensure t
  :config
	(lorem-ipsum-use-default-bindings)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;* e.g. 4hugo
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package datetime-format
  :ensure t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;
;;* git, subversion, ...
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  )

;;(use-package egg
;;  :ensure t
;;  )

;;;;;;;;;;;;;;;;;;;;;;;;;
;;* ascii art
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package boxquote
  :ensure t
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* prog modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
	:ensure t
	:config
	(global-flycheck-mode t)
	)

(use-package yasnippet
	:ensure t
	:config
	(yas-global-mode 1)
	)

;;(use-package yasnippet-snippets
;;  :ensure t
;;  )
;;(use-package go-snippets
;;  :ensure t
;;  )
;;(use-package java-snippets
;;  :ensure t
;;  )
;;
;;
;;(use-package go-mode
;;  :ensure t
;;  )
;;
;;(use-package go-playground
;;  :ensure t
;;  )

(use-package sqlup-mode
	:ensure t
	)

(use-package basic-mode
	:ensure t
	)

(use-package markdown-mode
	:ensure t
	)

(use-package markdown-mode+
	:ensure t
	)

(use-package powershell
	:ensure t
	)

(use-package csharp-mode
	:ensure t
	)

(use-package web-mode
	:ensure t
	:config
	(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	(setq web-mode-engines-alist
				'(
					("go"  . "/layouts/.*\\.html\\'")  ;; layouts
					)
				)
	(setq web-mode-extra-snippets
				'(
					("go" . (
									 ("x"	. "{{ | }}")
									 ("dot"	. "{{ . }}")
									 ("if"	. "{{ if | }} {{ end }}")
									 ("ife"	. "{{ if | }} {{ else }} {{ end }}")
									 ("ifei"	. "{{ if | }} {{ else if }} {{ end }}")
									 ("with"	. "{{ with | }} {{ end }}")
									 ("withe"	. "{{ with | }} {{ else }} {{ end }}")
									 ("range"	. "{{ range | }} {{ end }}")
									 ("partial" . "{{ partial \"|\" . }}")
									 ("block"	. "{{ block \"|\" . }} {{ end }}")
									 ("define"	. "{{ define \"|\" }} {{ end }}")
									 )
					 )
					;;(nil . (
					;;	      ("input" . ("\t<input type=\"" . "\"/>"))
					;;	      ("a" . ("\t<a href=\"#\">" . "</a>"))
					;;	      ("img" . ("\t<img src=\"" . "\">"))
					;;	      ("div" . ("\t<div>" . "</div>"))
					;;	      )
					;;	   )
					)
				)
	;;(setq web-mode-ac-sources-alist
	;;	    '(("css" . (ac-source-css-property))
	;;	      ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
	)

(use-package yaml-mode
	:ensure t
	)

(use-package toml-mode
	:ensure t
	)

(use-package csv-mode
	:ensure t
	)

(use-package dockerfile-mode
	:ensure t
	)

(use-package htmlize
	:ensure t
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* generic-x
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'generic-x)
(setq auto-mode-alist (cons '("\\.cmd$\\|\\.CMD$" . bat-generic-mode) auto-mode-alist))
;;(setq generic-define-mswindows-modes t
;;      generic-define-unix-modes t
;;      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;** CUA (Motif/Windows GUI style shortcuts)
;;   still some problems when mixing C-w M-w C-y with C-c C-x C-v
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cua-base)
(cua-mode 't)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C-x C-j
(require 'dired-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* show line and col pos in modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display-time)
;;(line-number-mode t)
(global-linum-mode t)
(column-number-mode t)
(global-hl-line-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* ibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(defalias 'list-buffers 'ibuffer)
(defalias 'list-buffers 'ibuffer-other-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* revert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq save-interprogram-paste-before-kill 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* winner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(winner-mode 1)
;; C left, C right
;;(windmove-default-keybindings)
;; S up, down, left
;; => ace-window

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* indo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(ido-mode 'files)
;;(ido-mode 'buffer)
;;(ido-mode 1)
;;(ido-everywhere)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* hippie expand
;;  Try to expand text before point, using multiple methods.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq hippie-expand-try-functions-list
      '(
				try-expand-dabbrev
				try-complete-file-name-partially
				try-complete-file-name
				try-expand-all-abbrevs
				try-expand-list
				try-expand-line
				try-expand-dabbrev-all-buffers
				try-expand-dabbrev-from-kill
				)
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* backups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq make-backup-files t)
(setq version-control t)		; 'never
(setq kept-old-versions 1)
(setq kept-new-versions 10)
(setq delete-old-versions t)
(setq vc-make-backup-files t)

(setq auto-save-list-file-prefix "~/.emacs.d/auto-save-list/.saves-")

(setq backup-directory-alist nil)
(add-to-list 'backup-directory-alist '("."	           . "~/.emacs.d/backups/"))
(add-to-list 'backup-directory-alist '(".\\.awk$"      . "~/.emacs.d/backups/awk"))
(add-to-list 'backup-directory-alist '(".\\.bas$"      . "~/.emacs.d/backups/bas"))
(add-to-list 'backup-directory-alist '(".\\.bat$"      . "~/.emacs.d/backups/bat"))
(add-to-list 'backup-directory-alist '(".\\.bmk$"      . "~/.emacs.d/backups/bmk"))
(add-to-list 'backup-directory-alist '(".\\.cfg$"      . "~/.emacs.d/backups/cfg"))
(add-to-list 'backup-directory-alist '(".\\.cmd$"      . "~/.emacs.d/backups/cmd"))
(add-to-list 'backup-directory-alist '(".\\.conf$"     . "~/.emacs.d/backups/conf"))
(add-to-list 'backup-directory-alist '(".\\.css$"      . "~/.emacs.d/backups/css"))
(add-to-list 'backup-directory-alist '(".\\.ctl$"      . "~/.emacs.d/backups/ctl"))
(add-to-list 'backup-directory-alist '(".\\.cvs.*$"    . "~/.emacs.d/backups/cvs"))
(add-to-list 'backup-directory-alist '(".\\.el$"       . "~/.emacs.d/backups/el"))
(add-to-list 'backup-directory-alist '(".\\.html$"     . "~/.emacs.d/backups/html"))
(add-to-list 'backup-directory-alist '(".\\.ini$"      . "~/.emacs.d/backups/ini"))
(add-to-list 'backup-directory-alist '(".\\.java$"     . "~/.emacs.d/backups/java"))
(add-to-list 'backup-directory-alist '(".\\.js$"       . "~/.emacs.d/backups/js"))
(add-to-list 'backup-directory-alist '(".\\.ksh$"      . "~/.emacs.d/backups/ksh"))
(add-to-list 'backup-directory-alist '(".\\.perl$"     . "~/.emacs.d/backups/perl"))
(add-to-list 'backup-directory-alist '(".\\.php$"      . "~/.emacs.d/backups/php"))
(add-to-list 'backup-directory-alist '(".\\.pl$"       . "~/.emacs.d/backups/pl"))
(add-to-list 'backup-directory-alist '(".\\.pls$"      . "~/.emacs.d/backups/pls"))
(add-to-list 'backup-directory-alist '(".\\.py$"       . "~/.emacs.d/backups/py"))
(add-to-list 'backup-directory-alist '(".\\.reg$"      . "~/.emacs.d/backups/reg"))
(add-to-list 'backup-directory-alist '(".\\.sh$"       . "~/.emacs.d/backups/sh"))
(add-to-list 'backup-directory-alist '(".\\.sql$"      . "~/.emacs.d/backups/sql"))
(add-to-list 'backup-directory-alist '(".\\.tex$"      . "~/.emacs.d/backups/tex"))
(add-to-list 'backup-directory-alist '(".\\.texi$"     . "~/.emacs.d/backups/texi"))
(add-to-list 'backup-directory-alist '(".\\.txt$"      . "~/.emacs.d/backups/txt"))
(add-to-list 'backup-directory-alist '(".\\.vb$"       . "~/.emacs.d/backups/vb"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* win32 configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (memq window-system '(win32 w32))
    (progn
      (setq grep-command "findstr /n ")
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* new keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<f2>")    'toggle-truncate-lines)
(global-set-key (kbd "<f3>")	  'list-matching-lines)
(global-set-key (kbd "<f5>")    'revert-buffer)
(global-set-key (kbd "<f9>")    'eval-region)
(global-set-key (kbd "<f12>")   'hippie-expand)
(global-set-key (kbd "<S-f12>") 'complete-symbol)
(global-set-key (kbd "<C-f12>") 'cua-set-rectangle-mark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* fix bug emacs 24.3 / ubunut 14.04
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/DeadKeys
(if (>= emacs-major-version 24)
    (require 'iso-transl)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;** 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun align-to-colon (begin end)
  "Align region to colon (:) signs"
  (interactive "r")
  (align-regexp
	 begin end
   (rx (group (zero-or-more (syntax whitespace))) ":") 1 1 )
	)

(defun align-to-comma (begin end)
  "Align region to comma  signs"
  (interactive "r")
  (align-regexp
	 begin end
   (rx "," (group (zero-or-more (syntax whitespace))) ) 1 1 )
	)


(defun align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp
	 begin end
   (rx (group (zero-or-more (syntax whitespace))) "=") 1 1 )
	)


(defun align-to-hash (begin end)
  "Align region to hash ( => ) signs"
  (interactive "r")
  (align-regexp
	 begin end
   (rx (group (zero-or-more (syntax whitespace))) "=>") 1 1 )
	)

(defun align-to-comma-before (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp
	 begin end
   (rx (group (zero-or-more (syntax whitespace))) ",") 1 1 )
	)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
	(mapc 'kill-buffer 
				(delq (current-buffer) (buffer-list))
				)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;** scroll
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gro-scroll-down1 ()
	"Scroll one line down and recenter."
  (interactive)
  (scroll-down 1)
  (move-to-window-line nil)
  )

(defun gro-scroll-up1 ()
  "Scroll one line up and recenter."
  (interactive)
  (scroll-up 1)
  (move-to-window-line nil)
  )

(defun gro-scroll-down2 ()
  "Scroll one line down."
  (interactive)
  (scroll-down 1)
  )

(defun gro-scroll-up2 ()
  "Scroll one line up."
  (interactive)
  (scroll-up 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;** cmd
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (memq window-system '(w32))
    (progn
      (defun gro-cmd-help
	(name)
	"Get cmd (NT) help"
	(interactive "sHelp on cmd: \n")
	(switch-to-buffer "*cmd help*")
	(erase-buffer)
	(insert (format "%s" (shell-command-to-string (format "help %s" name))))
	(beginning-of-buffer)
	)
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;** perl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gro-perldoc-help
  (name)
  "Get perldoc help on 'NAME' (global)."
  (interactive "sHelp on global: \n")
  (switch-to-buffer "*perl doc*")
  (erase-buffer)
  (insert (format "%s" (shell-command-to-string (format "perldoc -t %s" name))))
  (beginning-of-buffer)
)

(defun gro-perldoc-func-help
  (name)
  "Get perldoc help on 'NAME' (function)."
  (interactive "sHelp on function: \n")
  (switch-to-buffer "*perl function*")
  (erase-buffer)
  (insert (format "%s" (shell-command-to-string (format "perldoc -t -f %s" name))))
  (beginning-of-buffer)
  )

(defun gro-perldoc-package-help
  (name)
  "Get perldoc help on 'NAME' () package."
  (interactive "sHelp on package: \n")
  (switch-to-buffer "*perl package doc*")
  (erase-buffer)
  (insert (format "%s" (shell-command-to-string (format "perldoc -t %s" name))))
  (beginning-of-buffer)
  )

(defun gro-perldoc-faq-help
  (name)
  "Get perldoc help on 'NAME' (faq)."
  (interactive "sFAQ on: \n")
  (switch-to-buffer "*perl faq*")
  (erase-buffer)
  (insert (format "%s" (shell-command-to-string (format "perldoc -t -q %s" name))))
  (beginning-of-buffer)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;** misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gro-insert-date ()
  "Insert date at point."
  (interactive)
  ;;(insert (format-time-string "%Y-%m-%d"))
  ;;(insert (format-time-string "%Y-%m-%dT%T%z"))
  ;;(insert (format-time-string "%FT%T%Z"))
  (insert (format-time-string "%Y-%m-%d %H:%M"))
  )

;; datetime-format
;; (package-install 'datetime-format)
(defun gro-insert-date-atom ()
  "Insert date at point."
  (interactive)
  (insert (datetime-format 'atom))
  )

(defun gro-ascii-table ()
  "Print the ascii table.  Based on a defun by Alex Schroeder <asc@bsiag.com>."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert "ASCII\n")
  (insert "-----\n")
  (let ((i 31))				;or 0 (non printable)
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%3d %2xh '%c'   " i i i) ;%X from  20.3
	      )
      (if (= 0 (% (- i 31) 4) )
	  (insert "\n") )
      )
    )
  (beginning-of-buffer)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;** text sauber machen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-buffer-file-coding-system
;; C-x RET f
(defun gro-unix2dos ()
  "Dos file."
  (interactive)
  (set-buffer-file-coding-system 'dos)
  ;;"Setzt alle \r"
  ;;(interactive)
  ;;(goto-line 1)
  ;;(replace-regexp "\n" "\r\n")
  )

(defun gro-clean ()
  "Untabify, delete-trailing-whitespace."
  (interactive)
  (untabify (point-min) (point-max) )
  ;;(gro-dos2unix)
  (delete-trailing-whitespace)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;** texi helper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gro-convert-to-texi-coding ()
  "Ersetzt alle Umlaute so wie in texi verlangt."
  (interactive)
  (goto-line 1)
  ;;(iso-iso2tex)
  (goto-line 1) (while (search-forward "ä" nil t) (replace-match "@\"a" nil t))
  (goto-line 1) (while (search-forward "ö" nil t) (replace-match "@\"o" nil t))
  (goto-line 1) (while (search-forward "ü" nil t) (replace-match "@\"u" nil t))
  (goto-line 1) (while (search-forward "Ä" nil t) (replace-match "@\"A" nil t))
  (goto-line 1) (while (search-forward "Ö" nil t) (replace-match "@\"O" nil t))
  (goto-line 1) (while (search-forward "Ü" nil t) (replace-match "@\"U" nil t))
  )

(defun gro-convert-to-html-coding ()
  "Ersetzt alle Umlaute so wie in html verlangt."
  (interactive)
  (goto-line 1)
  ;;(iso-iso2sgml)
  (goto-line 1) (while (search-forward "ä" nil t) (replace-match "&auml;" nil t))
  (goto-line 1) (while (search-forward "ö" nil t) (replace-match "&ouml;" nil t))
  (goto-line 1) (while (search-forward "ü" nil t) (replace-match "&uuml;" nil t))
  (goto-line 1) (while (search-forward "Ä" nil t) (replace-match "&Auml;" nil t))
  (goto-line 1) (while (search-forward "Ö" nil t) (replace-match "&Ouml;" nil t))
  (goto-line 1) (while (search-forward "Ü" nil t) (replace-match "&Uuml;" nil t))
  )

(defun gro-texi-build ()
  "Initialisiere texi-datei neu."
  (interactive)
  (untabify (point-min) (point-max) )
  (delete-trailing-whitespace)
  (gro-convert-to-texi-coding)
  (texinfo-insert-node-lines (point-min) (point-max) t)
  (texinfo-update-node (point-min) (point-max) )
  (texinfo-all-menus-update)
  (delete-trailing-whitespace)
  )

(defun gro-texi-delete-all-nodes ()
  "loesche node informationen"
  (interactive)
  (goto-line 1)
  (replace-regexp "^@node.*$" "@node")
	;;  (replace-regexp "^@menu.*@end menu$" "")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;** tmp helper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gro-convert-from-html-coding ()
  "Ersetzt alle html-Umlaute in Umlaute (local coding)"
  (interactive)
  (goto-line 1)
  ;;(iso-sgml2iso)
  (goto-line 1) (while (search-forward "&auml;" nil t) (replace-match "ä" nil t))
  (goto-line 1) (while (search-forward "&ouml;" nil t) (replace-match "ö" nil t))
  (goto-line 1) (while (search-forward "&uuml;" nil t) (replace-match "ü" nil t))
  (goto-line 1) (while (search-forward "&Auml;" nil t) (replace-match "Ä" nil t))
  (goto-line 1) (while (search-forward "&Ouml;" nil t) (replace-match "Ö" nil t))
  (goto-line 1) (while (search-forward "&Uuml;" nil t) (replace-match "Ü" nil t))
  )

(defun gro-convert-wordcol ()
  ""
  (interactive)
  (goto-line 1)
  (replace-regexp "^" "\"")
  (goto-line 1)
  (replace-regexp "\n" "\" ")
  )

(defun gro-trim-string-spaces ()
  ""
  (interactive)
  (goto-line 1)
  (replace-regexp "[ \t]+\"" "\"")
  )

(defun gro-finalize-string ()
  ""
  (interactive)
  (goto-line 1)
  (replace-regexp "\n" "\"\n")
  )

(defun gro-M2Newline ()
  ""
  (interactive)
  (goto-line 1)
  (replace-regexp "\r" "\n")
  )

;;(defun gro-Xls2Sql-Cleanup ()
;;  ""
;;  (interactive)
;;  (goto-line 1) (while (search-forward "\n" nil t) (replace-match "X" nil t))
;;  (goto-line 1) (while (search-forward "\t" nil t) (replace-match "Y" nil t))
;;  ;;(goto-line 1)
;;  ;;(replace-regexp "\\n" "\n")
;;  ;;(replace-regexp "\\t" "\t")
;;  )

(fset 'gro-kbd-Xls2Sql
			[escape ?< escape ?% ?\\ ?t return ?\C-q tab return ?! escape ?< escape ?% ?\\ ?n return ?\C-q ?\C-j return ?! escape ?<])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* Keys for own functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<M-down>")    'gro-scroll-down1)
(global-set-key (kbd "<M-up>")			'gro-scroll-up1)
(global-set-key (kbd "<M-S-down>")	'gro-scroll-down2)
(global-set-key (kbd "<M-S-up>")		'gro-scroll-up2)
(global-set-key (kbd "C-;")				  'gro-insert-date)
(global-set-key (kbd "C-:")				  'gro-insert-date-atom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(css-indent-level 2)
 '(electric-indent-mode nil)
 '(js-indent-level 2)
 '(package-selected-packages
	 '(shell-pop powershell neotree csharp-mode toml-mode htmlize csv-mode dockerfile-mode yaml-mode yamel-mode gruber-darker-theme smex theme-looper counsel iedit expand-region aggressive-indent aggressice-indent hungry-delete boxquote egg magit web-mode markdown-mode+ markdown-mode basic-mode sqlup-mode go-playground go-mode java-snippets go-snippets yasnippet-snippets yasnippet flycheck datetime-format lorem-ipsum ivy ace-window beacon editorconfig auto-complete which-key try use-package))
 '(safe-local-variable-values '((engine . go) (engine . ENGINE_NAME)))
 '(sql-product 'ms)
 '(tab-width 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
;;; init.el ends here

