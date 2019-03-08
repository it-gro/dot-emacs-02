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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;; my mini Emacs config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* how to use in a sandbox:
;; $ \emacs -Q -l $HOME/.emacs.d/mini/init.mini.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar user-home-directory
	(concat (getenv "HOME") "/")
	) ; must end with /

(setq user-emacs-directory
	(concat user-home-directory ".emacs.d/mini/")
	) ; must end with /


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* initialize packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     )


(if (<= emacs-major-version 26)
		(package-initialize)
  )

(package-initialize)
;;(unless package--initialized (package-initialize t))
;; M-x package-refresh-contents
;; M-x package-list-packages
;; M-x list-packages

;; https://github.com/jwiegley/use-package
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
	)

(require 'use-package)
(setq use-package-always-ensure t
  use-package-verbose t
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* boot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t)
;;(view-echo-area-messages)
;;(tool-bar-mode -1)
(load-theme 'manoj-dark)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add use-package functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/myrjola/diminish.el
(use-package diminish
  :ensure t
	)
;; :diminish

;; https://savannah.nongnu.org/projects/delight
;;(use-package delight
;;  :ensure t
;; 	)
;; :delight

;; https://github.com/joewreschnig/auto-minor-mode
(use-package auto-minor-mode
  :ensure t
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; most important packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(use-package hungry-delete
  :ensure t
  :init
  (global-hungry-delete-mode)
)

(use-package expand-region
  :init
	(global-set-key (kbd "C-=") 'er/expand-region)
	)

(use-package smex
  :bind
	:init
	(global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
)

(use-package iedit
  :config
  (delete-selection-mode t)
 	;; (global-set-key (kbd "M-;") 'iedit-mode)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* visual upgrade packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package theme-looper
	:init
	(global-set-key (kbd "C-t") 'theme-looper-enable-next-theme)
	)

(use-package beacon
  :config
  (beacon-mode 1)
  )

;; https://github.com/doublep/logview
(use-package logview
  :config
  (add-to-list 'auto-mode-alist '("syslog\\(?:\\.[0-9]+\\)" . logview-mode))
  (add-to-list 'auto-mode-alist '("\\.log\\(?:\\.[0-9]+\\)?\\'" . logview-mode))
)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;* ascii art
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package boxquote
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* prog modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
	:ensure t
	:config
	(global-flycheck-mode t)
	)


(use-package basic-mode
	)

(use-package powershell
	)

(use-package sqlup-mode
	)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* fileformat modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode   )
(use-package markdown-mode+	 )
(use-package yaml-mode			 )
(use-package yaml-tomato		 )
(use-package toml-mode			 )
(use-package dockerfile-mode )

(use-package csv-mode
  :config
  ;; Define separators
  (setq csv-separators '("," ";" ":" " "))
)
(use-package csv-nav          :disabled)

(use-package htmlize         )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* my modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-if-exists (f)
  "Load the elisp file F only if it exists and is readable."
  (if (file-readable-p f)
    (load-file f)
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq system-time-locale "en_US.utf8")
(prefer-coding-system 'utf-8)
;;(set-selection-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* generic-x
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'generic-x)
;;(setq auto-mode-alist (cons '("\\.cmd$\\|\\.CMD$" . bat-generic-mode) auto-mode-alist))
(setq generic-define-mswindows-modes t
      generic-define-unix-modes t
      )

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
;;* revert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq save-interprogram-paste-before-kill 1)

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
;;* recentf (build in (27.0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package recentf
  :init
  (recentf-mode 1)

  :config
  ;; Increase limit
  (setq recentf-max-menu-items 100)
  )

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
   (align-regexp begin end "\\(\\s-*\\)=" 1 1 )
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gro-clean ()
  "Untabify, delete-trailing-whitespace."
  (interactive)
  (untabify (point-min) (point-max) )
  ;;(gro-dos2unix)
  (delete-trailing-whitespace)
  )


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
;;* server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(server-start)
(load "server") (unless (server-running-p) (server-start))

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
 '(powershell-indent 2)
 '(sql-product 'ms)
 '(tab-width 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
;;; init.el ends here
