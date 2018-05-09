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

;; https://github.com/jwiegley/use-package

;;; code:
;(require 'package)
;(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     )

;;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(unless package--initialized (package-initialize t))
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
;; (org-babel-load-file (expand-file-name "~./.emacs.d/myinit.org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold (* 511 1024 1024))
(setq gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)
(setq garbage-collection-messages t)

(setq inhibit-startup-message t)
;;(view-echo-area-messages)
;;(tool-bar-mode -1)
(load-theme 'manoj-dark)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; => paradox-list-packages
;;(use-package paradox
;;  :config
;;  (setq paradox-spinner-type 'progress-bar)
;;  )

;;;; => (auto-package-update-now)
;;(use-package auto-package-update
;;  :ensure t
;;  :config
;;  (auto-package-update-at-time "03:00")
;;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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
(use-package delight
  :ensure t
	)
;; :delight

;; https://github.com/joewreschnig/auto-minor-mode
(use-package auto-minor-mode
  :ensure t
)
;; :minor and :magic-minor
;; auto-minor-mode-alist
;; auto-minor-mode-magic-alist


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; most important packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package try
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

(use-package hungry-delete
  :ensure t
  :init
  (global-hungry-delete-mode)
)

(use-package expand-region
  :init
	(global-set-key (kbd "C-=") 'er/expand-region)
	)

;;(use-package counsel
;; 	:ensure t
;;  :bind (
;; 	        ("M-y" . counsel-yank-pop)
;;          :map ivy-minibuffer-map
;;          ("M-y" . ivy-next-line)
;; 	        )
;; 	)

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

;;(use-package undo-tree
;;  :config
;;  (global-undo-tree-mode)
;;)

;;(use-package auto-complete
;;  :ensure t
;;  :init
;;  (ac-config-default)
;;  :config
;;  (global-auto-complete-mode t)
;;  )

(use-package company
  :config
  ;; Global
  (setq company-idle-delay 1
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-limit 20
    )

  ;; Facing
  (unless (face-attribute 'company-tooltip :background)
    (set-face-attribute 'company-tooltip nil :background "black" :foreground "gray40")
    (set-face-attribute 'company-tooltip-selection nil :inherit 'company-tooltip :background "gray15")
    (set-face-attribute 'company-preview nil :background "black")
    (set-face-attribute 'company-preview-common nil :inherit 'company-preview :foreground "gray40")
    (set-face-attribute 'company-scrollbar-bg nil :inherit 'company-tooltip :background "gray20")
    (set-face-attribute 'company-scrollbar-fg nil :background "gray40")
    )

  ;; Default backends
  (setq company-backends '((company-files)))

  ;; Activating globally
  (global-company-mode t)
)


(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 1)
)
;;(eval-after-load 'company
;;  '(define-key company-active-map (kbd "C-c q") #'company-quickhelp-manual-begin)
;;  )

;; M-h
;;(use-package company-quickhelp					; Documentation popups for Company
;;  :ensure t
;;;;  :config
;;  (eval-after-load 'company
;;    '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)
;;    )
;;  :hook
;;  (global-company-mode-hook)
;;  (company-quickhelp-mode)   	)

(use-package company-dict
  :after company
  :bind
  ("C-c h" . company-dict)
  :init
  (setq company-dict-dir (concat user-emacs-directory "dict/"))
  :config
  (add-to-list 'company-backends 'company-dict)
  )

;; company-dict.el
;;(defun company-dict--quickhelp-string (data)
;;  (get-text-property 0 :meta data))
;;
;;      (quickhelp-string (company-dict--quickhelp-string arg))


(use-package company-web
  :after company
  :bind
  ("C-c w" . company-web-html)
  :config
  (add-to-list 'company-backends 'company-web-html)
  )

;;(use-package company-plsense
;;  :ensure t
;;  )

;;(use-package company-emoji
;;  :ensure t
;;  :after company
;;  :config
;;  (add-to-list 'company-backends 'company-emoji)
;;  (company-emoji-init)
;;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(use-package company
;;  :ensure t
;;;;  :bind ("C-<tab>" . company-complete)
;;  :init (global-company-mode)
;;  :config
;;  (bind-key [remap completion-at-point] #'company-complete company-mode-map)
;;  (setq
;;    company-tooltip-align-annotations t
;;    company-show-numbers t
;;    company-dabbrev-downcase nil
;;    )
;;  (setq company-backends (delete 'company-semantic company-backends))
;;  ;;(setq company-backends '(company-clang))
;;  ;;(add-to-list 'company-backends 'company-gtags)
;;  ;;(add-to-list 'company-backends 'company-c-headers)
;;  ;;:diminish company-mode
;;  )

;;(delete 'company-dabbrev company-backends)
;;(delete '(company-dabbrev-code company-gtags company-etags company-keywords) company-backends)
;;(add-to-list 'company-backends '(company-auctex)
;;
;;  (company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
;;  	(company-dabbrev-code company-gtags company-etags company-keywords)
;;  	company-oddmuse company-dabbrev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(use-package yasnippet
  :after company
  :config

  ;; Adding yasnippet support to company
  (add-to-list 'company-backends '(company-yasnippet))

  ;; Activate global
  (yas-global-mode)
)

(use-package yatemplate
  :after yasnippet
  :config

  ;; Define template directory
  ;;(setq yatemplate-dir (concat config-basedir "/third_parties/templates"))

  ;; Coupling with auto-insert
  (setq auto-insert-alist nil)
  (yatemplate-fill-alist)
  ;; (add-hook 'find-file-hook 'auto-insert)
  )


;;(use-package yasnippet
;;  :ensure t
;;  :diminish yas-minor-mode
;;  :config
;;  (yas-global-mode 1)
;;  ;; Remove Yasnippet's default tab key binding
;;  (define-key yas-minor-mode-map (kbd "TAB") nil)
;;  ;; Set Yasnippet's key binding to shift+tab
;;  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
;;  )

;;(use-package yasnippet-snippets
;;  :ensure t
;;  )
;;(use-package go-snippets
;;  :ensure t
;;  )
;;(use-package java-snippets
;;  :ensure t
;;  )


;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
    backend
    (append (if (consp backend) backend (list backend))
      '(:with company-yasnippet))))

;;(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))




(use-package neotree
	)

(use-package treemacs
  :after hl-line-mode
  :bind
  (:map global-map
        ([f8]        . treemacs-toggle)
    )
  :config
  (setq treemacs-follow-after-init          t
        treemacs-width                      35
        treemacs-indentation                2
        treemacs-git-integration            t
        treemacs-collapse-dirs              3
        treemacs-silent-refresh             nil
        treemacs-change-root-without-asking nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-show-hidden-files          t
        treemacs-never-persist              nil
        treemacs-is-never-other-window      nil
        treemacs-goto-tag-strategy          'refetch-index
    )
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  )

(use-package treemacs-projectile
  :after treemacs
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header)
  )

;;(use-package aggressive-indent
;; 	:ensure t
;; 	:config
;; 	(global-aggressive-indent-mode 1)
;; 	)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* visual upgrade packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package theme-looper
	:init
	(global-set-key (kbd "C-t") 'theme-looper-enable-next-theme)
	)

;;(use-package gruber-darker-theme
;; 	:ensure t
;; 	)

(use-package beacon
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


;; https://github.com/doublep/logview
(use-package logview
  :config
  (add-to-list 'auto-mode-alist '("syslog\\(?:\\.[0-9]+\\)" . logview-mode))
  (add-to-list 'auto-mode-alist '("\\.log\\(?:\\.[0-9]+\\)?\\'" . logview-mode))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package org
;; 	)
;;
;;(use-package org-bullets
;; 	:config
;; 	(add-hook 'org-mode-hook
;; 						(lambda () (org-bullets-mode 1))
;; 						)
;; 	)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-window
	:init
	:config
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
(use-package smart-tab
  :ensure t
	:config
    (global-smart-tab-mode 1)
		)


;;(use-package shell-pop
;;  :ensure t
;;  :bind
;;  (
;;   ("C-t" . shell-pop)
;;   )
;;  :config
;;  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
;;  )


;;(use-package company-go
;;  :ensure t
;;;;  :init
;;  (with-eval-after-load 'company
;;    (add-to-list 'company-backends 'company-go))
;;)
;;
;;
;;(use-package go-mode
;;  :ensure t
;;  :init
;;  (progn
;;    (setq gofmt-command "goimports")
;;    (add-hook 'before-save-hook 'gofmt-before-save)
;;    (bind-key [remap find-tag] #'godef-jump))
;;  :config
;;  (add-hook 'go-mode-hook 'electric-pair-mode)
;;  )
;;
;;(use-package go-eldoc
;;  :ensure t
;;  :defer
;;  :init
;;  (add-hook 'go-mode-hook 'go-eldoc-setup)
;;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* useful packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package font-lock-studio
	)

;; comments and strings are pre-colored, as they are part of the
;; earlier syntactic phase (which isn't supported by Font Lock
;; Studio).


;;;;;;;;;;;;;;;;;;;;;;;;;
;;* e.g. 4hugo
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lorem-ipsum
  :config
	(lorem-ipsum-use-default-bindings)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;* e.g. 4hugo
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package datetime-format
  )

;;;;;;;;;;;;;;;;;;;;;;;;;
;;* git, subversion, ...
;;;;;;;;;;;;;;;;;;;;;;;;;
;;(use-package magit
;;  )

;;(use-package egg
;;  :ensure t
;;  )

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

;;(use-package go-mode
;;  :ensure t
;;  )
;;
;;(use-package go-playground
;;  :ensure t
;;  )

(use-package basic-mode
	)

;; slow startup
;;(use-package csharp-mode
;; 	)

(use-package powershell
	)

(use-package sqlup-mode
	)

(use-package web-mode
	:config
	;;(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	(setq web-mode-engines-alist
				'(
					("go"  . "/layouts/.*\\.html\\'")  ;; layouts
					)
				)
	(setq web-mode-enable-auto-indentation nil)
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* fileformat modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode   )
(use-package markdown-mode+	 )
(use-package yaml-mode			 )
(use-package yaml-tomato		 )
(use-package toml-mode			 )
(use-package dockerfile-mode )

;;(use-package csv-mode
;;  :ensure t
;;  )

(use-package csv-mode
  :config
  ;; Define separators
  (setq csv-separators '("," ";" ":" " "))
)
(use-package csv-nav          :disabled)

(use-package htmlize         )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* debug emacs init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (profiler-start 'cpu)
;; ...
;; (profiler-report)
;; (profiler-stop)
;; https://emacs.stackexchange.com/questions/39484/speed-up-emacs-start-up-time
;; ESUP - Emacs Start Up Profiler
;; (use-package :defer t)

(use-package esup )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* my modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-if-exists (f)
  "Load the elisp file F only if it exists and is readable."
  (if (file-readable-p f)
    (load-file f)
    )
  )

(if (file-readable-p "~/.emacs.d/hugo-tmpl-mode.el")
  (progn
    (load-file "~/.emacs.d/hugo-tmpl-mode.el")
    (font-lock-add-keywords 'mhtml-mode hugo-tmpl-font-lock-keywords)
    ;;(font-lock-add-keywords 'html-mode hugo-tmpl-font-lock-keywords)
    (add-to-list 'auto-mode-alist '("/layouts/.*\\.html\\'" . hugo-tmpl-mode))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq system-time-locale "en_US.utf8")
(prefer-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

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
;;* ibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(defalias 'list-buffers 'ibuffer)
;;(defalias 'list-buffers 'ibuffer-other-window)

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
;;* recentf (build in (27.0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package recentf
  :init
  (recentf-mode 1)

  :config
  ;; Increase limit
  (setq recentf-max-menu-items 100)

  ;;; ;; Emacs
  ;;; (add-to-list 'recentf-exclude (format "%s/configuration/emacs\\.d/\\(?!\\(main.*\\)\\)" (getenv "HOME")))
  ;;; (add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/.*" (getenv "HOME")))
  ;;;
  ;;; ;; Some caches
  ;;; (add-to-list 'recentf-exclude (format "%s/\\.ido\\.last" (getenv "HOME")))
  ;;; (add-to-list 'recentf-exclude (format "%s/\\.recentf" (getenv "HOME")))
  ;;;
  ;;;
  ;;; ;; elfeed
  ;;; (add-to-list 'recentf-exclude (format "%s/\\.elfeed/.*" (getenv "HOME")))
  ;;; (add-to-list 'recentf-exclude (format "%s/Dropbox/emacs/elfeed/.*" (getenv "HOME")))
  ;;;
  ;;; ;; Org-mode organisation
  ;;; (add-to-list 'recentf-exclude (format "%s/Dropbox/org/organisation/.*" (getenv "HOME")))
  ;;;
  ;;; ;; Org/todo/calendars
  ;;; (add-to-list 'recentf-exclude ".*todo.org")
  ;;; (add-to-list 'recentf-exclude (format "%s/Calendars/.*" (getenv "HOME")))
  ;;;
  ;;; ;; Maildir
  ;;; (add-to-list 'recentf-exclude (format "%s/maildir.*" (getenv "HOME")))

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
;;** check out
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my:auth-source-get-passwd (&rest spec &allow-other-keys)
  (let ((founds (apply 'auth-source-search spec)))
    (when founds
      (funcall (plist-get (nth 0 founds) :secret)))))

(defun my:auth-source-get-user (&rest spec &allow-other-keys)
  (let ((founds (apply 'auth-source-search spec)))
    (when founds
      (plist-get (nth 0 founds) :user))))

(defun edit-current-file-as-root ()
  "Edit the file that is associated with the current buffer as root"
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq file (concat "/sudo:localhost:" (buffer-file-name)))
        (find-file file))
    (message "Current buffer does not have an associated file.")))

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


;;(defun align-to-equals (begin end)
;;  "Align region to equal signs"
;;  (interactive "r")
;;  (align-regexp
;;   begin end
;;   (rx (group (zero-or-more (syntax whitespace))) "=") 1 1 )
;;  )

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
 '(package-selected-packages
	 '(esup htmlize csv-mode dockerfile-mode toml-mode yaml-tomato yaml-mode markdown-mode+ markdown-mode web-mode sqlup-mode powershell csharp-mode basic-mode flycheck boxquote datetime-format lorem-ipsum font-lock-studio smart-tab logview beacon theme-looper treemacs-projectile treemacs neotree yatemplate yasnippet company-web company-dict company-quickhelp company undo-tree iedit smex counsel expand-region hungry-delete editorconfig which-key try auto-minor-mode delight diminish use-package))
 '(paradox-github-token t)
 '(powershell-indent 2)
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
