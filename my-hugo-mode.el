;;; my-huho-mode.el --- major mode for editing hugo files. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2018, by it-gro

;; Author: it-gro
;; Version: 1.0.0
;; Created: 2018-02-10
;; Keywords: languages
;; Homepage:

;; http://ergoemacs.org/emacs/elisp_syntax_coloring.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Functions.html
;; https://www.emacswiki.org/emacs/RegularExpression


;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms
;; of the GNU General Public License version 2.

;;; Commentary:

;; short description here

;; full doc on how to use here

;;; Code:

;; create the list for font-lock.
;; each category of keyword is given a particular face

(defconst hugo-actions
  '(
     "block"
     "define"
     "else"
     "end"
     "if"
     "partial"
     "range"
     "template"
     "with"
     )
  "TODO."
  )

(defconst hugo-vars
  '(
     ".File"
     ".GitInfo"
     ".Hugo"
     ".Menu"
     ".Page"
     ".Site"
     ".Sitemap"
     )
  "TODO."
  )

(defconst hugo-var-frontmatter
  '(
           ".Aliases"
           ".Date"
           ".Description"
           ".Draft"
           ".ExpiryDate"
           ".Keywords"
           ".Lastmod"
           ".Layout"
           ".LinkTitle"
           ".Markup"
           ".PublishDate"
           ".Slug"
           ".Title"
           ".Type"
           ".Weight"
     )
  "TODO."
  )

(defconst hugo-var-file
  '(
           ".BaseFileName"
           ".Dir"
           ".Ext"
           ".Lang"
           ".LogicalName"
           ".Path"
           ".TranslationBaseName"
     )
  "TODO."
  )

(defconst hugo-var-page
  '(
           ".AlternativeOutputFormats"
           ".Data"
           ".Date"
           ".Description"
           ".Dir"
           ".Draft"
           ".ExpiryDate"
           ".File"
           ".FuzzyWordCount"
           ".Hugo"
           ".IsHome"
           ".IsNode"
           ".IsPage"
           ".IsTranslated"
           ".Keywords"
           ".Kind"
           ".Lang"
           ".Language"
           ".Lastmod"
           ".LinkTitle"
           ".Next"
           ".NextInSection"
           ".OutputFormats"
           ".Pages"
           ".Permalink"
           ".PlainWords"
           ".Prev"
           ".PrevInSection"
           ".PublishDate"
           ".RSSLink"
           ".ReadingTime"
           ".Ref"
           ".RelPermalink"
           ".RelRef"
           ".Site"
           ".Title"
           ".Translations"
           ".Truncated"
           ".Type"
           ".URL"
           ".UniqueID"
           ".Weight"
           ".WordCount"
     )
  "TODO."
  )

(defconst hugo-var-gitinfo
  '(
           ".Subject"
           ".AuthorDate"
     )
  "TODO."
  )

(defconst hugo-var-menu
  '(
           ".Children"
           ".Identifier"
           ".Menu"
           ".Name"
           ".Parent"
           ".Post"
           ".Pre"
           ".URL"
           ".Weight"
     )
  "TODO."
  )

(defconst hugo-var-hugo
  '(
           ".Version"
           ".CommitHash"
           ".BuildDate"
     )
  "TODO."
  )

(defconst hugo-var-sitemap
  '(
           ".ChangeFreq"
           ".Priority"
           ".Filename"
     )
  "TODO."
  )

(defconst hugo-var-site
  '(
           ".AllPages"
           ".Author"
           ".BaseURL"
           ".BuildDrafts"
           ".Copyright"
           ".Data"
           ".DisqusShortname"
           ".GoogleAnalytics"
           ".IsMultiLingual"
           ".Language"
           ".LanguageLang"
           ".LanguageLanguageName"
           ".LanguageWeight"
           ".LanguageCode"
           ".LanguagePrefix"
           ".Languages"
           ".LastChange"
           ".Menus"
           ".Pages"
           ".Params"
           ".Permalinks"
           ".RSSLink"
           ".RegularPages"
           ".Sections"
           ".Taxonomies"
           ".Title"
     )
  "TODO."
  )

(defconst hugo-functions
  '(
           ".AddDate"
           ".Format"
           ".Get"
           ".GetPage"
           ".Param"
           ".Scratch"
           ".Unix"
           "index"
           "absLangURL"
           "absURL"
           "after"
           "apply"
           "base64"
           "chomp"
           "cond"
           "countrunes"
           "countwords"
           "dateFormat"
           "default"
           "delimit"
           "dict"
           "echoParam"
           "emojify"
           "eq"
           "errorf"
           "fileExists"
           "findRE"
           "first"
           "float"
           "ge"
           "getenv"
           "gt"
           "hasPrefix"
           "highlight"
           "htmlEscape"
           "htmlUnescape"
           "humanize"
           "i18n"
           "imageConfig"
           "in"
           "int"
           "intersect"
           "isset"
           "jsonify"
           "lang.NumFmt"
           "last"
           "le"
           "lower"
           "lt"
           "markdownify"
           "md5"
           "ne"
           "now"
           "partialCached"
           "plainify"
           "pluralize"
           "print"
           "printf"
           "println"
           "querify"
           "readDir"
           "readFile"
           "ref"
           "relLangURL"
           "relURL"
           "relref"
           "render"
           "replace"
           "replaceRE"
           "safeCSS"
           "safeHTML"
           "safeHTMLAttr"
           "safeJS"
           "safeURL"
           "seq"
           "sha"
           "shuffle"
           "singularize"
           "slice"
           "slicestr"
           "sort"
           "split"
           "string"
           "TrimLeft"
					 "TrimPrefix"
					 "TrimRight"
					 "TrimSuffix"
           "substr"
           "time"
           "title"
           "trim"
           "truncate"
           "union"
           "uniq"
           "upper"
           "urlize"
           "urls.Parse"
           "where"
     )
  "TODO."
  )


(defconst hugo-variable-regexp
  "\\b\\(\\$[_[:alpha:]]+\\)"
  "Regexp string to highlight variables."
  )

(defconst hugo-string-regexp
  "\\(`.*?`\\)"
  "Regexp string to highlight strings."
  )

(defconst hugo-comment-regexp
  "{{-?\s*\\(/\\*\\(?:.\\|\n\\)*?\\*/\\)\s*}}"
  "Regexp string to highlight comments."
  )

(defconst hugo-start-end-regexp
  "\\({{\\|}}\\)"
  "Regexp string to highlight start and end."
  )

(defconst hugo-expand
  (append
    hugo-actions
    hugo-vars
    hugo-var-frontmatter
    hugo-var-file
    hugo-var-page
    hugo-var-gitinfo
    hugo-var-menu
    hugo-var-hugo
    hugo-var-sitemap
    hugo-var-site
    hugo-functions
    )
  "TODO."
  )



(setq my-hugo-font-lock-keywords
  (let*
    (
      ;; ;; define several category of keywords
      ;; (x-actions
      ;;   '(
      ;;      "block"
      ;;      "define"
      ;;      "else"
      ;;      "end"
      ;;      "if"
      ;;      "partial"
      ;;      "range"
      ;;      "template"
      ;;      "with"
      ;;      )
      ;;   )
      ;;
      ;; (x-vars
      ;;   '(
      ;;      ".File"
      ;;      ".GitInfo"
      ;;      ".Hugo"
      ;;      ".Menu"
      ;;      ".Page"
      ;;      ".Site"
      ;;      ".Sitemap"
      ;;      )
      ;;   )
      ;;
      ;; (x-var-frontmatter
      ;;   '(
      ;;      ".Aliases"
      ;;      ".Date"
      ;;      ".Description"
      ;;      ".Draft"
      ;;      ".ExpiryDate"
      ;;      ".Keywords"
      ;;      ".Lastmod"
      ;;      ".Layout"
      ;;      ".LinkTitle"
      ;;      ".Markup"
      ;;      ".PublishDate"
      ;;      ".Slug"
      ;;      ".Title"
      ;;      ".Type"
      ;;      ".Weight"
      ;;      )
      ;;   )
      ;;
      ;; (x-var-file
      ;;   '(
      ;;      ".BaseFileName"
      ;;      ".Dir"
      ;;      ".Ext"
      ;;      ".Lang"
      ;;      ".LogicalName"
      ;;      ".Path"
      ;;      ".TranslationBaseName"
      ;;      )
      ;;   )
      ;;
      ;; (x-var-page
      ;;   '(
      ;;      ".AlternativeOutputFormats"
      ;;      ".Data"
      ;;      ".Date"
      ;;      ".Description"
      ;;      ".Dir"
      ;;      ".Draft"
      ;;      ".ExpiryDate"
      ;;      ".File"
      ;;      ".FuzzyWordCount"
      ;;      ".Hugo"
      ;;      ".IsHome"
      ;;      ".IsNode"
      ;;      ".IsPage"
      ;;      ".IsTranslated"
      ;;      ".Keywords"
      ;;      ".Kind"
      ;;      ".Lang"
      ;;      ".Language"
      ;;      ".Lastmod"
      ;;      ".LinkTitle"
      ;;      ".Next"
      ;;      ".NextInSection"
      ;;      ".OutputFormats"
      ;;      ".Pages"
      ;;      ".Permalink"
      ;;      ".PlainWords"
      ;;      ".Prev"
      ;;      ".PrevInSection"
      ;;      ".PublishDate"
      ;;      ".RSSLink"
      ;;      ".ReadingTime"
      ;;      ".Ref"
      ;;      ".RelPermalink"
      ;;      ".RelRef"
      ;;      ".Site"
      ;;      ".Title"
      ;;      ".Translations"
      ;;      ".Truncated"
      ;;      ".Type"
      ;;      ".URL"
      ;;      ".UniqueID"
      ;;      ".Weight"
      ;;      ".WordCount"
      ;;      )
      ;;   )
      ;;
      ;; (x-var-gitinfo
      ;;   '(
      ;;      ".Subject"
      ;;      ".AuthorDate"
      ;;      )
      ;;   )
      ;;
      ;; (x-var-hugo
      ;;   '(
      ;;      ".Version"
      ;;      ".CommitHash"
      ;;      ".BuildDate"
      ;;      )
      ;;   )
      ;;
      ;; (x-var-menu
      ;;   '(
      ;;      ".Children"
      ;;      ".Identifier"
      ;;      ".Menu"
      ;;      ".Name"
      ;;      ".Parent"
      ;;      ".Post"
      ;;      ".Pre"
      ;;      ".URL"
      ;;      ".Weight"
      ;;      )
      ;;   )
      ;;
      ;; (x-var-sitemap
      ;;   '(
      ;;      ".ChangeFreq"
      ;;      ".Priority"
      ;;      ".Filename"
      ;;      )
      ;;   )
      ;;
      ;; (x-var-site
      ;;   '(
      ;;      ".AllPages"
      ;;      ".Author"
      ;;      ".BaseURL"
      ;;      ".BuildDrafts"
      ;;      ".Copyright"
      ;;      ".Data"
      ;;      ".DisqusShortname"
      ;;      ".GoogleAnalytics"
      ;;      ".IsMultiLingual"
      ;;      ".Language"
      ;;      ".LanguageLang"
      ;;      ".LanguageLanguageName"
      ;;      ".LanguageWeight"
      ;;      ".LanguageCode"
      ;;      ".LanguagePrefix"
      ;;      ".Languages"
      ;;      ".LastChange"
      ;;      ".Menus"
      ;;      ".Pages"
      ;;      ".Params"
      ;;      ".Permalinks"
      ;;      ".RSSLink"
      ;;      ".RegularPages"
      ;;      ".Sections"
      ;;      ".Taxonomies"
      ;;      ".Title"
      ;;      )
      ;;   )
      ;;
      ;; (x-functions
      ;;   '(
      ;;      ".AddDate"
      ;;      ".Format"
      ;;      ".Get"
      ;;      ".GetPage"
      ;;      ".Param"
      ;;      ".Scratch"
      ;;      ".Unix"
      ;;      "index"
      ;;      "absLangURL"
      ;;      "absURL"
      ;;      "after"
      ;;      "apply"
      ;;      "base64"
      ;;      "chomp"
      ;;      "cond"
      ;;      "countrunes"
      ;;      "countwords"
      ;;      "dateFormat"
      ;;      "default"
      ;;      "delimit"
      ;;      "dict"
      ;;      "echoParam"
      ;;      "emojify"
      ;;      "eq"
      ;;      "errorf"
      ;;      "fileExists"
      ;;      "findRE"
      ;;      "first"
      ;;      "float"
      ;;      "ge"
      ;;      "getenv"
      ;;      "gt"
      ;;      "hasPrefix"
      ;;      "highlight"
      ;;      "htmlEscape"
      ;;      "htmlUnescape"
      ;;      "humanize"
      ;;      "i18n"
      ;;      "imageConfig"
      ;;      "in"
      ;;      "int"
      ;;      "intersect"
      ;;      "isset"
      ;;      "jsonify"
      ;;      "lang.NumFmt"
      ;;      "last"
      ;;      "le"
      ;;      "lower"
      ;;      "lt"
      ;;      "markdownify"
      ;;      "md5"
      ;;      "ne"
      ;;      "now"
      ;;      "partialCached"
      ;;      "plainify"
      ;;      "pluralize"
      ;;      "print"
      ;;      "printf"
      ;;      "println"
      ;;      "querify"
      ;;      "readDir"
      ;;      "readFile"
      ;;      "ref"
      ;;      "relLangURL"
      ;;      "relURL"
      ;;      "relref"
      ;;      "render"
      ;;      "replace"
      ;;      "replaceRE"
      ;;      "safeCSS"
      ;;      "safeHTML"
      ;;      "safeHTMLAttr"
      ;;      "safeJS"
      ;;      "safeURL"
      ;;      "seq"
      ;;      "sha"
      ;;      "shuffle"
      ;;      "singularize"
      ;;      "slice"
      ;;      "slicestr"
      ;;      "sort"
      ;;      "split"
      ;;      "string"
      ;;      "TrimLeft"
			;;   	 "TrimPrefix"
			;;   	 "TrimRight"
			;;   	 "TrimSuffix"
      ;;      "substr"
      ;;      "time"
      ;;      "title"
      ;;      "trim"
      ;;      "truncate"
      ;;      "union"
      ;;      "uniq"
      ;;      "upper"
      ;;      "urlize"
      ;;      "urls.Parse"
      ;;      "where"
      ;;      )
      ;;   )
      ;;
      ;; (x-regex-variable
      ;;   "\\b\\(\\$[_[:alpha:]]+\\)"
      ;;   )
      ;;
      ;; (x-regex-string
	    ;;   "\\(`.*?`\\)"
      ;;   )
      ;;
      ;; (x-regex-comment
	    ;;   "{{-?\s*\\(/\\*\\(?:.\\|\n\\)*?\\*/\\)\s*}}"
      ;;   )
      ;;
      ;; (x-regex-hugo-start-end
      ;;   "\\({{\\|}}\\)"
      ;;   )

      ;; generate regex string for each category of keywords
      (x-actions-regexp           (regexp-opt hugo-actions					'symbols))
			(x-vars-regexp							(regexp-opt hugo-vars							nil))
			(x-var-frontmatter-regexp		(regexp-opt hugo-var-frontmatter	nil))
			(x-var-file-regexp					(regexp-opt hugo-var-file					nil))
			(x-var-page-regexp					(regexp-opt hugo-var-page					nil))
			(x-var-gitinfo-regexp				(regexp-opt hugo-var-gitinfo			nil))
			(x-var-hugo-regexp					(regexp-opt hugo-var-hugo					nil))
			(x-var-menu-regexp					(regexp-opt hugo-var-menu					nil))
			(x-var-sitemap-regexp				(regexp-opt hugo-var-sitemap			nil))
			(x-var-site-regexp					(regexp-opt hugo-var-site					nil))
			(x-functions-regexp					(regexp-opt hugo-functions				'symbols))
      )

    `(
       (,hugo-start-end-regexp       1 font-lock-preprocessor-face)
			 (,hugo-comment-regexp				 1 font-lock-comment-face)
			 (,hugo-string-regexp					 1 font-lock-string-face)
			 (,hugo-variable-regexp				 1 font-lock-variable-name-face)

       (,x-actions-regexp               . font-lock-keyword-face)
       (,x-vars-regexp                  . font-lock-keyword-face)
       (,x-var-frontmatter-regexp       . font-lock-doc-face)
       (,x-var-file-regexp              . font-lock-doc-face)
       (,x-var-page-regexp              . font-lock-doc-face)
       (,x-var-gitinfo-regexp           . font-lock-doc-face)
       (,x-var-hugo-regexp              . font-lock-doc-face)
       (,x-var-menu-regexp              . font-lock-doc-face)
       (,x-var-sitemap-regexp           . font-lock-doc-face)
       (,x-var-site-regexp              . font-lock-doc-face)
       (,x-functions-regexp             . font-lock-function-name-face)

       ;; note: order above matters, because once colored, that part won't change.
       ;; in general, put longer words first
       ;; font-lock-constant-face
			 ;; font-lock-builtin-face
			 ;; font-lock-preprocessor-face

       )
    )
  )

(font-lock-add-keywords 'mhtml-mode my-hugo-font-lock-keywords)
;;(font-lock-add-keywords 'html-mode my-hugo-font-lock-keywords)


(defun my-hugo-completion-at-point2 ()
  (interactive)
  (let 
			(
       (pt (point)) ;; collect point
       start end
       )
		
    (save-excursion ;; collect the program name
      (comint-bol)
      (re-search-forward "\\(\\S +\\)\\s ?")
      )
    (if (and (>= pt (match-beginning 1)
               )
          (<= pt (match-end 1)
            )
          )
      () ;; if we're still entering the command, pass completion on to
      ;; comint-completion-at-point by returning nil

      (let ((command (match-string-no-properties 1)))
        (when (member* command hugo-expand  :test 'string= :key 'car)
          ;; If the command is one of my-commands, use the associated completions
          (goto-char pt)
          (let ((start
                  (save-excursion
                    (skip-syntax-backward "^ ")
                    (point))))

            (list start pt (cdr (assoc command hugo-expand )) :exclusive 'no)))))))


(defun my-hugo-completion-at-point ()
  "This is the function to be used for the hook `completion-at-point-functions'."
  (interactive)
  (let*
    (
      ;;(bds (bounds-of-thing-at-point 'symbol)
      (bds (bounds-of-thing-at-point 'word)
        )
      (start (car bds))
      (end (cdr bds)))
    (list start end hugo-expand . nil )
    )
  )

;;(define-derived-mode my-hugo-mode c-mode "my-hugo"
;;  "Major mode for editing my-hugo lang code …"
;;  (add-hook 'completion-at-point-functions 'my-hugo-completion-at-point nil 'local))

;;
 ;;;###autoload
 (define-derived-mode my-hugo-mode mhtml-mode "hugo mode"
	 "Major mode for editing hugo files"

	 ;; code for syntax highlighting
	 (setq font-lock-defaults '((my-hugo-font-lock-keywords)))

   ;;(add-hook 'completion-at-point-functions 'my-hugo-completion-at-point nil 'local)
   ;;(add-hook 'completion-at-point-functions 'my-hugo-completion-at-point2 nil 'local)
   )

 ;; add the mode to the `features' list
 (provide 'my-hugo-mode)

;;; my-hugo-mode.el ends here
