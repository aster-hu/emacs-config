;; Maintain startup speed
;; ensure shell-file-name doesn't perform expensive startup
;; https://github.com/jmccarrell/literate-emacs.d/blob/master/jeff-emacs-config.org#what-is--conorcs
(defun jwm/shell-is-zsh-p ()
  (string-suffix-p "zsh" shell-file-name))

(when (jwm/shell-is-zsh-p)
  (setq shell-command-switch "-cf"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  PACKAGE MANAGEMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-and-compile
  (require 'package)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (require 'use-package)
  ;; i don't really know why this isn't the default...
  (setf use-package-always-ensure t))

;; ensure environment variables inside Emacs look the same as in the user's shell
;;(when (memq window-system '(mac ns x))
;;  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  THEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load theme without the pop up message
;; (load-theme 'doom-monokai-machine t)
;; (load-theme 'doom-nord-light t)
;; (load-theme 'doom-monokai-ristretto t)

;; Auto switch light/dark theme
;; Adapted from https://yannesposito.com/posts/0014-change-emacs-theme-automatically/index.html
(defun y/auto-update-theme ()
  "depending on time use different theme"
  ;; morning to afternoon: nord-light
  ;; night: monokai-machine
  (let* ((hour (nth 2 (decode-time (current-time))))
         (theme (cond ((<= 6 hour 18)   'doom-earl-grey)
                      (t               'doom-monokai-machine))))
    (when (not (equal custom-enabled-themes theme))
      (disable-theme custom-enabled-themes)
      (load-theme theme t))
    ;; run that function again next hour
    (run-at-time (format "%02d:%02d" (+ hour 1) 0) nil 'y/auto-update-theme)))

(y/auto-update-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Set up org directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'org)
(use-package org
  :init
  (setq org-directory "~/Library/CloudStorage/Dropbox/000_Org-mode")
  (setq org-default-notes-file "capture.org")
  (setq org-roam-directory (concat org-directory "/roam"))
  )
;; (setq org-directory "~/Library/CloudStorage/Dropbox/000_Org-mode")
;; (setq org-default-notes-file "capture.org")
;; (setq org-roam-directory (concat org-directory "/roam"))

;; get rid of all of the backup files; that is what revision control is for.
(setq backup-before-writing nil)
(setq make-backup-files nil)

;; get rid of auto save file # #
(auto-save-mode -1)

;; prefer utf-8 encoding in all cases.
(let ((lang 'utf-8))
  (set-language-environment lang)
  (prefer-coding-system lang))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  TODO keywords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Set up TODO keywords globally
;;(setq org-todo-keywords
  ;;  (quote ((sequence "NEXT(n)" "LATER(l)" "WAIT(w)" "|" "DONE(d@/!)" "CANCEL(c@/!)")))) ;; Log a timestamp and a note when "DONE" is entered

;; Headline cannot be DONE unless all children are DONE
(setq-default org-enforce-todo-dependencies t)
;; Hide the first N-1 stars in a headline
(setq org-hide-leading-stars t)


;; Set font
(set-face-attribute
'default nil :font "Hack" :height 201)
;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
(set-fontset-font (frame-parameter nil 'font)
charset
(font-spec :family "WenQuanYi Micro Hei Mono" :size 24)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t nil)))
 '(holiday ((t (:background "chartreuse" :foreground "black"))))
 '(mode-line ((t nil)))
 '(org-agenda-date-today ((t (:foreground "light green" :slant italic :weight bold))))
 '(org-document-title ((t (:height 1.5 :underline nil))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "#9c9197" :strike-through nil))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.25 :family "DejaVu Sans Mono" :weight bold))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2 :weight normal))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.15 :weight normal))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1 :weight normal))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0 :weight normal))))
 '(variable-pitch ((t (:family "Hack")))))


;; Customize highlight TODO keywords
;; (setq org-todo-keyword-faces
;;       (quote (;; 
;; 	      ("NEXT" :foreground "#E8A0BF" :weight bold)
;;               ("LATER" :foreground "#BA90C6" :weight bold)
;;               ("WAIT" :foreground "#76b1d1" :weight bold)
;;               ("DONE" :foreground "#97dc97" :weight bold)
;;               ("ACHIEVE" :foreground "#97dc97" :weight bold)    
;;               ("MISS" :foreground "#ffe599" :weight bold)
;;               ("CANCEL" :foreground "#7C9D96" :weight bold)
;;               ("NOTE" :foreground "#d0bf8f" :weight bold))))

;; Alternative highlight TODO keywords style for both dark and light themes
(setq org-todo-keyword-faces
      (quote (;; 
	      ("NEXT" :foreground "#d66a6a" :weight bold)
        ("GOAL" :foreground "#2aa198" :weight bold)
              ("LATER" :foreground "#5c778d" :weight bold)
              ("WAIT" :foreground "#948f85" :weight bold)
              ("DONE" :foreground "#859900" :weight bold)
              ("ACHIEVE" :foreground "#859900" :weight bold)    
              ("MISS" :foreground "#c29747" :weight bold)
              ("CANCEL" :foreground "#948f85" :weight bold))))

;; Define bullets style
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "☷" "○" "◆" "▲" "▶")))

;; Define ellipsis
; (setq org-ellipsis " ⏷ ")
(setq org-ellipsis "⤵")

;; Change priority style via org-fancy-priorities
(use-package org-fancy-priorities
  :ensure t
  :config
  (setq org-fancy-priorities-list '("⬆" "⬌" "⬇" "☕"))
  (add-hook 'org-mode-hook 'org-fancy-priorities-mode))



;; Strikethrough the DONE items and set fonts
(setq org-fontify-done-headline t)


;; Only leave empty line for heading
;;(setq org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))

;; Function to ensure blank lines between headings and before contents
(add-to-list 'load-path "~/.emacs.d/plugins")
(load "org-fix-blank-lines.el")


;; when marking a todo as done, at the time
;; log into drawers right underneath the heading
(setq org-log-done 'time  
      org-log-into-drawer t)

;; Parent todo can only be marked as completed even when there're dependencies
(setq org-enforce-todo-dependencies t)

;; Set the tags location
(setq org-tags-column -72
      org-agenda-tags-column -102)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  BUFFER MANAGEMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove messages from the *Messages* buffer.
;; (setq-default message-log-max nil)

;; Remove messages and scratch buffer
;; (add-hook 'after-init-hook (lambda () (when (get-buffer "*scratch*") (kill-buffer "*scratch*") (when (get-buffer "*Messages*") (kill-buffer "*Messages*")))))

;; (add-hook 'after-init-hook (lambda () (when (get-buffer "*scratch*") (kill-buffer "*scratch*"))))

;; Kill all buffer except current one
(defun my/save-kill-other-buffers ()
      "Kill all other buffers."
      (interactive)
      (save-some-buffers t)
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; Enable a interactive divider
(setq window-divider-mode t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ORG-AGENDA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-agenda-files (list "gtd.org"))
			   ;;  "work.org"))

;; Open org-agenda at startup
;;(add-hook 'after-init-hook 'org-agenda-list)
(add-hook 'after-init-hook '(lambda () (org-agenda nil "z")))

;; org-super-agenda custom commands
;;(add-to-list 'load-path "~/plugins/org-super-agenda.el")
;; (require 'org-super-agenda)
(use-package org-super-agenda
:config
(setq org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-include-deadlines t
      org-agenda-skip-additional-timestamps-same-entry t
      org-agenda-start-with-log-mode t
      org-agenda-format-date "%F %a"
      org-agenda-remove-tags nil ;; don't hide all tags
      org-agenda-dim-blocked-tasks 'invisible ;; Make blocked tasks invisible
      org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈┈┈┈┈ Now"
      org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (timeline . "  % s")
	;; Use the OR condition to check if either (not (car (last (org-get-outline-path))))
	;; - meaning the last element is nil) or
	;; (string-blank-p (car (last (org-get-outline-path))))
	;; - meaning the last element is a blank string
	;; evaluates to true. In either of these cases, "gtd" will be displayed. Otherwise, it will truncate the last element of the outline path
	;; This displays the TODO keyword, left-aligned in a 30-character wide column.
	;; It takes the last element of the outline path (the current headline) and truncates it to a maximum of 25 characters
	(todo . " %-30 (if (or (not (car (last (org-get-outline-path)))) (string-blank-p (car (last (org-get-outline-path))))) \"\" (truncate-string-to-width (car (last (org-get-outline-path))) 25 nil nil t))")
        (tags . " %i %-12:c")
        (search . " %i %-12:c"))
      )
      (setq org-agenda-custom-commands
      '(
        ("z" "Super view"
         ((agenda "" ((org-agenda-span 'day)
		      (org-super-agenda-groups
                       '((:name "====================================================================================="
                          :time-grid t
                          :date today
                          :todo "NEXT"
                          :scheduled today
                          :order 1)
			 (:name "Meeting"
				:todo "MEETING")
			 ))))
	  (alltodo "" ((org-agenda-overriding-header "")
		       (org-super-agenda-groups
                        ;; Each group has an implicit boolean OR operator between its selectors.
			'((:name "🔥 Due Today"
                           :deadline today
                           :face (:foreground "#FF80ED"))
                          (:name "☠️ Passed deadline"
				 :and (
				 :deadline past
				 :todo ("NEXT" "LATER"))
                           :face (:foreground "#FF7980"))
			  (:name "🛁 Reschedule"
				 :scheduled past)
			   ;; Ignore items that are already scheduled
			  (:discard (:scheduled t))			  
			  (:name "🌸 Important"
				 :and (
				 :priority>= "B"
					     :todo ("NEXT")))			 
			  (:name "⏳ Do it later"
				 :and (
				       :priority>= "B"
						   :todo ("LATER")
						   ))
                          (:name "🧺 Fill-ins"
				 ;; Show this section after "Today" and "Important", because their order is unspecified, defaulting to 0. Sections are displayed lowest-number-first.
				 :priority< "B"
                                 :order 1)
                          (:name "✈️ Waiting"
				 :todo "WAIT"
				 :order 9)
			  ))))))
  ;;       ("g" "Goal review panel"
	;;  ((tags "Goal=\"Epic\""
	;; 	((org-agenda-overriding-header "Epic goals (evergreen)")))
	;;   (tags "Goal=\"Long\""
	;; 	((org-agenda-overriding-header "Long term goals (2-5 years)")))
	;;   (tags "Goal=\"Medium\""
	;; 	((org-agenda-overriding-header "Medium term goals (half year to 2 years)")))
	;;   (tags "Goal=\"Short\""
	;; 	((org-agenda-overriding-header "Short term goals (within 6 months)")))
	;;   (tags-todo "Goal=\"\""
	;; 	     ((org-agenda-overriding-header "Dormant goal / non-goal"))))
	;;  ((org-agenda-files (list "goal.org"))))
   ))
   :hook (org-agenda-mode-hook . org-super-agenda-mode)
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  SET REFILE TARGET LOCATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-refile-targets '((nil :maxlevel . 9)
                           ("gtd.org" :maxlevel . 9)))

;; Show outline path when refiling
(setq org-outline-path-complete-in-steps nil
      org-refile-use-outline-path t
      org-refile-allow-creating-parent-nodes (quote confirm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  QUERY SEARCH FOR BACKLOG ITEMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Search items that are not have TODO keywords,
;; no children headlines, and
;; no timestamp

(defun my/view-backlog ()
  (interactive)
  (require 'org-ql)
  (org-ql-search
    (org-agenda-files)
    '(and (not (todo))
          (not (done))
          (not (ts))
          (not (children))
    )
    :sort nil
    :super-groups '((:auto-outline-path t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  CALENDAR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package calfw)
;; (use-package calfw-cal)
(use-package calfw-org)

;; (defun my/open-calendar ()
;;   (interactive)
;;   (cfw:open-calendar-buffer
;;    :contents-sources
;;    (list
;; ;;    (cfw:org-create-source "Green")  ; orgmode source
;;     (cfw:org-create-file-source "Blog" "gtd.org")  ; our blog organizational calendar
;; ;;    (cfw:cal-create-source "Orange") ; diary source
;;     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  CAPTURE TEMPLATE KEYS BINDING
;;  GOAL SETTING REVIEW
;;  WEEKLY REVIEW AND MORE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (setq org-capture-templates
       '(("c" "capture" entry
	  (file "capture.org")
          "* %?\n"
          :empty-lines-after 1)
	 ("t" "Org-todo" entry
	  (file "gtd.org")
	  (file "templates/tpl-todo.org")
	  :empty-lines-after 0)
	 ("g" "Goals") 
	 ("ge" "Epic goals" entry (file+headline "goal.org" 
						 "Epic goals") (file "templates/tpl-goal.org") :empty-lines-after 1) 
	 ("gl" "Long term goal (2-5 years from now)" entry (file+headline "goal.org" 
									  "Long term goals") (file "templates/tpl-goal.org") :empty-lines-after 1) 
	 ("gm" "Medium term goal (6 months up to 2 years)" entry (file+headline "goal.org" 
										"Medium term goals") (file "templates/tpl-goal.org") :empty-lines-after 1) 
	 ("gs" "Short term goals (next 6 months)" entry (file+headline "goal.org" 
								       "Short term goals") (file "templates/tpl-goal.org") :empty-lines-after 1)
	 ("r" "Review") 
	 ("rw" "Weekly review" entry 
          (file buffer-name)
          (file "templates/tpl-w-review.org")) 
	 ("rm" "Monthly review" entry 
          (file buffer-name)
          (file "templates/tpl-m-review.org"))
	 ("rq" "Quarterly review" entry 
          (file buffer-name)
          (file "templates/tpl-q-review.org")) 
	 ("ra" "Annual review" entry 
          (file buffer-name)
          (file "templates/tpl-a-review.org"))            
	 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ORG-JOURNAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package org-journal
;;   :ensure t
;;   :defer t
;;   :init
;;   ;; Change default prefix key; needs to be set before loading org-journal
;;   (setq org-journal-prefix-key "C-c j")
;;   :config
;;   (setq org-journal-dir (concat org-directory "/journal")
;; 	org-journal-file-type 'weekly
;; 	org-journal-date-prefix "* "
;; 	org-journal-time-prefix "** "
;; 	org-journal-date-format "%A, %Y-%m-%d"
;; 	org-journal-created-property-timestamp-format "%F"
;; 	org-journal-file-header
;; 	"#+TITLE: Week %W, %Y\n#+category: journal\n\n"
;; 	org-journal-file-format "%Y-W%V.org")
;; 	org-journal-carryover-items "TODO=\"NEXT\""
;; 	org-journal-enable-agenda-integration t
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ORG-ROAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam
  :ensure t
  :custom
  ;; (org-roam-directory (concat org-directory "/roam"))
  (org-roam-dailies-directory "journals/")
  (org-roam-completion-everywhere t)
  ;; (org-roam-capture-templates
  ;;   '(("d" "default" plain "%?"
  ;; :if-new (file+head "%<%y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+roam_alias:\n#+filetags: \n\n")
  ;; :unnarrowed t))) 
  (org-roam-dailies-capture-templates
    '(
      ("d" "default" entry "* %<%H:%M>\n  %?"
       :if-new (file+head "%<%Y-%m-%d %a>.org" "#+title: %<%Y-%m-%d %a>\n\n")
       :unnarrowed t)
      ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-ui-mode)
         ("C-c n c" . org-roam-capture)
         ("C-c n i" . my-org-roam-node-insert)
	 ("C-c n t a" . org-roam-tag-add)
	 ("C-c n t r" . org-roam-ref-add)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n d d" . org-roam-dailies-goto-today)
         ("C-c n d y" . org-roam-dailies-goto-yesterday)
         ("C-c n d t" . org-roam-dailies-goto-tomorrow)
        ;;  :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup)
  (setq org-roam-node-display-template (concat "${title:*}" (propertize "${type:15} ${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  )


(with-eval-after-load 'org-roam
        (cl-defmethod org-roam-node-type ((node org-roam-node))
           "Return the TYPE of NODE."
           (condition-case nil
               (file-name-nondirectory
                (directory-file-name
                 (file-name-directory
                  (file-relative-name (org-roam-node-file node) org-roam-directory))))
             (error ""))
           ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ORG-ROAM CAPTURE TEMPLATES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-roam-capture-templates
      '(
        ("m" "main" plain
         "\n*Metadata*\nArea: %?\nResource: \nLink: \n\n"
         :if-new (file+head "1-main/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t
         :empty-lines-after 1)
        ("r" "resource" plain
        "\n*Metadata*\nArea: %?\nResource: \nLink: \n\n"
         :if-new
         (file+head "2-resource/${slug}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t
         :empty-lines-after 1)
        ("a" "article" plain
        "\n*Metadata*\nArea: %?\nResource: \nLink: \n\n"
         :if-new
         (file+head "3-article/${slug}.org" "#+title: ${title}\n#+filetags: :article:\n")
         :immediate-finish t
         :unnarrowed t
         :empty-lines-after 1)
         ("e" "review" plain "%?"
         :if-new
         (file+head "4-review/${slug}.org" "#+title: ${title}\n#+filetags: :review:\n\n\n")
         :immediate-finish t
         :unnarrowed t
         :empty-lines-after 1)
               ))

;; preview link at the mouse 
(defun my/link-preview ()
  (interactive)
  (let ((display-buffer-alist
         '(;; Bottom side window
           (".*"
            (display-buffer-in-side-window)
            (window-height . 0.25)
            (side . bottom)
            (slot . 0)))))
    (org-open-at-point)))


;; Function to show all backlinks
;; https://emacs-china.org/t/org-roam-v2-backlinks-buffer-headlines/23368/2
;; https://github.com/czqhurricnae/spacemacs-private/blob/master/layers/hurricane-org/local/org-roam-backlink-collections/org-roam-backlink-collections.el
(add-to-list 'load-path "~/.emacs.d/plugins")
(load "org-roam-backlink-collections.el")

;; Define function to exclude files in archive nodes
;; https://www.reddit.com/r/emacs/comments/veesun/orgroam_is_absolutely_fantastic/
(defun my-org-roam-node-exclude-archive (node)
    (and
   ;; no journal files
   ;; (not (string-match my-date-regexp (org-roam-node-title node)))
   ;; not tagged `archive'
   (not (member "archive" (org-roam-node-tags node)))
   ;; not in any folder named `archive'
   (not (string-match-p "archive/" (org-roam-node-file node)))))

;;;;; Define custom `org-roam-node-insert' functions with filters.
(defun my-org-roam-node-insert nil
 ;; Refined search for org-roam nodes to exclude elements tagged `archive'
  (interactive)
  ;; nb: can add initial search string like "^"
;;  (org-roam-node-find :other-window nil #'my-org-roam-node-exclude-archive)
  (org-roam-node-insert #'my-org-roam-node-exclude-archive)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  AUTO COMPLETION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This enables candidates matching to be case-insensitive
(setq completion-ignore-case t)

;; (use-package vertico
;;   :ensure t
;;   :custom
;;   (vertico-cycle t) ;; Cycle through result
;;   :init
;;   (vertico-mode))

;; Optionally use the `orderless' completion style.
;; (use-package orderless
;;   :init
;;   ;; Configure a custom style dispatcher (see the Consult wiki)
;;   ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
;;   ;;       orderless-component-separator #'orderless-escapable-split-on-space)
;;   (setq completion-styles '(orderless basic)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles partial-completion)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Company
;; You need package called `company`.
;; I believe what these variables are meant to do is self-explanatory.
;; You type minimum 2 characters and wait for ¼ seconds for the candidates
;; to appear automatically. It uses a backend `company-capf` (part of
;; `company`; capf stands for "completion-at-point function"). I would
;; call it inline automatic completion. Org-roam has functions to work
;; with `company-capf`.
;; (use-package company)
;; ;; To automatically close "]]" brackets and other parentheses,
;; ;; you need a package called "smartparens" Set it up globally.
;; (smartparens-global-mode t)
;; (global-company-mode)
;; (setq company-minimum-prefix-length 2)
;; (setq company-idle-delay 0.25)
;; (setq company-backends '(company-capf))

(use-package company
:ensure t
:after org-roam
:config
(add-hook 'after-init-hook 'global-company-mode)
(setq company-minimum-prefix-length 2)
(setq company-idle-delay 0.25)
:init
(with-eval-after-load 'company
;; (define-key company-active-map (kbd “C-n”) #'company-select-next)
;; (define-key company-active-map (kbd “C-p”) #'company-select-previous)
(add-to-list 'company-backends 'company-capf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  HELM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://tuhdo.github.io/helm-intro.html

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(use-package helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(setq helm-candidate-number-limit 999
      helm-autoresize-max-height 40
      helm-autoresize-min-height 40)

(helm-mode 1)
(add-hook 'helm-mode-hook 'helm-autoresize-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  helm-ag
;;  The silver searcher
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm-ag)

;; Enable electric pair mode and add auto complete for ~tilde~ and {bracket}
(electric-pair-mode 1)
(setq electric-pair-pairs
      '((?\~ . ?\~)
        (?\{ . ?\})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  CASE INSENSITIVE SEARCH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun case-insensitive-org-roam-node-read (orig-fn &rest args)
;;   (let ((completion-ignore-case t))
;;     (apply orig-fn args)))
    
;; (advice-add 'org-roam-node-read :around #'case-insensitive-org-roam-node-read)

;; Insert link with org id for headline
(setq org-id-link-to-org-use-id "create-if-interactive")

;; Text wrap
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)


;; consult-org-roam

(use-package consult-org-roam
   :ensure t
   :after org-roam
   :init
;;   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
;;   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key (kbd "M-."))
   :bind
   ;; Define some convenient keybindings as an addition
  ;; ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n l" . consult-org-roam-forward-links)
   ;; ("C-c n r" . consult-org-roam-search)
   )

;; org-transclusion
(use-package org-transclusion
  :ensure t
  :after org-roam
  :bind
  ("<f12>" . org-transclusion-mode)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  BLOGGING WITH ORG-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exclude :article: in all export

;;(setq org-export-exclude-tags "article")

;; hugo blog
(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)

;; support quarto
(use-package quarto-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  BABEL SRC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (R . t)
;;    (css . t)
;;    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  KEYBINDING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Custom keyboard shortcuts
(progn
  (global-set-key (kbd "M-s") 'save-buffer)
  (global-set-key (kbd "M-w") 'kill-buffer)
  (global-set-key (kbd "M-f") 'swiper)
  (global-set-key (kbd "M-c") 'kill-ring-save)
  (global-set-key (kbd "M-v") 'yank)
  (global-set-key (kbd "M-z") 'undo)
  (global-set-key (kbd "C-c <left>")  'windmove-left)
  (global-set-key (kbd "C-c <right>") 'windmove-right)
  (global-set-key (kbd "C-c <up>")    'windmove-up)
  (global-set-key (kbd "C-c <down>")  'windmove-down)
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)
  (global-set-key (kbd "C-c b") #'helm-buffers-list)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-M-b") 'eval-buffer)
  (global-set-key (kbd "C-M-f" ) 'helm-do-grep-ag) ;; search in current directory
  (global-set-key (kbd "C-M-s" ) 'helm-do-ag)      ;; search in specified directory
  (global-set-key (kbd "C-c i d" ) 'org-id-get-create)
  )

;; Remap TAB for completion
;; Source: https://emacs.stackexchange.com/questions/33727/how-does-spacemacs-allow-tab-completion-in-helm#38235
;; https://writequit.org/denver-emacs/presentations/2016-03-01-helm.html
;;   (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;;   (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
;; ;; make TAB works in terminal, C-i is tha same as TAB
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  CUSTOMIZED VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-holidays
   '((holiday-fixed 1 1 "New Year's Day")
     (holiday-float 2 1 3 "Family Day")
     (holiday-easter-etc -2 "Good Friday")
     (holiday-easter-etc 1 "Easter Monday")
     (holiday-float 5 1 4 "xVictoria Day")
     (holiday-fixed 7 1 "Canada Day")
     (holiday-float 8 1 1 "Civic Day")
     (holiday-float 9 1 1 "Labour Day")
     (holiday-fixed 9 30 "National Day of Truth and Reconciliation")
     (holiday-float 10 1 2 "Thanksgiving Day")
     (holiday-fixed 11 11 "Remembrance Day")
     (holiday-fixed 12 25 "Christmas Day")
     (holiday-fixed 12 26 "Boxing Day")))
 '(calendar-mark-holidays-flag t)
 '(custom-safe-themes
   '("43b657205e9abc390460938ad9083aa8d7572fd08cb73504d04ca3c2c6337e5e" "a589c43f8dd8761075a2d6b8d069fc985660e731ae26f6eddef7068fece8a414" "b273d59159ef19d49ddb6176eee2b3283dbe1afbed931d7affae4508e560eac1" "319c0d5bea1d32bbcf0bfb9acb3ed8dbb1e1afc0feec81ae0b2f20ce7d7104b4" "f1b2de4bc88d1120782b0417fe97f97cc9ac7c5798282087d4d1d9290e3193bb" "aeb5508a548f1716142142095013b6317de5869418c91b16d75ce4043c35eb2b" "7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "f458b92de1f6cf0bdda6bce23433877e94816c3364b821eb4ea9852112f5d7dc" "a5270d86fac30303c5910be7403467662d7601b821af2ff0c4eb181153ebfc0a" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "5586a5db9dadef93b6b6e72720205a4fa92fd60e4ccfd3a5fa389782eab2371b" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "ddffe74bc4bf2c332c2c3f67f1b8141ee1de8fd6b7be103ade50abb97fe70f0c" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "2bcd3850ef2d18a4c9208fe3e2a78c95fb82f48c26661c86a51ea39152f3577e" "d537a9d42c6f5349d1716ae9be9a0645cc168f7aff2a8353819d570e5d02c0b3" "2d70bca08b194d0becf19a1df2c54fcb78daeeebc880042de47c735a5c837af0" "ec8d9249bfb886752ee7a12bf6668665b1d053f30122720a99ef60f444a13652" "6c655326d9bb38d4be02d364d344bfa61b3c8fdabd1cf4b97dddc8c0b3047b47" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "ae426fc51c58ade49774264c17e666ea7f681d8cae62570630539be3d06fd964" "545ab1a535c913c9214fe5b883046f02982c508815612234140240c129682a68" "ce4234c32262924c1d2f43e6b61312634938777071f1129c7cde3ebd4a3028da" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8" "046a2b81d13afddae309930ef85d458c4f5d278a69448e5a5261a5c78598e012" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "a876039e0832c9a0e11af80ffbdbb4539aede1fbdc19460290fc4d1bf3a21741" "76c646974f43b321a8fd460a0f5759f916654575da5927d3fd4195029c158018" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7" "f1882fc093d7af0794aa8819f15aab9405ca109236e5f633385a876052532468" "37c8c2817010e59734fe1f9302a7e6a2b5e8cc648cf6a6cc8b85f3bf17fececf" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "251ed7ecd97af314cd77b07359a09da12dcd97be35e3ab761d4a92d8d8cf9a71" "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f" "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da" "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "89d9dc6f4e9a024737fb8840259c5dd0a140fd440f5ed17b596be43a05d62e67" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee" "49acd691c89118c0768c4fb9a333af33e3d2dca48e6f79787478757071d64e68" "f46ebf04f3877132b28a245b063a51bc8c4c2a68bbf58ef4257fae613a6447c4" "31deed4ac5d0b65dc051a1da3611ef52411490b2b6e7c2058c13c7190f7e199b" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "7e068da4ba88162324d9773ec066d93c447c76e9f4ae711ddd0c5d3863489c52" default))
 '(desktop-save-mode t)
 '(doom-earl-grey-brighter-comments t)
 '(doom-earl-grey-brighter-modeline t)
 '(global-display-line-numbers-mode t)
 '(helm-follow-mode-persistent t)
 '(holiday-general-holidays
   '((holiday-fixed 1 1 "New Year's Day")
     (holiday-float 2 1 3 "Family Day")
     (holiday-easter-etc -2 "Good Friday")
     (holiday-float 5 1 4 "xVictoria Day")
     (holiday-fixed 7 1 "Canada Day")
     (holiday-float 8 1 1 "Civic Day")
     (holiday-float 9 1 1 "Labour Day")
     (holiday-fixed 9 30 "National Day of Truth and Reconciliation")
     (holiday-float 10 1 2 "Thanksgiving Day")
     (holiday-fixed 11 11 "Remembrance Day")
     (holiday-fixed 12 25 "Christmas Day")
     (holiday-fixed 12 26 "Boxing Day")))
 '(line-spacing 0.3)
 '(org-M-RET-may-split-line nil)
 '(org-adapt-indentation t)
 '(org-agenda-format-date "%F %a")
 '(org-agenda-loop-over-headlines-in-active-region nil)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy
   '((agenda habit-down time-up priority-down category-keep)
     (todo priority-down category-keep effort-up)
     (tags priority-down category-keep)
     (search category-keep)))
 '(org-agenda-start-with-log-mode t)
 '(org-agenda-time-grid
   '((daily today require-timed remove-match)
     (900 1200 1800 2300)
     "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈"))
 '(org-cycle-separator-lines 1)
 '(org-hierarchical-todo-statistics nil)
 '(org-priority-faces '((67 . "#3a67bf") (66 . "#9D80E1") (65 . "#ff7fb8")))
 '(org-show-notification-handler "")
 '(org-sparse-tree-open-archived-trees t)
 '(org-super-agenda-mode t)
 '(org-support-shift-select nil)
 '(package-selected-packages
   '(projectile ox-hugo quarto quarto-mode calfw-org calfw-cal calfw exec-path-from-shell helm-ag consult-org-roam cloud-theme sunburn-theme magit flycheck elpy ess markdown-mode swiper org-transclusion org-roam-ui timu-macos-theme treemacs htmlize windresize doom-themes gruvbox-theme org-super-agenda zenburn-theme spacemacs-theme))
 '(shift-select-mode t))
