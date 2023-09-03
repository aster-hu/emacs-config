;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Set up org directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(setq org-directory "~/Library/CloudStorage/Dropbox/000_Org-mode")
(setq org-default-notes-file "capture.org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  MELPA PACKAGE REPOSITORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add melpa repository
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; LANGUAGE
;;(set-language-environment 'UTF-8)
;;(set-locale-environment "UTF-8")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  THEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load theme without the pop up message
(load-theme 'doom-monokai-machine t)
;; (load-theme 'doom-nord-light t)


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
;;  '(default ((t (:family "Hack" :foundry "nil" :slant normal :weight normal :height 201 :width normal))))
 '(fixed-pitch ((t nil)))
 '(holiday ((t (:background "chartreuse" :foreground "black"))))
 '(mode-line ((t nil)))
 '(org-agenda-date-today ((t (:foreground "light green" :slant italic :weight bold))))
 '(org-document-title ((t (:height 1.5 :underline nil))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "#9c9197" :strike-through t))))
 '(org-journal-calendar-entry-face ((t (:foreground "light pink" :slant italic))))
 '(org-journal-calendar-scheduled-face ((t (:foreground "HotPink1" :slant italic))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.35 :family "DejaVu Sans Mono" :weight bold))))
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

;; Parent todo can be marked as completed even when there're dependencies
(setq org-enforce-todo-dependencies nil)

;; Set the tags location
(setq org-tags-column -72
      org-agenda-tags-column -102)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  BUFFER MANAGEMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove messages from the *Messages* buffer.
(setq-default message-log-max nil)

;; Remove messages and scratch buffer
(add-hook 'after-init-hook (lambda () (when (get-buffer "*scratch*") (kill-buffer "*scratch*") (when (get-buffer "*Messages*") (kill-buffer "*Messages*")))))

;; Enable a interactive divider
(setq window-divider-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  KEYBOARD SHORTCUTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Custom keyboard shortcuts
(progn
  (global-set-key (kbd "M-s") 'save-buffer)
  (global-set-key (kbd "M-w") 'kill-buffer)
  (global-set-key (kbd "M-f") 'isearch-forward)
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
  (global-set-key (kbd "C-c b") #'org-switchb)
  ;; (global-set-key (kbd "C-c j")  'org-journal-new-date-entry)
  (global-set-key (kbd "C-M-f" ) 'consult-org-roam-search)
  (global-set-key (kbd "C-c i d" ) 'org-id-get-create)
  (global-set-key (kbd "<f12>" ) 'org-transclusion-add)
  (global-set-key (kbd "C-c n t" ) 'org-transclusion-mode)
  )

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
(require 'org-super-agenda)
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

(add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  SET REFILE TARGET LOCATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-refile-targets '((nil :maxlevel . 2)
                           ("gtd.org" :maxlevel . 3)))

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

(require 'calfw-ical)
(require 'calfw-org)
;;(setq cfw:org-overwrite-default-keybinding t) ;; org like keybinding
;;(cfw:open-ical-calendar "http://www.google.com/calendar/ical/.../basic.ics")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  CAPTURE TEMPLATE KEYS BINDING
;;  GOAL SETTING REVIEW
;;  WEEKLY REVIEW AND MORE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (setq org-capture-templates
   '(("t" "Org-todo" entry
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
;;  AUTO COMPLETION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vertico
  :ensure t
  :init
  (vertico-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ORG-ROAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (concat org-directory "/roam"))
  (org-roam-dailies-directory "journals/")
  (org-roam-completion-everywhere t)
  ;; (org-roam-capture-templates
  ;;   '(("d" "default" plain "%?"
  ;; :if-new (file+head "%<%y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+roam_alias:\n#+filetags: \n\n")
  ;; :unnarrowed t))) 
  (org-roam-dailies-capture-templates
    '(("d" "default" plain "- %<%H:%M> %?"
       :if-new (file+head "%<%Y-%m-%d %a>.org" "#+title: %<%Y-%m-%d %a>\n\n")
       :unnarrowed t)
       ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-ui-mode)
         ("C-c n c" . org-roam-capture)
         ("C-c n i" . org-roam-node-insert)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
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
         "\n*Metadata*\nLink: %?\nArea: \nResource: \n\n"
         :if-new (file+head "1-main/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "resource" plain
        "\n*Metadata*\nLink: %?\nArea: \nResource: \n\n"
         :if-new
         (file+head "2-resource/${slug}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain
        "\n*Metadata*\nLink: %?\nArea: \nResource: \n\n"
         :if-new
         (file+head "3-article/${slug}.org" "#+title: ${title}\n#+filetags: :article:\n")
         :immediate-finish t
         :unnarrowed t)
         ("e" "review" plain "%?"
         :if-new
         (file+head "4-review/${slug}.org" "#+title: ${title}\n#+filetags: :review:\n\n\n")
         :immediate-finish t
         :unnarrowed t)
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  CASE INSENSITIVE SEARCH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun case-insensitive-org-roam-node-read (orig-fn &rest args)
  (let ((completion-ignore-case t))
    (apply orig-fn args)))
    
(advice-add 'org-roam-node-read :around #'case-insensitive-org-roam-node-read)

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
   '("f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7" "f1882fc093d7af0794aa8819f15aab9405ca109236e5f633385a876052532468" "37c8c2817010e59734fe1f9302a7e6a2b5e8cc648cf6a6cc8b85f3bf17fececf" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "251ed7ecd97af314cd77b07359a09da12dcd97be35e3ab761d4a92d8d8cf9a71" "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f" "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da" "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "89d9dc6f4e9a024737fb8840259c5dd0a140fd440f5ed17b596be43a05d62e67" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee" "49acd691c89118c0768c4fb9a333af33e3d2dca48e6f79787478757071d64e68" "f46ebf04f3877132b28a245b063a51bc8c4c2a68bbf58ef4257fae613a6447c4" "31deed4ac5d0b65dc051a1da3611ef52411490b2b6e7c2058c13c7190f7e199b" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "7e068da4ba88162324d9773ec066d93c447c76e9f4ae711ddd0c5d3863489c52" default))
 '(desktop-save-mode t)
 '(global-display-line-numbers-mode t)
 '(global-visual-line-mode nil)
 '(global-visual-line-mode-hook nil)
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
   '(org-transclusion ivy consult-org-roam org-roam-ui monokai-theme rg timu-macos-theme timu-spacegrey-theme treemacs ef-themes leuven-theme org-beautify-theme org-journal moe-theme espresso-theme htmlize calfw-org calfw-ical org-notifications alert windresize doom-themes gruvbox-theme org-caldav org-super-agenda calfw zenburn-theme spacemacs-theme color-theme-sanityinc-tomorrow catppuccin-theme atom-one-dark-theme))
 '(shift-select-mode t))
