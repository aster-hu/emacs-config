;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Set up org directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(setq org-directory "~/Library/CloudStorage/Dropbox/000_Org-mode")
(setq org-default-notes-file (concat org-directory "/capture.org"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  THEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load theme without the pop up message
(load-theme 'zenburn t nil)


;; Customize highlight TODO keywords
(setq org-todo-keyword-faces
      (quote (;; ("PROJ" :foreground "#ffe599" :weight bold)
	      ("NEXT" :foreground "#E8A0BF" :weight bold)
              ("LATER" :foreground "#BA90C6" :weight bold)
              ("WAIT" :foreground "#76b1d1" :weight bold)
              ("DONE" :foreground "#97dc97" :weight bold)    
              ("CANCEL" :foreground "#7C9D96" :weight bold)
              ("NOTE" :foreground "#d0bf8f" :weight bold))))

;; Define bullets style
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "☷" "○" "◆" "▲" "▶")))

;; Define ellipsis
;; (setq org-ellipsis " ⏷ ")

;; Change priority style via org-fancy-priorities
(use-package org-fancy-priorities
  :ensure t
  :config
  (setq org-fancy-priorities-list '("⬆" "⬌" "⬇" "☕"))
  (add-hook 'org-mode-hook 'org-fancy-priorities-mode))


;; Strikethrough the DONE items and set fonts
(setq org-fontify-done-headline t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "nil" :slant normal :weight normal :height 201 :width normal))))
 '(holiday ((t (:background "chartreuse" :foreground "black"))))
 '(mode-line ((t nil)))
 '(org-agenda-date-today ((t (:foreground "light green" :slant italic :weight bold))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "#9c9197" :strike-through t))))
 '(org-journal-calendar-entry-face ((t (:foreground "light pink" :slant italic))))
 '(org-journal-calendar-scheduled-face ((t (:foreground "HotPink1" :slant italic)))))

;; Prevent empty line when adding new entries
(setq org-blank-before-new-entry (quote ((heading . nil)
                                         (plain-list-item . nil))))


;; when marking a todo as done, at the time
;; log into drawers right underneath the heading
(setq org-log-done 'time  
      org-log-into-drawer t)

;; Set the tags location
(setq org-tags-column -72)

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
  (global-set-key (kbd "C-c j")  'org-journal-new-date-entry)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ORG-AGENDA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-agenda-files (list (concat org-directory "/gtd.org")
			     (concat org-directory "/work.org")))

;; Open org-agenda at startup
;;(add-hook 'after-init-hook 'org-agenda-list)
(add-hook 'after-init-hook '(lambda () (org-agenda nil "z")))

;; org-super-agenda custom commands
(add-to-list 'load-path "~/plugins/org-super-agenda.el")
(require 'org-super-agenda)
(setq org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-include-deadlines t
      org-agenda-skip-additional-timestamps-same-entry t
      org-agenda-start-with-log-mode t
      org-agenda-format-date "%F %a"
      org-agenda-remove-tags t ;; hide all tags
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
      '(("z" "Super view"
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
                        '(;; Each group has an implicit boolean OR operator between its selectors.
			 (:name "🔥 Due Today"
                           :deadline today
                           :face (:foreground "#FF80ED"))
                          (:name "☠️ Passed deadline"
				 :and (
				 :deadline past
				 :todo ("NEXT" "LATER"))
                           :face (:background "#7f1b19"))
			  (:name "🛁 Reschedule"
				 :scheduled past)
			  (:name "🌸 Important"
				 :and (
				 :priority>= "B"
					     :todo ("NEXT")
				 :not (:scheduled today)))
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
			 (:name "Unclassified"
				 :discard (:scheduled today)
				 ;; :anything t
				 :order 10)
			  ))))))))
(add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  SET REFILE TARGET LOCATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-refile-targets '((nil :maxlevel . 2)
                           ("gtd.org" :maxlevel . 3)))

;; Show outline path when refiling
(setq org-outline-path-complete-in-steps nil
      org-refile-use-outline-path t)

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
;;  ORG-JOURNAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-dir (concat org-directory "/journal")
	org-journal-file-type 'weekly
	org-journal-date-prefix "* "
	org-journal-time-prefix "** "
	org-journal-date-format "%A, %Y-%m-%d"
	org-journal-created-property-timestamp-format "%F"
	org-journal-file-header
	"#+TITLE: Week %W, %Y\n#+category: journal\n\n"
	org-journal-file-format "%Y-W%V.org")
	org-journal-carryover-items "TODO=\"NEXT\""
	org-journal-enable-agenda-integration t
  )

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
 '(org-M-RET-may-split-line nil)
 '(org-adapt-indentation t)
 '(org-agenda-current-time-string "now - - - - - - - - - - - - - - - - - - - - - ")
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
     "......" "----------------"))
 '(org-capture-templates
   '(("t" "Org-todo" entry
      (file "gtd.org")
      (file "~templates/tpl-todo.org")
      :empty-lines-after 1)))
 '(org-cycle-separator-lines 1)
 '(org-hierarchical-todo-statistics nil)
 '(org-priority-faces '((67 . "#3a67bf") (66 . "#ad95e6") (65 . "#ff7fb8")))
 '(org-show-notification-handler "")
 '(org-sparse-tree-open-archived-trees t)
 '(org-super-agenda-mode t)
 '(org-support-shift-select nil)
 '(org-wild-notifier-alert-time '(5))
 '(org-wild-notifier-keyword-blacklist '("DONE" "CANCEL"))
 '(org-wild-notifier-keyword-whitelist nil)
 '(org-wild-notifier-mode t)
 '(package-selected-packages
   '(org-beautify-theme org-journal moe-theme espresso-theme htmlize calfw-org calfw-ical ## org-gcal org-notifications alert windresize doom-themes gruvbox-theme org-caldav org-super-agenda calfw zenburn-theme spacemacs-theme color-theme-sanityinc-tomorrow catppuccin-theme atom-one-dark-theme))
 '(shift-select-mode t))
