;;; scratch.el -*- lexical-binding: t; -*-
(require 'doom-lib)

(defun my/agenda-filter-canvas-property (span property regex)
  `(
    (org-agenda-span ,span)
    (org-agenda-start-day "-sunday") 
    ;; show DONE items
    (org-agenda-skip-scheduled-if-done nil)
    (org-agenda-skip-deadline-if-done nil)
    (org-agenda-skip-timestamp-if-done nil)
    (org-agenda-skip-function
     #'(lambda ()
         ;; Get the position of the end of the current subtree
         (let ((subtree-end (org-entry-end-position)))
           ;; Check the CUSTOM_ID property
           (if (or (not (string-match-p "canvas.org" (buffer-file-name)))
                   (string-match-p
                    ,regex
                    (or (org-entry-get (point) ,property) "")))
               ;; If the file is not the canvas calendar or if it matches:
               nil ; Don't skip this entry
             ;; If it does NOT match (or has no CUSTOM_ID):
             subtree-end)))))) ; Skip the entire subtree

(defun my/agenda-filter-on-title (regex)
  `(org-agenda-skip-function
    #'(lambda ()
        ;; Get the position of the end of the current subtree
        (let ((subtree-end (org-entry-end-position)))
          ;; Check the entry's TITLE (heading)
          (if (string-match-p
               ;; This is a regex: we must "escape" the [ and ]
               ;; to match them literally.
               ,regex
               ;; This function gets the text of the entry's headline
               (org-get-heading))
              
              ;; If it matches:
              nil ; return the heading
            
            ;; If it does NOT match:
            subtree-end))))) ; skip the entire subtree 

(after! org
  ;; Set the list of files org-agenda works on
  (setq org-agenda-files '("~/org/"
                           "~/org/calendars/canvas.org"))

  ;; Make the current clock for a clocked in task shows up in only in the frame
  ;; title
  (setq org-clock-clocked-in-display 'frame-title)

  ;; TODO Make this an 'add-to-list'
  (setq org-agenda-custom-commands
        `(
          ;; School agenda view
          ("S" "School" ;; Describe the name prefix
           ;; List of commands
           ((agenda ""
                    ;; Settings for command
                    ((org-agenda-overriding-header "Day View")
                     (org-agenda-span 'day)
                     (org-agenda-start-day "0d")))
            (tags-todo "@ai" 
                       ;; Settings for command
                       ((org-agenda-overriding-header "AI Tasks")))
            (tags-todo "@robotics" 
                       ;; Settings for command
                       ((org-agenda-overriding-header "Robotics Tasks")))
            (tags-todo "@ml" 
                       ;; Settings for command
                       ((org-agenda-overriding-header "Machine Learning Tasks")))
            (tags-todo "@dataeng" 
                       ;; Settings for command
                       ((org-agenda-overriding-header "Data Engineering Tasks"))))
           ;; List of 'general-settings-for-whole-set'
           ((org-agenda-files '("~/org/school.org")))
           ;; Files to write to
           ("~/org/school.html"))
          
          ;; View canvas assignments
          ("b" "Canvas Assignments Agenda"
           agenda ""
           ,(append '((org-agenda-files '("~/org/calendars/canvas.org")))
                    (my/agenda-filter-canvas-property ''fortnight "CUSTOM_ID" "^event-assignment-")))

          ;; View canvas classes
          ("B" "Canvas Class Agenda"
           agenda ""
           ,(append '((org-agenda-files '("~/org/calendars/canvas.org")))
                    (my/agenda-filter-canvas-property 9 "CUSTOM_ID" "^event-calendar-event-")))

          ("o" "Combined Assignments Agenda"
           agenda ""
           ,(append '((org-agenda-files '("~/org/calendars/canvas.org" "~/org/school.org")))
                    (my/agenda-filter-canvas-property ''fortnight "CUSTOM_ID" "^event-assignment-")))
          
          ;; TODO: This view needs some work.
          ;; Canvas agenda view
          ("c" "Canvas" ;; Describe the name prefix
           ;; List of commands
           ((agenda ""
                    ;; Settings for command
                    ((org-agenda-overriding-header "Next 3 Days")
                     (org-agenda-span 3)
                     (org-agenda-start-day "today")))
            (todo "" 
                  ;; Settings for command
                  ((org-agenda-overriding-header "Robotics Tasks")
                   ,(my/agenda-filter-on-title ".*W25M-MOBBA.*")
                   (org-agenda-max-entries 5)
                   (org-agenda-todo-show-schedule t)
                   ))
            (todo "" 
                  ;; Settings for command
                  ((org-agenda-overriding-header "Machine Learning Tasks")
                   ,(my/agenda-filter-on-title ".*W25M-MALBA.*")
                   (org-agenda-max-entries 5)
                   (org-agenda-todo-show-schedule t)
                   ))
            (todo "" 
                  ;; Settings for command
                  ((org-agenda-overriding-header "Data Engineering Tasks")
                   ,(my/agenda-filter-on-title ".*W25M-DENBA.*")
                   (org-agenda-max-entries 5)
                   (org-agenda-todo-show-schedule t)
                   ))
            (todo "" 
                  ;; Settings for command
                  ((org-agenda-overriding-header "AI Tasks")
                   ,(my/agenda-filter-on-title ".*W25M-AINB1.*")
                   (org-agenda-max-entries 5)
                   (org-agenda-todo-show-schedule t)
                   )))
           ;; List of 'general-settings-for-whole-set'
           ((org-agenda-files '("~/org/calendars/canvas.org")))
           ;; Files to write to
           ("~/org/canvas.html"))
          ) ;; list of things to set custom commands to
        ) ;; custom commands
  ) ;; after org

