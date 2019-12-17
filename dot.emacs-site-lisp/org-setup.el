(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-agenda-files '("~/Dropbox/work/journal/bdc34workjournal.org"
                         "~/Dropbox/work/journal/todo.org" ))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/work/journal/todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "WorkJournal" entry (file+olp "~/Dropbox/work/journal/bdc34workjournal.org" "2019" )
         "* %T %?\n%i\n  %a" :tree-type month)
        ("p" "PersonalJournal" entry (file+olp "~/Dropbox/personalNotes/journal.org" "2019")
         "* %T %?\n" :tree-type month)
        )
      )
