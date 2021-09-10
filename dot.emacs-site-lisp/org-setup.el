(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-agenda-files (append
                        (file-expand-wildcards "~/Dropbox/work/journal/*.org")
                        (file-expand-wildcards "~/Dropbox/personalNotes/*.org")
                        (file-expand-wildcards "~/Dropbox/1008NCayuga/*.org")
                        (file-expand-wildcards "~/Dropbox/712/*.org")
                        (file-expand-wildcards "~/Dropbox/sailing/*.org")
                        (file-expand-wildcards "~/Dropbox/LakeGeroge/*.org")
                        ))

(setq org-capture-templates
      '(("j" "WorkJournal" entry (file+olp "~/Dropbox/work/journal/bdc34workjournal.org" "2021" )
         "* %T %?\n%i\n  %a" :tree-type month)
        ("m" "Modui3Journal" entry (file "~/Dropbox/work/journal/modui3.org"  )
         "* %T %?\n%i\n  %a")
        ("p" "PersonalJournal" entry (file+olp "~/Dropbox/personalNotes/journal.org" "2021")
         "* %T %?\n" :tree-type month)
        )
      )
