;;; dired+.el --- Extensions to Dired.
;; 
;; Filename: dired+.el
;; Description: Extensions to Dired.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1999-2005, Drew Adams, all rights reserved.
;; Created: Fri Mar 19 15:58:58 1999
;; Version: 21.2
;; Last-Updated: Wed Feb 16 16:47:59 2005
;;           By: dradams
;;     Update #: 1036
;; Keywords: unix, mouse, local
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;;    Extensions to Dired
;;
;;  This file extends functionalities provided by standard GNU Emacs
;;  files `dired.el', `dired-aux.el', and `dired-x.el'.
;;
;;  Key bindings changed.  Menus redefined.  `dired-mouse-3-menu'
;;  popup menu added.
;;
;;  New functions defined here (each is actually prefixed by
;;  `dired-'):
;;
;;    `byte-compile-this-file', `capitalize', `capitalize-this-file',
;;    `chgrp-this-file', `chmod-this-file', `chown-this-file',
;;    `compress-this-file', `copy-this-file', `delete-this-file',
;;    `downcase-this-file', `ediff', `fewer-than-2-files-p',
;;    `find-file-other-frame', `flag-region-files-for-deletion',
;;    `hardlink-this-file' (also without `dired-' prefix),
;;    `load-this-file', `mark/unmark-extension',
;;    `(un)mark-region-files', `mouse-3-menu', `mouse-backup-diff',
;;    `mouse-(e)diff', `mouse-do-byte-compile', `mouse-do-chgrp',
;;    `mouse-do-chmod', `mouse-do-chown', `mouse-do-compress',
;;    `mouse-do-copy', `mouse-do-delete', `mouse-do-hardlink',
;;    `mouse-do-load', `mouse-do-print', `mouse-do-rename',
;;    `mouse-do-shell-command', `mouse-do-symlink', `mouse-downcase',
;;    `mouse-find-file(-other-frame)', `mouse-flag-file-deletion',
;;    `mouse-(un)mark', `mouse-mark/unmark',
;;    `mouse-mark-region-files', `mouse-upcase', `mouse-view-file',
;;    `omit-(un)marked', `print-this-file', `(rel)symlink-this-file',
;;    `rename-this-file', `shell-command-this-file',
;;    `upcase-this-file'.
;;
;;  New user options (variables) defined here (each is actually
;;  prefixed by `dired-'):
;;
;;    `dired-compressed-file-suffix-face', `dired-deletion-face',
;;    `dired-dir-heading-face', `dired-dir-priv-face',
;;    `dired-exec-priv-face', `dired-executable-tag-face',
;;    `dired-file-name-face', `dired-file-suffix-face',
;;    `dired-flag-mark-face', `dired-flag-mark-line-face',
;;    `dired-ignored-file-name-face', `dired-link-priv-face',
;;    `dired-no-priv-face', `dired-other-priv-face',
;;    `dired-rare-priv-face', `dired-read-priv-face',
;;    `dired-symlink-face', `dired-write-priv-face'.
;;
;;  Other variables defined here:
;;
;;    `dired-file-line-overlay', `dired-font-lock-keywords-1',
;;    `menu-bar-dired-immediate-menu', `menu-bar-dired-mark-menu',
;;    `menu-bar-dired-operate-menu', `menu-bar-dired-regexp-menu',
;;    `menu-bar-dired-subdir-menu'.
;;
;;
;;  ***** NOTE: The following functions defined in `dired.el' have
;;              been REDEFINED HERE:
;;
;;  `dired-do-delete' - Displays a message to warn that marked, not
;;     flagged, files will be deleted.
;;
;;;;;;  `dired-find-buffer-nocreate' (also in `dired-x.el') - 
;;;;;;     If both DIRNAME and `dired-directory' are conses, then only
;;;;;;     compare their cars (directories), not their explicit file lists
;;;;;;     too.  If equal, then update `dired-directory's file list to
;;;;;;     that of DIRNAME.  This prevents `dired-internal-noselect'
;;;;;;     from creating a new buffer in this case whenever a different
;;;;;;     set of files is present in the cdr of DIRNAME and DIRNAME
;;;;;;     represents the same buffer as `dired-directory'.  If only one
;;;;;;     of DIRNAME & `dired-directory' is a cons, this returns nil.
;;
;;  `dired-insert-set-properties' - `mouse-face' on whole line.
;;  `dired-revert' - Resets `mode-line-process' to nil.
;;
;;
;;  ***** NOTE: The following functions defined in `dired-aux.el' have
;;              been REDEFINED HERE:
;;
;;  `dired-do-byte-compile', `dired-do-compress', `dired-do-load' -
;;     Redisplay only if at most one file is being treated.
;;
;;
;;  ***** NOTE: The following functions defined in `dired-x.el' have
;;              been REDEFINED HERE:
;;
;;  `dired-do-find-marked-files' - Doc string reflects the change (see
;;     below) to `dired-simultaneous-find-file'.
;;  `dired-find-buffer-nocreate' - Also in `dired.el'---See above.
;;     This incorporates `dired-x.el's changes to the `dired.el'
;;     definition.  This version works with or without using dired-x.
;;  `dired-mark-sexp' - 1. Variable `s' -> `blks'.
;;                      2. Fixes to `uid' and `gid'.
;;  `dired-simultaneous-find-file' - Uses separate frames instead of
;;     windows if `pop-up-frames' is non-nil, or if prefix arg < 0.
;;
;;
;;
;;  This file should be loaded after loading either of the GNU files
;;  `dired.el' or `dired-x.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "dired" '(require 'dired+))
;;  (eval-after-load "dired-x" '(require 'dired+))
;;
;;  Library `dired+' requires these libraries:
;;
;;    `avoid', `cl', `custom', `dired', `dired+', `dired-aux', `dired-x',
;;    `easymenu', `ediff-diff', `ediff-help', `ediff-init', `ediff-merg',
;;    `ediff-mult', `ediff-util', `ediff-wind', `fit-frame',
;;    `frame-cmds', `frame-fns', `icomplete', `icomplete+', `info',
;;    `info+', `misc-fns', `mkhtml', `mkhtml-htmlize', `setup-info',
;;    `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;; 2005/02/16 dadams
;;     Added dired-mark/unmark-extension. Replaced dired-mark-extension with it everywhere.
;; 2005/01/08 dadams
;;     Bind [S-mouse-1], instead of [S-down-mouse-1], to dired-mouse-mark-region-files.
;; 2004/11/20 dadams
;;     dired-mark-sexp: Search for literal month names only for versions before Emacs 20.
;;     Refined to deal with Emacs 21 < 21.3.50 (soon to be 22.x)
;; 2004/11/14 dadams
;;     Bound dired-no-confirm to non-nil for dired-mouse-*.
;;     Updated for Emacs 21 and improved highlighting:
;;       Spaces OK in file and directory names. Highlight date/time and size.
;; 2004/10/17 dadams
;;     Require cl only for Emacs 20, and only when compile.
;; 2004/10/01 dadams
;;     Updated to work with Emacs 21 also.
;; 2004/04/02 dadams
;;     dired-font-lock-keywords-1: Prefer using dired-omit-extensions
;;     to completion-ignored-extensions, if available.
;; 2004/03/22 dadams
;;     Added dired-mouse-mark-region-files and dired-mouse-mark/unmark.
;; 2000/09/27 dadams
;;     1. dired-font-lock-keywords-1: fixed for spaces in dir names.
;;     2. Added: dired-buffers-for-dir.
;; 1999/09/06 dadams
;;     Added S-*-mouse-2 bindings (same as C-*-mouse-2).
;; 1999/08/26 dadams
;;     1. Added *-face vars and dired-font-lock-keywords-1.
;;     2. Added possibility to use dired-font-lock-keywords-1 via hook.
;; 1999/08/26 dadams
;;     Changed key binding of dired-mouse-find-file from down-mouse-2 to mouse-2.
;; 1999/08/25 dadams
;;     Changed (C-)(M-)mouse-2 bindings.
;; 1999/08/25 dadams
;;     1. Added cmds & menu bar and key bindings: (dired-)find-file-other-frame.
;;     2. Changed binding for dired-display-file.
;; 1999/03/26 dadams
;;     1. Get rid of Edit menu-bar menu.
;;     2. dired-mouse-3-menu: Changed popup titles and item names.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code: 

(and (< emacs-major-version 21)
     (eval-when-compile (require 'cl))) ;; pop (plus, for Emacs <20: when, unless)

;; Get macro `define-face-const' when this is compiled,
;; or run interpreted, but not when the compiled code is loaded.
(eval-when-compile (require 'def-face-const))

(require 'misc-fns nil t) ;; (no error if not found): undefine-killer-commands
(when (< emacs-major-version 21)
  (require 'mkhtml nil t)) ;; (no error if not found): mkhtml-dired-files
(require 'strings nil t) ;; (no error if not found): display-in-minibuffer

(require 'dired) ;; dired-revert
(require 'dired-aux) ;; dired-bunch-files, dired-do-chxxx, dired-do-create-files,
                     ;; dired-map-over-marks-check, dired-mark-read-string, 
                     ;; dired-read-shell-command, dired-run-shell-command, dired-shell-stuff-it
(require 'dired-x) ;; dired-do-relsymlink
(require 'ediff-util) ;; ediff-read-file-name

 ;; Autoloaded from `view.el': view-file


;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dired+)
(require 'dired+)                       ; Ensure loaded before compile this.

;;;;;;;;;;;;;;;;;;;;;;;


;;; This is duplicated in `diff.el' and `vc.el'.
(defvar diff-switches "-c"
  "*A string or list of strings specifying switches to be be passed to diff.")


;; This is also defined in `menu-bar+.el'.
;; Note: COMMAND must be a command (commandp); it cannot be an expression.
(defmacro menu-item-any-version (item-string command &rest keywords)
  "Returns valid menu-item spec, whether Emacs 21 or not.
KEYWORDS are used only for Emacs 21."
  (if (or (< emacs-major-version 21) (null keywords))
      `(cons ,item-string ',command)
    `'(menu-item ,item-string ,command ,@keywords)))

;;;-----------------------------------------------------------------
;;; Key Bindings.


;;; Menu Bar.
;;; New order is (left -> right):
;;;
;;;     Dir  Regexp  Mark  Multiple  Single

;; Get rid of menu bar predefined in `dired.el'.
(define-key dired-mode-map [menu-bar] nil)
;; Get rid of Edit menu bar menu to save space.
(define-key dired-mode-map [menu-bar edit] 'undefined)


;; "Single" menu.
;;
;; REPLACES ORIGINAL "Immediate" menu in `dired.el'.
;;;###autoload
(defvar menu-bar-dired-immediate-menu (make-sparse-keymap "Single"))
(define-key dired-mode-map [menu-bar immediate]
  (cons "Single" menu-bar-dired-immediate-menu))
(define-key menu-bar-dired-immediate-menu [chown]
  (menu-item-any-version "Change Owner..." dired-chown-this-file
                         :visible (not (memq system-type '(ms-dos windows-nt)))
                         :help "Change the owner of file at cursor"))
(define-key menu-bar-dired-immediate-menu [chgrp]
  (menu-item-any-version "Change Group..." dired-chgrp-this-file
                         :visible (not (memq system-type '(ms-dos windows-nt)))
                         :help "Change the group of file at cursor"))
(define-key menu-bar-dired-immediate-menu [chmod]
  (menu-item-any-version "Change Mode..." dired-chmod-this-file
                         :help "Change mode (attributes) of file at cursor"))
(define-key menu-bar-dired-immediate-menu [separator-ch] '("--"))
(define-key menu-bar-dired-immediate-menu [load]
  (menu-item-any-version "Load" dired-load-this-file
                         :help "Load this Emacs Lisp file"))
(define-key menu-bar-dired-immediate-menu [compile]
  (menu-item-any-version "Byte Compile" dired-byte-compile-this-file
                         :help "Byte-compile this Emacs Lisp file"))
(define-key menu-bar-dired-immediate-menu [command]
  (menu-item-any-version "Shell Command..." dired-shell-command-this-file
                         :help "Run a shell command on file at cursor"))
(define-key menu-bar-dired-immediate-menu [compress]
  (menu-item-any-version "Compress/Decompress" dired-compress-this-file
                         :help "Compress/uncompress file at cursor"))
(define-key menu-bar-dired-immediate-menu [print]
  (menu-item-any-version "Print..." dired-print-this-file
                         :help "Print file at cursor, supplying print command"))
(when (fboundp 'mkhtml-dired-files)
  (define-key menu-bar-dired-immediate-menu [mkhtml-dired-files]
    (menu-item-any-version "Create HTML" mkhtml-dired-files
                           :help "Create an HTML file corresponding to file at cursor")))
(define-key menu-bar-dired-immediate-menu [separator-misc] '("--"))
(define-key menu-bar-dired-immediate-menu [hardlink]
  (menu-item-any-version "Hardlink to..." dired-hardlink-this-file
                         :help "Make hard links for current or marked files"))
(if (not (fboundp 'dired-relsymlink-this-file))
    (define-key menu-bar-dired-immediate-menu [symlink]
      (menu-item-any-version "Symlink to..." dired-symlink-this-file
                             :visible (fboundp 'make-symbolic-link)
                             :help "Make symbolic link for file at cursor"))
  (define-key menu-bar-dired-immediate-menu [symlink]
    (menu-item-any-version
     "Symlink to (Absolute)..." dired-symlink-this-file
     :help "Make absolute symbolic link for file at cursor"))
  (define-key menu-bar-dired-immediate-menu [relsymlink]
    (menu-item-any-version
     "Symlink to (Relative)..." dired-relsymlink-this-file ; In `dired-x.el'.
     :help "Make relative symbolic link for file at cursor")))
(define-key menu-bar-dired-immediate-menu [separator-link] '("--"))
(define-key menu-bar-dired-immediate-menu [delete]
  (menu-item-any-version "Delete" dired-delete-this-file :help "Delete file at cursor"))
(define-key menu-bar-dired-immediate-menu [capitalize]
  (menu-item-any-version "Capitalize" dired-capitalize-this-file
                         :help "Capitalize (initial caps) name of file at cursor"))
(define-key menu-bar-dired-immediate-menu [downcase]
  (menu-item-any-version "Downcase" dired-downcase-this-file
                         ;; When running on plain MS-DOS, there's only one
                         ;; letter-case for file names.
                         :enable (or (not (fboundp 'msdos-long-file-names))
                                     (msdos-long-file-names))
                         :help "Rename file at cursor to a lower-case name"))
(define-key menu-bar-dired-immediate-menu [upcase]
  (menu-item-any-version "Upcase" dired-upcase-this-file
                         :enable (or (not (fboundp 'msdos-long-file-names))
                                     (msdos-long-file-names))
                         :help "Rename file at cursor to an upper-case name"))
(define-key menu-bar-dired-immediate-menu [rename]
  (menu-item-any-version "Rename to..." dired-rename-this-file
                         :help "Rename file at cursor"))
(define-key menu-bar-dired-immediate-menu [copy]
  (menu-item-any-version "Copy to..." dired-copy-this-file :help "Copy file at cursor"))
(define-key menu-bar-dired-immediate-menu [separator-chg] '("--"))
(define-key menu-bar-dired-immediate-menu [backup-diff]
  (menu-item-any-version "Diff with Backup" dired-backup-diff
                         :help "Diff file at cursor with its latest backup"))
(define-key menu-bar-dired-immediate-menu [diff]
  (menu-item-any-version "Diff..." dired-diff
                         :help "Compare file at cursor with another file using `diff'"))
(define-key menu-bar-dired-immediate-menu [ediff]
  (menu-item-any-version "Compare..." dired-ediff
                         :help "Compare file at cursor with another file"))
(define-key menu-bar-dired-immediate-menu [separator-diff] '("--"))
(define-key menu-bar-dired-immediate-menu [view]
  (menu-item-any-version "View (Read Only)" dired-view-file
                         :help "Examine file at cursor in read-only mode"))
(define-key menu-bar-dired-immediate-menu [display]
  (menu-item-any-version "Display in Other Window" dired-display-file
                         :help "Display file at cursor in a different window"))
(define-key menu-bar-dired-immediate-menu [find-file-other-frame]
  (menu-item-any-version "Open in Other Frame" dired-find-file-other-frame
                         :help "Edit file at cursor in a different frame"))
(define-key menu-bar-dired-immediate-menu [find-file-other-window]
  (menu-item-any-version "Open in Other Window" dired-find-file-other-window
                         :help "Edit file at cursor in a different window"))
(define-key menu-bar-dired-immediate-menu [find-file]
  (menu-item-any-version "Open" dired-find-file :help "Edit file at cursor"))


;; "Multiple" menu.
;;
;; REPLACES ORIGINAL "Operate" menu in `dired.el'.
;;;###autoload
(defvar menu-bar-dired-operate-menu (make-sparse-keymap "Multiple"))
(define-key dired-mode-map [menu-bar operate]
  (cons "Multiple" menu-bar-dired-operate-menu))
(define-key menu-bar-dired-operate-menu [chown]
  (menu-item-any-version "Change Owner..." dired-do-chown
                         :visible (not (memq system-type '(ms-dos windows-nt)))
                         :help "Change the owner of marked files"))
(define-key menu-bar-dired-operate-menu [chgrp]
  (menu-item-any-version "Change Group..." dired-do-chgrp
                         :visible (not (memq system-type '(ms-dos windows-nt)))
                         :help "Change the owner of marked files"))
(define-key menu-bar-dired-operate-menu [chmod]
  (menu-item-any-version "Change Mode..." dired-do-chmod
                         :help "Change mode (attributes) of marked files"))
;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
(when (string-match "21.3.50" emacs-version)
  (define-key menu-bar-dired-operate-menu [touch]
    '(menu-item "Change Timestamp..." dired-do-touch
                :help "Change timestamp of marked files")))
(define-key menu-bar-dired-operate-menu [separator-ch] '("--"))
(define-key menu-bar-dired-operate-menu [load]
  (menu-item-any-version "Load" dired-do-load :help "Load marked Emacs Lisp files"))
(define-key menu-bar-dired-operate-menu [compile]
  (menu-item-any-version "Byte Compile" dired-do-byte-compile
                         :help "Byte-compile marked Emacs Lisp files"))
(define-key menu-bar-dired-operate-menu [command]
  (menu-item-any-version "Shell Command..." dired-do-shell-command
                         :help "Run a shell command on each of marked files"))
(define-key menu-bar-dired-operate-menu [compress]
  (menu-item-any-version "Compress/Uncompress" dired-do-compress
                         :help "Compress/uncompress marked files"))
(define-key menu-bar-dired-operate-menu [query-replace]
  (if (< emacs-major-version 21)
      (menu-item-any-version "Query Replace..." dired-do-query-replace)
    (menu-item-any-version "Query Replace..." dired-do-query-replace-regexp
                           :help "Replace regexp in marked files")))
(define-key menu-bar-dired-operate-menu [search]
  (menu-item-any-version "Search Files..." dired-do-search
                         :help "Search marked files for regexp"))
(define-key menu-bar-dired-operate-menu [print]
  (menu-item-any-version "Print..." dired-do-print
                         :help "Print marked files, supplying print command"))
(when (fboundp 'mkhtml-dired-files)
  (define-key menu-bar-dired-operate-menu [mkhtml-dired-files]
    (menu-item-any-version "Create HTML" mkhtml-dired-files
                           :help "Create HTML files corresponding to marked files")))
(define-key menu-bar-dired-operate-menu [separator-link] '("--"))
(define-key menu-bar-dired-operate-menu [hardlink]
  (menu-item-any-version "Hardlink to..." dired-do-hardlink
                         :help "Make hard links for current or marked files"))
(if (not (fboundp 'dired-do-relsymlink))
    (define-key menu-bar-dired-operate-menu [symlink]
      (menu-item-any-version "Symlink to..." dired-do-symlink
                             :visible (fboundp 'make-symbolic-link)
                             :help "Make symbolic links for current or marked files"))
  (define-key menu-bar-dired-operate-menu [symlink]
    (menu-item-any-version
     "Symlink to (Absolute)..." dired-do-symlink
     :help "Make absolute symbolic links for current or marked files"))
  (define-key menu-bar-dired-operate-menu [relsymlink] ; In `dired-x.el'.
    (menu-item-any-version
     "Symlink to (Relative)..." dired-do-relsymlink
     :help "Make relative symbolic links for current or marked files")))
(define-key menu-bar-dired-operate-menu [separator-move] '("--"))
(define-key menu-bar-dired-operate-menu [delete-flagged]
  (menu-item-any-version "Delete Flagged" dired-do-flagged-delete
                         :help "Delete all files flagged for deletion (D)"))
(define-key menu-bar-dired-operate-menu [delete]
  (menu-item-any-version
   "Delete Marked (not Flagged)" dired-do-delete
   :help "Delete current file or all marked files (not flagged files)"))
(define-key menu-bar-dired-operate-menu [capitalize]
  (menu-item-any-version "Capitalize" dired-capitalize
                         :help "Capitalize (initial caps) the names of all marked files"))
(define-key menu-bar-dired-operate-menu [downcase]
  (menu-item-any-version "Downcase" dired-downcase
                         :enable (or (not (fboundp 'msdos-long-file-names))
                                     (msdos-long-file-names))
                         :help "Rename marked files to lowercase names"))
(define-key menu-bar-dired-operate-menu [upcase]
  (menu-item-any-version "Upcase" dired-upcase
                         :enable (or (not (fboundp 'msdos-long-file-names))
                                     (msdos-long-file-names))
                         :help "Rename marked files to uppercase names"))
(define-key menu-bar-dired-operate-menu [rename]
  (menu-item-any-version "Rename to..." dired-do-rename
                         :help "Rename current file or move marked files"))
(define-key menu-bar-dired-operate-menu [copy]
  (menu-item-any-version "Copy to..." dired-do-copy
                         :help "Copy current file or all marked files"))
(define-key menu-bar-dired-operate-menu [separator-misc] '("--"))
(when (fboundp 'dired-copy-filename-as-kill)
  (define-key menu-bar-dired-operate-menu [kill-ring]
    (menu-item-any-version
     "Copy File Names (to Paste)" dired-copy-filename-as-kill
     :help "Copy names of marked files onto kill ring, for pasting")))
(when (fboundp 'dired-do-find-marked-files)
  (define-key menu-bar-dired-operate-menu [find-files]
    (menu-item-any-version "Open" dired-do-find-marked-files ; In `dired-x.el'.
                           :help "Open each marked file for editing")))


;; "Regexp" menu.
;;
;; REPLACES ORIGINAL "Regexp" menu in `dired.el'.
;;;###autoload
(defvar menu-bar-dired-regexp-menu (make-sparse-keymap "Regexp"))
(define-key dired-mode-map [menu-bar regexp]
  (cons "Regexp" menu-bar-dired-regexp-menu))
(define-key menu-bar-dired-regexp-menu [hardlink]
  (menu-item-any-version "Hardlink to..." dired-do-hardlink-regexp
                         :help "Make hard links for files matching regexp"))
(if (not (fboundp 'dired-do-relsymlink-regexp))
    (define-key menu-bar-dired-regexp-menu [symlink]
      (menu-item-any-version "Symlink to..." dired-do-symlink-regexp
                             :visible (fboundp 'make-symbolic-link)
                             :help "Make symbolic links for files matching regexp"))
  (define-key menu-bar-dired-regexp-menu [symlink]
    (menu-item-any-version
     "Symlink to (Absolute)..." dired-do-symlink-regexp
     :visible (fboundp 'make-symbolic-link)
     :help "Make absolute symbolic links for files matching regexp"))
  (define-key menu-bar-dired-regexp-menu [relsymlink] ; In `dired-x.el'.
    (menu-item-any-version
     "Symlink to (Relative)..." dired-do-relsymlink-regexp
     :visible (fboundp 'make-symbolic-link)
     :help "Make relative symbolic links for files matching regexp")))
(define-key menu-bar-dired-regexp-menu [rename]
  (menu-item-any-version "Rename to..." dired-do-rename-regexp
                         :help "Rename marked files matching regexp"))
(define-key menu-bar-dired-regexp-menu [copy]
  (menu-item-any-version "Copy to..." dired-do-copy-regexp
                         :help "Copy marked files matching regexp"))
(define-key menu-bar-dired-regexp-menu [flag]
  (menu-item-any-version "Flag..." dired-flag-files-regexp
                         :help "Flag files matching regexp for deletion"))
(define-key menu-bar-dired-regexp-menu [mark]
  (menu-item-any-version "Mark..." dired-mark-files-regexp
                         :help "Mark files matching regexp for future operations"))
(define-key menu-bar-dired-regexp-menu [mark-cont]
  (menu-item-any-version "Mark Containing..." dired-mark-files-containing-regexp
                         :help "Mark files whose contents matches regexp"))


;; "Mark" menu.
;;
;; REPLACES ORIGINAL "Mark" menu in `dired.el'.
;;;###autoload
(defvar menu-bar-dired-mark-menu (make-sparse-keymap "Mark"))
(define-key dired-mode-map [menu-bar mark] (cons "Mark" menu-bar-dired-mark-menu))

(when (fboundp 'dired-flag-extension)
  (define-key menu-bar-dired-mark-menu [flag-extension] ; In `dired-x.el'
    (menu-item-any-version
     "Flag Extension..." dired-flag-extension
     :help "Flag all files that have a certain extension, for deletion")))
(define-key menu-bar-dired-mark-menu [garbage-files]
  (menu-item-any-version "Flag Garbage Files" dired-flag-garbage-files
                         :help "Flag unneeded files for deletion"))
(define-key menu-bar-dired-mark-menu [backup-files]
  (menu-item-any-version "Flag Backup Files" dired-flag-backup-files
                         :help "Flag all backup files for deletion"))
(define-key menu-bar-dired-mark-menu [auto-save-files]
  (menu-item-any-version "Flag Auto-save Files" dired-flag-auto-save-files
                         :help "Flag auto-save files for deletion"))
(define-key menu-bar-dired-mark-menu [flag-region]
  (menu-item-any-version "Flag Region" dired-flag-region-files-for-deletion
                         :enable mark-active
                         :help "Flag all files in the region (selection) for deletion"))
(when (< emacs-major-version 21)
  (put 'dired-flag-region-files-for-deletion 'menu-enable 'mark-active))
(define-key menu-bar-dired-mark-menu [deletion]
  (menu-item-any-version "Flag" dired-flag-file-deletion
                         :help "Flag current line's file for deletion"))
(define-key menu-bar-dired-mark-menu [separator-flag] '("--"))
(define-key menu-bar-dired-mark-menu [prev]
  (menu-item-any-version "Previous Marked" dired-prev-marked-file
                         :help "Move to previous marked file"))
(define-key menu-bar-dired-mark-menu [next]
  (menu-item-any-version "Next Marked" dired-next-marked-file
                         :help "Move to next marked file"))
(define-key menu-bar-dired-mark-menu [marks]
  (menu-item-any-version "Change Marks..." dired-change-marks
                         :help "Replace marker with another character"))
(define-key menu-bar-dired-mark-menu [revert]
  (menu-item-any-version "Refresh (Show All)" revert-buffer
                         :help "Update contents of shown directories"))
(define-key menu-bar-dired-mark-menu [omit-unmarked]
  (menu-item-any-version "Omit Unmarked" dired-omit-unmarked
                         :help "Hide lines of unmarked files"))
(define-key menu-bar-dired-mark-menu [omit-marked]
  (menu-item-any-version "Omit Marked" dired-omit-marked
                         :help "Hide lines of marked files"))
(define-key menu-bar-dired-mark-menu [toggle-marks]
;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
  (if (string-match "21.3.50" emacs-version)
      '(menu-item "Toggle Marked/Unmarked" dired-toggle-marks
                  :help "Mark unmarked files, unmark marked ones")
    '("Toggle Marked/Unmarked" . dired-do-toggle)))
(define-key menu-bar-dired-mark-menu [separator-mark] '("--"))
(when (fboundp 'dired-mark-sexp)
  (define-key menu-bar-dired-mark-menu [mark-sexp] ; In `dired-x.el'.
    (menu-item-any-version "Mark If..." dired-mark-sexp
                           :help "Mark files for which specified condition is true")))
(define-key menu-bar-dired-mark-menu [mark-extension]
  (menu-item-any-version "Mark Extension..." dired-mark/unmark-extension
                         :help "Mark all files with specified extension"))
(define-key menu-bar-dired-mark-menu [symlinks]
  (menu-item-any-version "Mark Symlinks" dired-mark-symlinks
                         :visible (fboundp 'make-symbolic-link)
                         :help "Mark all symbolic links"))
(define-key menu-bar-dired-mark-menu [directories]
  (menu-item-any-version "Mark Directories" dired-mark-directories
                         :help "Mark all directories except `.' and `..'"))
(define-key menu-bar-dired-mark-menu [directory]
  (menu-item-any-version "Mark Old Backups" dired-clean-directory
                         :help "Flag old numbered backups for deletion"))
(define-key menu-bar-dired-mark-menu [executables]
  (menu-item-any-version "Mark Executables" dired-mark-executables
                         :help "Mark all executable files"))
(define-key menu-bar-dired-mark-menu [mark-region]
  (menu-item-any-version "Mark Region" dired-mark-region-files
                         :enable mark-active
                         :help "Mark all of the files in the region (selection)"))
(when (< emacs-major-version 21)
  (put 'dired-mark-region-files 'menu-enable 'mark-active))
(define-key menu-bar-dired-mark-menu [mark] 
  (menu-item-any-version "Mark" dired-mark
                         :help "Mark current line's file for future operations"))
(define-key menu-bar-dired-mark-menu [separator-unmark] '("--"))
(define-key menu-bar-dired-mark-menu [unmark-all]
  (menu-item-any-version "Unmark All" dired-unmark-all-marks
                         :help "Remove all marks from all files"))
(define-key menu-bar-dired-mark-menu [unmark-with]
  (menu-item-any-version "Unmark Marked-With..." dired-unmark-all-files
                         :help "Remove a specific mark (or all marks) from every file"))
(define-key menu-bar-dired-mark-menu [unmark-region]
  (menu-item-any-version "Unmark Region" dired-unmark-region-files
                         :enable mark-active
                         :help "Unmark all files in the region (selection)"))
(when (< emacs-major-version 21)
  (put 'dired-unmark-region-files 'menu-enable 'mark-active))
(define-key menu-bar-dired-mark-menu [unmark]
  (menu-item-any-version "Unmark" dired-unmark
                         :help "Unmark or unflag current line's file"))


;; "Dir" menu.
;;
;; REPLACES ORIGINAL "Subdir" menu in `dired.el'.
;;;###autoload
(defvar menu-bar-dired-subdir-menu (make-sparse-keymap "Dir"))
(define-key dired-mode-map [menu-bar subdir]
  (cons "Dir" menu-bar-dired-subdir-menu))
(define-key menu-bar-dired-subdir-menu [hide-all]
  (menu-item-any-version "Hide/Show All" dired-hide-all
                         :help "Hide all subdirectories, leave only header lines"))
(define-key menu-bar-dired-subdir-menu [hide-subdir]
  (menu-item-any-version "Hide/Show Subdir" dired-hide-subdir
                         :help "Hide or unhide current directory listing"))
(define-key menu-bar-dired-subdir-menu [tree-down]
  (menu-item-any-version "Tree Down" dired-tree-down
                         :help "Go to first subdirectory header down the tree"))
(define-key menu-bar-dired-subdir-menu [tree-up]
  (menu-item-any-version "Tree Up" dired-tree-up
                         :help "Go to first subdirectory header up the tree"))
(define-key menu-bar-dired-subdir-menu [prev-subdir]
  (menu-item-any-version "Prev Subdir" dired-prev-subdir
                         :help "Go to previous subdirectory header line"))
(define-key menu-bar-dired-subdir-menu [next-subdir]
  (menu-item-any-version "Next Subdir" dired-next-subdir
                         :help "Go to next subdirectory header line"))
(define-key menu-bar-dired-subdir-menu [prev-dirline]
  (menu-item-any-version "Prev Dirline" dired-prev-dirline
                         :help "Move to previous directory-file line"))
(define-key menu-bar-dired-subdir-menu [next-dirline]
  (menu-item-any-version "Next Dirline" dired-next-dirline
                         :help "Move to next directory-file line"))
(define-key menu-bar-dired-subdir-menu [insert]
  (menu-item-any-version "Insert This Subdir" dired-maybe-insert-subdir
                         :help "Insert contents of subdirectory"))
(define-key menu-bar-dired-subdir-menu [separator-subdir] '("--"))
(define-key menu-bar-dired-subdir-menu [create-directory]
  '("Create Directory..." . dired-create-directory)) ; Moved from "Immediate".
(define-key menu-bar-dired-subdir-menu [up]
  (menu-item-any-version "Up Directory" dired-up-directory
                         :help "Edit the parent directory"))
;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
(when (string-match "21.3.50" emacs-version)
  (define-key menu-bar-dired-subdir-menu [wdired-mode]
    '(menu-item "Edit File Names" wdired-change-to-wdired-mode)))
(define-key menu-bar-dired-subdir-menu [dired]
  (menu-item-any-version "Dired (Filter via Wildcards)..." dired
                         :help "Explore a directory (you can provide wildcards)"))


;;; Mouse-3 menu binding.
(define-key dired-mode-map [mouse-3] 'dired-mouse-3-menu)
;;;;;;;;(define-key dired-mode-map [mouse-3] 'ignore)


;;; Non-menu Dired bindings.

;; `dired-mouse-mark-region-files' provides Windows-Explorer behavior
;; for selecting (marking) files.
(define-key dired-mode-map [S-down-mouse-1] 'ignore) ; (normally `mouse-set-font')
(define-key dired-mode-map [S-mouse-1] 'dired-mouse-mark-region-files)
(define-key dired-mode-map [mouse-2] 'dired-mouse-find-file-other-window)
(define-key dired-mode-map [S-down-mouse-2] 'dired-mouse-find-file)
(define-key dired-mode-map [S-mouse-2] 'ignore)
(define-key dired-mode-map [M-mouse-2] 'dired-mouse-find-file-other-frame)
(define-key dired-mode-map "\C-\M-o" 'dired-display-file) ; Was C-o.
(define-key dired-mode-map "\C-o" 'dired-find-file-other-frame)
(define-key dired-mode-map "U" 'dired-unmark-all-files-no-query)
(define-key dired-mode-map "=" 'dired-ediff)
(substitute-key-definition 'next-line 'dired-next-line
                           dired-mode-map (current-global-map))
(substitute-key-definition 'previous-line 'dired-previous-line
                           dired-mode-map (current-global-map))
;; Commands for operating on the current line's file.  When possible,
;; these are lower-case versions of the upper-case commands for operating on
;; the marked files.  (The other corresponding lower-case letters are already
;; defined and cannot be used here.)
(define-key dired-mode-map "b" 'dired-byte-compile-this-file)
(define-key dired-mode-map "r" 'dired-rename-this-file)
(define-key dired-mode-map "y" 'dired-relsymlink-this-file)
(define-key dired-mode-map "z" 'dired-compress-this-file)
(define-key dired-mode-map "\r" 'dired-find-file)
(when (fboundp 'mkhtml-dired-files)
  (define-key dired-mode-map [?\M-h] 'mkhtml-dired-files))
(define-key dired-mode-map [?\M-u] 'dired-upcase-this-file)
(define-key dired-mode-map [?\M-l] 'dired-downcase-this-file)
(define-key dired-mode-map [?\M-c] 'dired-capitalize-this-file)
(define-key dired-mode-map [?\M-m] 'dired-chmod-this-file)
(define-key dired-mode-map [?\M-p] 'dired-print-this-file)
(substitute-key-definition 'kill-line 'dired-delete-this-file
                           dired-mode-map (current-global-map))
;; This replaces the `dired-x.el' binding of `dired-mark-extension'.
(define-key dired-mode-map "*." 'dired-mark/unmark-extension)


;;; Undefine some bindings that would try to modify a Dired buffer.
;;; Their key sequences will then appear to the user as available for
;;; local (Dired) definition.
(when (fboundp 'undefine-killer-commands)
  (undefine-killer-commands dired-mode-map (current-global-map)))



;;;-----------------------------------------------------------------
;;; Variable Definitions

;;; Faces used to fontify buffer when using second level of fontifying.
(unless (boundp 'blue-on-pink-face) (define-face-const "Blue" "Pink"))
(defvar dired-dir-heading-face blue-on-pink-face
  "*Face used for directory headings in dired buffers.")
(unless (boundp 'aquamarine-on-red-face) (define-face-const "Aquamarine" "Red"))
(defvar dired-deletion-face aquamarine-on-red-face
  "*Face used for deletion flags (D) in dired buffers.")
(unless (boundp 'yellow-on-blueviolet-face) (define-face-const "Yellow" "Blueviolet"))
(defvar dired-flag-mark-face yellow-on-blueviolet-face
  "*Face used for flags and marks (except D) in dired buffers.")
(unless (boundp 'skyblue-background-face) (define-face-const nil "Skyblue"))
(defvar dired-flag-mark-line-face skyblue-background-face
  "*Face used for flagged and marked lines in dired buffers.")
(unless (boundp 'darkmagenta-foreground-face) (define-face-const "DarkMagenta" nil))
(defvar dired-file-suffix-face darkmagenta-foreground-face
  "*Face used for file suffixes in dired buffers.")
(unless (boundp 'darkorange-foreground-face) (define-face-const "DarkOrange" nil))
(defvar dired-symlink-face darkorange-foreground-face
  "*Face used for symbolic links in dired buffers.")
(unless (boundp 'darkgoldenrod4-foreground-face) (define-face-const "DarkGoldenrod4" nil))
(defvar dired-date-time-face darkgoldenrod4-foreground-face
  "*Face used for date and time in dired buffers.")
(unless (boundp 'blue-foreground-face) (define-face-const "Blue" nil))
(defvar dired-file-name-face blue-foreground-face
  "*Face used for file names (without suffixes) in dired buffers.")
(unless (boundp 'darkcyan-foreground-face) (define-face-const "DarkCyan" nil))
(defvar dired-ignored-file-name-face darkcyan-foreground-face
  "*Face used for file names (without suffixes) in dired buffers.")
(unless (boundp 'yellow-foreground-face) (define-face-const "Yellow" nil))
(defvar dired-compressed-file-suffix-face yellow-foreground-face
  "*Face used for names of compressed file suffixes in dired buffers.")
(unless (boundp 'red-foreground-face) (define-face-const "Red" nil))
(defvar dired-executable-tag-face red-foreground-face
  "*Face used for executable tag (*) on file names in dired buffers.")
(unless (boundp 'darkred-on-lightgray-face) (define-face-const "DarkRed" "LightGray"))
(defvar dired-dir-priv-face darkred-on-lightgray-face
  "*Face used for directory privilege indicator (d) in dired buffers.")
(unless (boundp 'lightsteelblue-background-face) (define-face-const nil "LightSteelBlue"))
(defvar dired-exec-priv-face lightsteelblue-background-face
  "*Face used for execute privilege indicator (x) in dired buffers.")
(unless (boundp 'palegoldenrod-background-face) (define-face-const nil "PaleGoldenrod"))
(defvar dired-other-priv-face palegoldenrod-background-face
  "*Face used for l,s,S,t,T privilege indicators in dired buffers.")
(unless (boundp 'orchid-background-face) (define-face-const nil "Orchid"))
(defvar dired-write-priv-face orchid-background-face
  "*Face used for write privilege indicator (w) in dired buffers.")
(unless (boundp 'mediumaquamarine-background-face)
  (define-face-const nil "MediumAquamarine"))
(defvar dired-read-priv-face mediumaquamarine-background-face
  "*Face used for read privilege indicator (w) in dired buffers.")
(unless (boundp 'lightgray-background-face) (define-face-const nil "LightGray"))
(defvar dired-no-priv-face lightgray-background-face
  "*Face used for no privilege indicator (-) in dired buffers.")
(unless (boundp 'magenta-on-springgreen-face) (define-face-const "Magenta" "SpringGreen"))
(defvar dired-rare-priv-face magenta-on-springgreen-face
  "*Face used for rare privilege indicators (b,c,s,m,p,S) in dired buffers.")
(unless (boundp 'darkorange-foreground-face) (define-face-const "DarkOrange" nil))
(defvar dired-link-priv-face darkorange-foreground-face
  "*Face used for link privilege indicator (l) in dired buffers.")


;;; Define second level of fontifying.
(defvar dired-font-lock-keywords-1
  (list
   '("^  \\(.+:\\)$" 1 dired-dir-heading-face) ; Directory headers
   '("[^ .]\\.\\([^. /]+\\)$" 1 dired-file-suffix-face) ; Suffix
   '("\\([^ ]+\\) -> [^ ]+$" 1 dired-symlink-face) ; Symbolic links
   ;; 1) Date/time and 2) filename w/o suffix:
   (list dired-move-to-filename-regexp  '(1 dired-date-time-face t t) ; Date/time
         (list "\\(.+\\)$" nil nil (list 0 dired-file-name-face 'keep t))) ; Filename
   ;; Files to ignore
   (list (concat "^  \\(.*\\("
                 (concat (mapconcat 'regexp-quote
                                    (or (and (boundp 'dired-omit-extensions)
                                             dired-omit-extensions)
                                        completion-ignored-extensions)
                                    "[*]?\\|")
                         "[*]?")      ; Allow for executable flag (*).
                 "\\|\\.\\(g?z\\|Z\\)[*]?\\)\\)$") ; Compressed.
         1 dired-ignored-file-name-face t)
   '("[^ .]\\.\\(g?[zZ]\\)[*]?$" 1 dired-compressed-file-suffix-face t) ; Compressed (*.z)
   '("\\([*]\\)$" 1 dired-executable-tag-face t) ; Executable (*)
   '("[0-9][kKMGTPEZY]?" 0 dired-file-suffix-face) ; File size and inode number
   ;; Directory names
   (list "^..\\([0-9]* \\)*d"
         (list dired-move-to-filename-regexp nil nil)
         (list "\\(.+\\)" nil nil '(0 dired-dir-priv-face t t)))
   '("^..\\([0-9]* \\)*.........\\(x\\)" 2 dired-exec-priv-face) ;o x
   '("^..\\([0-9]* \\)*.........\\([lsStT]\\)" 2 dired-other-priv-face) ; o misc
   '("^..\\([0-9]* \\)*........\\(w\\)" 2 dired-write-priv-face) ; o w
   '("^..\\([0-9]* \\)*.......\\(r\\)" 2 dired-read-priv-face)   ; o r
   '("^..\\([0-9]* \\)*......\\(x\\)" 2 dired-exec-priv-face)    ; g x
   '("^..\\([0-9]* \\)*....[^0-9].\\([lsStT]\\)" 2 dired-other-priv-face) ; g misc
   '("^..\\([0-9]* \\)*.....\\(w\\)" 2 dired-write-priv-face) ; g w
   '("^..\\([0-9]* \\)*....\\(r\\)" 2 dired-read-priv-face)   ; g r
   '("^..\\([0-9]* \\)*...\\(x\\)" 2 dired-exec-priv-face)    ; u x
   '("^..\\([0-9]* \\)*...\\([lsStT]\\)" 2 dired-other-priv-face) ; u misc
   '("^..\\([0-9]* \\)*..\\(w\\)" 2 dired-write-priv-face) ; u w
   '("^..\\([0-9]* \\)*.\\(r\\)" 2 dired-read-priv-face)   ; u r
   '("^..\\([0-9]* \\)*.\\([-rwxlsStT]+\\)" 2 dired-no-priv-face keep) ;-
   '("^..\\([0-9]* \\)*\\([bcsmpS]\\)[-rwxlsStT]" 2 dired-rare-priv-face) ; (rare)
   '("^..\\([0-9]* \\)*\\(l\\)[-rwxlsStT]" 2 dired-link-priv-face) ; l
   '("^\\([^ ].*$\\)" 1 dired-flag-mark-line-face t) ; Flag/mark lines
   '("^\\(D\\)" 1 dired-deletion-face t)       ; Deletion flags (D)
   '("^\\([^ D]\\)" 1 dired-flag-mark-face t) ; Flags & marks (except D)
   ) "Expressions to highlight in Dired mode.")


;;; Provide for the second level of fontifying.
(add-hook 'dired-mode-hook
          '(lambda () (if (and (boundp 'font-lock-maximum-decoration)
                               font-lock-maximum-decoration)
                          (set (make-local-variable 'font-lock-defaults)
                               '(dired-font-lock-keywords-1 t)))))


;;;-----------------------------------------------------------------
;;; Function Definitions


;; Similar to `dired-mark-extension' in `dired-x.el'.
;; The difference is that this uses prefix arg to unmark, not to determine the mark character.
(defun dired-mark/unmark-extension (extension &optional unmark-p)
  "Mark all files with a certain EXTENSION for use in later commands.
A `.' is not automatically prepended to the string entered."
  ;; EXTENSION may also be a list of extensions instead of a single one.
  ;; Optional MARKER-CHAR is marker to use.
  (interactive
   (list (dired-read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
				    "ing extension: "))
	 current-prefix-arg))
  (or (listp extension)
      (setq extension (list extension)))
  (dired-mark-files-regexp
   (concat ".";; don't match names with nothing but an extension
           "\\("
           (mapconcat 'regexp-quote extension "\\|")
           "\\)$")
   (and current-prefix-arg ?\040)))



;; REPLACES ORIGINAL in `dired.el'.
;; Allows for consp `dired-directory' too.
;;
(defun dired-buffers-for-dir (dir &optional file)
;; Return a list of buffers that dired DIR (top level or in-situ subdir).
;; If FILE is non-nil, include only those whose wildcard pattern (if any)
;; matches FILE.
;; The list is in reverse order of buffer creation, most recent last.
;; As a side effect, killed dired buffers for DIR are removed from
;; dired-buffers.
  (setq dir (file-name-as-directory dir))
  (let ((alist dired-buffers) result elt buf pattern)
    (while alist
      (setq elt (car alist)
	    buf (cdr elt))
      (if (buffer-name buf)
	  (if (dired-in-this-tree dir (car elt))
	      (with-current-buffer buf
		(and (assoc dir dired-subdir-alist)
		     (or (null file)
			 (let ((wildcards
                                ;; Allow for consp `dired-directory' too.
                                (file-name-nondirectory
                                 (if (consp dired-directory)
                                     (car dired-directory)
                                   dired-directory))))
			   (or (= 0 (length wildcards))
			       (string-match (dired-glob-regexp wildcards)
					     file))))
		     (setq result (cons buf result)))))
	;; else buffer is killed - clean up:
	(setq dired-buffers (delq elt dired-buffers)))
      (setq alist (cdr alist)))
    result))

;;;###autoload
(defun dired-find-file-other-frame ()
  "In dired, visit this file or directory in another frame."
  (interactive)
  (find-file-other-frame (file-name-sans-versions (dired-get-filename) t)))

;;;###autoload
(defun dired-mouse-find-file-other-frame (event)
  "In dired, visit file or directory clicked on in another frame."
  (interactive "e")
  (let ((pop-up-frames t))
    (dired-mouse-find-file-other-window event)))

;;;###autoload
(defun dired-omit-marked ()
  "Omit lines of marked files.  Returns the number of lines omitted."
  (interactive)
  (let ((old-modified-p (buffer-modified-p))
        count)
    (when (interactive-p) (message "Omitting marked lines..."))
    (setq count (dired-do-kill-lines nil "Omitted %d line%s."))
    (set-buffer-modified-p old-modified-p) ; So no `%*' appear in mode-line.
    count))

;;;###autoload
(defun dired-omit-unmarked ()
  "Omit lines of unmarked files.  Returns the number of lines omitted."
  (interactive)
  (let ((old-modified-p (buffer-modified-p))
        count)
    (dired-do-toggle)
    (message "Omitting unmarked lines...")
    (setq count (dired-omit-marked))
    (dired-do-toggle)                   ; Marks all except `.', `..'
    (set-buffer-modified-p old-modified-p) ; So no `%*' appear in mode-line.
    count))

;;;###autoload
(defun dired-ediff (file2)
  "Compare file at cursor with file FILE2 using `ediff'.
FILE2 defaults to the file at the cursor as well.  If you enter just a
directory name for FILE2, then the file at the cursor is compared with
a file of the same name in that directory.  FILE2 is the second file
given to `ediff'; the file at the cursor is the first."
  (interactive
   (progn
     (require 'ediff)
     (list (ediff-read-file-name        ; In `ediff.el'.
            (format "Compare %s with" (dired-get-filename t))
            (dired-current-directory) (dired-get-filename)))))
  (ediff-files (dired-get-filename) file2)) ; In `ediff.el'.


(defsubst dired-fewer-than-2-files-p (arg)
  "Returns non-nil iff fewer than two files are to be treated by dired.
More precisely, returns non-nil iff ARG is nil and fewer than two
files are marked, or ARG is -1, 0 or 1."
  (if arg
      (and (integerp arg) (< (abs arg) 2)) ; Next or previous file (or none).
    (not (save-excursion                ; Fewer than two marked files.
           (goto-char (point-min))
           (re-search-forward (dired-marker-regexp) nil t 2)))))


;; REPLACES ORIGINAL version in `dired-aux.el':
;; Redisplay only if at most one file is being treated.
;;;###autoload
(defun dired-do-compress (&optional arg)
  "Compress or uncompress marked (or next prefix arg) files."
  (interactive "P")
  (dired-map-over-marks-check (function dired-compress) arg 'compress
                              (dired-fewer-than-2-files-p arg)))


;; REPLACES ORIGINAL version in `dired-aux.el':
;; Redisplay only if at most one file is being treated.
;;;###autoload
(defun dired-do-byte-compile (&optional arg)
  "Byte compile marked (or next ARG) Emacs Lisp files."
  (interactive "P")
  (dired-map-over-marks-check (function dired-byte-compile) arg 'byte-compile
                              (dired-fewer-than-2-files-p arg)))
   

;; REPLACES ORIGINAL version in `dired-aux.el':
;; Redisplay only if at most one file is being treated.
;;;###autoload
(defun dired-do-load (&optional arg)
  "Load the marked (or next ARG) Emacs Lisp files."
  (interactive "P")
  (dired-map-over-marks-check (function dired-load) arg 'load
                              (dired-fewer-than-2-files-p arg)))


;;; VISIT ALL MARKED FILES SIMULTANEOUSLY.

;;; Brief Description:
;;;
;;; `dired-do-find-marked-files' is bound to `F' by dired-x.el.
;;;
;;; * Use `dired-get-marked-files' to collect the marked files in the current
;;;   Dired Buffer into a list of filenames `FILE-LIST'.
;;;
;;; * Pass FILE-LIST to `dired-simultaneous-find-file' all with
;;;   `dired-do-find-marked-files''s prefix argument OPTION.
;;;
;;; * `dired-simultaneous-find-file' runs through FILE-LIST decrementing the
;;;   list each time.
;;;
;;; * If OPTION and `pop-up-frames' are both nil, then calculate the
;;; `size' of the window for each file by dividing the `window-height'
;;; by length of FILE-LIST.  Thus, `size' is cognizant of the
;;; window-configuration.
;;;
;;; * If `size' is too small abort, otherwise run `find-file' on each element
;;;   of FILE-LIST giving each a window of height `size'.

;; REPLACES ORIGINAL version in `dired-x.el':
;; Doc string updated to reflect change to `dired-simultaneous-find-file'.
;;;###autoload
(defun dired-do-find-marked-files (&optional option)
  "Find marked files, displaying all of them simultaneously.
With a prefix arg >= 0, just find files but do not select them.

If no prefix arg, and variable `pop-up-frames' is non-nil, or
if prefix arg < 0, then each file is displayed in a separate frame.

Otherwise (no prefix arg and nil `pop-up-frames'), the current window
is split across all marked files, as evenly as possible.  Remaining
lines go to the bottom-most window.  The number of files that can be
displayed this way is restricted by the height of the current window
and `window-min-height'.

To keep the Dired buffer displayed, type \\[split-window-vertically] first.
To display just the marked files, type \\[delete-other-windows] first."
  (interactive "P")
  (setq option (and option (prefix-numeric-value option)))
  (dired-simultaneous-find-file (dired-get-marked-files) option))


;; REPLACES ORIGINAL version in `dired-x.el':
;; Uses separate frames instead of windows if `pop-up-frames' is non-nil,
;; or if prefix arg is negative.
(defun dired-simultaneous-find-file (file-list option)
  "Visit all files in list FILE-LIST and display them simultaneously.

With non-nil OPTION >= 0, the files are found but not selected.

If `pop-up-frames' is non-nil or OPTION < 0, use a separate frame
for each file.

Otherwise, the current window is split across all files in
FILE-LIST, as evenly as possible.  Remaining lines go to the
bottom-most window.  The number of files that can be displayed
this way is restricted by the height of the current window and
the variable `window-min-height'."

  ;; This is not interactive because it is usually too clumsy to
  ;; specify FILE-LIST interactively unless via dired.

  (let (size)
    (cond ((and option (natnump option))
           (while file-list (find-file-option (car file-list)) (pop file-list)))
          ((or pop-up-frames option)
           (while file-list (find-file-other-frame (car file-list)) (pop file-list)))
          (t
           (setq size (/ (window-height) (length file-list)))
           (when (> window-min-height size)
             (error "Too many files to visit simultaneously.  Try C-u prefix."))
           (find-file (car file-list))
           (pop file-list)
           (while file-list
             ;; Vertically split off a window of desired size.
             ;; Upper window will have SIZE lines.
             ;; Select lower (larger) window.  We split it again.
             (select-window (split-window nil size))
             (find-file (car file-list))
             (pop file-list))))))
          

;;;;;; REPLACES ORIGINAL versions in both `dired.el' and `dired-x.el':
;;;;;;
;;;;;; 1. This incorporates the `dired-x.el' change to the `dired.el'
;;;;;;    definition.  This version works with or without using dired-x.
;;;;;;    The `dired-x.el' version respects the var `dired-find-subdir'.
;;;;;;    When `dired-find-subdir' is non-nil, this version is the same
;;;;;;    as the `dired-x.el' version, except that a bug is corrected:
;;;;;;    Whenever the argument to `dired-find-buffer-nocreate' is a cons,
;;;;;;    the call to `dired-buffers-for-dir' gave a wrong type error.
;;;;;;    This has been avoided by not respecting `dired-find-subdir'
;;;;;;    whenever `dired-find-buffer-nocreate' is a cons.
;;;;;;    For the case when `dired-find-subdir' is nil, see #2, below.
;;;;;;
;;;;;; 2. Unless `dired-find-subdir' is bound and non-nil:
;;;;;;    If both DIRNAME and `dired-directory' are conses, then only
;;;;;;    compare their cars (directories), not their explicit file lists
;;;;;;    too.  If equal, then update `dired-directory's file list to that
;;;;;;    of DIRNAME.
;;;;;;
;;;;;;    This prevents `dired-internal-noselect' (which is currently
;;;;;;    `dired-find-buffer-nocreate's only caller) from creating a new
;;;;;;    buffer in this case whenever a different set of files is present
;;;;;;    in the cdr of DIRNAME and DIRNAME represents the same buffer as
;;;;;;    `dired-directory'.
;;;;;;
;;;;;;    If only one of DIRNAME and `dired-directory' is a cons, then
;;;;;;    this returns nil.
;;;;;;;###autoload
;;;;(defun dired-find-buffer-nocreate (dirname &optional mode)
;;;;  (let ((atomic-dirname-p (atom dirname)))
;;;;    (if (and (boundp 'dired-find-subdir) dired-find-subdir atomic-dirname-p)
;;;;        ;; This is the `dired-x.el' change:
;;;;        (let* ((cur-buf (current-buffer))
;;;;               (buffers (nreverse (dired-buffers-for-dir dirname)))
;;;;               (cur-buf-matches (and (memq cur-buf buffers)
;;;;                                     ;; Files list (wildcards) must match, too:
;;;;                                     (equal dired-directory dirname))))
;;;;          (setq buffers (delq cur-buf buffers)) ; Avoid using same buffer---
;;;;          (or (car (sort buffers (function dired-buffer-more-recently-used-p)))
;;;;              (and cur-buf-matches cur-buf))) ; ---unless no other possibility.
;;;;      ;; Comment from `dired.el':
;;;;      ;;  This differs from `dired-buffers-for-dir' in that it doesn't consider
;;;;      ;;  subdirs of `default-directory' and searches for the first match only.
;;;;      (let ((blist dired-buffers)       ; was (buffer-list)
;;;;            found)
;;;;        (or mode (setq mode 'dired-mode))
;;;;        (while blist
;;;;          (if (null (buffer-name (cdr (car blist))))
;;;;              (setq blist (cdr blist))
;;;;            (save-excursion
;;;;              (set-buffer (cdr (car blist)))
;;;;              (if (not (and (eq major-mode mode)
;;;;                            ;; DIRNAME and `dired-directory' have the same dir,
;;;;                            ;; and if either of them has an explicit file list,
;;;;                            ;; then both of them do.  In that case, update
;;;;                            ;; `dired-directory's file list from DIRNAME.
;;;;                            (if atomic-dirname-p
;;;;                                (and (atom dired-directory) ; Both are atoms.
;;;;                                     (string= (file-truename dirname)
;;;;                                              (file-truename dired-directory)))
;;;;                              (and (consp dired-directory) ; Both are conses.
;;;;                                   (string=
;;;;                                    (file-truename (car dirname))
;;;;                                    (file-truename (car dired-directory)))
;;;;                                   ;; Update `dired-directory's file list.
;;;;                                   (setq dired-directory dirname)))))
;;;;                  (setq blist (cdr blist))
;;;;                (setq found (cdr (car blist)))
;;;;                (setq blist nil)))))
;;;;        found))))


;; Reverting a dired buffer

;; REPLACES ORIGINAL in `dired.el':
;; Resets `mode-line-process' to nil.
;;;###autoload
(when (< emacs-major-version 21)
  (or (fboundp 'old-dired-revert) (fset 'old-dired-revert (symbol-function 'dired-revert)))
  (defun dired-revert (&optional arg noconfirm)
    (setq mode-line-process nil)        ; Set by, e.g., `find-dired'.
    (old-dired-revert arg noconfirm)))


;; REPLACES ORIGINAL in `dired.el':
;; `mouse-face' on whole line, not just file name.
;;;###autoload
(defun dired-insert-set-properties (beg end)
  ;; Highlight file names when the mouse is on them.
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (condition-case nil
          (when (dired-move-to-filename)
            (add-text-properties
             (save-excursion (beginning-of-line) (point))
             (save-excursion (end-of-line) (point))
             '(mouse-face highlight
                          help-echo "mouse-2: visit this file in other window")))
	(error nil))
      (forward-line 1))))


;; REPLACES ORIGINAL in `dired.el':
;; Display a message to warn that flagged, not marked, files will be deleted.
;;;###autoload
(defun dired-do-flagged-delete (&optional no-msg)
  "In dired, delete the files flagged for deletion.
NOTE: This deletes flagged, not marked, files.
If arg NO-MSG is non-nil, no messages are displayed."
  (interactive)
  (unless no-msg
    (ding)
    (if (fboundp 'display-in-minibuffer)
        (display-in-minibuffer
         1 "NOTE: Deletion of files flagged `"
         (list blue-foreground-face (format "%c" dired-del-marker))
         "' (not those marked `"
         (list blue-foreground-face (format "%c" dired-marker-char)) "').")
      (message "NOTE: Deletion of files flagged `%c' (not those marked `%c')."
               dired-del-marker dired-marker-char)))
  (let* ((dired-marker-char dired-del-marker)
	 (regexp (dired-marker-regexp))
	 case-fold-search)
    (if (save-excursion (goto-char (point-min))
                        (re-search-forward regexp nil t))
	(dired-internal-do-deletions
	 ;; This can't move point since last arg is nil.
	 (dired-map-over-marks (cons (dired-get-filename) (point)) nil)
	 nil)
      (unless no-msg
        (message "(No deletions requested.)")))))


;; REPLACES ORIGINAL in `dired.el':
;; Display a message to warn that marked, not flagged, files will be deleted.
;;;###autoload
(defun dired-do-delete (&optional arg)
  "Delete all marked (or next ARG) files.
NOTE: This deletes marked, not flagged, files."
  (interactive "P")
  ;; This is more consistent with the file-marking feature than
  ;; `dired-do-flagged-delete'.  But it can be confusing to the user,
  ;; especially since this is usually bound to `D', which is also the
  ;; `dired-del-marker'.  So offer this warning message:
  (unless arg
    (ding)
    (if (fboundp 'display-in-minibuffer)
        (display-in-minibuffer
         1 "NOTE: Deletion of files marked `"
         (list blue-foreground-face (format "%c" dired-marker-char))
         "' (not those flagged `"
         (list blue-foreground-face (format "%c" dired-del-marker)) "').")
      (message "NOTE: Deletion of files marked `%c' (not those flagged `%c')."
               dired-marker-char dired-del-marker)))
  (dired-internal-do-deletions
   ;; This may move point if ARG is an integer.
   (dired-map-over-marks (cons (dired-get-filename) (point)) arg)
   arg))

;;;###autoload
(defun dired-capitalize (&optional arg)
  "Rename all marked (or next ARG) files by capitilizing them.
This gives the file name(s) a first character in upper case and the
rest lower case."
  (interactive "P")
  (dired-rename-non-directory (function capitalize) "Rename by capitalizing:" arg))


;;; Versions of `dired-do-*' commands for just this line's file.
(defsubst dired-delete-this-file ()
  "In dired, delete the file on the cursor line, upon confirmation."
  (interactive) (dired-do-delete 1))
(defsubst dired-capitalize-this-file ()
  "In dired, rename the file on the cursor line by capitilizing it.
This gives the file name a first character in upper case and the rest
lower case."
  (interactive) (dired-capitalize 1))
(defsubst dired-downcase-this-file ()
  "In dired, rename the file on the cursor line to lower case."
  (interactive) (dired-downcase 1))
(defsubst dired-upcase-this-file ()
  "In dired, rename the file on the cursor line to upper case."
  (interactive) (dired-upcase 1))
(defsubst dired-rename-this-file ()
  "In dired, rename the file on the cursor line."
  (interactive) (dired-do-rename 1))
(defsubst dired-copy-this-file ()
  "In dired, copy the file on the cursor line."
  (interactive) (dired-do-copy 1))
(defsubst dired-relsymlink-this-file ()
  "In dired, make a relative symbolic link to file on cursor line."
  (interactive) (dired-do-relsymlink 1))
(defsubst dired-symlink-this-file ()
  "In dired, make a symbolic link to the file on the cursor line."
  (interactive) (dired-do-symlink 1))
(defsubst dired-hardlink-this-file ()
  "In dired, add a name (hard link) to the file on the cursor line."
  (interactive) (dired-do-hardlink 1))
(defsubst dired-print-this-file ()
  "In dired, print the file on the cursor line."
  (interactive) (dired-do-print 1))
(defsubst dired-compress-this-file ()
  "In dired, compress or uncompress the file on the cursor line."
  (interactive) (dired-do-compress 1))
(defsubst dired-shell-command-this-file (command)
  "In dired, run a shell command on the file on the cursor line."
  (interactive
   (list (dired-read-shell-command (concat "! on " "%s: ") 1
                                   (list (dired-get-filename t)))))
  (dired-do-shell-command command 1))
(defsubst dired-byte-compile-this-file ()
  "In dired, byte compile the (Lisp source) file on the cursor line."
  (interactive) (dired-do-byte-compile 1))
(defsubst dired-load-this-file ()
  "In dired, load the file on the cursor line."
  (interactive) (dired-do-load 1))
(defsubst dired-chmod-this-file ()
  "In dired, change the mode of the file on the cursor line."
  (interactive) (dired-do-chmod 1))
(defsubst dired-chgrp-this-file ()
  "In dired, change the group of the file on the cursor line."
  (interactive) (dired-do-chgrp 1))
(defsubst dired-chown-this-file ()
  "In dired, change the owner of the file on the cursor line."
  (interactive) (dired-do-chown 1))


;; REPLACES ORIGINAL in `dired-x.el':
;; 1. Variable (symbol) `s' -> `blks'.
;; 2. Fixes to remove leading space from `uid' and allow `.' in `gid'.
;; 3. Cleaned up doc string and code a bit.
;;;###autoload
(defun dired-mark-sexp (predicate &optional unmark-p)
  "Mark files for which PREDICATE returns non-nil.
With a prefix arg, unmark those files instead.

PREDICATE is a lisp sexp that can refer to the following variables:

    `mode'   [string]  file permission bits, e.g. \"-rw-r--r--\"
    `nlink'  [integer] number of links to file
    `size'   [integer] file size in bytes
    `uid'    [string]  owner
    `gid'    [string]  group (If the gid is not displayed by `ls',
                       this will still be set (to the same as uid))
    `time'   [string]  the time that `ls' displays, e.g. \"Feb 12 14:17\"
    `name'   [string]  the name of the file
    `sym'    [string]  if file is a symbolic link, the linked-to name,
                       else \"\"
    `inode'  [integer] the inode of the file (only for `ls -i' output)
    `blks'   [integer] the size of the file for `ls -s' output
                       (ususally in blocks or, with `-k', in Kbytes)
Examples:
  Mark zero-length files: `(equal 0 size)'
  Mark files last modified on Feb 2: `(string-match \"Feb  2\" time)'
  Mark uncompiled Emacs Lisp files (`.el' file without a `.elc' file):
     First, dired just the source files: `dired *.el'. 
     Then, use \\[dired-mark-sexp] with this sexp:
          (not (file-exists-p (concat name \"c\")))"

  ;; Using `sym' = "", instead of nil, for non-linked files avoids the trap of
  ;; (string-match "foo" sym) into which a user would soon fall.
  ;; Use `equal' instead of `=' in the example, as it works on integers and strings.
  (interactive "xVars: inode,blks,mode,nlink,uid,gid,size,time,name,sym -> \nP")
  (message "%s" predicate)
  (let ((dired-marker-char (if unmark-p ?\040 dired-marker-char))
        (inode nil)
        (blks nil)
        mode nlink uid gid size time name sym)
    (dired-mark-if
     (save-excursion
       (and
        ;; Sets vars INODE BLKS MODE NLINK UID GID SIZE TIME NAME and SYM
        ;; according to current file line.  Returns `t' for success, nil if
        ;; there is no file line.  Upon success, these vars are set, to either
        ;; nil or the appropriate value, so they need not be initialized.
        ;; Moves point within the current line.
        (dired-move-to-filename)
        (let ((mode-len 10)             ; Length of mode string.
              ;; As in `dired.el', but with subexpressions \1=inode, \2=blks:
              (dired-re-inode-size "\\s *\\([0-9]*\\)\\s *\\([0-9]*\\) ?")
              pos)
          (beginning-of-line)
          (forward-char 2)
          (when (looking-at dired-re-inode-size)
            (goto-char (match-end 0))
            (setq inode (string-to-int (buffer-substring (match-beginning 1)
                                                         (match-end 1))))
            (setq blks (string-to-int (buffer-substring (match-beginning 2)
                                                        (match-end 2)))))
          (setq mode (buffer-substring (point) (+ mode-len (point))))
          (forward-char mode-len)
          (setq nlink (read (current-buffer)))
          (forward-char 1)              ; Fix: skip space.
          ;; Karsten Wenger <kw@cis.uni-muenchen.de> fixed uid.
          (setq uid (buffer-substring (+ (point) 1) (progn (forward-word 1) (point))))
          (re-search-forward
           (if (< emacs-major-version 20)
               "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)"
             dired-move-to-filename-regexp))
          (goto-char (match-beginning 1))
          (forward-char -1)
          (setq size (string-to-int (buffer-substring (save-excursion
                                                        (backward-word 1)
                                                        (setq pos (point)))
                                                      (point))))
          (goto-char pos)
          (backward-word 1)
          ;; if no gid is displayed, gid will be set to uid
          ;; but user will then not reference it anyway in PREDICATE.
          (setq gid (buffer-substring (save-excursion (forward-word 1) (point))
                                      (point)))
          (setq time (buffer-substring (match-beginning 1)
                                       (1- (dired-move-to-filename))))
          (setq name (buffer-substring (point)
                                       (or (dired-move-to-end-of-filename t)
                                           (point))))
          (setq sym  (if (looking-at " -> ")
                         (buffer-substring (progn (forward-char 4) (point))
                                           (progn (end-of-line) (point)))
                       "")))
        (eval predicate)))
     (format "'%s file" predicate))))

;;;###autoload
(defun dired-mark-region-files (&optional unmark-p)
  "Mark all of the files in the current region (if it is active).
With a non-nil prefix arg, unmark them instead."
  (interactive "P")
  (let ((beg (min (point) (mark)))
        (end (max (point) (mark))))
    (setq beg (save-excursion (goto-char beg) (beginning-of-line) (point)))
    (setq end (save-excursion (goto-char end) (end-of-line) (point)))
    (let ((dired-marker-char (if unmark-p ?\040 dired-marker-char)))
      (dired-mark-if (and (<= (point) end) (>= (point) beg)) "in region"))))

;;;###autoload
(defun dired-unmark-region-files (&optional mark-p)
  "Unmark all of the files in the current region (if it is active).
With a non-nil prefix arg, mark them instead."
  (interactive "P")
  (let ((beg (min (point) (mark)))
        (end (max (point) (mark))))
    (setq beg (save-excursion (goto-char beg) (beginning-of-line) (point)))
    (setq end (save-excursion (goto-char end) (end-of-line) (point)))
    (let ((dired-marker-char (if mark-p dired-marker-char ?\040)))
      (dired-mark-if (and (<= (point) end) (>= (point) beg)) "in region"))))

;;;###autoload
(defun dired-flag-region-files-for-deletion ()
  "Flag all of the files in the current region (if it is active) for deletion."
  (interactive)
  (let ((beg (min (point) (mark)))
        (end (max (point) (mark))))
    (setq beg (save-excursion (goto-char beg) (beginning-of-line) (point)))
    (setq end (save-excursion (goto-char end) (end-of-line) (point)))
    (let ((dired-marker-char dired-del-marker))
      (dired-mark-if (and (<= (point) end) (>= (point) beg)) "in region"))))



;;; Mouse 3 menu.
;;;;;;;;;;;;;;;;;

;;;###autoload
(defvar dired-file-line-overlay nil)

;;;###autoload
(defun dired-mouse-3-menu (event)
  "Pop-up menu on Mouse-3 for a file or directory listed in dired buffer."
  (interactive "e")
  (let (selection)
    (if mark-active
        (setq selection
              (x-popup-menu
               event
               (list
                "Files in Region"
                (list
                 ""
                 '("Mark" . dired-mark-region-files)
                 '("Unmark" . dired-unmark-region-files)
                 '("Flag for Deletion" .
                   dired-flag-region-files-for-deletion)))))
      (let* ((mouse-pos (event-start event))
             bol eol
             (file/dir-name
              (save-excursion
                (set-buffer (window-buffer (posn-window mouse-pos)))
                (save-excursion
                  (goto-char (posn-point mouse-pos))
                  (save-excursion
                    (setq bol (progn (beginning-of-line) (point)))
                    (setq eol (progn (end-of-line) (point))))
                  (if dired-file-line-overlay ; Don't recreate if exists.
                      (move-overlay dired-file-line-overlay bol eol
                                    (current-buffer))
                    (setq dired-file-line-overlay (make-overlay bol eol))
                    (overlay-put dired-file-line-overlay 'face 'region))
                  (and (not (eobp)) (dired-get-filename nil t))))))
        (sit-for 0)
        (setq selection
              (x-popup-menu
               (and file/dir-name event)
               (list
                "This File"
                (if file/dir-name
                    (list
                     file/dir-name

                     ;; Stuff from `Mark' menu.
                     (if (dired-file-marker file/dir-name)
                         '("Unmark" . dired-mouse-unmark) ; It's now marked.
                       '("Mark" . dired-mouse-mark)) ;  It's now unmarked.
                     '("Flag for Deletion" . dired-mouse-flag-file-deletion)
                     '("--")            ; Separator.

                     ;; Stuff from `Single' / `Multiple' menus.
                     '("Open" . dired-mouse-find-file)
                     '("Open in Other Window" .
                       dired-mouse-find-file-other-window)
                     '("Open in Other Frame" .
                       dired-mouse-find-file-other-frame)
                     '("View (Read Only)" . dired-mouse-view-file)
                     '("Compare..." . dired-mouse-ediff)
                     '("Diff..." . dired-mouse-diff)
                     '("Diff with Backup" . dired-mouse-backup-diff)
                     '("Copy to..." . dired-mouse-do-copy)
                     '("Rename to..." . dired-mouse-do-rename)
                     '("Upcase" . dired-mouse-upcase)
                     '("Downcase" . dired-mouse-downcase)
                     '("Delete" . dired-mouse-do-delete)
                     '("Shell Command..." . dired-mouse-do-shell-command)
                     (and (fboundp 'dired-do-relsymlink)
                          '("Symlink to (Relative)..."
                                        . dired-do-relsymlink))
                     '("Symlink to..." . dired-mouse-do-symlink)
                     '("Hardlink to..." . dired-mouse-do-hardlink)
                     '("Print" . dired-mouse-do-print)
                     '("Compress/Decompress" . dired-mouse-do-compress)
                     '("Byte Compile" . dired-mouse-do-byte-compile)
                     '("Load" . dired-mouse-do-load)
                     '("Change Mode..." . dired-mouse-do-chmod)
                     '("Change Group..." . dired-mouse-do-chgrp)
                     '("Change Owner..." . dired-mouse-do-chown)
                     )
                  '("" (""))))))        ; No menu: not on a file line.
        (when dired-file-line-overlay
          (delete-overlay dired-file-line-overlay))))
    (and selection (call-interactively selection))))
  
;;;###autoload
(defun dired-mouse-find-file (event)
  "Replace dired in its window by this file or directory."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq file (dired-get-filename))))
    (select-window (posn-window (event-end event)))
    (find-file (file-name-sans-versions file t))))

;;;###autoload
(defun dired-mouse-view-file (event)
  "Examine this file in view mode, returning to dired when done.
When file is a directory, show it in this buffer if it is inserted;
otherwise, display it in another buffer."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq file (dired-get-filename))))
    (select-window (posn-window (event-end event)))
    (if (file-directory-p file)
        (or (and (cdr dired-subdir-alist) (dired-goto-subdir file))
            (dired file))
      (view-file file))))               ; In `view.el'.

;;;###autoload
(defun dired-mouse-ediff (event)
  "Compare this file (pointed by mouse) with file FILE2 using `ediff'.
FILE2 defaults to this file as well.  If you enter just a directory
name for FILE2, then this file is compared with a file of the same
name in that directory.  FILE2 is the second file given to `ediff';
this file is the first given to it."
  (interactive "e")
  (require 'ediff)
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (call-interactively 'dired-ediff)))

;;;###autoload
(defun dired-mouse-diff (event &optional switches)
  "Compare this file (pointed by mouse) with file FILE2 using `diff'.
FILE2 defaults to the file at the mark.  This file is the first file
given to `diff'.  With prefix arg, prompt for second arg SWITCHES,
which are options for `diff'."
  (interactive "e")
  (let ((default (if (mark t)
                     (save-excursion (goto-char (mark t))
                                     (dired-get-filename t t))))
        (mouse-pos (event-start event)))
    (require 'diff)
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (let ((file2 (read-file-name (format "Diff %s with: %s"
                                         (dired-get-filename t)
                                         (if default
                                             (concat "(default " default ") ")
                                           ""))
                                 (dired-current-directory) default t)))
      (setq switches
            (and current-prefix-arg
                 (read-string "Options for diff: "
                              (if (stringp diff-switches)
                                  diff-switches
                                (mapconcat 'identity diff-switches " ")))))
      (diff file2 (dired-get-filename t) switches))))

;;;###autoload
(defun dired-mouse-backup-diff (event)
  "Diff this file with its backup file or vice versa.
Use the latest backup, if there are several numerical backups.
If this file is a backup, diff it with its original.
The backup file is the first file given to `diff'.
With prefix arg, prompt for SWITCHES which are the options for `diff'."
  (interactive "e")
  (let ((switches (and current-prefix-arg
                       (read-string "Options for diff: "
                                    (if (stringp diff-switches)
                                        diff-switches
                                      (mapconcat 'identity diff-switches
                                                 " ")))))
        (mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (diff-backup (dired-get-filename) switches)))

;;;###autoload
(defun dired-mouse-mark (event)
  "In dired, mark this file.
If on a subdir headerline, mark all its files except `.' and `..'.

Use \\[dired-unmark-all-files] to remove all marks,
and \\[dired-unmark] on a subdir to remove the marks in this subdir."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (if (and (cdr dired-subdir-alist) (dired-get-subdir))
      (save-excursion (dired-mark-subdir-files))
    (let (buffer-read-only)
      (dired-repeat-over-lines 1 (function (lambda ()
                                             (delete-char 1)
                                             (insert dired-marker-char))))
      (dired-previous-line 1))))

;;;###autoload
(defun dired-mouse-unmark (event)
  "In dired, unmark this file.
If looking at a subdir, unmark all its files except `.' and `..'."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (let ((dired-marker-char ?\040)) (dired-mark nil))
  (dired-previous-line 1))

;;; Typically bound to [C-down-mouse-1] to give behavior similar to Windows Explorer.
;;;###autoload
(defun dired-mouse-mark/unmark (event)
  "Mark/unmark file or directory at mouse EVENT."
  (interactive "e")
  (let* ((mouse-pos (event-start event))
         bol eol
         (file/dir-name
          (save-excursion
            (set-buffer (window-buffer (posn-window mouse-pos)))
            (save-excursion
              (goto-char (posn-point mouse-pos))
              (save-excursion
                (setq bol (progn (beginning-of-line) (point)))
                (setq eol (progn (end-of-line) (point))))
              (and (not (eobp)) (dired-get-filename nil t))))))
    ;; Return nil iff not on a file or directory name.
    (and file/dir-name (cond ((dired-file-marker file/dir-name)
                              (dired-mouse-unmark event)
                              (message "Unmarked: %s" file/dir-name))
                             (t
                              (dired-mouse-mark event)
                              (message "Marked: %s" file/dir-name))))))

;;;###autoload
(defun dired-mouse-flag-file-deletion (event)
  "In dired, flag this file for deletion.
If on a subdir headerline, mark all its files except `.' and `..'."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (let ((dired-marker-char dired-del-marker)) (dired-mark 1))
  (dired-previous-line 1))

;;;###autoload
(defun dired-mouse-do-copy (event)
  "In dired, copy this file.
This normally preserves the last-modified date when copying."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-create-files 'copy (function dired-copy-file)
                         (if dired-copy-preserve-time "Copy [-p]" "Copy")
                         1 dired-keep-marker-copy))

;;;###autoload
(defun dired-mouse-do-rename (event)
  "In dired, rename this file."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-create-files 'move (function dired-rename-file)
                         "Move" 1 dired-keep-marker-rename "Rename"))

;;;###autoload
(defun dired-mouse-upcase (event)
  "In dired, rename this file to upper case."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-rename-non-directory (function upcase) "Rename to uppercase:" nil))

;;;###autoload
(defun dired-mouse-downcase (event)
  "In dired, rename this file to lower case."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-rename-non-directory (function downcase) "Rename to lowercase:" nil))

;;;###autoload
(defun dired-mouse-do-delete (event)
  "In dired, delete this file, upon confirmation."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-internal-do-deletions (dired-map-over-marks (cons (dired-get-filename)
                                                           (point)) 1)
                               1)
  (dired-previous-line 1))

;;;###autoload
(defun dired-mouse-do-shell-command (event)
  "Run a shell COMMAND on this file.
If there is output, it goes to a separate buffer.

No automatic redisplay of dired buffers is attempted, as there's no
telling what files the command may have changed.  Type
\\[dired-do-redisplay] to redisplay.

The shell command has the top level directory as working directory, so
output files usually are created there instead of in a subdir."
;;Functions dired-run-shell-command and dired-shell-stuff-it do the
;;actual work and can be redefined for customization.
  (interactive "e")
  (let ((mouse-pos (event-start event))
        (command   (dired-read-shell-command "! on %s: " nil
                                             (dired-get-marked-files t nil))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-bunch-files (- 10000 (length command))
                       (function (lambda (&rest files)
                                   (dired-run-shell-command
                                    (dired-shell-stuff-it command files t 1))))
                       nil
                       (dired-get-marked-files t 1))))

;;;###autoload
(defun dired-mouse-do-symlink (event)
  "Make symbolic link to this file."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-create-files 'symlink (function make-symbolic-link)
                         "Symlink" 1 dired-keep-marker-symlink))

;;;###autoload
(defun dired-mouse-do-hardlink (event)
  "Make hard link (alias) to this file."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-create-files 'hardlink (function add-name-to-file)
                         "Hardlink" 1 dired-keep-marker-hardlink))
  
;;;###autoload
(defun dired-mouse-do-print (event)
  "Print this file.
Uses the shell command coming from variables `lpr-command' and
`lpr-switches' as default."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (let* ((file (dired-get-filename))
         (command (dired-mark-read-string
                   "Print %s with: "
                   (apply 'concat lpr-command " " lpr-switches)
                   'print 1 (list file))))
    (dired-run-shell-command (dired-shell-stuff-it command (list file) nil))))

;;;###autoload
(defun dired-mouse-do-compress (event)
  "Compress or uncompress this file."
  (interactive "e")
  (let ((mouse-pos (event-start event))
        (dired-no-confirm t))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check (function dired-compress) 1 'compress t))
  (dired-previous-line 1))

;;;###autoload
(defun dired-mouse-do-byte-compile (event)
  "Byte compile this file."
  (interactive "e")
  (let ((mouse-pos (event-start event))
        (dired-no-confirm t))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check (function dired-byte-compile) 1 'byte-compile t))
  (dired-previous-line 1))

;;;###autoload
(defun dired-mouse-do-load (event)
  "Load this Emacs Lisp file."
  (interactive "e")
  (let ((mouse-pos (event-start event))
        (dired-no-confirm t))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check (function dired-load) 1 'load t))
  (dired-previous-line 1))

;;;###autoload
(defun dired-mouse-do-chmod (event)
  "Change the mode of this file.
This calls chmod, so symbolic modes like `g+w' are allowed."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-chxxx "Mode" "chmod" 'chmod 1)
  (dired-previous-line 1))

;;;###autoload
(defun dired-mouse-do-chgrp (event)
  "Change the group of this file."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-chxxx "Group" "chgrp" 'chgrp 1)
  (dired-previous-line 1))

;;;###autoload
(defun dired-mouse-do-chown (event)
  "Change the owner of this file."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-chxxx "Owner" dired-chown-program 'chown 1)
  (dired-previous-line 1))

;;; Typically bound to [S-down-mouse-1] to give behavior similar to Windows Explorer.
;;;###autoload
(defun dired-mouse-mark-region-files (click)
  "Mark files between point and the mouse." 
  (interactive "e")
  (call-interactively 'mouse-save-then-kill)
  (dired-mark-region-files))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired+.el ends here
