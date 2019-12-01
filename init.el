;;; init.el --- yuskam's config  -*- lexical-binding: t; coding:utf-8; fill-column: 119 -*-

;;; Commentary:
;; My personal config. Use `outshine-cycle-buffer' (<S-Tab>) to navigate through sections, and `counsel-imenu' (C-c i)
;; to locate individual use-package definition.


(progn ;startup 
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file))


;; Speed up bootstrapping
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook `(lambda ()
                              (setq gc-cons-threshold 800000
                                    gc-cons-percentage 0.1)
                              (garbage-collect)) t)


;;; Bootstrap `straight.el'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Bootstrap `use-package'
(setq-default use-package-always-defer t ; Always defer load package to speed up startup time
              use-package-verbose nil ; Don't report loading details
              use-package-expand-minimally t  ; make the expanded code as minimal as possible
              use-package-enable-imenu-support t) ; Let imenu finds use-package definitions

;; Integration with use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; Early load Org from Git version instead of Emacs built-in version
;; (straight-use-package 'org-plus-contrib)
(straight-use-package '(org :local-repo nil))

;;;;  package.el
;;; so package-list-packages includes them
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; newer than byte-compiled file issues
(setq load-prefer-newer t)


;; Always follow symlinks. init files are normally stowed/symlinked.
(setq vc-follow-symlinks t
      find-file-visit-truename t)


;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; Symbolic link and folders
(defun my-init-file ()
  "Open my emacs init.el file"
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

;; Emacs configuration, along with many other journals, are synchronized across machines
(setq my-sync-directory "~/Dropbox")
;; Define configuration directory.
(setq my-emacs-conf-directory (expand-file-name "dotemacs/" my-sync-directory)
      my-private-conf-directory (expand-file-name "private/" my-emacs-conf-directory))
;; For packages not available through MELPA, save it locally and put under load-path
(add-to-list 'load-path (expand-file-name "elisp" my-emacs-conf-directory))

;; Setup catch folder to put related files at one place
(defvar my-emacs-cache (concat user-emacs-directory "cache/")
  "Folder to store cache files in. Should end with a forward slash.")

;; Customize to be pc specific if customize.el exist
(setq custom-file (concat my-emacs-cache "customize.el"))
;; (when (load custom-file t)
;;   (load custom-file))
(when (file-exists-p custom-file)
  (load custom-file :noerror))

;;; General setup
(setq-default ;; Use setq-default to define global default
 ;; Don't show scratch message, and use fundamental-mode for *scratch*
 ;; Remove splash screen and the echo area message
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 initial-scratch-message 'nil
 initial-major-mode 'fundamental-mode
 ;; Emacs modes typically provide a standard means to change the
 ;; indentation width -- eg. c-basic-offset: use that to adjust your
 ;; personal indentation width, while maintaining the style (and
 ;; meaning) of any files you load.
 indent-tabs-mode nil ; don't use tabs to indent
 tab-width 8 ; but maintain correct appearance
 ;; Use one space as sentence end
 sentence-end-double-space 'nil
 ;; Newline at end of file
 require-final-newline t
 ;; Don't adjust window-vscroll to view tall lines.
 auto-window-vscroll nil
 ;; Leave some rooms when recentering to top, useful in emacs ipython notebook.
 recenter-positions '(middle 1 bottom)
 ;; Move files to trash when deleting
 delete-by-moving-to-trash t
 ;; Show column number
 column-number-mode t
 ;; More message logs
 message-log-max 16384
 ;; No electric indent
 electric-indent-mode nil
 ;; Place all auto-save files in one directory.
 backup-directory-alist `(("." . ,(concat my-emacs-cache "backups")))
 ;; more useful frame title, that show either a file or a
 ;; buffer name (if the buffer isn't visiting a file)
 frame-title-format '((:eval (if (buffer-file-name)
                                 (abbreviate-file-name (buffer-file-name))
                               "%b")))
 ;; warn when opening files bigger than 100MB
 large-file-warning-threshold 100000000
 ;; Don't create backup files
 make-backup-files nil ; stop creating backup~ files
 ;; Remember my location when the file is last opened
 ;; activate it for all buffers
 save-place-file (expand-file-name "saveplace" my-emacs-cache)
 save-place t
 ;; smooth scrolling
 scroll-conservatively 101
 ;; Reserve one line when scrolling
 scroll-margin 1
 ;; turn off the bell
 ring-bell-function 'ignore
 ;; Smoother scrolling
 mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
 mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
 mouse-wheel-follow-mouse 't ;; scroll window under mouse
 scroll-step 1 ;; keyboard scroll one line at a time
 )

;; Misc
(set-frame-name "ybka:emacs")
(delete-selection-mode 1)
;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
;; Set paste system
;; (set-clipboard-coding-system 'utf-16le-dos)
;; Set paste error under linux
(set-selection-coding-system 'utf-8)
;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)
;; Don't blink
(blink-cursor-mode 0)
;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; ESC is mapped as metakey by default, very counter-intuitive.

;;;; Some functions to be used
(defun suppress-messages (func &rest args)
  "Suppress message output from FUNC."
  ;; Some packages are too noisy.
  ;; https://superuser.com/questions/669701/emacs-disable-some-minibuffer-messages
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
        (apply func args)
      (advice-remove 'message #'silence))))

(defun the-the ()
  ;; https://www.gnu.org/software/emacs/manual/html_node/eintr/the_002dthe.html
  "Search forward for for a duplicated word."
  (interactive)
  (message "Searching for for duplicated words ...")
  (push-mark)
  ;; This regexp is not perfect
  ;; but is fairly good over all:
  (if (re-search-forward
       "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil 'move)
      (message "Found duplicated word.")
    (message "End of buffer")))

(defun sudo-shell-command (command)
  "Run COMMAND as root."
  (interactive "MShell command (root): ")
  (with-temp-buffer
    (cd "/sudo::/")
    (async-shell-command command)))

;;; Keybindings 
;; Personal map
(bind-keys :prefix "<f12>"
           :prefix-map my-personal-map)

(bind-keys :prefix "C-c c"
           :prefix-map my-code-map)


;; Early unbind keys for customization
(unbind-key "C-s") ; Reserve for search related commands
(bind-keys :prefix "C-s"
           :prefix-map my-search-map)

(unbind-key "C-z") ;; Reserve for hydra related commands
(bind-keys :prefix "C-z"
           :prefix-map my-assist-map)


;;; General purpose packages
;; Hide and show the content in this file by pressing S-tab
(use-package outshine
  ;; Easier navigation for source code files
  :straight t
  :defer 3
  :bind (:map outshine-mode-map
              ("<S-<backtab>" . outshine-cycle-buffer)
              ;; ("<backtab>" . outshine-cycle-buffer) ;; For Windows
              )
  :hook (emacs-lisp-mode . outshine-mode)
  :config
  (setq outshine-cycle-emulate-tab t)
  )

(use-package beacon
  ;; Highlight the cursor whenever it scrolls
  :straight t
  :defer 5
  :bind (("C-<f12>" . beacon-blink)) ;; useful when multiple windows
  :config
  (setq beacon-size 10)
  (beacon-mode 1))


(use-package hungry-delete
  :defer 3
  :config
  (global-hungry-delete-mode))

(use-package autorevert
  ;; revert buffers when files on disk change
  :defer 3
  :config
  (setq
   ;; Also auto refresh dired, but be quiet about it
   global-auto-revert-non-file-buffers t
   auto-revert-verbose nil
   ;; Revert pdf without asking
   revert-without-query '("\\.pdf"))
  (global-auto-revert-mode 1) ;; work with auto-save with Org files in Dropbox
  )

;;; Cache related
;;;; Things that use the catche folder
(use-package recentf
  :defer 5
  :config
  (setq recentf-save-file (expand-file-name "recentf" my-emacs-cache)
        recentf-max-saved-items 'nil ;; Save the whole list
        recentf-max-menu-items 50
        ;; Cleanup list if idle for 10 secs
        recentf-auto-cleanup 10)
  ;; save it every 60 minutes
  (run-at-time t (* 60 60) 'recentf-save-list)
  ;; Suppress output "Wrote /home/ybka/.emacs.d/catche/recentf"
  (advice-add 'recentf-save-list :around #'suppress-messages)
  ;; Suppress output "Cleaning up the recentf list...done (0 removed)"
  (advice-add 'recentf-cleanup :around #'suppress-messages)
  (recentf-mode +1)
  )


(use-package aggressive-indent
  ;; Aggressive indent mode
  :hook ((emacs-lisp-mode ess-mode-hook org-src-mode-hook) . aggressive-indent-mode)
  )

(use-package ibuffer
  ;; Better buffer management
  :defer 3
  :straight ibuffer-tramp
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("M-o"     . nil)) ;; unbind ibuffer-visit-buffer-1-window
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-tramp-set-filter-groups-by-tramp-connection)
              (ibuffer-do-sort-by-alphabetic)))
  )

(use-package which-key
  :defer 3
  :config
  (setq which-key-idle-delay 1.0)
  (which-key-mode)
  )

(use-package whole-line-or-region
  ;; If no region is active, C-w M-w will act on current line
  :defer 5
  ;; Right click to paste: I don't use the popup menu a lot.
  :bind ("<mouse-3>" . whole-line-or-region-yank)
  :bind (:map whole-line-or-region-local-mode-map 
              ("C-w" . kill-region-or-backward-word)) ;; Reserve for backward-kill-word
  :init
  (defun kill-region-or-backward-word ()
    "Kill selected region if region is active. Otherwise kill a backward word."
    (interactive)
    (if (region-active-p)
        (kill-region (region-beginning) (region-end))
      (backward-kill-word 1)))
  :config
  (whole-line-or-region-global-mode)
  )

(use-package crux
  ;; A handful of useful functions
  :defer 1
  :bind (
         ("C-x t"         . 'crux-swap-windows)
         ("C-c b"         . 'crux-create-scratch-buffer)
         ("C-x o"         . 'crux-open-with)
         ("C-x f"         . 'crux-recentf-find-file)
         ("C-x 4 t"       . 'crux-transpose-windows)
         ("C-x C-k"       . 'crux-delete-buffer-and-file)
         ("C-c n"         . 'crux-cleanup-buffer-or-region)
         ("s-<return>"    . 'crux-cleanup-buffer-or-region)
         (:map my-assist-map
               ("<backspace>" . crux-kill-line-backwards))
         )
  :init
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  (global-set-key [(shift return)] #'crux-smart-open-line)
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
  :config
  ;; Retain indentation in these modes.
  (add-to-list 'crux-indent-sensitive-modes 'markdown-mode)
  )

(use-package simple
  ;; Improvements over simple editing commands
  :straight nil
  :defer 5
  :hook ((prog-mode) . auto-fill-mode)
  :bind (("<f8>" . (lambda () (interactive) (progn (visual-line-mode)
                                              (follow-mode))))
         ;; M-backspace to backward-delete-word
         ("M-S-<backspace>" . backward-kill-sentence)
         ("M-C-<backspace>" . backward-kill-paragraph)
         ("C-x C-o"         . remove-extra-blank-lines)
         )
  :init
  ;; Move more quickly
  (global-set-key (kbd "C-S-n")
                  (lambda ()
                    (interactive)
                    (ignore-errors (next-line 5))))
  (global-set-key (kbd "C-S-p")
                  (lambda ()
                    (interactive)
                    (ignore-errors (previous-line 5))))
  ;; Show line num temporarily
  (defun goto-line-with-feedback ()
    "Show line numbers temporarily, while prompting for the line number input"
    (interactive)
    (unwind-protect
        (progn
          (linum-mode 1)
          (goto-line (read-number "Goto line: ")))
      (linum-mode -1)))
  (global-set-key [remap goto-line] 'goto-line-with-feedback)

  (defun kill-region-or-backward-word ()
    (interactive)
    (if (region-active-p)
        (kill-region (region-beginning) (region-end))
      (backward-kill-word 1)))
  ;; (global-set-key (kbd "M-h") 'kill-region-or-backward-word)

  (defun remove-extra-blank-lines (&optional beg end)
    "If called with region active, replace multiple blank lines
with a single one.
Otherwise, call `delete-blank-lines'."
    (interactive)
    (if (region-active-p)
        (save-excursion
          (goto-char (region-beginning))
          (while (re-search-forward "^\\([[:blank:]]*\n\\)\\{2,\\}" (region-end) t)
            (replace-match "\n")
            (forward-char 1)))
      (delete-blank-lines)))

  (defun alert-countdown ()
    "Show a message after timer expires. Based on run-at-time and can understand time like it can."
    (interactive)
    (let* ((msg-to-show (read-string "Message to show: "))
           (time-duration (read-string "Time: ")))
      (message time-duration)
      (run-at-time time-duration nil #'alert msg-to-show)))

  ;; Activate `visual-fill-column-mode' in every buffer that uses `visual-line-mode'
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  (setq-default visual-fill-column-width 119
                visual-fill-column-center-text nil)
  )


(use-package expand-region
  ;; Incrementally select a region
  ;; :after org ;; When using straight, er should byte-compiled with the latest Org
  :bind (("C-'" . er/expand-region)
         ("C-M-'" . er/contract-region))
  :config
  (defun org-table-mark-field ()
    "Mark the current table field."
    (interactive)
    ;; Do not try to jump to the beginning of field if the point is already there
    (when (not (looking-back "|[[:blank:]]?"))
      (org-table-beginning-of-field 1))
    (set-mark-command nil)
    (org-table-end-of-field 1))

  (defun er/add-org-mode-expansions ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list (append
                              er/try-expand-list
                              '(org-table-mark-field))))

  (add-hook 'org-mode-hook 'er/add-org-mode-expansions)

  (setq expand-region-fast-keys-enabled nil
        er--show-expansion-message t))


(use-package wrap-region
  ;; Wrap selected region
  :hook ((prog-mode text-mode) . wrap-region-mode)
  :config
  (wrap-region-add-wrappers
   '(
     ("$" "$")
     ("*" "*")
     ("=" "=")
     ("`" "`")
     ("/" "/")
     ("_" "_")
     ("~" "~")
     ("+" "+")
     ("/* " " */" "#" (java-mode javascript-mode css-mode))))
  (add-to-list 'wrap-region-except-modes 'ibuffer-mode)
  (add-to-list 'wrap-region-except-modes 'magit-mode)
  (add-to-list 'wrap-region-except-modes 'magit-todo-mode)
  (add-to-list 'wrap-region-except-modes 'magit-popup-mode)
  )


(use-package change-inner
  :straight t
  :bind (("M-I" . copy-inner)
         ("M-O" . copy-outer)
         ("s-i" . change-inner)
         ("s-o" . change-outer))
  )



;;; Text Editing / Substitution / Copy-Pasting

(use-package multiple-cursors
  ;; Read https://github.com/magnars/multiple-cursors.el for common use cases
  :defer 10
  :commands (mc/mark-next-like-this)
  :bind
  (
   ;; Common use case: er/expand-region, then add curors.
   ("C-}" . mc/mark-next-like-this)
   ("C-{" . mc/mark-previous-like-this)
   ;; After selecting all, we may end up with cursors outside of view
   ;; Use C-' to hide/show unselected lines.
   ("C-*" . mc/mark-all-like-this)
   ;; HOLLLY>>>> Praise Magnars.
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
   ;; highlighting symbols only
   ("C->" . mc/mark-next-word-like-this)
   ("C-<" . mc/mark-previous-word-like-this)
   ("C-M-*" . mc/mark-all-words-like-this)
   ;; Region edit.
   ("C-S-c C-S-c" . mc/edit-lines)
   )
  :config
  (define-key mc/keymap (kbd "<return>") nil)
  ;; ;; specify mc list
  ;; (setq mc/list-file (expand-file-name "mc-list.el" my-private-conf-directory))
  )


(bind-key "C-x u " #'undo)

(use-package undo-tree
  :bind (:map my-assist-map
              ("u" . undo-tree-visualize)
              ("r" . redo))
  :config
  ;; make ctrl-Z redo
  (defalias 'redo 'undo-tree-redo)

  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)

  (defun ybk/undo-tree-enable-save-history ()
    "Enable auto saving of the undo history."
    (interactive)

    (setq undo-tree-auto-save-history t)

    ;; Compress the history files as .gz files
    ;; (advice-add 'undo-tree-make-history-save-file-name :filter-return
    ;;             (lambda (return-val) (concat return-val ".gz")))

    ;; Persistent undo-tree history across emacs sessions
    (setq my-undo-tree-history-dir (let ((dir (concat my-emacs-cache
                                                      "undo-tree-history/")))
                                     (make-directory dir :parents)
                                     dir))
    (setq undo-tree-history-directory-alist `(("." . ,my-undo-tree-history-dir)))

    (add-hook 'write-file-functions #'undo-tree-save-history-hook)
    (add-hook 'find-file-hook #'undo-tree-load-history-hook))

  (defun my-undo-tree-disable-save-history ()
    "Disable auto saving of the undo history."
    (interactive)

    (setq undo-tree-auto-save-history nil)

    (remove-hook 'write-file-functions #'undo-tree-save-history-hook)
    (remove-hook 'find-file-hook #'undo-tree-load-history-hook))

  ;; Aktifkan
  (global-undo-tree-mode 1)

  :diminish (undo-tree-mode . " "))



;;; Completion Framework: Ivy / Swiper / Counsel
(use-package counsel
  ;; specifying counsel will bring ivy and swiper as dependencies
  :demand t
  :straight ivy-hydra
  :straight ivy-rich
  :straight counsel-projectile
  :straight ivy-posframe
  :straight smex
  :bind (("M-s"     . swiper)
         ("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume)
         ("C-h V"   . counsel-set-variable)
         ("C-s l"   . counsel-find-library)
         ("C-s u"   . counsel-unicode-char)
         ("C-c j"   . counsel-git-grep)
         ("C-s p"   . counsel-git-grep)
         ("C-c i"   . counsel-imenu)
         ("C-x l"   . counsel-locate)
         ("C-x C-r" . counsel-recentf)
         ;; Search-replace with ag and rg:
         ;; C-u prefix to choose search directory
         ;; C-c C-o opens an occur buffer
         ;; e to toggle writable state
         ("C-s C-s" . counsel-ag)
         ("C-s r"   . counsel-rg)
         ("C-s f"   . counsel-file-jump) ;; Jump to a file below the current directory.
         ("C-s j"   . counsel-dired-jump);; Jump to directory under current directory
         )
  :init
  (setq ivy-rich--display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 50))  ; return the candidate itself
            (ivy-rich-switch-buffer-size (:width 7))  ; return the buffer size
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
            (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))  ; thr original transformer
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable
          counsel-recentf
          (:columns
           ((ivy-rich-candidate (:width 0.8)) ; return the candidate itself
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))))) ; return the last modified time of the file
  :config
  (ivy-mode 1)
  (ivy-rich-mode 1)
  (counsel-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (counsel-projectile-mode 1)
  (setq smex-save-file (expand-file-name "smex-items" my-private-conf-directory))
  (setq ivy-height 10
        ivy-fixed-height-minibuffer t
        ivy-use-virtual-buffers t ;; show recent files as buffers in C-x b
        ivy-use-selectable-prompt t ;; C-M-j to rename similar filenames
        enable-recursive-minibuffers t
        ivy-re-builders-alist '((t . ivy--regex-plus))
        ivy-count-format "(%d/%d) "
        ;; Useful settings for long action lists
        ;; See https://github.com/tmalsburg/helm-bibtex/issues/275#issuecomment-452572909
        max-mini-window-height 0.30
        ;; Don't parse remote files
        ivy-rich-parse-remote-buffer 'nil
        )
  (defvar dired-compress-files-alist
    '(("\\.tar\\.gz\\'" . "tar -c %i | gzip -c9 > %o")
      ("\\.zip\\'" . "zip %o -r --filesync %i"))
    "Control the compression shell command for `dired-do-compress-to'.
Each element is (REGEXP . CMD), where REGEXP is the name of the
archive to which you want to compress, and CMD the the
corresponding command.
Within CMD, %i denotes the input file(s), and %o denotes the
output file. %i path(s) are relative, while %o is absolute.")

  ;; Offer to create parent directories if they do not exist
  ;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
  (defun my-create-non-existent-directory ()
    (let ((parent-directory (file-name-directory buffer-file-name)))
      (when (and (not (file-exists-p parent-directory))
                 (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
        (make-directory parent-directory t))))
  (add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

  ;; Kill virtual buffer too
  ;; https://emacs.stackexchange.com/questions/36836/how-to-remove-files-from-recentf-ivy-virtual-buffers
  (defun my-ivy-kill-buffer (buf)
    (interactive)
    (if (get-buffer buf)
        (kill-buffer buf)
      (setq recentf-list (delete (cdr (assoc buf ivy--virtual-buffers)) recentf-list))))

  (ivy-set-actions 'ivy-switch-buffer
                   '(("k" (lambda (x)
                            (my-ivy-kill-buffer x)
                            (ivy--reset-state ivy-last))  "kill")
                     ("j" switch-to-buffer-other-window "other window")
                     ("x" browse-file-directory "open externally")
                     ))

  (ivy-set-actions 'counsel-find-file
                   '(("j" find-file-other-window "other window")
                     ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
                     ("x" counsel-find-file-extern "open externally")
                     ("k" delete-file "delete")
                     ("g" magit-status-internal "magit status")
                     ("r" counsel-find-file-as-root "open as root")))
  ;; display at `ivy-posframe-style'
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-point)))
  ;; (ivy-posframe-mode 1)
  )


;;; Version-control

(use-package magit
  :defer 10
  ;;:straight gitignore-templates
  :straight diff-hl
  :straight git-timemachine
  ;;display flycheck errors only on added/modified lines
  :straight magit-todos
  :straight ediff
  :straight magit-diff-flycheck
  :bind (:map vc-prefix-map
              ("s" . 'git-gutter:stage-hunk)
              ("c" . 'magit-clone))
  :bind (("C-x v r" . 'diff-hl-revert-hunk)
         ("C-x v n" . 'diff-hl-next-hunk)
         ("C-x v p" . 'diff-hl-previous-hunk))
  :bind (("C-x M-g" . 'magit-dispatch-popup)
         ("C-x g" . magit-status)
         ("C-x G" . magit-dispatch))
  :config
  ;; Enable magit-file-mode, to enable operations that touches a file, such as log, blame
  (global-magit-file-mode)

  ;; Prettier looks, and provides dired diffs
  (use-package diff-hl
    :defer 3
    :commands (diff-hl-mode diff-hl-dired-mode)
    :hook (magit-post-refresh . diff-hl-magit-post-refresh)
    :hook (dired-mode . diff-hl-dired-mode)
    )

  ;; Provides stage hunk at buffer, more useful
  (use-package git-gutter
    :defer 3
    :commands (git-gutter:stage-hunk)
    :bind (:map vc-prefix-map
                ("s" . 'git-gutter:stage-hunk))
    )

  ;; Someone says this will make magit on Windows faster.
  (setq w32-pipe-read-delay 0)

  (set-default 'magit-push-always-verify nil)
  (set-default 'magit-revert-buffers 'silent)
  (set-default 'magit-no-confirm '(stage-all-changes
                                   unstage-all-changes))
  (set-default 'magit-diff-refine-hunk t)
  ;; change default display behavior
  (setq magit-completing-read-function 'ivy-completing-read
        magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
        magit-clone-set-remote.pushDefault nil
        magit-clone-default-directory "~/projects/")

  (defun magit-status-with-prefix ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'magit-status)))

  ;; Set magit password authentication source to auth-source
  (add-to-list 'magit-process-find-password-functions
               'magit-process-password-auth-source)

  ;; Useful functions copied from
  ;; https://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version/29757750#29757750
  ;; Combined with ~ to swap the order of the buffers you can get A then B or B then A
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

  ;; Always expand file in ediff
  (add-hook 'ediff-prepare-buffer-hook #'show-all)
  ;; Do everything in one frame
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  )

;;; Window and Buffer management
(use-package windmove
  :straight nil
  :bind (
         ("s-j" . windmove-down)
         ("s-k" . windmove-up)
         ("s-h" . windmove-left)
         ("s-l" . windmove-right)
         ("C-x <down>" . windmove-down)
         ("C-x <up>" . windmove-up)
         ("C-x <left>" . windmove-left)
         ("C-x <right>" . windmove-right)
         )
  )

(use-package winum
  ;; Select windows with Meta key
  :straight t
  :defer 1
  :init
  (setq winum-keymap
        (let ((map (make-sparse-keymap)))
          ;; (define-key map (kbd "<f2> w") 'winum-select-window-by-number)
          (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
          (define-key map (kbd "M-1") 'winum-select-window-1)
          (define-key map (kbd "M-2") 'winum-select-window-2)
          (define-key map (kbd "M-3") 'winum-select-window-3)
          (define-key map (kbd "M-4") 'winum-select-window-4)
          (define-key map (kbd "M-5") 'winum-select-window-5)
          (define-key map (kbd "M-6") 'winum-select-window-6)
          (define-key map (kbd "M-7") 'winum-select-window-7)
          (define-key map (kbd "M-8") 'winum-select-window-8)
          map))
  :config
  (winum-mode))

(use-package window
  ;; Handier movement over default window.el
  :straight nil
  :bind (
         ("C-x 2"             . split-window-below-and-move-there)
         ("C-x 3"             . split-window-right-and-move-there)
         ("C-x \\"            . toggle-window-split)
         ("C-0"               . delete-window)
         ("C-1"               . delete-other-windows)
         ("C-2"               . split-window-below-and-move-there)
         ("C-3"               . split-window-right-and-move-there)
         ("M-o"               . 'other-window)
         ("M-O"               . (lambda () (interactive) (other-window -1))) ;; Cycle backward
         ("M-<tab>"           . 'other-frame)
         ("<M-S-iso-lefttab>" . (lambda () (interactive) (other-frame -1))) ;; Cycle backwards
         )
  :init
  ;; Functions for easier navigation
  (defun split-window-below-and-move-there ()
    (interactive)
    (split-window-below)
    (windmove-down))

  (defun split-window-right-and-move-there ()
    (interactive)
    (split-window-right)
    (windmove-right))

  (defun toggle-window-split ()
    "When there are two windows, toggle between vertical and
horizontal mode."
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))
  )

(use-package ace-window
  :defer 3
  :bind (("<C-return>" . ace-window))
  :custom-face (aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
  :config
  (setq
   ;; Home row is more convenient. Use home row keys that prioritize fingers that don't move.
   aw-keys '(?j ?k ?l ?f ?d ?s ?g ?h ?\; ?a)
   aw-scope 'visible)
  )

(use-package winner
  ;; Enable window restoration
  :defer 1
  :config
  (winner-mode 1))

(use-package nswbuff
  ;; Quickly switching buffers. Quite useful!
  :bind (("<C-tab>"           . nswbuff-switch-to-next-buffer)
         ("<C-S-iso-lefttab>" . nswbuff-switch-to-previous-buffer))
  :config
  (setq nswbuff-display-intermediate-buffers t)
  )


(use-package golden-ratio
  ;; Resize windows with ratio https://github.com/roman/golden-ratio.el
  :straight t
  :defer 5
  :bind* (:map my-assist-map
               ("g" . golden-ratio-mode))
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode 1)
  (setq golden-ratio-auto-scale t))


(use-package transpose-frame
  :straight t
  :defer 5
  :commands (transpose-frame))
;;Transpose utk perkataan guna M-t
(bind-keys :prefix "C-t"
           :prefix-map transpose-map
           ("f" . transpose-frame)
           ("c" . transpose-chars)
           ("w" . transpose-words)
           ("l" . transpose-lines)
           ("p" . transpose-paragraphs)
           ("s" . transpose-sentences)
           ("x" . transpose-sexps))


;;; Navigation
;;;; Register
(use-package register
  :straight nil
  :bind* (:map my-assist-map
               ("<SPC>" . point-to-register)
               ("j" . jump-to-register)))

;;;; Avy

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(use-package avy
  :bind  (("C-,"   . avy-goto-char-2)
          ("C-M-," . avy-goto-line))
  :commands (avy-with)
  :config
  (setq avy-timeout-seconds 0.3
        avy-all-windows 'all-frames
        avy-style 'at-full)
  )

(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))


;;; Workspace Mgmt: eyebrowse + projectile

(use-package projectile
  :defer 5
  :straight ripgrep ;; required by projectile-ripgrep
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind (("C-c o" . 'projectile-find-file))
  :config
  ;; Where my projects and clones are normally placed.
  (setq projectile-project-search-path '("~/projects")
        projectile-completion-system 'ivy)
  (projectile-mode +1)
  )

(use-package eyebrowse
  :defer 2
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c w")) ;; w for workspace
  :bind
  (
   ;; ("<f9>"      . 'eyebrowse-last-window-config)
   ;; ("<f10>"     . 'eyebrowse-prev-window-config)
   ;; ("<f11>"     . 'eyebrowse-switch-to-window-config)
   ;; ("<f12>"     . 'eyebrowse-next-window-config)
   ("C-c w s"   . 'eyebrowse-switch-to-window-config)
   ("C-c w k"   . 'eyebrowse-close-window-config)
   ("C-c w w"   . 'eyebrowse-last-window-config)
   ("C-c w n"   . 'eyebrowse-next-window-config)
   ("C-c w p"   . 'eyebrowse-prev-window-config))
  :config
  (setq eyebrowse-wrap-around t
        eyebrowse-close-window-config-prompt t
        eyebrowse-mode-line-style 'smart
        eyebrowse-tagged-slot-format "%t"
        eyebrowse-new-workspace t)
  (eyebrowse-mode)
  )

;;; Programming

;; General conventions on keybindings:
;; Use C-c C-z to switch to inferior process
;; Use C-c C-c to execute current paragraph of code

;;;; General settings: prog-mode, whitespaces, symbol-prettifying, highlighting
(use-package prog-mode
  ;; Generic major mode for programming
  :straight rainbow-delimiters
  :defer 5
  :hook (org-mode . prettify-symbols-mode)
  :hook (prog-mode . rainbow-delimiters-mode) ; Prettify parenthesis
  :hook (prog-mode . show-paren-mode)
  :init
  ;; Default to 80 fill-column
  (setq-default fill-column 80)
  ;; Prettify symbols
  (setq-default prettify-symbols-alist
                '(("#+BEGIN_SRC"     . "λ")
                  ("#+END_SRC"       . "λ")
                  ("#+RESULTS"       . ">")
                  ("#+BEGIN_EXAMPLE" . "¶")
                  ("#+END_EXAMPLE"   . "¶")
                  ("#+BEGIN_QUOTE"   . "『")
                  ("#+END_QUOTE"     . "』")
                  ("#+begin_src"     . "λ")
                  ("#+end_src"       . "λ")
                  ("#+results"       . ">")
                  ("#+begin_example" . "¶")
                  ("#+end_example"   . "¶")
                  ("#+begin_quote"   . "『")
                  ("#+end_quote"     . "』")
                  ))
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  :config
  (global-prettify-symbols-mode +1) ;; This only applies to prog-mode derived modes.
  )


(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :commands (whitespace-buffer
             whitespace-cleanup
             whitespace-mode
             whitespace-turn-off)
  :preface
  (defvar normalize-hook nil)

  (defun normalize-file ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (whitespace-cleanup)
      (run-hook-with-args normalize-hook)
      (delete-trailing-whitespace)
      (goto-char (point-max))
      (delete-blank-lines)
      (set-buffer-file-coding-system 'unix)
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
        (replace-match ""))
      (set-buffer-file-coding-system 'utf-8)
      (let ((require-final-newline t))
        (save-buffer))))

  (defun maybe-turn-on-whitespace ()
    "depending on the file, maybe clean up whitespace."
    (when (and (not (or (memq major-mode '(markdown-mode))
                        (and buffer-file-name
                             (string-match "\\(\\.texi\\|COMMIT_EDITMSG\\)\\'"
                                           buffer-file-name))))
               (locate-dominating-file default-directory ".clean")
               (not (locate-dominating-file default-directory ".noclean")))
      (whitespace-mode 1)
      ;; For some reason, having these in settings.el gets ignored if
      ;; whitespace loads lazily.
      (setq whitespace-auto-cleanup t
            whitespace-line-column 80
            whitespace-rescan-timer-time nil
            whitespace-silent t
            whitespace-style '(face trailing lines space-before-tab empty))
      (add-hook 'write-contents-hooks
                #'(lambda () (ignore (whitespace-cleanup))) nil t)
      (whitespace-cleanup)))

  :init
  (add-hook 'find-file-hooks 'maybe-turn-on-whitespace t)
  :config
  (remove-hook 'find-file-hooks 'whitespace-buffer)
  (remove-hook 'kill-buffer-hook 'whitespace-buffer))

(use-package whitespace-cleanup-mode
  ;; Automatically cleanup whitespace
  :defer 3
  :config
  (add-to-list 'whitespace-cleanup-mode-ignore-modes 'python-mode)
  (global-whitespace-cleanup-mode))


;; Check the great gist at
;; https://gist.github.com/pvik/8eb5755cc34da0226e3fc23a320a3c95
;; And this tutorial: https://ebzzry.io/en/emacs-pairs/
(use-package smartparens
  :defer 5
  :bind (:map smartparens-mode-map
              ("M-("           . sp-wrap-round)
              ("M-["           . sp-wrap-square)
              ("M-{"           . sp-wrap-curly)
              ("M-<backspace>" . sp-backward-unwrap-sexp)
              ("M-<del>"       . sp-unwrap-sexp)
              ("C-<right>"     . sp-forward-slurp-sexp)
              ("C-<left>"      . sp-backward-slurp-sexp)
              ("C-M-<right>"   . sp-forward-barf-sexp)
              ("C-M-<left>"    . sp-backward-barf-sexp)
              ("C-M-a"         . sp-beginning-of-sexp)
              ("C-M-e"         . sp-end-of-sexp))
  :config
  (require 'smartparens-config)
  (--each '(css-mode-hook
            restclient-mode-hook
            js-mode-hook
            java-mode
            emacs-lisp-mode-hook
            ruby-mode
            ;; org-mode-hook
            org-src-mode-hook
            ess-mode-hook
            inferior-ess-mode-hook
            markdown-mode
            groovy-mode
            scala-mode)
    (add-hook it 'turn-on-smartparens-strict-mode))

  (add-hook 'inferior-ess-mode-hook #'smartparens-mode)
  (add-hook 'LaTeX-mode-hook #'smartparens-mode)
  (add-hook 'markdown-mode-hook #'smartparens-mode)
  )



;;; Commenting

(defun comment-eclipse ()
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))

(global-set-key (kbd "M-'") 'comment-eclipse)


;;;; Auto-completion with Company

(use-package company
  :defer 3
  :straight company-quickhelp ; Show short documentation at point
  :straight company-shell
  :bind (
         :map company-active-map
         ("C-c ?" . company-quickhelp-manual-begin)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-d" . company-show-doc-buffer)
         ("<tab>" . company-complete)
         ("C-i" . company-complete-selection)
         )
  :config
  (global-company-mode t)

  (setq company-show-numbers t
        ;; invert the navigation direction if the the completion
        ;; popup-isearch-match is displayed on top (happens near the bottom of
        ;; windows)
        company-tooltip-flip-when-above t)

  ;; Directly press [1..9] to insert candidates
  ;; See http://oremacs.com/2017/12/27/company-numbers/
  (defun ora-company-number ()
    "Forward to `company-complete-number'.
Unless the number is potentially part of the candidate.
In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (or (cl-find-if (lambda (s) (string-match re s))
                          company-candidates)
              (> (string-to-number k)
                 (length company-candidates)))
          (self-insert-command 1)
        (company-complete-number
         (if (equal k "0")
             10
           (string-to-number k))))))

  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x) 'ora-company-number))
          (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1)))
    (define-key map (kbd "<return>") nil))

  ;; company-shell
  (add-to-list 'company-backends 'company-shell)

  ;; aktifkan di org-mode selepas pastikan company-capf di company-backends
  ;; https://github.com/company-mode/company-mode/issues/50
  (defun add-pcomplete-to-capf ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
  (add-hook 'org-mode-hook #'add-pcomplete-to-capf)

  (setq company-tooltip-align-annotations t   ; align
        company-tooltip-limit 6               ; list to show
        company-tooltip-flip-when-above t
        company-show-numbers t                ; Easy navigation to candidates with M-<n>
        company-idle-delay .2                 ; delay before autocomplete popup
        company-minimum-prefix-length 4       ; 4 prefix sebelum tunjukkan cadangan (default)
        company-abort-manual-when-too-short t ; tanpa company sekiranya prefix pendek dari 'minimum-prefix-length'
        )
  )


;;;; Heuristic text completion: hippie expand + dabbrev
(use-package hippie-exp
  :straight nil
  :defer 3
  :bind (("M-/"   . hippie-expand-no-case-fold)
         ("C-M-/" . dabbrev-completion))
  :config
  ;; Activate globally
  ;; (global-set-key (kbd "") 'hippie-expand)
  
  ;; Don't case-fold when expanding with hippe
  (defun hippie-expand-no-case-fold ()
    (interactive)
    (let ((case-fold-search nil))
      (hippie-expand nil)))

  ;; hippie expand is dabbrev expand on steroids
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))

(use-package abbrev
  :straight nil
  :defer 5
  :hook ((text-mode prog-mode erc-mode LaTeX-mode) . abbrev-mode)
  :init
  (setq save-abbrevs 'silently)
  :config
  (setq-default abbrev-file-name (expand-file-name "abbrev_defs" my-private-conf-directory))
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))


;;; Terminal
;;;; Eshell

;; Emacs command shell
(use-package eshell
  :straight nil
  :defines eshell-prompt-function
  :functions eshell/alias
  :bind (:map my-assist-map
              ("x" . eshell))
  :hook (eshell-mode . (lambda ()
                         (bind-key "C-l" 'eshell/clear eshell-mode-map)
                         (eshell/alias "f" "find-file $1")
                         (eshell/alias "fo" "find-file-other-window $1")
                         (eshell/alias "d" "dired $1")
                         (eshell/alias "ll" "ls -l")
                         (eshell/alias "la" "ls -al")
                         (eshell/alias "gw" "cd ~/Git-work")
                         (eshell/alias "gp" "cd ~/Git-personal")))
  :config
  (setq eshell-list-files-after-cd t) ;ls after cd

  (with-no-warnings
    (unless (fboundp #'flatten-tree)
      (defalias #'flatten-tree #'eshell-flatten-list))

    (defun eshell/clear ()
      "Clear the eshell buffer."
      (interactive)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (eshell-send-input)))

    (defun eshell/emacs (&rest args)
      "Open a file (ARGS) in Emacs.  Some habits die hard."
      (if (null args)
          ;; If I just ran "emacs", I probably expect to be launching
          ;; Emacs, which is rather silly since I'm already in Emacs.
          ;; So just pretend to do what I ask.
          (bury-buffer)
        ;; We have to expand the file names or else naming a directory in an
        ;; argument causes later arguments to be looked for in that directory,
        ;; not the starting directory
        (mapc #'find-file (mapcar #'expand-file-name (flatten-tree (reverse args))))))

    (defalias 'eshell/e 'eshell/emacs)

    (defun eshell/ec (&rest args)
      "Compile a file (ARGS) in Emacs.  Use `compile' to do background make."
      (if (eshell-interactive-output-p)
          (let ((compilation-process-setup-function
                 (list 'lambda nil
                       (list 'setq 'process-environment
                             (list 'quote (eshell-copy-environment))))))
            (compile (eshell-flatten-and-stringify args))
            (pop-to-buffer compilation-last-buffer))
        (throw 'eshell-replace-command
               (let ((l (eshell-stringify-list (flatten-tree args))))
                 (eshell-parse-command (car l) (cdr l))))))
    (put 'eshell/ec 'eshell-no-numeric-conversions t)

    (defun eshell-view-file (file)
      "View FILE.  A version of `view-file' which properly rets the eshell prompt."
      (interactive "fView file: ")
      (unless (file-exists-p file) (error "%s does not exist" file))
      (let ((buffer (find-file-noselect file)))
        (if (eq (get (buffer-local-value 'major-mode buffer) 'mode-class)
                'special)
            (progn
              (switch-to-buffer buffer)
              (message "Not using View mode because the major mode is special"))
          (let ((undo-window (list (window-buffer) (window-start)
                                   (+ (window-point)
                                      (length (funcall eshell-prompt-function))))))
            (switch-to-buffer buffer)
            (view-mode-enter (cons (selected-window) (cons nil undo-window))
                             'kill-buffer)))))

    (defun eshell/less (&rest args)
      "Invoke `view-file' on a file (ARGS).  \"less +42 foo\" will go to line 42 in the buffer for foo."
      (while args
        (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
            (let* ((line (string-to-number (match-string 1 (pop args))))
                   (file (pop args)))
              (eshell-view-file file)
              (forward-line line))
          (eshell-view-file (pop args)))))

    (defalias 'eshell/more 'eshell/less))

  ;;  Display extra information for prompt
  (use-package eshell-prompt-extras
    :after esh-opt
    :defines eshell-highlight-prompt
    :commands (epe-theme-lambda epe-theme-dakrone epe-theme-pipeline)
    :init (setq eshell-highlight-prompt nil
                eshell-prompt-function 'epe-theme-lambda))

  (use-package esh-autosuggest
    ;; Fish-like history autosuggestions https://github.com/dieggsy/esh-autosuggest
    ;; C-f select suggestion and M-f select next word in suggestion
    :defines ivy-display-functions-alist
    :preface
    (defun setup-eshell-ivy-completion ()
      (setq-local ivy-display-functions-alist
                  (remq (assoc 'ivy-completion-in-region ivy-display-functions-alist)
                        ivy-display-functions-alist)))
    :bind (:map eshell-mode-map
                ([remap eshell-pcomplete] . completion-at-point))
    :hook ((eshell-mode . esh-autosuggest-mode)
           (eshell-mode . setup-eshell-ivy-completion)))

  ;; Eldoc support
  (use-package esh-help
    :init (setup-esh-help-eldoc))

  ;; `cd' to frequent directory in eshell
  (use-package eshell-z
    :hook (eshell-mode
           .
           (lambda () (require 'eshell-z)))))


(use-package eshell-git-prompt
  ;; show git status and branch
  :straight t
  :config
  (eshell-git-prompt-use-theme 'powerline)
  )

;; ;; Disable company-mode for eshell, falling back to pcomplete,
;; ;; which feels more natural for a shell.
;; (add-hook 'eshell-mode-hook
;;           (lambda ()
;;             (company-mode 0)))

;; ;; When completing with multiple options, complete only as much as
;; ;; possible and wait for further input.
;; (setq eshell-cmpl-cycle-completions nil)

;;;; Shell
(use-package shell
  :straight nil
  :commands comint-send-string comint-simple-send comint-strip-ctrl-m
  :hook ((shell-mode . ansi-color-for-comint-mode-on)
         (shell-mode . n-shell-mode-hook)
         (comint-output-filter-functions . comint-strip-ctrl-m))
  :bind (:map shell-mode-map
              ([tab] . company-manual-begin))
  :init
  (setq system-uses-terminfo nil)

  ;; File path clickable
  (add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

  ;; Make URL clikable
  (add-hook 'shell-mode-hook (lambda () (goto-address-mode )))

  ;; Include company
  (add-hook 'shell-mode-hook #'company-mode)

  (defun n-shell-simple-send (proc command)
    "Various PROC COMMANDs pre-processing before sending to shell."
    (cond
     ;; Checking for clear command and execute it.
     ((string-match "^[ \t]*clear[ \t]*$" command)
      (comint-send-string proc "\n")
      (erase-buffer))
     ;; Checking for man command and execute it.
     ((string-match "^[ \t]*man[ \t]*" command)
      (comint-send-string proc "\n")
      (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
      (setq command (replace-regexp-in-string "[ \t]+$" "" command))
      ;;(message (format "command %s command" command))
      (funcall 'man command))
     ;; Send other commands to the default handler.
     (t (comint-simple-send proc command))))

  (defun n-shell-mode-hook ()
    "Shell mode customizations."
    (local-set-key '[up] 'comint-previous-input)
    (local-set-key '[down] 'comint-next-input)
    (local-set-key '[right] 'comint-next-matching-input-from-input)
    (setq comint-input-sender 'n-shell-simple-send)))

;; ANSI & XTERM 256 color support
(use-package xterm-color
  :defines (compilation-environment
            eshell-preoutput-filter-functions
            eshell-output-filter-functions)
  :functions (compilation-filter my-advice-compilation-filter)
  :init
  ;; For shell
  (setenv "TERM" "xterm-256color")
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'shell-mode-hook
            (lambda ()
              ;; Disable font-locking in this buffer to improve performance
              (font-lock-mode -1)
              ;; Prevent font-locking from being re-enabled in this buffer
              (make-local-variable 'font-lock-function)
              (setq font-lock-function (lambda (_) nil))
              (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

  ;; For eshell
  (with-eval-after-load 'esh-mode
    (add-hook 'eshell-before-prompt-hook
              (lambda ()
                (setq xterm-color-preserve-properties t)))
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions
          (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

  ;; For compilation buffers
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my-advice-compilation-filter (f proc string)
    (funcall f proc
             (if (eq major-mode 'rg-mode) ; compatible with `rg'
                 string
               (xterm-color-filter string))))
  (advice-add 'compilation-filter :around #'my-advice-compilation-filter)
  (advice-add 'gud-filter :around #'my-advice-compilation-filter)

  ;; For prolog inferior
  (with-eval-after-load 'prolog
    (add-hook 'prolog-inferior-mode-hook
              (lambda ()
                (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))))

;; Better term
;; @see https://github.com/akermu/emacs-libvterm#installation
(when (and (executable-find "cmake")
           (executable-find "libtool")
           (executable-find "make"))
  (use-package vterm
    :init (defalias #'term #'vterm)))

;; Shell Pop
(use-package shell-pop
  :straight t
  :defer 2
  :bind ([f9] . shell-pop)
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/bash")
  ;; (setq shell-pop-universal-key "C-t") ;use for eshell keybind

  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
  )



;;; Code folding

(use-package hideshow
  :bind (:map prog-mode-map
              ("C-c h" . hs-toggle-hiding)))

(use-package origami
  ;; Code folding
  :defer 3
  :after hydra
  :bind(
        ;; ("C-c f" . 'origami-toggle-node)
        ("C-z o" . hydra-origami/body)
        )
  :config
  (global-origami-mode)
  (defhydra hydra-origami (:color red)
    "
        _o_pen node    _n_ext fold       toggle _f_orward
        _c_lose node   _p_revious fold   toggle _a_ll
        "
    ("o" origami-open-node)
    ("c" origami-close-node)
    ("n" origami-next-fold)
    ("p" origami-previous-fold)
    ("f" origami-forward-toggle-node)
    ("a" origami-toggle-all-nodes))
  )

;;; Documentation

(use-package eldoc
  ;; Show argument list of function call at echo area
  :hook ((c-mode-common
          emacs-lisp-mode
          lisp-interaction-mode
          eval-expression-minibuffer-setup
          ielm-mode) . eldoc-mode)
  )


;;;; File explorer
(use-package neotree
  :straight t
  :defer 3
  :bind (("<f4>" . neotree-toggle))
  :init
  (progn
    (setq-default neo-smart-open t) ;  every time when the neotree window is
                                        ;  opened, it will try to find current
                                        ;  file and jump to node.
    (setq-default neo-dont-be-alone t) ; Don't allow neotree to be the only open
                                        ; window
    )
  :config
  (progn
    (setq neo-theme 'ascii) ; 'classic, 'nerd, 'ascii, 'arrow

    (setq neo-vc-integration '(face char))

    ;; Patch to fix vc integration
    (defun neo-vc-for-node (node)
      (let* ((backend (vc-backend node))
             (vc-state (when backend (vc-state node backend))))
        ;; (message "%s %s %s" node backend vc-state)
        (cons (cdr (assoc vc-state neo-vc-state-char-alist))
              (cl-case vc-state
                (up-to-date       neo-vc-up-to-date-face)
                (edited           neo-vc-edited-face)
                (needs-update     neo-vc-needs-update-face)
                (needs-merge      neo-vc-needs-merge-face)
                (unlocked-changes neo-vc-unlocked-changes-face)
                (added            neo-vc-added-face)
                (removed          neo-vc-removed-face)
                (conflict         neo-vc-conflict-face)
                (missing          neo-vc-missing-face)
                (ignored          neo-vc-ignored-face)
                (unregistered     neo-vc-unregistered-face)
                (user             neo-vc-user-face)
                (t                neo-vc-default-face)))))

    (defun ybk/neotree-go-up-dir ()
      (interactive)
      (goto-char (point-min))
      (forward-line 2)
      (neotree-change-root))

    ;; http://emacs.stackexchange.com/a/12156/115
    (defun ybk/find-file-next-in-dir (&optional prev)
      "Open the next file in the directory.
    When PREV is non-nil, open the previous file in the directory."
      (interactive "P")
      (let ((neo-init-state (neo-global--window-exists-p)))
        (if (null neo-init-state)
            (neotree-show))
        (neo-global--select-window)
        (if (if prev
                (neotree-previous-line)
              (neotree-next-line))
            (progn
              (neo-buffer--execute nil
                                   (quote neo-open-file)
                                   (lambda (full-path &optional arg)
                                     (message "Reached dir: %s/" full-path)
                                     (if prev
                                         (neotree-next-line)
                                       (neotree-previous-line)))))
          (progn
            (if prev
                (message "You are already on the first file in the directory.")
              (message "You are already on the last file in the directory."))))
        (if (null neo-init-state)
            (neotree-hide))))

    (defun ybk/find-file-prev-in-dir ()
      "Open the next file in the directory."
      (interactive)
      (ybk/find-file-next-in-dir :prev))

    (bind-keys
     :map neotree-mode-map
     ("^"          . ybk/neotree-go-up-dir)
     ("C-c +"      . ybk/find-file-next-in-dir)
     ("C-c -"      . ybk/find-file-prev-in-dir)
     ("<C-return>" . neotree-change-root)
     ("C"          . neotree-change-root)
     ("c"          . neotree-create-node)
     ("+"          . neotree-create-node)
     ("d"          . neotree-delete-node)
     ("r"          . neotree-rename-node)
     ("h"          . neotree-hidden-file-toggle)
     ("f"          . neotree-refresh))))


;; (use-package treemacs
;;   :commands treemacs)

;;; ESS
(use-package ess-site
  :disabled nil
  :straight ess  
  :bind
  (:map ess-mode-map ;ESS
        ("C-c s" . run-shiny-app)
        ("C-c C-g" . ess-describe-object-at-point)
        ("C-a" . crux-move-beginning-of-line)
        ("M--" . ess-insert-assign) ; assign <-
        ("_"   . self-insert-command)
        ("M-+" . my/dt-update)
        ("M-m" . my/add-match)
        ("M-." . ess-switch-process) ;sama dengan C-c C-s
        ("M-p" . my/add-pipe)
        ("C-|" . my/ess-eval-pipe-through-line)
        ("C-M-<return>" . ess-eval-region-or-function-or-paragraph-and-step)
        ("C-M--" . ess-eval-region-or-function-or-paragraph)
        :map inferior-ess-mode-map ;iESS
        ("M--" . ess-insert-assign)
        ("_"   . self-insert-command)
        ("M-+" . my/dt-update)
        ("M-m" . my/add-match)
        ("C-S-<up>" . ess-readline);previous cmd from script
        ;; :map ybk/r-map
        ;; ("." . ess-describe-object-at-point)
        ;; ("d" . ess-dev-map)
        ;; ("r" . ess-r-package-dev-map)
        )
  :init
  ;; Tetapkan Rsetting folder
  (defvar ybk/r-dir "~/Rsetting/") ;definere hvor epost skal være
  ;; lage direktori om ikke allerede finnes
  (unless (file-exists-p ybk/r-dir)
    (make-directory ybk/r-dir t))

  :config
  ;; ess-company
  ;; https://stackoverflow.com/questions/49232454/emacs-ess-how-to-auto-complete-library-function
  (defun my-ess-hook ()
    ;; ensure company-R-library is in ESS backends
    (make-local-variable 'company-backends)
    (cl-delete-if (lambda (x) (and (eq (car-safe x) 'company-R-args))) company-backends)
    (push (list 'company-R-args 'company-R-objects 'company-R-library :separate)
          company-backends))

  (add-hook 'ess-mode-hook 'my-ess-hook)

  ;; (with-eval-after-load 'ess
  ;;   (setq ess-use-company t))
  (setq ess-use-company t)

  ;; Quick help
  (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)

  ;; whitespace
  (setq ess-nuke-trailing-whitespace-p t)

  ;; pakai indentation cara RStudio
  (setq ess-default-style 'RStudio-)
  ;; (add-hook 'ess-mode-hook
  ;;           (lambda ()
  ;;             (ess-set-style 'RStudio)))

  (setq ess-eval-visibly 'nowait) ; don't hog Emacs
  (setq ess-ask-for-ess-directory nil) ; don't ask for dir when starting a process
  (setq ess-eldoc-show-on-symbol t) ; show eldoc on symbol instead of only inside of parens
  (setq ess-use-ido nil) ; rely on ivy instead of ido

  ;; History
  (setq ess-history-directory ybk/r-dir) ;tetapkan folder utk history
  (setq ess-history-file t) ;nil if not saving .Rhistory
  ;; (setq inferior-R-args "--no-restore-history --no-save") ;utk R comman not to restore and save Rhistory
  (setq inferior-R-args "--no-restore-history --vanilla") ; guna vanilla sebagai default utk reproducibility
  (setq ess-pdf-viewer-pref "emacsclient") ; guna pdf-tools

  ;; data.table update
  (defun my/dt-update ()
    "Adds a data.table update."
    (interactive)
    ;;(just-one-space 1) ;delete whitespace around cursor
    (insert " := "))

  ;; Match
  (defun my/add-match ()
    "Adds match."
    (interactive)
    (insert " %in% "))

  ;; pipe
  (defun my/add-pipe ()
    "Adds a pipe operator %>% with one space to the left and then
starts a newline with proper indentation"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (ess-newline-and-indent))

  ;; I sometimes want to evaluate just part of a piped sequence. The
  ;; following lets me do so without needing to insert blank lines or
  ;; something:
  (defun my/ess-beginning-of-pipe-or-end-of-line ()
    "Find point position of end of line or beginning of pipe %>%"
    (if (search-forward "%>%" (line-end-position) t)
        (let ((pos (progn
                     (beginning-of-line)
                     (search-forward "%>%" (line-end-position))
                     (backward-char 3)
                     (point))))
          (goto-char pos))
      (end-of-line)))

  (defun my/ess-eval-pipe-through-line (vis)
    "Like `ess-eval-paragraph' but only evaluates up to the pipe on this line.

If no pipe, evaluate paragraph through the end of current line.

Prefix arg VIS toggles visibility of ess-code as for `ess-eval-region'."
    (interactive "P")
    (save-excursion
      (let ((end (progn
                   (my/ess-beginning-of-pipe-or-end-of-line)
                   (point)))
            (beg (progn (backward-paragraph)
                        (ess-skip-blanks-forward 'multiline)
                        (point))))
        (ess-eval-region beg end vis))))

  ;; Get commands run from script or console
  ;; https://stackoverflow.com/questions/27307757/ess-retrieving-command-history-from-commands-entered-in-essr-inferior-mode-or
  (defun ess-readline ()
    "Move to previous command entered from script *or* R-process and copy
   to prompt for execution or editing"
    (interactive)
    ;; See how many times function was called
    (if (eq last-command 'ess-readline)
        (setq ess-readline-count (1+ ess-readline-count))
      (setq ess-readline-count 1))
    ;; Move to prompt and delete current input
    (comint-goto-process-mark)
    (end-of-buffer nil) ;; tweak here
    (comint-kill-input)
    ;; Copy n'th command in history where n = ess-readline-count
    (comint-previous-prompt ess-readline-count)
    (comint-copy-old-input)
    ;; Below is needed to update counter for sequential calls
    (setq this-command 'ess-readline)
    )

  ;; (global-set-key (kbd "\C-cp") 'ess-readline)
  )

(use-package ess-R-data-view
  ;; Use M-x ess-R-dv-ctable or ess-R-dv-pprint
  :requires ess
  :straight t)

;; (use-package ess-site
;;   :straight ess
;;   :commands (inferior-ess-mode ess-help-mode)
;;   :init
;;   (setq inferior-R-args "--quiet")

;;   :config
;;   (bind-key "C-c C-w" nil inferior-ess-mode-map))

;; (straight-use-package 'ess)
;;(require 'ess-site)
;; (require 'ess-r-mode)
;; (when (not (boundp 'ess-r-mode-hook))
;;   (setq ess-r-mode-hook nil ))
;; (defun emacsmate-turn-on-ess-eldoc ()
;;   (require 'ess-eldoc))
;; (add-hook 'ess-r-mode-hook 'emacsmate-turn-on-ess-eldoc)
;; (autoload 'R-mode "ess-site" "ESS" 't)

;; (add-to-list 'auto-mode-alist '("\\.[rR]\\'" . ess-r-mode))

;; (defun gtrun/add-company-backend-ess ()
;;   (pop company-backends)
;;   (setq-local company-backends
;;               (append '((company-R-args company-R-objects company-R-library
;;                                         company-tabnine))
;;                       company-backends)))

;; (add-hook 'ess-r-mode-hook 'gtrun/add-company-backend-ess)

;; (require 'ess-r-mode)
;; (straight-use-package 'ess)
;; (require 'ess)
;; (straight-use-package 'ess-R-data-view)
;; (require 'ess-R-data-view)



;;; Appearance
;; (use-package naysayer-theme)
;; (load-theme 'naysayer t)

(use-package doom-themes
  :straight t
  :init
  ;; need to load at init for cyclye theme to work
  (load-theme 'doom-one t)
  :bind ("C-9" . cycle-my-theme)
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; utk tukar tema f10-t
  (setq my-themes '(
                    doom-vibrant
                    doom-nord-light
                    doom-dracula
                    doom-oceanic-next
                    doom-Iosvkem ;bold has bigger font
                    ))

  (setq my-cur-theme nil)
  (defun cycle-my-theme ()
    "Cycle through a list of themes, my-themes"
    (interactive)
    (when my-cur-theme
      (disable-theme my-cur-theme)
      (setq my-themes (append my-themes (list my-cur-theme))))
    (setq my-cur-theme (pop my-themes))
    (load-theme my-cur-theme :no-confirm)
    (message "Tema dipakai: %s" my-cur-theme))

  ;; Switch to the first theme in the list above
  (cycle-my-theme)

  )

(use-package solaire-mode
  ;; visually distinguish file-visiting windows from other types of windows (like popups or sidebars) by giving them a
  ;; slightly different -- often brighter -- background
  :defer 3
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-mode-swap-bg)
  (solaire-global-mode +1))


;; Adjust for time display in modeline
(defface egoge-display-time
  '((((type x w32 mac))
     ;; #006655 is the background colour of my default face.
     (:foreground "#006655" :inherit bold))
    (((type tty))
     (:foreground "blue")))
  "Face used to display the time in the mode line.")


;; This causes the current time in the mode line to be displayed in
;; `egoge-display-time-face' to make it stand out visually.
(setq display-time-string-forms
      '((propertize (concat " " 24-hours ":" minutes " ")
 		    'face 'egoge-display-time)))

;; display time
(display-time-mode 1)

;; from https://dev.to/gonsie/beautifying-the-mode-line-3k10
(setq-default mode-line-format
              (list
               ;; day and time
               '(:eval (propertize (format-time-string " %b %d %H:%M ")
                                   'face 'font-lock-builtin-face))


               '(:eval (propertize (substring vc-mode 5)
                                   'face 'font-lock-comment-face))

               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize " %b "
                                   'face
                                   (let ((face (buffer-modified-p)))
                                     (if face 'font-lock-warning-face
                                       'font-lock-type-face))
                                   'help-echo (buffer-file-name)))

               ;; line and column
               " (" ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "%02l" 'face 'font-lock-keyword-face) ","
               (propertize "%02c" 'face 'font-lock-keyword-face)
               ") "

               ;; relative position, size of file
               " ["
               (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
               "/"
               (propertize "%I" 'face 'font-lock-constant-face) ;; size
               "] "

               ;; spaces to align right
               '(:eval (propertize
                        " " 'display
                        `((space :align-to (- (+ right right-fringe right-margin)
                                              ,(+ 3 (string-width mode-name)))))))

               ;; the current major mode
               (propertize " %m " 'face 'font-lock-string-face)
               ;;minor-mode-alist
               ))


(use-package all-the-icons
  ;; needed to display icon correctly in doom-modeline
  :straight t)

(use-package doom-modeline
  ;; Run M-x all-the-icons-install-fonts to install all-the-icons
  :straight t
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (set-face-attribute 'mode-line nil
                      :background "#353644"
                      :foreground "white"
                      :box '(:line-width 6 :color "#353644")
                      :overline nil
                      :underline nil)

  (set-face-attribute 'mode-line-inactive nil
                      :background "#565063"
                      :foreground "white"
                      :box '(:line-width 6 :color "#565063")
                      :overline nil
                      :underline nil)

  )

;; Show hexadecimal color in the background they represent
(use-package rainbow-mode
  :straight t
  :hook
  ((prog-mode
    inferior-ess-mode
    ess-mode text-mode
    markdown-mode
    LaTeX-mode) . rainbow-mode)
  :diminish (rainbow-mode . ""))
