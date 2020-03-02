;;; init.el --- yuskam's config  -*- lexical-binding: t; coding:utf-8; fill-column: 119 -*-

;;; Commentary:
;; My personal config. Use `outshine-cycle-buffer' (<S-Tab>) to navigate through sections, and `counsel-imenu' (C-c i)
;; to locate individual use-package definition.
;; M-x describe-personal-keybindings to see all personally defined keybindings

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
(setq straight-use-package-by-default t) ;always-ensure

;; Early load Org from Git version instead of Emacs built-in version
(straight-use-package 'org-plus-contrib)
;; Use build-in and not save to local disk
;; (straight-use-package '(org :local-repo nil))

;;;;  package.el
;;; so package-list-packages includes them
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("melpa-stable" . "https://stable.melpa.org/packages/")) ;melpa-stable is not in straight.el


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

;;; Personal keybindings
;; Personal map activate as early as possible
(unbind-key "C-p")
(bind-keys :prefix "C-p"
           :prefix-map my-personal-map)

(bind-keys :prefix "C-c o"
           :prefix-map my-org-map)


;; Early unbind keys for customization
(unbind-key "C-s") ; Reserve for search related commands
(bind-keys :prefix "C-s"
           :prefix-map my-search-map)

(unbind-key "C-q") ;; Reserve for assist related commands
(bind-keys :prefix "C-q"
           :prefix-map my-assist-map)

;; Exit Emacs
;; C-x C-c is originally bound to kill emacs. I accidentally type this
;; from time to time which is super-frustrating.  Get rid of it:
(unbind-key "C-x C-c")
(bind-key "0" 'save-buffers-kill-emacs my-personal-map)

;;; Symbolic link and folders
(use-package my-init
  :straight nil
  :bind (:map my-personal-map
              ("y" . my-init-file))
  :init
  (defun my-init-file ()
    "Open my emacs init.el file"
    (interactive)
    (find-file (concat user-emacs-directory "init.el")))
  )

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
 indent-tabs-mode nil ; don't use tabs to indent, use spaces
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
 scroll-preserve-screen-position 'always
 ;; Hide warning redefinition
 ad-redefinition-action 'accept
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
(define-key isearch-mode-map [escape] 'isearch-abort)   ;; isearch
(define-key isearch-mode-map "\e" 'isearch-abort)   ;; \e seems to work better for terminals
(global-set-key [escape] 'keyboard-escape-quit)


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


;;; General purpose packages
;; Keep .emacs.d folder clean and save things in etc and var folders
;; using no-littering-var-directory or no-littering-etc-directory
;; (use-package no-littering
;;   :demand t)


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

;;; Misc

;; don't bind C-x C-z to suspend-frame:
(unbind-key "C-x C-z")
;; if frame freeze then use xkill -frame $emacs

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


(use-package hungry-delete
  :defer 3
  :config
  (global-hungry-delete-mode))


(use-package which-key
  :defer 3
  :config
  (setq which-key-idle-delay 1.0)
  (which-key-mode)
  )


(use-package whole-line-or-region
  ;; If no region is active, C-w and M-w will act on current line
  :defer 5
  ;; Right click to paste: I don't use the popup
  ;; :bind ("<mouse-3>" . whole-line-or-region-
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
         ;; ("C-x f"         . 'crux-recentf-find-file) ;C-s f counsel-recent-file
         ;; ("C-x 4 t"       . 'crux-transpose-windows)
         ("C-x C-k"       . 'crux-delete-buffer-and-file)
         ("C-c n"         . 'crux-cleanup-buffer-or-region)
         (:map my-assist-map
               ("<backspace>" . crux-kill-line-backwards) ;C-S-backspace sp-kill-whole-line
               ;; ("t" . crux-transpose-windows)
               )
         (:map my-personal-map
               ("<return>" . crux-cleanup-buffer-or-region))
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
  ;; resize buffer accordingly
  :bind
  ("<f8>" . (lambda () (interactive) (progn (visual-line-mode)
                                            (follow-mode))))
  ;; M-backspace to backward-delete-word
  ;; C-S-backspace is used by sp-kill-whole-line
  ("M-S-<backspace>" . backward-kill-sentence)
  ("M-C-<backspace>" . backward-kill-paragraph)
  ("C-x C-o"         . remove-extra-blank-lines)
  ("C-z" . undo)
  ;; The -dwim versions of these three commands are new in Emacs 26 and
  ;; better than their non-dwim counterparts, so override those default
  ;; bindings:
  ("M-l" . downcase-dwim)
  ("M-c" . capitalize-dwim)
  ("M-u" . upcase-dwim)
  ;; Super useful for "merging" lines together, overrides the much less
  ;; useful tab-to-tab-stop:
  ("M-i" . delete-indentation)

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

  (use-package visual-fill-column)
  ;; Activate `visual-fill-column-mode' in every buffer that uses `visual-line-mode'
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  (setq-default visual-fill-column-width 119
                visual-fill-column-center-text nil)
  :config

  )



(use-package expand-region
  ;; Incrementally select a region
  ;; :after org ;; When using straight, er should byte-compiled with the latest Org
  :bind (("C-\\" . er/expand-region)
         ("M-\\" . er/contract-region))
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
  :straight t
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


;; (bind-key "C-x u " #'undo)

(use-package undo-tree
  :straight t
  :diminish undo-tree-mode
  :bind ("C-x u" . undo-tree-visualize) ;else C-z bind to undo
  :custom
  (undo-tree-visualize-timestamps t)
  (undo-tree-visualize-diff t)
  :config
  (defalias 'redo 'undo-tree-redo)

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
  (global-undo-tree-mode 1))


;;; Completion
;;;; Auto-completion with Company
(use-package company
  :defer 3
  :straight company-quickhelp ; Show short documentation at point
  :straight company-shell
  ;; :bind* ("C-i" . company-complete) ;activate globally doesn't work in Swiper
  :bind (
         :map company-active-map
         ("C-c ?" . company-quickhelp-manual-begin)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-d" . company-show-doc-buffer)
         ("<tab>" . company-complete)
         ("C-i" . company-complete-common)
         :map my-search-map
         ;; ("c" . company-complete-selection)
         ("c" . company-complete-common)
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


;;;; Text completion
(use-package hippie-exp
  :straight nil
  :defer 3
  :bind (("M-/"   . hippie-expand-no-case-fold)
         ("C-M-/" . dabbrev-completion)
         :map my-assist-map
         ("h" . hippie-expand)
         ([?\t] . dabbrev-completion))
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
  ;;M-x a
  :straight nil
  :defer 5
  :hook ((text-mode prog-mode erc-mode LaTeX-mode) . abbrev-mode)
  :init
  (setq save-abbrevs 'silently)
  :config
  (setq-default abbrev-file-name (expand-file-name "abbrev_defs" my-private-conf-directory))
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))


(use-package pabbrev
  :diminish pabbrev-mode
  :hook ((org-mode
          ess-r-mode
          emacs-lisp-mode
          text-mode). pabbrev-mode)

  :init
  (setq pabbrev-idle-timer-verbose nil
        pabbrev-read-only-error nil
        pabbrev-scavenge-on-large-move nil)
  :bind (:map my-assist-map ("i" . pabbrev-expand-maybe))
  :config
  (put 'yas-expand 'pabbrev-expand-after-command t)

  ;;aktifkan pabbrev
  (global-pabbrev-mode)

  ;; Fix for pabbrev not working in org mode
  ;; http://lists.gnu.org/archive/html/emacs-orgmode/2016-02/msg00311.html
  ;; (define-key pabbrev-mode-map (kbd "C-i") 'pabbrev-expand-maybe)
  ;; (define-key pabbrev-mode-map [tab] 'pabbrev-expand-maybe) ;default

  ;; kill all possible overlay from current view
  (setq pabbrev-debug-erase-all-overlays t)

  ;; ;; hook to text-mode-hook
  ;; (add-hook 'text-mode-hook (lambda () (pabbrev-mode)))

  ;; pretty print a hash
  (setq pabbrev-debug-print-hash t)

  ;;limit suggestions and sort
  (setq pabbrev-suggestions-limit-alpha-sort 5)
  )


(use-package yasnippet
  :straight t
  :init
  ;; Guna snippet sendiri (bukan bundled punya)
  (setq yas-snippet-dirs '("~/Dropbox/snippets"))

  (yas-global-mode 1)
  :mode ("\\.yas" . snippet-mode) ;aktifkan mode bila ada fail dengan .yas
  :bind (:map my-assist-map
              ("y" . yas-ido-expand))
  :config
  ;; ;; Matikan TAB
  ;; (eval-after-load 'yasnippet
  ;;   '(progn
  ;;      (define-key yas-keymap (kbd "TAB") nil)
  ;;      (define-key yas-keymap (kbd "C-<") 'yas-next-field-or-maybe-expand)))

  ;; Jump to end of snippet definition
  (define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)

  ;; Inter-field navigation
  (defun yas/goto-end-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas--snippets-at-point)))
           (position (yas--field-end (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-end-of-line 1)
        (goto-char position))))

  (defun yas/goto-start-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas--snippets-at-point)))
           (position (yas--field-start (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-beginning-of-line 1)
        (goto-char position))))

  (define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
  (define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)

  ;; No dropdowns please, yas
  (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

  ;; No need to be so verbose
  (setq yas-verbosity 1)

  ;; Wrap around region
  (setq yas-wrap-around-region t)

  ;; Completing point by some yasnippet key
  ;; https://www.emacswiki.org/emacs/Yasnippet
  (defun yas-ido-expand ()
    "Lets you select (and expand) a yasnippet key"
    (interactive)
    (let ((original-point (point)))
      (while (and
              (not (= (point) (point-min) ))
              (not
               (string-match "[[:space:]\n]" (char-to-string (char-before)))))
        (backward-word 1))
      (let* ((init-word (point))
             (word (buffer-substring init-word original-point))
             (list (yas-active-keys)))
        (goto-char original-point)
        (let ((key (remove-if-not
                    (lambda (s) (string-match (concat "^" word) s)) list)))
          (if (= (length key) 1)
              (setq key (pop key))
            (setq key (ido-completing-read "key: " list nil nil word)))
          (delete-char (- init-word original-point))
          (insert key)
          (yas-expand)))))

  (define-key yas-minor-mode-map (kbd "C-c <tab>") 'yas-ido-expand)

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
  ;; use M-x v for vc-prefix-map
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
    :config
    (global-diff-hl-mode)
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


(use-package ediff
  ;; Ediff is great, but I have to tell it to use one frame (since I start
  ;; Emacs before X/wayland, it defaults to using two frames).
  :defer t
  :straight t
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  :hook
  (ediff-prepare-buffer . my/ediff-prepare-buffer)
  :config
  ;; Change some keybindings
  (defun ora-ediff-hook ()
    (ediff-setup-keymap)
    (define-key ediff-mode-map "j" 'ediff-next-difference)
    (define-key ediff-mode-map "k" 'ediff-previous-difference))

  (add-hook 'ediff-mode-hook 'ora-ediff-hook)

  ;; Restore window after Ediff quit
  (winner-mode)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)


  (defun my/ediff-prepare-buffer ()
    "Function to prepare ediff buffers.

Runs with `ediff-prepare-buffer-hook' so that it gets run on all
three ediff buffers (A, B, and C)."
    (when (memq major-mode '(org-mode emacs-lisp-mode))
      ;; unfold org/elisp files
      (outline-show-all))))


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
  :bind ([S-return] . ace-window)
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
  :init
  (use-package crux)
  (bind-keys :prefix "C-t"
             :prefix-map transpose-map
             ("t" . my/toggle-window-split)
             ("f" . transpose-frame)
             ("c" . transpose-chars)
             ("w" . transpose-words) ;similar to M-t
             ("l" . transpose-lines)
             ("p" . transpose-paragraphs)
             ("s" . transpose-sentences)
             ("x" . transpose-sexps)
             ("b" . crux-transpose-windows) ;transpose-buffer
             )


  :defer 5
  :commands (transpose-frame)
  :config
  (defun my/toggle-window-split (&optional arg)
    "Switch between 2 windows split horizontally or vertically.
With ARG, swap them instead."
    (interactive "P")
    (unless (= (count-windows) 2)
      (user-error "Not two windows"))
    ;; Swap two windows
    (if arg
        (let ((this-win-buffer (window-buffer))
              (next-win-buffer (window-buffer (next-window))))
          (set-window-buffer (selected-window) next-win-buffer)
          (set-window-buffer (next-window) this-win-buffer))
      ;; Swap between horizontal and vertical splits
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



;;; Navigation
;;;; Find-replace
(use-package xah-find
  ;; find text from all files in a folder
  :bind (("C-s w" . xah-find-text)
         ("C-s o" . xah-find-replace-text)
         ("C-s e" . xah-find-text-regex)
         ("C-s k" . xah-find-count))
  )

;;;; Ivy / Swiper / Counsel
(use-package ivy
  :bind
  ("C-M-z" . ivy-resume)
  ([remap list-buffers] . ivy-switch-buffer)
  :config
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-virtual-buffers t)
  (setq ivy-extra-directories '("./"))
  (dolist (fun '(org-refile org-agenda-refile org-capture-refile))
    (setq ivy-initial-inputs-alist
          (delete `(,fun . "^") ivy-initial-inputs-alist)))
  (ivy-mode))

(use-package counsel
  ;; specifying counsel will bring ivy and swiper as dependencies
  :demand t
  :straight ivy-hydra
  :straight ivy-rich
  :straight counsel-projectile
  :straight ivy-posframe
  :straight smex
  :bind (("M-s"     . swiper)
         ("<f6>"    . ivy-resume) ;C-s C-r
         :map my-search-map
         ("a" . counsel-ag)
         ("d" . counsel-dired-jump)
         ("f" . counsel-find-file)
         ("g" . counsel-git-grep)
         ("i" . counsel-imenu)
         ("j" . counsel-file-jump)
         ("l" . counsel-find-library)
         ("r" . counsel-recentf)
         ("s" . counsel-locate)
         ("u" . counsel-unicode-char)
         ("v" . counsel-set-variable)
         ("C-r" . ivy-resume))
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

  ;; search in current file directory
  (setq counsel-find-file-at-point t)
  ;; ignore . files or temporary files
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)"))

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
                     ("d" delete-file "delete")
                     ("g" magit-status-internal "magit status")
                     ("r" counsel-find-file-as-root "open as root")))
  ;; display at `ivy-posframe-style'
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-point)))
  ;; (ivy-posframe-mode 1)
  )


;;;; Register
(use-package register
  :straight nil
  :bind* (:map my-assist-map
               ("<SPC>" . point-to-register)
               ("j" . jump-to-register)))

;;;; Bookmark
(use-package bookmark
  :straight t
  :init
  (setq bookmark-default-file (concat my-emacs-cache "bookmarks") ;bookmarks dir
        bookmark-save-flag 1) ;auto save when chnage else use "t" to autosave when emacs quits
  :bind (:map my-assist-map
              ("b" . bookmark-set)
              ("c" . bookmark-jump)
              ("l" . bookmark-bmenu-list))
  :config
  ;; bookmark+ harus di download di GitHub dan pasang di load-path
  ;; http://blog.binchen.org/posts/hello-ivy-mode-bye-helm.html
  (defun ivy-bookmark-goto ()
    "Open ANY bookmark"
    (interactive)
    (let (bookmarks filename)
      ;; load bookmarks
      (unless (featurep 'bookmark)
        (require 'bookmark))
      (bookmark-maybe-load-default-file)
      (setq bookmarks (and (boundp 'bookmark-alist) bookmark-alist))

      ;; do the real thing
      (ivy-read "bookmarks:"
                (delq nil (mapcar (lambda (bookmark)
                                    (let (key)
                                      ;; build key which will be displayed
                                      (cond
                                       ((and (assoc 'filename bookmark) (cdr (assoc 'filename bookmark)))
                                        (setq key (format "%s (%s)" (car bookmark) (cdr (assoc 'filename bookmark)))))
                                       ((and (assoc 'location bookmark) (cdr (assoc 'location bookmark)))
                                        ;; bmkp-jump-w3m is from bookmark+
                                        (unless (featurep 'bookmark+)
                                          (require 'bookmark+))
                                        (setq key (format "%s (%s)" (car bookmark) (cdr (assoc 'location bookmark)))))
                                       (t
                                        (setq key (car bookmark))))
                                      ;; re-shape the data so full bookmark be passed to ivy-read:action
                                      (cons key bookmark)))
                                  bookmarks))
                :action (lambda (bookmark)
                          (bookmark-jump bookmark)))
      ))


  ;; Last visited bookmark on top
  (defadvice bookmark-jump (after bookmark-jump activate)
    (let ((latest (bookmark-get-bookmark bookmark)))
      (setq bookmark-alist (delq latest bookmark-alist))
      (add-to-list 'bookmark-alist latest)))
  )

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

;;;; Copy file path
(defun xah-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not dired, copy value of `default-directory' (which is usually the “current” dir when that buffer was created)

URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2017-09-01"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: 「%s」" $fpath)
         $fpath )))))

(global-set-key (kbd "C-c d") 'xah-copy-file-path)
(bind-key "d" 'xah-copy-file-path my-assist-map)

;;; Workspace Mgmt: eyebrowse + projectile

(use-package projectile
  :defer 5
  :straight ripgrep ;; required by projectile-ripgrep
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind* (("C-c p f" . 'projectile-find-file))
  :config
  ;; Where my projects and clones are normally placed.
  (setq projectile-project-search-path '("~/projects")
        projectile-completion-system 'ivy)
  (projectile-mode +1)

  ;; Different than projectile-switch-project coz this works globally
  (defun counsel-switch-project ()
    (interactive)
    (ivy-read "Switch to project: "
              projectile-known-projects
              :sort t
              :require-match t
              :preselect (when (projectile-project-p) (abbreviate-file-name (projectile-project-root)))
              :action '(1
                        ("o" projectile-switch-project-by-name "goto")
                        ("g" magit-status "magit")
                        ("s" (lambda (a) (setq default-directory a) (counsel-git-grep)) "git grep"))
              :caller 'counsel-switch-project))
  (bind-key* "C-c p p" 'counsel-switch-project)
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
  :hook ((ess-r-mode
          inferior-ess-r-mode
          markdown-mode
          prog-mode) . smartparens-mode)
  ;; (add-hook 'inferior-ess-mode-hook #'smartparens-mode)
  ;; (add-hook 'LaTeX-mode-hook #'smartparens-mode)
  ;; (add-hook 'markdown-mode-hook #'smartparens-mode)
  )

;; gives spaces automatically
(use-package electric-operator
  :straight t
  :hook ((ess-r-mode python-mode) . electric-operator-mode)
  :config
  ;; edit rules for ESS mode
  (electric-operator-add-rules-for-mode 'ess-r-mode
                                        (cons ":=" " := ")
                                        ;; (cons "%" "%")
                                        (cons "%in%" " %in% ")
                                        (cons "%>%" " %>% "))

  (setq electric-operator-R-named-argument-style 'spaced) ;if unspaced will be f(foo=1)
  ;; (add-hook 'ess-r-mode-hook #'electric-operator-mode)
  ;; (add-hook 'python-mode-hook #'electric-operator-mode)
  )

(use-package csv-mode
  :mode "\\.csv$"
  :init
  (setq csv-separators '(";"))
  )


(use-package find-func
  :defer
  :bind (:map my-search-map
              ("x f" . find-function)
              ("x v" . find-variable)
              ("x l" . find-library))
  :hook
  (find-function-after . reposition-window))


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




;;; Display
;;Activate with M-x display-ansi-colors
(use-package ansi-color
  :straight nil
  :init
  (setq ansi-color-faces-vector
        [default bold shadow italic underline bold bold-italic bold])

  :bind (:map my-assist-map
              ("a" . display-ansi-col))
  :hook ((ess-r-mode inferior-ess-r-mode) . display-ansi-col)
  :config
  (defun display-ansi-col ()
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max)))
  )

(use-package battery
  :straight nil
  ;; :disabled t
  :init
  (display-battery-mode 1)
  :config
  (when (and battery-status-function
             (not (string-match-p "N/A"
                                  (battery-format "%B"
                                                  (funcall battery-status-function)))))
    )

  )

;; (require 'battery)
;; (when (and battery-status-function
;;            (not (string-match-p "N/A"
;;                                 (battery-format "%B"
;;                                                 (funcall battery-status-function)))))
;;   (display-battery-mode 1))

;; ;; If not on AC power line, then display battery status on the mode line
;; (and (require 'battery nil t)
;;      (functionp battery-status-function)
;;      (or (equal (cdr (assoc ?L (funcall battery-status-function))) "on-line")
;;          (display-battery-mode 1)))

(use-package default-text-scale
  :straight t
  :bind (("C--" . default-text-scale-decrease)
         ("C-+" . default-text-scale-increase))
  :config
  (default-text-scale-mode))

(use-package faces
  ;; faces are how Emacs determines how to display characters (font, size,
  ;; color, etc)
  :straight nil
  :defer t
  :bind
  ("C-h c" . describe-face) ; overrides describe-key-briefly from help.el
  :custom-face
  (help-argument-name ((t (:inherit font-lock-doc-face))))
  :config
  (add-to-list 'default-frame-alist
               '(font . "monospace-12")))

;;; Terminal
;;;; Dired
(use-package dired
  ;; Emacs can act as your file finder/explorer.  Dired is the built-in way
  ;; to do this.
  :straight nil
  :bind
  (("C-x C-d" . dired) ; overrides list-directory
   :map  dired-mode-map
   ("l" . dired-up-directory) ;go up in dired
   ("i" . swiper)
   ("j" . dired-next-line)
   ("k" . dired-previous-line)) ;
  :config
  ;; other settings
  (setq dired-auto-revert-buffer t)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-dwim-target t)
  (setq dired-isearch-filenames 'dwim)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  ;; -l: long listing format REQUIRED in dired-listing-switches
  ;; -a: show everything (including dotfiles)
  ;; -h: human-readable file sizes
  (setq dired-listing-switches "-alh --group-directories-first")
  (defun my/dired-ediff-marked ()
    "Run `ediff' on two marked files in a dired buffer."
    (interactive)
    (unless (eq 'dired-mode major-mode)
      (error "For use in dired buffers only"))
    (let ((files (dired-get-marked-files)))
      (when (not (eq 2 (length files)))
        (error "Two files not marked"))
      (ediff (car files) (nth 1 files)))))

(use-package dired-x
  :straight nil
  :hook
  (dired-load . (lambda () (load "dired-x" nil t)))
  :bind
  ("C-x C-j" . dired-jump)
  :custom
  ;; By default, dired asks you if you want to delete the dired buffer if
  ;; you delete the folder. I can't think of a reason I'd ever want to do
  ;; that.
  (dired-clean-confirm-killing-deleted-buffers nil))

;;;; Eshell
;; Emacs command shell
(use-package eshell
  :straight nil
  :defines eshell-prompt-function
  :functions eshell/alias
  :bind (:map my-personal-map
              ("s" . eshell))
  :hook (eshell-mode . (lambda ()
                         (bind-key "C-l" 'eshell/clear eshell-mode-map)
                         (eshell/alias "f" "find-file $1")
                         (eshell/alias "fo" "find-file-other-window $1")
                         (eshell/alias "d" "dired $1")
                         (eshell/alias "ll" "ls -l")
                         (eshell/alias "la" "ls -al")
                         (eshell/alias "gw" "cd ~/Git-work")
                         (eshell/alias "gp" "cd ~/Git-personal")
                         (eshell/alias "gf" "cd ~/Git-fhi")
                         ;; folkehelseprofil mappen
                         (eshell/alias "cdf" "cd /f/Prosjekter/Kommunehelsa")))
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
      (interactive "View file: ")
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

;; ;; ANSI & XTERM 256 color support
;; (use-package xterm-color
;;   :defines (compilation-environment
;;             eshell-preoutput-filter-functions
;;             eshell-output-filter-functions)
;;   :functions (compilation-filter my-advice-compilation-filter)
;;   :init
;;   ;; For shell
;;   (setenv "TERM" "xterm-256color")
;;   (setq comint-output-filter-functions
;;         (remove 'ansi-color-process-output comint-output-filter-functions))
;;   (add-hook 'shell-mode-hook
;;             (lambda ()
;;               ;; Disable font-locking in this buffer to improve performance
;;               (font-lock-mode -1)
;;               ;; Prevent font-locking from being re-enabled in this buffer
;;               (make-local-variable 'font-lock-function)
;;               (setq font-lock-function (lambda (_) nil))
;;               (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

;;   ;; For eshell
;;   (with-eval-after-load 'esh-mode
;;     (add-hook 'eshell-before-prompt-hook
;;               (lambda ()
;;                 (setq xterm-color-preserve-properties t)))
;;     (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
;;     (setq eshell-output-filter-functions
;;           (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

;;   ;; For compilation buffers
;;   (setq compilation-environment '("TERM=xterm-256color"))
;;   (defun my-advice-compilation-filter (f proc string)
;;     (funcall f proc
;;              (if (eq major-mode 'rg-mode) ; compatible with `rg'
;;                  string
;;                (xterm-color-filter string))))
;;   (advice-add 'compilation-filter :around #'my-advice-compilation-filter)
;;   (advice-add 'gud-filter :around #'my-advice-compilation-filter)

;;   ;; For prolog inferior
;;   (with-eval-after-load 'prolog
;;     (add-hook 'prolog-inferior-mode-hook
;;               (lambda ()
;;                 (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))))

;; ;; Better term
;; ;; @see https://github.com/akermu/emacs-libvterm#installation
;; (when (and (executable-find "cmake")
;;            (executable-find "libtool")
;;            (executable-find "make"))
;;   (use-package vterm
;;     :init (defalias #'term #'vterm)))

;; Shell Pop
(use-package shell-pop
  :straight t
  :defer 2
  ;; :bind ([f9] . shell-pop)
  :bind (:map my-personal-map
              ("x" . shell-pop))
  :custom
  (shell-pop-full-span t)
  (shell-pop-shell-type '("eshell" "*eshell" (lambda nil (eshell))))
  :config
  ;; ;;shell terminal
  ;; (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  ;; (setq shell-pop-term-shell "/bin/bash")
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
        :map my-assist-map
        ("o" . hydra-origami/body)
        )
  :config
  (global-origami-mode)
  (defhydra hydra-origami (:color red)
    "
        _o_pen node    _n_ext fold       toggle _f_orward  _t_oggle node
        _c_lose node   _p_revious fold   toggle _a_ll
        "
    ("o" origami-open-node)
    ("c" origami-close-node)
    ("n" origami-next-fold)
    ("p" origami-previous-fold)
    ("f" origami-forward-toggle-node)
    ("a" origami-toggle-all-nodes)
    ("t" origami-toggle-node)
    )
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
  :bind ("<f4>" . neotree-toggle)
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


;;; ESS
;; C-c general keymap for ESS
;; C-c C-t for debugging
;; C-c C-d explore object
;; Tooltips
;; C-c C-d C-e ess-describe-object-at-point
(use-package ess-mode
  :straight ess
  :bind
  (:map inferior-ess-mode-map
        ;; Usually I bind C-z to `undo', but I don't really use `undo' in
        ;; inferior buffers. Use it to switch to the R script (like C-c
        ;; C-z):
        ("C-z ." . ess-switch-to-inferior-or-script-buffer))
  )

(use-package ess-r-mode
  :straight nil
  ;; :mode ("\\.r[R]\\'" . ess-r-mode)
  ;; :commands (R
  ;;            R-mode
  ;;            r-mode)
  :init
  ;; Tetapkan Rsetting folder
  (defvar ybk/r-dir "~/Rsetting/") ;definere hvor epost skal være
  ;; lage direktori om ikke allerede finnes
  (unless (file-exists-p ybk/r-dir)
    (make-directory ybk/r-dir t))

  :bind (:map ess-r-mode-map
              ("M--" . ess-cycle-assign)
              ;; ("M--" . ess-insert-assign)
              ("C-c +" . my-add-column)
              ("C-c ," . my-add-match)
              ("C-c \\" . my-add-pipe)
              ("M-|" . my-ess-eval-pipe-through-line)
              ("C-S-<return>" . ess-eval-region-or-function-or-paragraph-and-step)
              ("C-." . ess-eval-paragraph-and-step)
              ("M-." . ess-eval-paragraph-and-go)
              ("C-S-<tab>" . ess-indent-region-with-styler)
              :map inferior-ess-r-mode-map
              ("C-S-<up>" . ess-readline) ;previous command from script
              ("M--" . ess-cycle-assign)
              )

  :custom
  (ess-plain-first-buffername nil "Name first R process R:1")
  (ess-tab-complete-in-script t "TAB should complete.")

  :config
  ;; ESS-company
  (setq ess-use-company t)

  ;; Must-haves for ESS
  ;; http://www.emacswiki.org/emacs/CategoryESS
  (setq ess-eval-visibly 'nowait) ;print input without waiting the process to finish

  ;; Auto-scrolling of R console to bottom and Shift key extension
  ;; http://www.kieranhealy.org/blog/archives/2009/10/12/make-shift-enter-do-a-lot-in-ess/
  ;; Adapted with one minor change from Felipe Salazar at
  ;; http://www.emacswiki.org/emacs/ESSShiftEnter
  (setq ess-local-process-name "R")
  (setq ansi-color-for-comint-mode 'filter)
  (setq comint-prompt-read-only t)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t)

  ;; inferior not read-only
  ;; https://github.com/emacs-ess/ESS/issues/300#issuecomment-231314374
  (add-hook 'inferior-ess-mode-hook
            (lambda()
              (setq-local comint-use-prompt-regexp nil)
              (setq-local inhibit-field-text-motion nil)))

  ;; ess-trace-bug.el
  (setq ess-use-tracebug t) ; permanent activation
  ;;
  ;; Tooltip included in ESS
  (setq ess-describe-at-point-method 'tooltip) ; 'tooltip or nil (buffer)

  (setq inferior-R-args "--no-save")
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:fun-calls . nil)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . nil)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)
          (ess-R-fl-keyword:%op% . t)))
  (setq inferior-ess-r-font-lock-keywords
        '((ess-S-fl-keyword:prompt . t)
          (ess-R-fl-keyword:messages . t)
          (ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:matrix-labels . t)
          (ess-fl-keyword:fun-calls . nil)
          (ess-fl-keyword:numbers . nil)
          (ess-fl-keyword:operators . nil)
          (ess-fl-keyword:delimiters . nil)
          (ess-fl-keyword:= . nil)
          (ess-R-fl-keyword:F&T . nil)))


  ;; data.table update
  (defun my-add-column ()
    "Adds a data.table update."
    (interactive)
    ;;(just-one-space 1) ;delete whitespace around cursor
    (insert " := "))

  ;; Match
  (defun my-add-match ()
    "Adds match."
    (interactive)
    (insert " %in% "))

  ;; pipe
  (defun my-add-pipe ()
    "Adds a pipe operator %>% with one space to the left and then
  starts a newline with proper indentation"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (ess-newline-and-indent))


  ;; use styler package but it has to be install first
  (defun ess-indent-region-with-styler (beg end)
    "Format region of code R using styler::style_text()."
    (interactive "r")
    (let ((string
           (replace-regexp-in-string
            "\"" "\\\\\\&"
            (replace-regexp-in-string ;; how to avoid this double matching?
             "\\\\\"" "\\\\\\&"
             (buffer-substring-no-properties beg end))))
          (buf (get-buffer-create "*ess-command-output*")))
      (ess-force-buffer-current "Process to load into:")
      (ess-command
       (format
        "local({options(styler.colored_print.vertical = FALSE);styler::style_text(text = \"\n%s\", reindention = styler::specify_reindention(regex_pattern = \"###\", indention = 0), indent_by = 4)})\n"
        string) buf)
      (with-current-buffer buf
        (goto-char (point-max))
        ;; (skip-chars-backward "\n")
        (let ((end (point)))
          (goto-char (point-min))
          (goto-char (1+ (point-at-eol)))
          (setq string (buffer-substring-no-properties (point) end))
          ))
      (delete-region beg end)
      (insert string)
      (delete-char -1)
      ))

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

  (defun my-ess-eval-pipe-through-line (vis)
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


  ;; Run ShinyApp
  ;; Source  https://jcubic.wordpress.com/2018/07/02/run-shiny-r-application-from-emacs/
  (defun shiny ()
    "run shiny R application in new shell buffer
if there is displayed buffer that have shell it will use that window"
    (interactive)
    (let* ((R (concat "shiny::runApp('" default-directory "')"))
           (name "*shiny*")
           (new-buffer (get-buffer-create name))
           (script-proc-buffer
            (apply 'make-comint-in-buffer "script" new-buffer "R" nil `("-e" ,R)))
           (window (get-window-with-mode '(comint-mode eshell-mode)))
           (script-proc (get-buffer-process script-proc-buffer)))
      (if window
          (set-window-buffer window new-buffer)
        (switch-to-buffer-other-window new-buffer))))

  (defun search-window-buffer (fn)
    "return first window for which given function return non nil value"
    (let ((buffers (buffer-list))
          (value))
      (dolist (buffer buffers value)
        (let ((window (get-buffer-window buffer)))
          (if (and window (not value) (funcall fn buffer window))
              (setq value window))))))

  (defun get-window-with-mode (modes)
    "return window with given major modes"
    (search-window-buffer (lambda (buff window)
                            ((let ((mode (with-current-buffer buffer major-mode)))
                               (member mode modes))))))

  )

;; View data like View()
(use-package ess-R-data-view
  ;; Use M-x ess-R-dv-ctable or ess-R-dv-pprint
  :after ess
  :bind (:map my-personal-map
              ("r" . ess-R-dev-ctable)
              ("p" . ess-R-dev-pprint)))

;; Open buffer to test R code
(defun test-R-buffer ()
  "Create a new empty buffer with R-mode."
  (interactive)
  (let (($buf (generate-new-buffer "*r-test*"))
        (test-mode2 (quote R-mode)))
    (switch-to-buffer $buf)
    (insert (format "## == Test %s == \n\n" "R script"))
    (funcall test-mode2)
    (setq buffer-offer-save t)
    $buf
    ))

(bind-key "R" 'test-R-buffer my-personal-map)


;;; Python
(use-package python
  ;; The package is called python, the mode is python-mode. Confusingly, there's
  ;; also python-mode.el but I don't use that.
  :disabled t
  :defer t
  :bind
  (:map python-mode-map
        ("C-<return>" . my/python-shell-send-region-or-statement-and-step))
  :custom
  ;; Use flake8 for flymake:
  (python-flymake-command '("flake8" "-"))
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-offset 4)
  (python-eldoc-function-timeout-permanent nil)
  :config
  (defun my/python-shell-send-region-or-statement ()
    "Send the current region to the inferior python process if there is an active one, otherwise the current line."
    (interactive)
    (if (use-region-p)
        (python-shell-send-region (region-beginning) (region-end))
      (my/python-shell-send-statement)))
  (defun my/python-shell-send-statement ()
    "Send the current line to the inferior python process for evaluation."
    (interactive)
    (save-excursion
      (let ((end (python-nav-end-of-statement))
            (beginning (python-nav-beginning-of-statement)))
        (python-shell-send-region beginning end))))
  (defun my/python-shell-send-region-or-statement-and-step ()
    "Call `python-shell-send-region-or-statement' and then `python-nav-forward-statement'."
    (interactive)
    (my/python-shell-send-region-or-statement)
    (python-nav-forward-statement))
  (define-minor-mode my/python-use-ipython-mode
    ;; I don't really get the allure of ipython, but here's something that
    ;; lets me switch back and forth:
    "Make python mode use the ipython interpreter."
    :lighter (" iPy")
    (unless (executable-find "ipython")
      (error "Could not find ipython executable"))
    (if my/python-use-ipython-mode
        ;; activate ipython stuff
        (setq python-shell-buffer-name "Ipython"
              python-shell-interpreter "ipython"
              ;; https://emacs.stackexchange.com/q/24453/115
              ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25306
              python-shell-interpreter-args "--simple-prompt -i")
      ;; else, deactivate everything
      (setq python-shell-buffer-name "Python"
            python-shell-interpreter "python" ;python3
            python-shell-interpreter-args "-i")))

  ;; (define-minor-mode my/python-use-jupyter
  ;;   ;; Use Jupyter Notebook after installing it
  ;;   ;; Use IPython for REPL
  ;;   (setq python-shell-interpreter "jupyter"
  ;;         python-shell-interpreter-args "console --simple-prompt"
  ;;         python-shell-prompt-detect-failure-warning nil)
  ;;   (add-to-list 'python-shell-completion-native-disabled-interpreters
  ;;                "jupyter")
  ;;   )
  )


;; Basically for the code below
;; I just follow https://realpython.com/emacs-the-best-python-editor

(use-package flycheck
  :hook (elpy-mode . flycheck-mode)
  )

(use-package elpy
  :straight t
  :straight flycheck
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :bind (:map python-mode-map
              ("C-<return>" . my/python-shell-send-region-or-statement-and-step))
  :custom
  ;; Use flake8 for flymake:
  (python-flymake-command '("flake8" "-"))
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-offset 4)
  (python-eldoc-function-timeout-permanent nil)
  :config
  ;; Enable Flycheck
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))

  (defun my/python-shell-send-region-or-statement ()
    "Send the current region to the inferior python process if there is an active one, otherwise the current line."
    (interactive)
    (if (use-region-p)
        (python-shell-send-region (region-beginning) (region-end))
      (my/python-shell-send-statement)))
  (defun my/python-shell-send-statement ()
    "Send the current line to the inferior python process for evaluation."
    (interactive)
    (save-excursion
      (let ((end (python-nav-end-of-statement))
            (beginning (python-nav-beginning-of-statement)))
        (python-shell-send-region beginning end))))
  (defun my/python-shell-send-region-or-statement-and-step ()
    "Call `python-shell-send-region-or-statement' and then `python-nav-forward-statement'."
    (interactive)
    (my/python-shell-send-region-or-statement)
    (python-nav-forward-statement))

  (define-minor-mode my/python-use-ipython-mode
    ;; I don't really get the allure of ipython, but here's something that
    ;; lets me switch back and forth:
    "Make python mode use the ipython interpreter."
    :lighter (" iPy")
    (unless (executable-find "ipython")
      (error "Could not find ipython executable"))
    (if my/python-use-ipython-mode
        ;; activate ipython stuff
        (setq python-shell-buffer-name "Ipython"
              python-shell-interpreter "ipython"
              ;; https://emacs.stackexchange.com/q/24453/115
              ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25306
              python-shell-interpreter-args "--simple-prompt -i")
      ;; else, deactivate everything
      (setq python-shell-buffer-name "Python"
            python-shell-interpreter "python" ;python3
            python-shell-interpreter-args "-i")))

  ;; (setq python-shell-interpreter "ipython"
  ;;       python-shell-interpreter-args "-i --simple-prompt")
  )

(use-package pyenv-mode
  ;; virtualenv python with M-x pyvenv-activate
  :config
  (pyenv-mode)
  )

;;; Database
(use-package sql
  :straight t
  :mode ("\\.\\(sqlite\\|db\\|sqlite3\\)\\'" . sql-mode)
  :interpreter ("sql" . sql-mode)
  ;; :custom
  ;; (sql-sqlite-program "c:/Program Files/R/sqlite/sqlite3.exe")

  :config
  ;; use only one database no need to login
  (defalias 'sql-get-login 'ignore)
  )

(use-package sqlup-mode
  ;;Upercase SQL words
  :straight t
  :bind ("C-c u" . sqlup-capitalize-keywords-in-region)
  ;; if activated in sql-interactive-mode can make trouble to run commands
  :hook ((sql-mode redis-mode) . sqlup-mode)
  )

;;; Graphics
(use-package graphviz-dot-mode
  ;; graphvis must be installed
  ;; :ensure t
  ;; :pin melpa-stable
  :mode "\\.dot\\'")



;;; Appearance
;; (use-package naysayer-theme)
;; (load-theme 'naysayer t)

(use-package all-the-icons
  ;; needed to display icon correctly in doom-modeline
  ;; Run M-x all-the-icons-install-fonts to install all-the-icons
  ;; if error then M-x package-install all-the-icons
  :straight t
  :defer 0.5)

(use-package major-mode-icons
  ;; Is disabled because doom-modeline is activated
  :disabled
  :straight t
  :defer 0.5
  :config
  (major-mode-icons-mode 1))


(use-package doom-themes
  :straight t
  :init
  ;; need to load at init for cyclye theme to work
  ;; but gives error at first use before installing doom-themes
  (require 'doom-themes)
  (load-theme 'doom-one t)
  :bind ("C-9" . cycle-my-theme)
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; ;; brighter source buffers (that represent files)
  ;; (add-hook 'find-file-hook #'doom-buffer-mode-maybe)
  ;; ;; ...if you use auto-revert-mode
  ;; (add-hook 'after-revert-hook #'doom-buffer-mode-maybe)
  ;; ;; And you can brighten other buffers (unconditionally) with:
  ;; (add-hook 'ediff-prepare-buffer-hook #'doom-buffer-mode)

  ;; ;; brighter minibuffer when active
  ;; (add-hook 'minibuffer-setup-hook #'doom-brighten-minibuffer)

  ;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

  ;; utk tukar tema
  (setq my-themes '(
                    doom-gruvbox
                    doom-snazzy
                    doom-acario-light
                    ;; doom-vibrant
                    doom-wilmersdorf
                    ;; doom-oceanic-next
                    ;; doom-Iosvkem ;bold has bigger font
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


(use-package doom-modeline
  ;; Customization guides https://seagle0128.github.io/doom-modeline/
  :straight t
  :defer t
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-project-detection 'project "Detect the project root")
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-env-load-string "..." "Display as the version while the new one is being loaded")
  :hook
  ;; (after-init . doom-modeline-mode)
  (after-init . doom-modeline-init)
  :config
  ;; (set-face-attribute 'mode-line nil
  ;;                     :background "#539"
  ;;                     :foreground "white"
  ;;                     :box '(:line-width 5 :color "#539")
  ;;                     :overline nil
  ;;                     :underline nil)

  ;; (set-face-attribute 'mode-line-inactive nil
  ;;                     :background "#565063"
  ;;                     :foreground "white"
  ;;                     :box '(:line-width 5 :color "#565063")
  ;;                     :overline nil
  ;;                     :underline nil)

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
     (:foreground "#ee7711" :inherit bold))
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


;; Show hexadecimal color in the background they represent
(use-package rainbow-mode
  :straight t
  :diminish rainbow-mode
  :hook
  ((prog-mode
    inferior-ess-mode
    ess-mode text-mode
    markdown-mode
    LaTeX-mode) . rainbow-mode)
  )


;;; Org
(use-package org
  ;; Org mode is a great thing. I use it for writing academic papers,
  ;; managing my schedule, managing my references and notes, writing
  ;; presentations, writing lecture slides, and pretty much anything
  ;; else.
  :straight org-plus-contrib
  :bind
  (("C-c l" . org-store-link)
   ("C-'" . org-cycle-agenda-files) ; quickly access agenda files
   :map org-mode-map
   ("C-a" . org-beginning-of-line)
   ("C-e" . org-end-of-line)
   ;; Bind M-p and M-n to navigate heading more easily (these are bound to
   ;; C-c C-p/n by default):
   ("M-p" . my/org-previous-visible-heading)
   ("M-n" . my/org-next-visible-heading)
   ;; C-c C-t is bound to `org-todo' by default, but I want it
   ;; bound to C-c t as well:
   ("C-c t" . org-todo)
   ;; Show hidden link
   ("M-L" . my/org-toggle-link-display)
   )
  :hook
  (org-mode . my/setup-org-mode)
  :custom
  (org-blank-before-new-entry nil)
  (org-cycle-separator-lines 0)
  (org-pretty-entities t "UTF8 all the things!")
  (org-support-shift-select t "Holding shift and moving point should select things.")
  (org-fontify-quote-and-verse-blocks t "Provide a special face for quote and verse blocks.")
  (org-M-RET-may-split-line nil "M-RET may never split a line.")
  (org-enforce-todo-dependencies t "Can't finish parent before children.")
  (org-enforce-todo-checkbox-dependencies t "Can't finish parent before children.")
  (org-hide-emphasis-markers t "Make words italic or bold, hide / and *.")
  (org-catch-invisible-edits 'show-and-error "Don't let me edit things I can't see.")
  (org-special-ctrl-a/e t "Make C-a and C-e work more like how I want:.")
  (org-preview-latex-default-process 'imagemagick "Let org's preview mechanism use imagemagick instead of dvipng.")
  ;; Let imenu go deeper into menu structure
  (org-imenu-depth 6)
  (org-image-actual-width '(300))
  (org-blank-before-new-entry '((heading . nil)
                                (plain-list-item . nil)))
  ;; For whatever reason, I have to explicitely tell org how to open pdf
  ;; links.  I use pdf-tools.  If pdf-tools isn't installed, it will use
  ;; doc-view (shipped with Emacs) instead.
  (org-file-apps
   '((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs)))
  (org-highlight-latex-and-related '(latex entities) "set up fontlocking for latex")
  (org-startup-with-inline-images t "Show inline images.")
  (org-log-done 'time)
  (org-goto-interface 'outline-path-completion)
  (org-ellipsis "⬎")
  ;; tags within start-endgroup will allow only one of those in a file
  ;; C-c C-q for setting tags
  (org-tag-persistent-alist '(("annent" . ?a)
                              ("fhprofil" . ?p)
                              (:startgroup . nil)
                              ("@work" . ?w)
                              ("@home" . ?h)
                              (:endgroup . nil)))

  ;; I keep my recipes in an org file and tag them based on what kind of
  ;; dish they are.  The level one headings are names, and each gets two
  ;; level two headings --- ingredients and directions.  To easily search via
  ;; tag, I can restrict org-agenda to that buffer using < then hit m to
  ;; match based on a tag.
  (org-tags-exclude-from-inheritance
   '("BREAKFAST" "DINNER" "DESSERT" "SIDE" "CHICKEN" "SEAFOOD"
     "BEEF" "PASTA" "SOUP" "SNACK" "DRINK" "LAMB" "VEGETARIAN"))
  ;; Org-refile lets me quickly move around headings in org files.  It
  ;; plays nicely with org-capture, which I use to turn emails into TODOs
  ;; easily (among other things, of course)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-use-outline-path 'file)
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6)))"Up to 6 level deep headlines")

  :custom-face
  (org-block ((t (:inherit default))))

  :config
  ;; Exclude DONE state tasks from refile targets
  (defun ybk/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  (setq org-refile-target-verify-function 'ybk/verify-refile-target)


  ;; These are the programming languages org should teach itself:
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (python . t)
     (R . t)
     (shell . t)))

  ;; remove C-c [ from adding or excluding org file to front of agenda
  ;; other then those specified in org-agenda-files
  (unbind-key "C-c [" org-mode-map)
  (unbind-key "C-c ]" org-mode-map)

  (defun my/setup-org-mode ()
    "Setup org-mode."
    ;; An alist of symbols to prettify, see `prettify-symbols-alist'.
    ;; Whether the symbol actually gets prettified is controlled by
    ;; `org-pretty-compose-p', which see.
    (setq-local prettify-symbols-unprettify-at-point nil)
    (setq-local prettify-symbols-alist '(("*" . ?•)))
    (setq-local prettify-symbols-compose-predicate #'my/org-pretty-compose-p))

  (defun my/org-next-visible-heading (arg)
    "Go to next heading and beginning of line."
    (interactive "p")
    (org-next-visible-heading arg)
    (org-beginning-of-line))

  (defun my/org-previous-visible-heading (arg)
    "Go to previous heading and beginning of line."
    (interactive "p")
    (org-previous-visible-heading arg)
    (org-beginning-of-line))

  (defun my/org-pretty-compose-p (start end match)
    "Return it if the symbol should be prettified.
START and END are the start and end points, MATCH is the string
match.  See also `prettify-symbols-compose-predicate'."
    (if (string= match "*")
        ;; prettify asterisks in headings
        (and (org-match-line org-outline-regexp-bol)
             (< end (match-end 0)))
      ;; else rely on the default function
      (prettify-symbols-default-compose-p start end match)))

  ;; use font-lock-mode or this function
  (defun my/org-toggle-link-display ()
    "Toggle the literal or descriptive display of links."
    (interactive)
    (if org-descriptive-links
        (progn (org-remove-from-invisibility-spec '(org-link))
               (org-restart-font-lock)
               (setq org-descriptive-links nil))
      (progn (add-to-invisibility-spec '(org-link))
             (org-restart-font-lock)
             (setq org-descriptive-links t))))


  ;; TODO keywords
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "HOLD(h@/!)" "CANCELLED(c@/!)"))))

  ;;Menyenagkan utk tukar kekunci TODO dengan C-c C-t KEKUNCI (org-todo-keywords)
  (setq org-use-fast-todo-selection t)

  ;;Tetapkan warna keyword
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "purple" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                )))


  ;;== Buat TAGS automatik
  ;; Status TODO memberikan atau menukarkan tag secara automatisk. Cth ke status 'HOLD'
  ;; memberikan tag 'HOLD' dan ke status 'DONE' membuang tag 'HOLD' dan 'CANCELLED'
  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("HOLD" ("HOLD" . t))
                (done ("HOLD"))
                ("TODO" ("CANCELLED") ("HOLD"))
                ("NEXT" ("CANCELLED") ("HOLD"))
                ("DONE" ("CANCELLED") ("HOLD")))))

  ;; Utk tukar status TODO menggunakan S-kiri dan S-kanan dan elakkan proses biasa seperti memasukkan masa
  ;; atau nota utk HOLD atau CANCELLED sekiranya yang ingin dibuat ialah pertukaran status TODO sahaja
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  ;;== Tukar parents status ke "DONE" hanya bila semua child tasks sudah ke status "DONE"
  (setq org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t)

  ;;== Masukkan annotation di task bila tukar status
  (setq org-log-done (quote time))

  ;;== Masukkan annotation bila tukar tarikh DEADLINE
  (setq org-log-redeadline (quote time))

  ;;== Masukkan annotation bila tukar tarikh SCHEDULE
  (setq org-log-reschedule (quote time))
  )


(use-package ob-core
  :straight org
  ;; ob is org-babel, which lets org know about code and code blocks
  :defer t
  :custom
  ;; I know what I'm getting myself into.
  (org-confirm-babel-evaluate nil "Don't ask to confirm evaluation."))


(use-package org-agenda
  ;; Here's where I set which files are added to org-agenda, which controls
  ;; org's global todo list, scheduling, and agenda features.  I use
  ;; Syncthing to keep these files in sync across computers.
  :straight org
  :bind
  (("C-c a" . org-agenda)
   ("<f5>" . org-agenda)
   :map org-agenda-mode-map
   ;; overrides org-agenda-redo, which I use "g" for anyway
   ("r" . org-agenda-refile)
   ;; overrides saving all org buffers, also bound to C-x C-s
   ("t" . org-agenda-schedule)
   ("d" . my/org-agenda-mark-done)
   ("n" . my/org-agenda-mark-next)
   )

  :init
  ;; create org folder if doesn't exist
  (defvar my-org-directory "~/Dropbox/org")
  (unless (file-exists-p my-org-directory)
    (make-directory my-org-directory))

  (defvar my-org-todo (expand-file-name "todo.org" my-org-directory)
    "Unstructure capture")
  (defvar my-org-misc (expand-file-name "misc.org" my-org-directory)
    "All other info for diary.")
  (defvar my-org-note (expand-file-name "notes.org" my-org-directory)
    "All other info for diary.")

  ;;Include all files under these folder in org-agenda-files
  (setq org-agenda-files `(,org-default-notes-file
                           ,my-org-todo
                           ,my-org-misc))
  (setq org-agenda-text-search-extra-files `(,my-org-note))

  :custom
  (org-directory "~/Dropbox/org/" "Kept in sync with syncthing.")
  (org-default-notes-file (concat org-directory "refile.org"))




  (org-agenda-skip-deadline-if-done t "Remove done deadlines from agenda.")
  (org-agenda-skip-scheduled-if-done t "Remove done scheduled from agenda.")
  (org-agenda-skip-timestamp-if-done t "Don't show timestamped things in agenda if they're done.")
  (org-agenda-skip-scheduled-if-deadline-is-shown 'not-today "Don't show scheduled if the deadline is visible unless it's also scheduled for today.")
  (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled "Skip deadline warnings if it is scheduled.")
  (org-deadline-warning-days 3 "warn me 3 days before a deadline")
  (org-agenda-tags-todo-honor-ignore-options t "Ignore scheduled items in tags todo searches.")
  (org-agenda-tags-column 'auto)
  (org-agenda-window-setup 'only-window "Use current window for agenda.")
  (org-agenda-restore-windows-after-quit t "Restore previous config after I'm done.")
  (org-agenda-span 'day) ; just show today. I can "vw" to view the week
  (org-agenda-time-grid
   '((daily today remove-match) (800 1000 1200 1400 1600 1800 2000)
     "" "") "By default, the time grid has a lot of ugly '-----' lines. Remove those.")
  (org-agenda-scheduled-leaders '("" "%2dx ") "I don't need to know that something is scheduled.  That's why it's appearing on the agenda in the first place.")
  (org-agenda-block-separator ?— "Use nice unicode character instead of ugly = to separate agendas:")
  (org-agenda-deadline-leaders '("Deadline: " "In %d days: " "OVERDUE %d day: ") "Make deadlines, especially overdue ones, stand out more:")
  (org-agenda-current-time-string "⸻ NOW ⸻")
  ;; The agenda is ugly by default. It doesn't properly align items and it
  ;; includes weird punctuation. Fix it:
  (org-agenda-prefix-format '((agenda . "%-12c%-14t%s")
                              (todo . " %i %-12:c")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c")))

  ;; Custom agenda
  (org-agenda-custom-commands
   '(
     ("h" "Home Agenda"
      ((agenda "" nil)
       (todo "NEXT"
             ((org-agenda-max-entries 5)
              (org-agenda-overriding-header "Dagens oppgaver:")
              ))
       (tags "@home"
             ((org-agenda-overriding-header "Samlet oppgaver:")
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "NEXT")))))
       (tags "REFILE"
             ((org-agenda-overriding-header "Refile:")
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "NEXT"))))))
      ((org-agenda-tag-filter-preset '("-@work"))))
     ("w" "Work Agenda"
      ((agenda "" nil)
       (todo "NEXT"
             ((org-agenda-max-entries 5)
              (org-agenda-overriding-header "Dagens oppgaver:")
              ))
       (tags "@work"
             ((org-agenda-overriding-header "Oppgavene som skal gjøres:")
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "NEXT")))))
       (tags "REFILE"
             ((org-agenda-overriding-header "Refile:")
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "NEXT"))))))
      ((org-agenda-tag-filter-preset '("-@home"))))
     ("d" "deadlines"
      ((agenda ""
               ((org-agenda-entry-types '(:deadline))
                (org-agenda-span 'fortnight)
                (org-agenda-time-grid nil)
                (org-deadline-warning-days 0)
                (org-agenda-skip-deadline-prewarning-if-scheduled nil)
                (org-agenda-skip-deadline-if-done nil)))))
     ("b" "bibliography"
      ((tags "CATEGORY=\"bib\"+LEVEL=2"
             ((org-agenda-overriding-header "")))))
     ("u" "unscheduled"
      ((todo  "TODO"
              ((org-agenda-overriding-header "Unscheduled tasks")
               (org-agenda-todo-ignore-with-date t)))))))

  :config
  (defun my/org-agenda-mark-done (&optional _arg)
    "Mark current TODO as DONE.
See `org-agenda-todo' for more details."
    (interactive "P")
    (org-agenda-todo "DONE"))

  (defun my/org-agenda-mark-next (&optional _arg)
    "Mark current TODO as NEXT.
See `org-agenda-todo' for more details."
    (interactive "P")
    (org-agenda-todo "NEXT"))
  )


(use-package org-capture
  :straight org
  :bind*
  ("C-c c" . org-capture)
  :bind
  ((:map org-capture-mode-map
         ("C-c C-j" . my/org-capture-refile-and-jump))
   (:map my-personal-map
         ("p" . ybk/org-task-capture)))
  ;; :init
  ;; (setq org-default-notes-file (concat org-directory "refile.org"))
  ;; (defconst my/org-inbox (concat org-directory "refile.org"))
  ;; (defconst my/org-notes (concat org-directory "notes.org"))
  :custom
  (org-capture-templates
   (quote (("a" "Avtale" entry (file+headline org-default-notes-file "Avtale")
            "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
           ("t" "task" entry (file  org-default-notes-file)
            "* TODO \n:PROPERTIES:\n:CREATED: %U\n:END:\n%i")
           ("m" "mail" entry (file org-default-notes-file)
            "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
           ("n" "note" entry (file my-org-note)
            "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n %i")
           )))
  :config
  (defun my/org-capture-refile-and-jump ()
    (interactive)
    (org-capture-refile)
    (org-refile-goto-last-stored))

  ;; Org-capture shortcut
  (defun ybk/org-task-capture ()
    "Capture a task with my default template."
    (interactive)
    (org-capture nil "t"))
  )


(use-package org-eww
  ;; Org-eww lets me capture eww webpages with org-mode
  :straight org
  :straight eww
  :after eww)

(use-package org-indent
  ;; org-indent-mode nicely aligns text with the outline level
  :straight org
  :hook
  (org-mode . org-indent-mode))


;;; Extra
;;;; Calendar
;; Manual setup
(setq calendar-week-start-day 1
      calendar-day-name-array ["Søndag" "Mandag" "Tirsdag" "Onsdag"
                               "Torsdag" "Fredag" "Lørdag"]
      calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
                                 "Juni" "Juli" "August" "September"
                                 "Oktober" "November" "Dezember"])

(use-package calendar-norway
  ;; :custom
  ;; (calendar-holidays 'calendar-norway-raude-dagar "Include days where you don't have to work")
  ;; (calendar-holidays 'calendar-norway-andre-merkedagar "Include other days that people celebrate")
  ;; (calendar-holidays 'calendar-norway-dst "Daylight saving")
  :config
  ;; Set what holidays you want in your calendar:
  (setq calendar-holidays
        (append
         ;; Include days where you don't have to work:
         calendar-norway-raude-dagar
         ;; Include other days that people celebrate:
         calendar-norway-andre-merkedagar
         ;; Include daylight savings time:
         calendar-norway-dst
         ;; And then you can add some non-Norwegian holidays etc. if you like:
         '((holiday-fixed 3 17 "St. Patricksdag")
           (holiday-fixed 10 31 "Hallowe'en")
           (holiday-float 11 4 4 "Thanksgiving")
           (solar-equinoxes-solstices))))
  )

;;;; Weather
(use-package weather-metno
  :straight t
  :bind (:map my-personal-map
              ("w" . weather-metno-forecast))
  :config
  (setq weather-metno-location-name "Oslo, Norge"
        weather-metno-location-latitude 59
        weather-metno-location-longitude 10)

  ;; ;; change icon size
  ;; (setq weather-metno-use-imagemagick t)
  ;; (setq weather-metno-get-image-props '(:width 10 :height 10 :ascent center))
  (setq weather-metno-get-image-props '(:ascent center))
  )
;;;; Calculator
(use-package calc
  :straight t
  :defer t
  :bind
  ("<XF86Calculator>" . quick-calc)
  ;; ("C-c =" . quick-calc)
  (:map my-personal-map
        ("c" . quick-calc)
        ("C" . my/calc-eval-region)))
;; ;;;; Google drive
;; ;; Access google drive folder
;; (defvar gdocs-folder-id "1G-5SDJMg8s074kgZ0qKijrgyrivxpQPS"
;;   "location for storing org to gdocs exported files, use 'gdrive list  -t <foldername>' to find the id")
;; (defun gdoc-upload-buffer-to-gdrive ()
;;   "Export current buffer to gdrive folder identified by gdocs-folder-id"
;;   (interactive)
;;   (shell-command
;;    (format "gdrive upload --parent %s %s"
;;            gdocs-folder-id buffer-file-name)))
