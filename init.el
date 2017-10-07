;; init.el --- Mula di sini

(if (version< emacs-version "25.1") ; Minimum version 
    (error "Your Emacs is too old -- this config requires v25.1 or higher"))

;; This file loads Org-mode and then loads the rest of our Emacs
;; initialization from Emacs lisp embedded in emacs.org.

;; setting utk garbage collection utk mempercepatkan pemasangan.
;; Beri nombor yang tinggi dipermulaan dan rendah di penhujung (lihat di bawah)
(setq gc-cons-threshold 20000000)

;; add MELPA, Org, and ELPA
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Ambil masa pemasangan
(defconst emacs-start-time (current-time))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; Get the latest version of org from gnu elpa:
(unless (package-installed-p 'org (version-to-list "8.3"))
  (package-refresh-contents)
  (package-install (cadr (assq 'org package-archive-contents))))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; load up all literate org-mode files in this directory
(require 'org)

(org-babel-load-file (concat user-emacs-directory "emacs25.org"))

;; Masa
;; Paparkan di =Messages= buffer masa yang diambil utk memasang semua pakej-pakej

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

;; garbage collection
(setq gc-cons-threshold 800000)



;; init.el --- Tamat di sini
