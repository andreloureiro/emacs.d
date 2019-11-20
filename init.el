;;; init.el

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(require 'use-package)

(use-package delight :ensure t)

;; Utils
(defun fix-lsp-company-prefix ()
  "Fix lsp-javascript company prefix
https://github.com/emacs-lsp/lsp-javascript/issues/9#issuecomment-379515379"
  (interactive)
  (defun lsp-prefix-company-transformer (candidates)
    (let ((completion-ignore-case t))
      (if (and (car candidates)
               (get-text-property 0 'lsp-completion-prefix (car candidates)))
          (all-completions (company-grab-symbol) candidates)
        candidates)))
  (make-local-variable 'company-transformers)
  (add-to-list 'company-transformers 'lsp-prefix-company-transformer))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

;; General

(setq visible-bell 1)
(set-default-coding-systems 'utf-8)
(modify-coding-system-alist 'file "" 'utf-8-unix)
(setq-default line-spacing 2)
(setq make-backup-files nil)
(setq auto-save-default nil)
(whitespace-mode 1)
(global-linum-mode 1)
(setq linum-format "%3d \u2502 ")

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(menu-bar-mode -1)

;; From https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org
(setq-default
 auto-window-vscroll nil
 confirm-kill-emacs 'yes-or-no-p
 display-time-format "%H:%M"
 fill-column 80
 indent-tabs-mode nil
 inhibit-startup-screen t
 initial-scratch-message ""
 ns-use-srgb-colorspace nil
 recenter-positions '(5 top bottom)
 scroll-conservatively most-positive-fixnum
 scroll-margin 10
 select-enable-clipboard t
 sentence-end-double-space nil
 show-trailing-whitespace nil
 tab-width 2
 uniquify-buffer-name-style 'forward
 window-combination-resize t)

(display-time-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

(if (eq window-system 'ns)
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

;; Should be set before both evil and evil-collection start
(setq evil-want-keybinding nil)

(add-hook 'focus-out-hook #'garbage-collect)

(load-theme 'minimal t)

;; Doom theme
(setq doom-themes-enable-bold t)
(doom-themes-org-config)

(setq markdown-command "/usr/bin/pandoc")

(use-package dashboard
  :ensure t
  :config
  (use-package page-break-lines)
  (dashboard-setup-startup-hook)
  (setq dashboard-page-separator "\n\n"
	show-week-agenda-p t
	dashboard-center-content nil
	dashboard-banner-logo-title ""
	dashboard-startup-banner 'logo
	dashboard-items '((agenda . 5)
			  (projects . 5)
			  (recents . t))))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(set-face-attribute 'default nil :font "Roboto Mono-10:antialias=natural")

;; (with-system windows-nt
;;   (set-face-attribute 'default nil :font "Fira Code-10:antialias=natural"))

;; (with-system darwin
;;   (set-face-attribute 'default nil :font "Fira Code-12:antialias=natural"))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'relative-from-project))

(global-set-key [remap kill-buffer] #'kill-this-buffer)

(require 'color)

;; (let ((bg (face-attribute 'default :background)))
;;   (custom-set-faces
;;    `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 20)))))
;;    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;;    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
;;    `(company-tooltip-selection ((t (:background , (color-lighten-name bg 10)))))
;;    `(company-tooltip-annotation ((t (:inherit default :background , (color-lighten-name bg 20) :foreground , (color-lighten-name bg 35)))))
;;    `(company-tooltip-annotation-selection ((t (:background , (color-lighten-name bg 10) :foreground , (color-lighten-name bg 35)))))
;;    `(company-tooltip-common ((t (:background , (color-lighten-name bg 2)))))))

(use-package smartparens
  :config
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'rjsx-mode-hook 'smartparens-mode))

(require 'ivy)
(require 'counsel)

(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-rich
  :after (:all ivy counsel)
  :init
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode 1))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;; Org

;; If you would like a TODO entry to automatically change to DONE when all children are done, you can use the following setup:
;; @todo: use use-package

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; Packages

(require 'evil)
(require 'evil-magit)
(require 'magit)
(require 'transient)
(require 'company)
(require 'projectile)
(require 'ag)

;; General

(defun add-to-map(keys func)
  "Add a keybinding in evil mode from keys to func."
  (define-key evil-normal-state-map (kbd keys) func)
  (define-key evil-motion-state-map (kbd keys) func))

(defun add-to-map-when-inserting (keys func)
  (define-key evil-insert-state-map (kbd keys) func))

(use-package evil
  :config
  (evil-mode 1)
  (add-to-map "<SPC>" nil)
  (add-to-map "C-u" nil)
  (add-to-map "C-u" 'evil-scroll-up)
  (add-to-map "<SPC> w v" 'evil-window-vsplit)
  (add-to-map "<SPC> w s" 'evil-window-split)
  (add-to-map "<SPC> w h" 'evil-window-left)
  (add-to-map "<SPC> w l" 'evil-window-right)
  (add-to-map "<SPC> w k" 'evil-window-up)
  (add-to-map "<SPC> w j" 'evil-window-down)
  (add-to-map "<SPC> w N" 'evil-window-vnew)
  (add-to-map "<SPC> w k" 'evil-window-delete)
  (add-to-map "<SPC> b b" 'ivy-switch-buffer)
  (add-to-map "<SPC> b B" 'ivy-switch-buffer-other-window)
  (add-to-map "<SPC> TAB" 'evil-switch-to-windows-last-buffer)
  (add-to-map "<SPC> b e" 'eval-buffer)
  (add-to-map "<SPC> b r" 'rename-buffer)
  (add-to-map "<SPC> b k" 'kill-buffer)
  (add-to-map "<SPC> b K" 'kill-other-buffers)
  (add-to-map "<SPC> b n" 'evil-buffer-new)
  (add-to-map "<SPC> g d" 'lsp-ui-peek-find-definitions)
  (add-to-map "<SPC> g r" 'lsp-ui-peek-find-references)
  (add-to-map "<SPC> g s" 'magit-status)
  (add-to-map "<SPC> TAB" 'evil-switch-to-windows-last-buffer)
  ;; TypeScript
  (add-to-map "<f12>" 'tide-jump-to-definition)
  (add-to-map "<SPC> t g i" 'tide-jump-to-implementation)
  (add-to-map "<SPC> t f r" 'tide-references)
  (add-to-map "<SPC> t i f" 'tide-fix)
  (add-to-map "<SPC> t r s" 'tide-rename-symbol)
  ;; eshell
  ;; Creates a eshell buffer, asking that you name it in order to avoid naming clashes and
  ;; enabling the creation of multiple eshells as needed
  (add-to-map
   "<SPC> e n"
   (lambda ()
     (interactive)
     (eshell)
     (rename-buffer (read-string "Enter the buffer name: "))))
  ;; General
  (add-to-map "<SPC> f f" 'find-file)
  ;; Code
  (add-to-map-when-inserting "TAB" 'company-complete-common-or-cycle)
  ;; Org
  (add-to-map "<SPC> o c c" 'org-capture)
  (add-to-map "<SPC> o c i" 'org-clock-in)
  (add-to-map "<SPC> o c o" 'org-clock-out)
  ;; Angular
  (add-to-map
   "<SPC> n g g c"
   (lambda ()
     (interactive)
     (shell-command-on-region
      (point-min)
      (point-max)
      (concat "ng g c "
              (read-string "Generate component with name: "))))))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Handle SSH pw on Windows
(setenv "SSH_ASKPASS" "git-gui--askpass")

(use-package company
  :bind
  (:map company-active-map
        ("RET" . nil)
        ([return] . nil)
        ("TAB" . company-complete-selection)
        ([tab] . company-complete-selection)
        ("<right>" . company-complete-common))
  :hook
  (after-init . global-company-mode)
  :custom
  (company-dabbrev-downcase nil)
  (company-idle-delay .2)
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (company-tooltip-align-annotations t))

(use-package flycheck
  :after js2-mode
  :init
  (add-hook 'js2-mode-hook 'flycheck-mode))

(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (add-to-map "<SPC> p" 'projectile-command-map)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien) ;; Fix indexing freeze on Windows
  (setq projectile-git-submodule-command nil) ;; Fix 'tr' is not recognized as an internal or external command
  )

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode 1))

(use-package eyebrowse
  :diminish eyebrowse-mode
  :config (progn
            (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
            (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
            (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
            (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
            (eyebrowse-mode 1)
            (setq eyebrowse-new-workspace t)))

;; HTML

(require 'emmet-mode)

(add-hook 'mhtml-mode-hook 'emmet-mode)

(require 'lsp-ui)
(require 'company-lsp)

;; JavaScript

(require 'js2-mode)
(require 'flycheck)

(use-package js2-mode
  :init
  :config
  (setq-default js-indent-level 2)
  (setq-default js2-basic-offset 2))

(require 'rjsx-mode)
(require 'lsp-mode)

(use-package rjsx-mode
  :delight "-- React "
  :init
  (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
  :config
  (setq-default js-indent-level 2)
  (setq-default js2-basic-offset 2)
  :config
  ;; From https://emacs.stackexchange.com/a/33544/690
  (defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
    "Workaround sgml-mode and follow airbnb component style."
    (save-excursion
      (beginning-of-line)
      (if (looking-at-p "^ +\/?> *$")
          (delete-char sgml-basic-offset))))
  (with-eval-after-load 'lsp-clients
    (add-to-list 'lsp-language-id-configuration '(rjsx-mode . "javascript"))
    (add-hook 'rjsx-mode-hook #'lsp))
  (fix-lsp-company-prefix))

(use-package lsp-mode
  :delight "-- LSP "
  :defer t
  :config
  (progn
    (require 'lsp-clients)
    (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
    (setf lsp-eldoc-render-all nil)
    (setq lsp-inhibit-message t)
    (setq lsp-message-project-root-warning t)
    (setf lsp-prefer-flymake nil)
    (setq lsp-enable-completion-at-point nil)
    ;; Need to npminstall typescript-language-server'
    ;; https://github.com/emacs-lsp/lsp-mode/issues/588
    (setq lsp-clients-typescript-server "typescript-language-server"
          lsp-clients-typescript-server-args '("--stdio"))))

(use-package lsp-ui
  :after lsp-mode
  :init
  (setf lsp-ui-sideline-enable nil)
  (setf lsp-ui-doc-enable nil)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package company-lsp
  :after lsp-mode
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-enable-recompletion t)
  (setq company-lsp-async t)
  (setq company-lsp-cache-candidates t))

(use-package prettier-js
  :after rjsx-mode
  :config
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'tide-mode-hook 'prettier-js-mode)
  (setq prettier-js-args '("--trailing-comma" "all" "--single-quote" "true")))

;; TypeScript

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :init
  (setq typescript-indent-level 2)
  (setq tide-format-options '(:indentSize 2 :tabSize 2))
  :hook ((typescript-mode . setup-tide-mode)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; Org

(use-package org
  :ensure nil
  :custom
  (org-descriptive-links nil)
  (org-edit-src-content-indentation 0)
  (org-edit-src-persistent-message nil)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-src-window-setup 'current-window)
  (org-startup-folded nil)
  (org-startup-truncated nil)
  (org-support-shift-select 'always)
  :config
  (setq org-directory "/mnt/c/Users/Andre/Documents/Dropbox/Notas")
  (setq org-agenda-files (list org-directory))
  (setq org-default-notes-file (concat org-directory "/captured-notes.org")))

(use-package org-journal
  :ensure t
  :defer t
  :custom
  (org-journal-dir "/mnt/c/Users/Andre/Documents/journal/")
  (org-journal-date-format "%A, %d %B %Y"))

;; Fun
(use-package wttrin
  :config
  (setq wttrin-default-cities '("Sao Paulo" "Guaruja" "Berlin"))
  (setq wttrin-default-accept-language '("Accept-Language" . "pt-BR")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0.2)
 '(company-minimum-prefix-length 1)
 '(company-require-match nil)
 '(company-tooltip-align-annotations t)
 '(custom-safe-themes
   (quote
    ("3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "4780d7ce6e5491e2c1190082f7fe0f812707fc77455616ab6f8b38e796cbffa9" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "b0fd04a1b4b614840073a82a53e88fe2abc3d731462d6fde4e541807825af342" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "4e132458143b6bab453e812f03208075189deca7ad5954a4abb27d5afce10a9a" "44247f2a14c661d96d2bff302f1dbf37ebe7616935e4682102b68c0b6cc80095" default)))
 '(ivy-count-format "(%d/%d) ")
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote full))
 '(org-agenda-files nil)
 '(org-descriptive-links nil)
 '(org-edit-src-content-indentation 0)
 '(org-edit-src-persistent-message nil)
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-journal-date-format "%A, %d %B %Y")
 '(org-journal-dir "c:/Users/Andre/Documents/journal/")
 '(org-src-window-setup (quote current-window))
 '(org-startup-folded nil)
 '(org-startup-truncated nil)
 '(org-support-shift-select (quote always))
 '(package-selected-packages
   (quote
    (evil-collection emmet-mode org-journal yasnippet evil-surround eyebrowse doom-themes tide typescript-mode prettier-js clojure-mode request dashboard minimal-theme eziam-theme wttrin twittering-mode evil-ediff ssh-agency delight all-the-icons-ivy counsel-projectile doom-modeline ivy-rich neotree evil-magit magit)))
 '(whitespace-style
   (quote
    (face tabs spaces lines newline empty indentation space-after-tab space-before-tab space-mark tab-mark))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(company-scrollbar-bg ((t (:background "#33b333b333b3"))))
;;  '(company-scrollbar-fg ((t (:background "#26e626e626e6"))))
;;  '(company-tooltip ((t (:inherit default :background "#4d4d4d4d4d4d"))))
;;  '(company-tooltip-annotation ((t (:inherit default :background "#4d4d4d4d4d4d" :foreground "#73b373b373b3"))))
;;  '(company-tooltip-annotation-selection ((t (:background "#33b333b333b3" :foreground "#73b373b373b3"))))
;;  '(company-tooltip-common ((t (:background "#1f381f381f38"))))
;;  '(company-tooltip-selection ((t (:background "#33b333b333b3")))))

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
