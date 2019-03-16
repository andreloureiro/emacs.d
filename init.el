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


;; General

(set-default-coding-systems 'utf-8)

(setq-default line-spacing 2)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(menu-bar-mode -1)

(setq make-backup-files nil)
(setq auto-save-default nil)

(global-linum-mode 1)
(setq linum-format "%3d \u2502 ")

(use-package monokai-theme
  :config
  (load-theme 'monokai t))

(use-package zoom
  :config
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618)))
  (zoom-mode t))

(set-face-attribute 'default nil :font "Fira Code-10:antialias=natural")

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

;; Fira ligatures

;; (when (window-system)
;;   (set-frame-font "Fira Code"))
;; (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;; 	       (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
;; 	       (36 . ".\\(?:>\\)")
;; 	       (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;; 	       (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;; 	       (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
;; 	       (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
;; 	       (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;; 	       (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
;; 	       (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
;; 	       (48 . ".\\(?:x[a-zA-Z]\\)")
;; 	       (58 . ".\\(?:::\\|[:=]\\)")
;; 	       (59 . ".\\(?:;;\\|;\\)")
;; 	       (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
;; 	       (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
;; 	       (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
;; 	       (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
;; 	       (91 . ".\\(?:]\\)")
;; 	       (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;; 	       (94 . ".\\(?:=\\)")
;; 	       (119 . ".\\(?:ww\\)")
;; 	       (123 . ".\\(?:-\\)")
;; 	       (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
;; 	       (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
;; 	       )
;; 	     ))
;;   (dolist (char-regexp alist)
;;     (set-char-table-range composition-function-table (car char-regexp)
;; 			  `([,(cdr char-regexp) 0 font-shape-gstring]))))

(require 'neotree)
;; (global-set-key (kbd "<SPC> t t") 'neotree-toggle)

;; (use-package neotree
;;   :config
;;   (global-set-key (kbd "<SPC>") 'neotree-enter))

(use-package smartparens
  :config
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'rjsx-mode-hook 'smartparens-mode))

;; (use-package emacs-lisp :ensure nil :delight "ξ ")

(require 'ivy)
(require 'counsel)

;; (use-package ivy
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq enable-recursive-minibuffers t))

;; (deftheme minimal-theme)

;; (use-package counsel
;;   :after ivy
;;   :config
;;   (global-set-key (kbd "M-x") 'counsel-M-x)
;;   (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;   )

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

;; Custom mode line

;; (setq mode-line-format
;;       (list
;;        mode-line-buffer-identification
;;        " "
;;        projectile-project-name
;;        "%b:"
;;        default-file-name-coding-system
;;        "%p (%l, %c)"
;;        (vc-working-revision (buffer-file-name (current-buffer)))
;;        ))

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
  (add-to-map "<SPC> b e" 'eval-buffer)
  (add-to-map "<SPC> b r" 'rename-buffer)
  (add-to-map "<SPC> b k" 'kill-buffer)
  (add-to-map "<SPC> b K" 'kill-other-buffers)
  (add-to-map "g d" 'lsp-ui-peek-find-definitions)
  (add-to-map "g r" 'lsp-ui-peek-find-references)
  (add-to-map "<SPC> g s" 'magit-status)
  ;; eshell
  (add-to-map "<SPC> e n" (lambda ()
			    (interactive)
			    (eshell)
			    (rename-buffer (read-string "Enter the buffer name: "))))
  ;; general
  (add-to-map "<SPC> f f" 'find-file)
  ;; code
  (add-to-map-when-inserting "TAB" 'company-complete-common-or-cycle)
  )

;; Handle SSH pw on Windows
(setenv "SSH_ASKPASS" "git-gui--askpass")

(use-package company
  :defer 0.5
  :delight
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package flycheck
  :after js2-mode
  :init
  (add-hook 'js2-mode-hook 'flycheck-mode))

(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (add-to-map "<SPC> p" 'projectile-command-map))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode 1))

(require 'lsp)
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
  ;; tomado de https://emacs.stackexchange.com/a/33544/690 mejora el sangrado
  ;; de lineas
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
    ;; arreglo rápido; hay que correr `yarn global add
    ;; typescript-language-server'
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-count-format "(%d/%d) ")
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote full))
 '(package-selected-packages
   (quote
    (evil-ediff ssh-agency delight all-the-icons-ivy counsel-projectile doom-modeline zoom ivy-rich monokai-theme neotree evil-magit magit)))
 '(zoom-mode t nil (zoom))
 '(zoom-size (quote (0.618 . 0.618))))
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
