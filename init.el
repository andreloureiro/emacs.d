(defconst emacs-dir "~/.emacs.d")
(defconst packages-dir (concat emacs-dir "/packages"))
(defconst packages '("ag.el"
		     "company-lsp"
                     "company-mode"
                     "evil"
                     "evil-magit"
                     "f"
                     "flycheck"
                     "ht"
                     "lsp-mode"
                     "lsp-ui"
                     "magit/lisp"
                     "dash"
                     "js2-mode"
                     "magit-popup"
                     "markdown-mode"
                     "minimal-theme"
                     "projectile"
                     "rjsx-mode"
                     "s"
                     "spinner"
                     "smartparens"
                     "swiper"
                     "transient/lisp"
                     "with-editor"
                     "use-package"))

(dolist (pkg packages)
  (add-to-list 'load-path (concat (file-name-as-directory packages-dir) pkg)))

(require 'use-package)

;; Utils
(defun fix-lsp-company-prefix ()
  "fix lsp-javascript company prefix
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


;; General

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(menu-bar-mode -1)

(setq make-backup-files nil)
(setq auto-save-default nil)

(global-linum-mode 1)
(setq linum-format "%3d \u2502 ")

(require 'minimal-theme)
(use-package minimal-theme)

(set-face-attribute 'default t :font "Fira Code")

(require 'color)

(let ((bg (face-attribute 'default :background)))
 (custom-set-faces
  `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 20)))))
  `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
  `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
  `(company-tooltip-selection ((t (:background , (color-lighten-name bg 10)))))
  `(company-tooltip-annotation ((t (:inherit default :background , (color-lighten-name bg 20) :foreground , (color-lighten-name bg 35)))))
  `(company-tooltip-annotation-selection ((t (:background , (color-lighten-name bg 10) :foreground , (color-lighten-name bg 35)))))
  `(company-tooltip-common ((t (:background , (color-lighten-name bg 2)))))))

;; Fira ligatures

(when (window-system)
  (set-frame-font "Fira Code"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(use-package smartparens
  :config
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'rjsx-mode-hook 'smartparens-mode))

(require 'ivy)
(require 'counsel)

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  )

(deftheme minimal-theme)

(use-package counsel
  :after ivy
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  )

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
  (add-to-map "<SPC> b b" 'list-buffers)
  (add-to-map "<SPC> b r" 'rename-buffer)
  (add-to-map "<SPC> b k" 'kill-buffer)
  (add-to-map "g d" 'lsp-ui-peek-find-definitions)
  (add-to-map "g r" 'lsp-ui-peek-find-references)
  (add-to-map "<SPC> g s" 'magit-status)
  ;; eshell
  (defun open-shell-and-name-it ()
    (interactive)
    (eshell)
    (rename-buffer))
  (add-to-map "<SPC> e n" (lambda ()
			    (interactive)
			    (eshell)
			    (rename-buffer (read-string "Enter the buffer name: "))))
  ;; general
  (add-to-map "<SPC> f f" 'find-file)
  ;; code
  (add-to-map-when-inserting "TAB" 'company-complete-common-or-cycle)
  )

(use-package company
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
