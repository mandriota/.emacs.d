(setenv "LDFLAGS" "-L/opt/homebrew/opt/openssl@3/lib")
(setenv "CFLAGS" "-I/opt/homebrew/opt/openssl@3/include")

(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/.cargo/bin")
(add-to-list 'exec-path "~/go/bin")

(setenv "PATH" (mapconcat #'identity exec-path path-separator))

(setenv "PATH"
		(concat
		 (getenv "PATH")
		 "/opt/homebrew/opt/llvm@14/bin" path-separator
		 "~/.emacs.d/bin" path-separator
		 "/opt/local/bin" path-separator
		 "/opt/local/sbin" path-separator
		 "/opt/homebrew/sbin" path-separator
		 "/System/Cryptexes/App/usr/bin" path-separator
		 "/usr/bin" path-separator
		 "/bin" path-separator
		 "/usr/sbin" path-separator
		 "/sbin" path-separator
		 "/usr/local/go/bin" path-separator
		 "/Library/TeX/Distributions/Programs/texbin"))

(add-hook 'after-init-hook (lambda () (message (emacs-init-time))))

(scroll-bar-mode -1)
(set-fringe-mode 5)
(tool-bar-mode -1)
(tooltip-mode -1)

(setq visible-bell t)
(setq-default tab-width 4)

(set-face-attribute 'default nil :font "Fira Code Retina" :height 135)

(global-auto-revert-mode 1)
(save-place-mode 1)
(recentf-mode 1)

(setq inhibit-startup-screen t
	  use-dialog-box nil)

(setq ns-alternate-modifier 'meta
	  ns-right-alternate-modifier 'none)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

(load-theme 'wombat)

(setq org-startup-indented t
	  org-confirm-babel-evaluate nil
	  org-edit-src-content-indentation 0
	  org-src-tab-acts-natively t
	  org-src-preserve-indentation t
	  org-image-actual-width nil)

(defun user/indent-org-block ()
  (interactive)
  (when (org-in-src-block-p)
	(org-edit-special)
	(indent-region (point-min) (point-max))
	(org-edit-src-exit)))

(define-key org-mode-map (kbd "C-i") #'user/indent-org-block)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
						 ("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t  ; if nil, bold is universally disabled
		doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;;(doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package god-mode)
(god-mode)

(global-set-key (kbd "<escape>") #'god-mode-all)

(defun user/god-mode-update-cursor ()
  (if (or god-local-mode buffer-read-only)
	  (progn (setq cursor-type 'box) (set-cursor-color "#ff007f"))
	(progn (setq cursor-type 'bar) (set-cursor-color "#ffffff"))))

(add-hook 'post-command-hook #'user/god-mode-update-cursor)

(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode))

(use-package go-mode
  :mode "\\.go\\'")
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq rustic-format-on-save t))
(use-package lsp-mode
  :hook (
		 (go-mode . lsp)
		 (rustic . lsp)
		 (elisp-mode . lsp)
		 (c-mode . lsp)
		 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package go-autocomplete)
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0
		company-minimum-prefix-length 1
		company-selection-wrap-around t)
  (company-tng-configure-default))
