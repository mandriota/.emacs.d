(setenv "LDFLAGS" "-L/opt/homebrew/opt/openssl@3/lib")
(setenv "CFLAGS" "-I/opt/homebrew/opt/openssl@3/include")

(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/.cargo/bin")
(add-to-list 'exec-path "~/go/bin")

(setenv "PATH"
		(concat
		 (mapconcat #'identity exec-path path-separator)
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

(global-auto-revert-mode 1)
(save-place-mode 1)
(recentf-mode 1)

(setq inhibit-startup-screen t
	  use-dialog-box nil
	  ns-pop-up-frames nil)

(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000
	  gc-cons-percentage 0.5)

(setq initial-scratch-message nil)

(setq column-number-mode t)
(blink-cursor-mode -1)

(let ((sh-path "/opt/homebrew/bin/fish"))
  (if (file-exists-p sh-path)
	  (setq-default explicit-shell-file-name sh-path)
	(message "fish shell not found")))

(setq ns-alternate-modifier 'meta
	  ns-right-alternate-modifier 'none)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'control))

(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-0") #'delete-window)

(setq visible-bell t)
(setq-default tab-width 4)

(require 'org)

(setq org-startup-indented t
	  org-confirm-babel-evaluate nil
	  org-edit-src-content-indentation 0
	  org-src-tab-acts-natively t
	  org-src-preserve-indentation t
	  org-image-actual-width nil
	  org-support-shift-select t)

(defun user/indent-org-block ()
  (interactive)
  (when (org-in-src-block-p)
    (org-edit-special)
    (indent-region (point-min) (point-max))
    (org-edit-src-exit)))

(define-key org-mode-map (kbd "TAB") #'user/indent-org-block)

(setq ispell-program-name "aspell") 
(setq ispell-list-command "list")

(setq treesit-language-source-alist
	  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
		(c "https://github.com/tree-sitter/tree-sitter-c")
		(cmake "https://github.com/uyha/tree-sitter-cmake")
		(common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
		(cpp "https://github.com/tree-sitter/tree-sitter-cpp")
		(css "https://github.com/tree-sitter/tree-sitter-css")
		(csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
		(elisp "https://github.com/Wilfred/tree-sitter-elisp")
		(go "https://github.com/tree-sitter/tree-sitter-go")
		(go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
		(html "https://github.com/tree-sitter/tree-sitter-html")
		(js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
		(json "https://github.com/tree-sitter/tree-sitter-json")
		(lua "https://github.com/Azganoth/tree-sitter-lua")
		(make "https://github.com/alemuller/tree-sitter-make")
		(markdown "https://github.com/ikatyang/tree-sitter-markdown")
		(python "https://github.com/tree-sitter/tree-sitter-python")
		(r "https://github.com/r-lib/tree-sitter-r")
		(rust "https://github.com/tree-sitter/tree-sitter-rust")
		(toml "https://github.com/tree-sitter/tree-sitter-toml")
		(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
		(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
		(typst "https://github.com/uben0/tree-sitter-typst")
		(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
    	doom-themes-enable-italic t)
  (load-theme 'doom-nord t)

  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '())
  :hook prog-mode)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :after all-the-icons
  :straight (:type git :host github :repo "jtbm37/all-the-icons-dired")
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ivy-rich
  :after (all-the-icons ivy-rich)
  :init (all-the-icons-ivy-rich-mode 1))

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map))

(use-package elgrep)

(use-package dashboard
  :after (projectile all-the-icons)
  :config
  (setq dashboard-items '((recents  . 7)
                          (projects . 4)
                          (agenda . 2)
                          (registers . 2)
						  (bookmarks . 4)))
  (setq dashboard-icon-type 'all-the-icons
		;; dashboard-set-heading-icons t
		dashboard-set-file-icons t
		dashboard-startup-banner nil
		dashboard-banner-logo-title nil
		dashboard-set-init-info nil)
  ;; (setq )
  (dashboard-setup-startup-hook))

(use-package whole-line-or-region
  :config (whole-line-or-region-global-mode))

(use-package god-mode
  :config
  (global-set-key (kbd "<escape>") #'god-mode)

  (define-key god-local-mode-map (kbd "z") #'repeat)
  
  (define-key god-local-mode-map (kbd "[") #'backward-paragraph)
  (define-key god-local-mode-map (kbd "]") #'forward-paragraph))

(defun user/god-mode-update-cursor ()
  (if (or god-local-mode buffer-read-only)
	  (set-cursor-color "cyan")
	(set-cursor-color "white")))

(add-hook 'post-command-hook #'user/god-mode-update-cursor)

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-s-c") 'mc/edit-lines)
  (global-set-key (kbd "C-(") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-)") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c C-(") 'mc/mark-all-like-this)
  (global-set-key (kbd "s-<mouse-1>") 'mc/add-cursor-on-click))

(use-package insert-kaomoji)

(global-set-key (kbd "C-s-k") #'insert-kaomoji)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '(;; "~/.emacs.d/user_snippets"
						   ;; "~/.emacs.d/doom_snippets"
						   "~/.emacs.d/AndreaCrotti_snippets"))
  (yas-global-mode 1))

(use-package which-key
  :config
  (which-key-mode))

(use-package ivy
  :config
  (ivy-mode)

  (global-set-key (kbd "C-r") 'swiper-thing-at-point)
  (global-set-key (kbd "C-s") 'swiper))

(use-package counsel
  :after ivy
  :config
  (counsel-mode))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package magit)

(use-package go-mode
  :mode "\\.go\\'")
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq rustic-format-on-save t))
(use-package fish-mode)
;; (use-package racket-mode)
(use-package geiser-guile)
(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration)
		 ;; (racket-mode . lsp)
		 (geiser-guil . lsp)
		 (elisp-mode . lsp)
		 (go-mode . lsp)
		 (rustic . lsp)
		 (c-mode . lsp))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode
  :config
  (setq lsp-ui-imenu-auto-refresh t)
  (setq lsp-ui-imenu-refresh-delay 0.1))
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "C-c y") 'company-yasnippet)

  (setq company-idle-delay 0
		company-minimum-prefix-length 1
		company-selection-wrap-around t)
  (company-tng-configure-default))

(use-package ob-go)

(use-package typst-ts-mode
  :straight (:type git :host sourcehut :repo "meow_king/typst-ts-mode")
  :custom
  (typst-ts-mode-watch-options "--open"))

(use-package visual-fill-column
  :commands visual-fill-column-mode
  :config
  (setq visual-fill-column-center-text t)
  (setq visual-fill-column-width 90))

(use-package nov
  :config
	(defun user/nov-font-setup ()
	  (face-remap-add-relative 'variable-pitch :family "Georgia"
                               :height 1.2))
	(add-hook 'nov-mode-hook 'user/nov-font-setup)

  (setq nov-text-width t)

  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)

  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(server-start)
