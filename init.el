(setenv "LDFLAGS" "-L/opt/homebrew/opt/openssl@3/lib")
(setenv "CFLAGS" "-I/opt/homebrew/opt/openssl@3/include")

(add-to-list 'exec-path "~/.nix-profile/bin")
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/.cargo/bin")
(add-to-list 'exec-path "~/go/bin")
(add-to-list 'exec-path "/opt/homebrew/Cellar/libgccjit/13.2.0/lib/gcc/current/")

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
(setq recentf-max-menu-items 50
	  recentf-max-saved-items 50)

(setq make-backup-files nil)

(setq inhibit-startup-screen t
	  use-dialog-box nil
	  ns-pop-up-frames nil)

(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000
	  gc-cons-percentage 0.5)

(setq initial-scratch-message nil)

(setq dired-dwim-target t)

(setq column-number-mode t)
(blink-cursor-mode -1)

(pixel-scroll-mode)
(setq pixel-dead-time 0)
(setq pixel-resolution-fine-flag t)
(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil)

(setq gamegrid-glyph-height-mm 7)

(add-hook 'tetris-mode-hook 'visual-fill-column-mode)

(let ((sh-path "/run/current-system/sw/bin/fish"))
  (if (file-exists-p sh-path)
	  (setq-default explicit-shell-file-name sh-path)
	(message "fish shell not found")))

(setq ns-alternate-modifier 'meta
	  ns-right-alternate-modifier 'none)

(global-set-key (kbd "C-c r") #'recentf)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'control))

(setq mouse-autoselect-window t)

(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-0") #'delete-window)

(global-set-key [wheel-right] #'(lambda () (interactive) (scroll-left 4)))
(global-set-key [wheel-left] #'(lambda () (interactive) (scroll-right 4)))

(defun user/get-current-buffer-directory ()
  "Get the directory of the current buffer."
  (if (and (buffer-file-name)
           (file-exists-p (buffer-file-name)))
      (file-name-directory (buffer-file-name))
    default-directory))

(defun user/macos-fullscreen-tile-terminal ()
  "Fullscreen tile Terminal window to the right half of the screen with the current directory opened."
  (interactive)
  (let* ((current-dir (user/get-current-buffer-directory))
         (escaped-dir (shell-quote-argument (expand-file-name current-dir))))
    
		(start-process 
     "terminal-tile" 
     nil 
     "osascript" 
     "-e" 
     (format "
tell application \"Terminal\"
    do script \"cd %s\"
end tell

tell application \"System Events\" to tell process \"Terminal\"
    set frontmost to true
        
    tell menu bar 1 to tell menu \"Window\" of menu bar item \"Window\"
        click menu item \"Right of Screen\" of menu \"Full Screen Tile\" of menu item \"Full Screen Tile\"
    end tell
end tell" escaped-dir))
		))

(global-set-key (kbd "C-c t") 'user/macos-fullscreen-tile-terminal)

(setq visible-bell t)
(setq-default tab-width 2)

(global-set-key (kbd "C-x s") #'replace-string)

(require 'org)

(setq org-startup-indented t
	  org-confirm-babel-evaluate nil
	  org-edit-src-content-indentation 0
	  org-image-actual-width nil
	  org-support-shift-select t)

(defun user/indent-org-block ()
  (interactive)
  (when (org-in-src-block-p)
    (org-edit-special)
    (indent-region (point-min) (point-max))
    (org-edit-src-exit)))

(define-key org-mode-map (kbd "C-i") #'user/indent-org-block)

(defun toggle-org-html-export-on-save ()
  (interactive)
  (if (memq 'org-html-export-to-html after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'org-html-export-to-html t)
        (message "Disabled org html export on save for current buffer..."))
    (add-hook 'after-save-hook 'org-html-export-to-html nil t)
    (message "Enabled org html export on save for current buffer...")))

(setq-default TeX-engine 'xetex)

(setq c-basic-offset 2)
(setq c-indent-level 2)
(setq c-ts-mode-indent-offset 2)
(setq tab-width 2)

(defun user/outline-level ()
  "Custom outline level based on the comment labels."
  (looking-at outline-regexp)
  (let ((match (match-string 0)))
    (if (null match) 1
      (length match))))

(add-hook 'c-mode-common-hook
          (lambda ()
            (outline-minor-mode 1)
            (setq outline-regexp "//=:[a-zA-Z]+\\(:[a-zA-Z]+\\)?")
            (setq outline-level 'user/outline-level)
						(hide-body)))

(global-set-key (kbd "C-c i") 'outline-hide-body)
(global-set-key (kbd "C-c o") 'outline-hide-other)
(global-set-key (kbd "C-c p") 'outline-show-entry)
(global-set-key (kbd "C-c u") 'outline-show-all)

(setq ispell-program-name "aspell") 
(setq ispell-list-command "list")

(setq epa-pinentry-mode 'loopback)

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
		(yaml "https://github.com/ikatyang/tree-sitter-yaml")
		(nix "https://github.com/nix-community/tree-sitter-nix")))

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-gruvbox t)

  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

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

(use-package dashboard
  :after (projectile all-the-icons)
  :custom
  (dashboard-items '((recents  . 7)
                     (projects . 4)
                     (agenda . 2)
                     (registers . 2)
					 (bookmarks . 4)))
  (dashboard-icon-type 'all-the-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-startup-banner 'ascii)
  (dashboard-banner-logo-title nil)
  (dashboard-set-init-info nil)
  :config
  (dashboard-setup-startup-hook))

(use-package dired-subtree
	:config
	(define-key dired-mode-map "i" 'dired-subtree-toggle)
	(advice-add 'dired-subtree-toggle :after (lambda ()
                                             (interactive)
                                             (when all-the-icons-dired-mode
                                               (revert-buffer)))))

;; (use-package vterm
;; 	:custom
;; 	(shell-file-name explicit-shell-file-name))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package move-text)
(move-text-default-bindings)

(use-package queue)
(use-package undo-tree
	:after queue
	:config
	(global-undo-tree-mode))

(use-package avy
	:config
	(global-set-key (kbd "C-w") 'avy-goto-word-0))

;; (use-package god-mode
;;   :config
;;   (global-set-key (kbd "<escape>") #'god-mode)

;;   (define-key god-local-mode-map (kbd "z") #'repeat)
  
;;   (define-key god-local-mode-map (kbd "[") #'backward-paragraph)
;;   (define-key god-local-mode-map (kbd "]") #'forward-paragraph))

;; (defun user/god-mode-update-cursor ()
;;   (if (or god-local-mode buffer-read-only)
;; 	  (set-cursor-color "cyan")
;; 	(set-cursor-color "white")))

;; (add-hook 'post-command-hook #'user/god-mode-update-cursor)

(use-package devil)

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-s-c") 'mc/edit-lines)
  (global-set-key (kbd "C-(") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-)") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c C-(") 'mc/mark-all-like-this)
  (global-set-key (kbd "s-<mouse-1>") 'mc/add-cursor-on-click))

(use-package kaomel
  :straight  (:type git :host github :repo "gicrisf/kaomel")
	:custom
	(kaomel-path "~/.emacs.d/kaomoji.json"))

(global-set-key (kbd "C-s-k") #'kaomel-insert)

(use-package yasnippet
  :custom
  (yas-snippet-dirs '(;; "~/.emacs.d/user_snippets"
					  "~/.emacs.d/AndreaCrotti_snippets"))
  :config
  (yas-global-mode 1))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("C-c c" . jinx-correct)
         ("C-c l" . jinx-languages)))

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

(use-package nix-ts-mode
	:mode "\\.nix\\'")

(setenv "PYTHONIOENCODING" "utf8")

(setq python-shell-interpreter-args "-m asyncio")

(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")))

(use-package py-autopep8
	:custom
	(py-autopep8-options '("--max-line-length=80")))

(use-package pyvenv
  :config
  (pyvenv-tracking-mode)
  (add-hook 'pyvenv-post-activate-hooks 'lsp))

(use-package ein)

(use-package typescript-mode)
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
	:config
	(add-hook 'js2-mode-hook #'setup-tide-mode)
	;; configure javascript-tide checker to run after your default javascript checker
	(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))

(defun user/clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
            (lambda ()
              (when (locate-dominating-file "." ".clang-format")
                (clang-format-buffer))
              ;; Continue to save.
              nil)
            nil
            ;; Buffer local hook.
            t))

(use-package clang-format
	:hook
	((c-mode . (lambda () (user/clang-format-save-hook-for-this-buffer)))))

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :custom
  (rustic-format-on-save t))

(use-package zig-mode)

(use-package go-mode)

(use-package ob-go
	:after go-mode)

(use-package fgscript-mode
  :straight (:type git :host github :repo "mandriota/fgscript"
                   :files ("editors/emacs/fgscript-mode.el")))

(use-package fish-mode
  :mode ("\\.fish$")
  :config
  (setq fish-enable-auto-indent t))

(use-package ob-fish
  :straight  (:type git :host github :repo "takeokunn/ob-fish"))

(use-package csv-mode)

(use-package typst-ts-mode
  :straight (:type git :host codeberg :repo "meow_king/typst-ts-mode")
  :custom
  (typst-ts-mode-watch-options "--open"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)
	 (shell . t)))

(use-package dap-mode)

(use-package flycheck)

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
	(lsp-enable-file-watchers nil)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all nil)
  ;;(lsp-inlay-hint-enable t)
	(lsp-headerline-breadcrumb-enable nil)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
	(lsp-go-analyses '((simplifycompositelit . :json-false)))
	:hook ((lsp-mode . lsp-enable-which-key-integration)
				 (typescript-mode . lsp)
				 (javascript-mode . lsp)
				 (python-mode . lsp)
				 (python-mode . py-autopep8-mode)
				 (elisp-mode . lsp)
				 (go-mode . lsp)
				 (rustic . lsp)
				 (c-mode . lsp)
				 (c-ts-mode . lsp)
				 (zig . lsp))
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
	(require 'dap-cpptools))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable t)
	(lsp-ui-sideline-enable nil))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package company
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "C-c y") 'company-yasnippet)

  (company-tng-configure-default))

(use-package visual-fill-column
  :commands visual-fill-column-mode
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-width 90))

(use-package nov
  :custom
  (nov-text-width t)
  :config
	(defun user/nov-font-setup ()
	  (face-remap-add-relative 'variable-pitch :family "Georgia"
                               :height 1.2))
	(add-hook 'nov-mode-hook 'user/nov-font-setup)
	
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)

  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(unless (server-running-p)
	(server-start))
