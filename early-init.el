(require 'server)
(when (server-running-p)
	(shell-command "tmux new-session -d 'emacsclient -c'")
	(kill-emacs))

;; appereance
(add-to-list 'default-frame-alist '(undecorated-round . t))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(left-fringe . 5))
(add-to-list 'default-frame-alist '(right-fringe . 5))

(load-theme 'wombat)

(set-face-attribute 'default nil :font "Fira Code Retina" :height 135)

;; packages build options
(setenv "LSP_USE_PLISTS" "true")

;; other
(setq package-enable-at-startup nil)

