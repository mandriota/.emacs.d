(require 'server)
(when (server-running-p)
	(shell-command "tmux new-session -d 'emacsclient -c'")
	(kill-emacs))

;; appereance
(add-to-list 'default-frame-alist '(undecorated-round . t))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(left-fringe . 5))
(add-to-list 'default-frame-alist '(right-fringe . 5))

;; theme
(defun system-dark-mode-p ()
  "Return non-nil if system is using dark mode."
  (cond
   ((eq system-type 'darwin)
    (executable-find "defaults")
		(string= 
     "Dark"
     (string-trim
			(shell-command-to-string
       "defaults read -g AppleInterfaceStyle 2>/dev/null || echo Light"))))
   
   ((and (eq system-type 'gnu/linux)
         (executable-find "gsettings"))
    (string-match-p 
     "prefer-dark"
     (string-trim
      (shell-command-to-string
       "gsettings get org.gnome.desktop.interface color-scheme"))))
   
   ((and (eq system-type 'gnu/linux)
         (executable-find "kreadconfig5"))
    (string= 
     "Breeze Dark"
     (string-trim
      (shell-command-to-string
       "kreadconfig5 --group General --key ColorScheme"))))
   
   ((and (eq system-type 'windows-nt)
         (executable-find "powershell"))
    (string= 
     "0"
     (string-trim
      (shell-command-to-string
       "powershell -command \"(Get-ItemProperty -Path 'HKCU:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize' -Name 'AppsUseLightTheme').AppsUseLightTheme\""))))
   (t nil)))

(load-theme (if (system-dark-mode-p) 'wombat 'leuven))

(set-face-attribute 'default nil :font "Fira Code Retina" :height 135)

;; packages build options
(setenv "LSP_USE_PLISTS" "true")

;; other
(setq package-enable-at-startup nil)

