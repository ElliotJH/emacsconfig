(package-initialize)

(if (eq system-type 'windows-nt) 
    (defconst user-requested-font "Consolas-11")
  (defvar user-requested-font "EspressoMono-Regular-12"))

;; Set a list of packages that I want
(defconst user-requested-packages
  '(
    helm
    solarized-theme
    auto-complete
    magit
    paredit
    ac-dabbrev
    auto-complete-clang
    outline-magic
    base16-theme
    rinari
    haskell-mode
    smartparens
    expand-region
    geiser
    ac-geiser
    exec-path-from-shell
    haskell-mode
    go-mode
    powerline
    projectile
    helm-projectile
    go-autocomplete
    smart-tabs-mode
    company
    fold-dwim
    expand-region
    wrap-region
    multiple-cursors
    ace-jump-mode
    org-plus-contrib
    autopair
    ace-window
))

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'logmode)

;; Define keyboard bindings that I like
;;;; C-C letter are owned by the user
;; Function 5 to Function 9 are owned by the user
(defconst user-requested-bindings
  '(
   ("C-c e" er/expand-region)
   ("C-c p" paredit-mode)
   ("C-c m" mc/edit-lines)
   ("C-c C-m" mc/edit-beginnings-of-lines)
   ("C-c i" find-user-init-file)
   ("C-c p" helm-projectile)
   ("C-c TAB" company-complete)
   ("C-x C-f" helm-find-files)
   ("C-c s" helm-swoop)
   ("C-c h" helm-mini)
   ("<f2> w" ace-jump-word-mode)
   ("<f2> l" ace-jump-line-mode)
   ("<f2> c" ace-jump-char-mode)
   ("C-c w" ace-window)
   ("C-c SPC" helm-resume)
   ("C-c C-r" revert-buffer)
   ("C-c l" logmode/helm)
   ))

;; Apply user requested bindings
(mapc
 (lambda (binding) (global-set-key (kbd (car binding)) (cadr binding)))
 user-requested-bindings)

(defconst user-requested-theme 'sanityinc-tomorrow-bright)
(defconst user-requested-autocomplete-tool 'helm)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(let
    ((themes '(
	       "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
	       "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb"
	       "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879")))
  (mapc (lambda (theme) (add-to-list 'custom-safe-themes theme)) themes))


;; Install user-requested-packages
(mapcar (lambda (package) (if (not (package-installed-p package))
			    (package-install package))) user-requested-packages)

;; Disable ugly shit
(tool-bar-mode -1)
(scroll-bar-mode -1)

(if (eq 'helm user-requested-autocomplete-tool)
    (helm-mode))
;;Define other modes here...

;; Set the theme
(load-theme user-requested-theme t)
(enable-theme user-requested-theme)

;; Autocomplete stuff
;; (auto-complete)
;; (global-auto-complete-mode)
;;(global-company-mode)
(global-linum-mode)

;; Bells are pointless and annoying
(setq ring-bell-function 'ignore)

;; Hide my backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))



;; Fix OS X
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
(set-variable 'magit-emacsclient-executable "/usr/local/bin/emacsclient")

;; Setup bindings

(defun
  find-user-init-file ()
  "Edit the init file"
  (interactive)
  (find-file user-init-file))



;; On OS X exec the shell so we get actual env variabls
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; Set Fonts
(set-face-attribute 'default nil :font user-requested-font)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#eaeaea" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#000000"))
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
 '(custom-safe-themes
   (quote
    ("dd1c2037ef66ee3f706bf687ccdb75fd7511bcc11d00fdb35d0535d80beb8b0a" "eb26ba64aa8e3726cb01b9de1a2f33647d4e0d6c8e36686d43a64d1cd76b7df1" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(custom-theme-load-path
   (quote
    ("c:/Users/e.hughes/AppData/Roaming/.emacs.d/elpa/color-theme-sanityinc-tomorrow-20140906.332/" "c:/Users/e.hughes/AppData/Roaming/.emacs.d/elpa/solarized-theme-20141004.2115/" custom-theme-directory "h:/Code/base16-builder/output/emacs" t)))
 '(fci-rule-color "#eee8d5")
 '(haskell-mode-hook
   (quote
    (turn-on-eldoc-mode turn-on-haskell-decl-scan turn-on-haskell-doc turn-on-haskell-indent)) t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(indent-tabs-mode nil)
 '(inferior-lisp-program "clisp")
 '(magit-diff-use-overlays nil)
 '(magit-emacsclient-executable "C:/Users/e.hughes/Emacs/bin/emacsclient.exe")
 '(magit-git-executable "c:/Users/e.hughes/AppData/Local/Programs/Git/bin/git")
 '(magit-use-overlays nil)
 '(python-shell-interpreter "python")
 '(scheme-program-name "guile")
 '(scss-compile-at-save nil)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sql-sqlite-options (quote ("-interactive")))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tramp-chunksize 200)
 '(url-proxy-services (quote (("\"http\"" . "\"srv-0067:8080\""))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-keyword-face ((t (:foreground "#b9ca4a" :slant italic)))))

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)


;; Tramp
(require 'tramp)
(setq tramp-default-method "ssh")
(global-hl-line-mode 1)

(setq inhibit-startup-echo-area-message "elliot")
;; Prevent the cursor from blinking
(blink-cursor-mode 0)
;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq initial-buffer-choice "c:\\Users\\e.hughes\\Code\\projects.org")
;; Don't let Emacs hurt your ears
(setq visible-bell t)

;;(powerline-default-theme)

(dolist ($hook '(css-mode-hook scss-mode-hook))
  (add-hook
   $hook (lambda ()
	   (local-set-key (kbd "C-c j") 'helm-css-scss))))

(add-hook 'go-mode (lambda() (add-hook 'before-save-hook 'gofmt-before-save)))

(setq c-basic-offset 4)
(setq cperl-indent-level 4)
(setq tab-width 4)

(smart-tabs-mode)
(visual-line-mode)

(setq default-directory "C:/Users/e.hughes/Code")
(defun start-pymacs ()
  (interactive)
  (load-file "~/.emacs.d/pymacs.el")
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-"))
(setq company-idle-delay 0)

(setq comint-completion-addsuffix '("\\" "."))


(defun
  switch-color-theme ()
  "Switch between dark-light solarized"
  (interactive)
  (if (null custom-enabled-themes)
    (progn 
      (load-theme 'solarized-dark)
      (enable-theme 'solarized-dark))
  (if (member 'solarized-dark custom-enabled-themes)
      (progn 
        (disable-theme 'solarized-dark)
        (enable-theme 'solarized-light))
    (if (member 'solarized-light custom-enabled-themes)
        (progn 
          (disable-theme 'solarized-light)
          (enable-theme 'solarized-dark))
      (progn 
        (mapc 'disable-theme custom-enabled-themes)
        (enable-theme 'solarized-dark))))))


(put 'narrow-to-region 'disabled nil)
(setq org-agenda-files "~/.agenda.orgmode")


(defvar org-map-menu (make-sparse-keymap "Org Mode"))
(define-key org-map-menu (kbd "s") '(menu-item "Store Link" org-store-link :help "Store a link"))
(define-key org-map-menu (kbd "c") '(menu-item "Capture" org-capture :help "Capture something"))
(define-key org-map-menu (kbd "a") '(menu-item "Agenda" org-agenda :help "Open agenda menu"))
(define-key org-map-menu (kbd "b") '(menu-item "Buffer switch" org-iswitchb :help "Switch to org buffer"))

(global-set-key (kbd "C-c o") org-map-menu)

(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "~/.ispell")

(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
(electric-indent-mode 1)
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
(autopair-mode 1)

(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

