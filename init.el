(package-initialize)

(if (eq system-type 'windows-nt) 
    (defconst user-requested-font "Consolas-10")
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
))

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
   ))

(defconst user-requested-theme 'solarized-dark)
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
(load-theme user-requested-theme)
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
;; (electric-indent-mode 1)
(electric-pair-mode 1)
(setq electric-pair-preserve-balance 1)

(defun
  find-user-init-file ()
  "Edit the init file"
  (interactive)
  (find-file user-init-file))

;; Apply user requested bindings
(mapc
 (lambda (binding) (global-set-key (kbd (car binding)) (cadr binding)))
 user-requested-bindings)

;; On OS X exec the shell so we get actual env variabls
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; Set Fonts
(ignore-errors (set-face-attribute 'default nil :font user-requested-font))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(haskell-mode-hook (quote (turn-on-eldoc-mode turn-on-haskell-decl-scan turn-on-haskell-doc turn-on-haskell-indent)) t)
 '(indent-tabs-mode nil)
 '(inferior-lisp-program "clisp")
 '(magit-emacsclient-executable "C:/Users/e.hughes/Emacs/bin/emacsclient.exe")
 '(magit-use-overlays nil)
 '(scheme-program-name "guile")
 '(scss-compile-at-save nil)
 '(tramp-chunksize 200))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
;; Don't let Emacs hurt your ears
(setq visible-bell t)

(powerline-default-theme)

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
;;(load-file "~/.emacs.d/pymacs.el")
;;(require 'pymacs)
;;(pymacs-load "ropemacs" "rope-")
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
