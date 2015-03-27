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
    ace-window
    jedi
    god-mode
    key-chord
    smartparens
    hydra
    ))

;;(powerline-default-theme)

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'logmode)

(defhydra hydra-mc (:color blue)
  "mark"
  ("r" mc/mark-all-in-region-regexp "mark regexp")
  ("a" mc/mark-all-in-region "mark string")
  ("s" mc/mark-all-symbols-like-this "mark all symbols like this"))

(defhydra hydra-org-map-menu (:color blue)
  "Org:"
  ("t" find-user-tasks-file "open tasks file")
  ("s" org-store-link  "Store Link")
  ("c" org-capture  "Capture")
  ("a" org-agenda  "Agenda")
  ("b" org-iswitchb "Buffer Switch"))


(defhydra hydra-search-and-replace (:color blue)
  "replace"
  ("r" replace-regexp "replace regexp")
  ("R" query-replace-regexp "query replace regexp")
  ("s" replace-string "replace string")
  ("S" query-replace-string "query replace string")
  ("f" isearch-forward "isearch forward")
  ("F" isearch-forward-regexp "isearch forward regexp")
  ("b" isearch-backward "isearch backward")
  ("B" isearch-backward-regexp "isearch backward regexp"))

(defhydra hydra-ace-jump (:color blue)
  "ace jump"
  ("w" ace-jump-word-mode "jump to word")
  ("l" ace-jump-line-mode "jump to line")
  ("c" ace-jump-char-mode "jump to char")
  ("o" ace-window "jump to window"))

(defun create-temp-buffer ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "temp")))

;; Define keyboard bindings that I like
;;;; C-C letter are owned by the user
;; Function 5 to Function 9 are owned by the user
(defconst user-requested-bindings
  '(
    ("C-c g" magit-status)
    ("C-c x" hydra-search-and-replace/body)
    ("C-c m" hydra-mc/body)
    ("C-c e" er/expand-region)
    ("C-c p" paredit-mode)
    ("C-c i" find-user-init-file)
    ("C-c t" find-user-tasks-file)
    ("C-c p" helm-projectile)
    ("C-c TAB" company-complete)
    ("C-x C-f" helm-find-files)
    ("C-c s" helm-swoop)
    ("C-c h" helm-mini)
    ("<f2>" hydra-ace-jump/body)
    ("C-c j" hydra-ace-jump/body)
    ("C-c SPC" helm-resume)
    ("C-c r" revert-buffer)
    ("C-c l" logmode/helm)
    ("C-S-f" sp-forward-slurp-sexp)
    ("C-S-b" sp-forward-barf-sexp)
    ("C-c v" mc/edit-lines)
    ("C-c f" iedit-mode)
    ("C-c o" hydra-org-map-menu/body)
    ("C-c b" create-temp-buffer)
    ))



(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c v") #'mc/edit-lines)

(setq
 helm-M-x-fuzzy-match t
 helm-buffers-fuzzy-matching t
 helm-recentf-fuzzy-match    t)


(require 'smartparens-config)
(turn-on-smartparens-mode)
(turn-on-show-smartparens-mode)

;; Apply user requested bindings
(mapc
 (lambda (binding) (global-set-key (kbd (car binding)) (cadr binding)))
 user-requested-bindings)

(defconst user-requested-theme 'material)
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
(menu-bar-mode -1)

(if (eq 'helm user-requested-autocomplete-tool)
    (helm-mode))
;;Define other modes here...

;; Set the theme
(load-theme user-requested-theme t)
(enable-theme user-requested-theme)

;; Autocomplete stuff
;; (auto-complete)
;; (global-auto-complete-mode)
(global-company-mode)
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

(defun find-user-tasks-file ()
  "Edit the tasks file"
  (interactive)
  (find-file "~/tasks.org"))




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
 '(ansi-term-color-vector
   [unspecified "#1d1f21" "#CC342B" "#198844" "#FBA922" "#3971ED" "#A36AC7" "#3971ED" "#c5c8c6"] t)
 '(bookmark-save-flag 0)
 '(company-idle-delay 0.2)
 '(compilation-message-face (quote default))
 '(compilation-read-command nil)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("be50a45342f47158a8d34f54ffecc63f55dbdf66ad672c171c48e9dac56fff52" "ee86f1325d42dd1ac16db6b0e2c2b22f36caf06c3313940332d64b271fcfbeb9" "293907f71094d7a1ecf5bcb366bf32c2af0df5f9f607ffb3cab14d1ae3a4262a" "154400194a1843a22063914137c5f1ffce55f3ac369bf26589ae61d42a4540f4" "aba5822b8386905fd32f90edbb839995ea9cfa280d270e085b10e1f2ae145835" "dc42b54f7344d149c2429dad105aa89ed49dfe93c109ee7bd734cca1178ceb48" "dd1c2037ef66ee3f706bf687ccdb75fd7511bcc11d00fdb35d0535d80beb8b0a" "eb26ba64aa8e3726cb01b9de1a2f33647d4e0d6c8e36686d43a64d1cd76b7df1" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(custom-theme-load-path
   (quote
    ("c:/Users/e.hughes/AppData/Roaming/.emacs.d/elpa/color-theme-sanityinc-tomorrow-20140906.332/" "c:/Users/e.hughes/AppData/Roaming/.emacs.d/elpa/solarized-theme-20141004.2115/" custom-theme-directory "h:/Code/base16-builder/output/emacs" t)))
 '(fci-rule-color "#eee8d5")
 '(flycheck-display-errors-delay 0.1)
 '(flycheck-idle-change-delay 0.2)
 '(haskell-mode-hook
   (quote
    (turn-on-eldoc-mode turn-on-haskell-decl-scan turn-on-haskell-doc turn-on-haskell-indent)) t)
 '(helm-adaptive-mode t nil (helm-adaptive))
 '(helm-autoresize-mode t)
 '(helm-candidate-number-limit nil)
 '(helm-ff-skip-boring-files t)
 '(helm-locate-command "Everything %s %s")
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
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(indent-tabs-mode nil)
 '(inferior-lisp-program "clisp")
 '(jedi:complete-on-dot t)
 '(jiralib-url "https://jira")
 '(large-file-warning-threshold 100000000)
 '(line-move-visual nil)
 '(magit-diff-use-overlays nil)
 '(magit-emacsclient-executable "C:/Users/e.hughes/Emacs/bin/emacsclient.exe" t)
 '(magit-git-executable "c:/Users/e.hughes/AppData/Local/Programs/Git/bin/git")
 '(magit-use-overlays nil)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (python . t) (haskell . t))))
 '(org-capture-templates
   (quote
    (("t" "Tasks" entry
      (file "~/tasks.org")
      (file "~/task_template.org"))
     ("c" "Tasks for Config Team (Rotation 2)" entry
      (file "~/config_tasks.org")
      (file "~/task_template.org")
      :kill-buffer t))))
 '(org-catch-invisible-edits (quote show))
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file "~\\tasks.org")
 '(org-hide-emphasis-markers t)
 '(org-html-doctype "html5")
 '(org-html-head
   "<link rel=\"stylesheet\" type=\"text/css\" href=\"file:///H:/orgmode.css\"/>")
 '(org-html-html5-fancy t)
 '(org-image-actual-width 300)
 '(org-list-allow-alphabetical t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m org-jira)))
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 100))))
 '(python-shell-interpreter "ipython")
 '(revert-without-query (quote ("*+.log")))
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
(setq initial-buffer-choice "~\\tasks.org")
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

(setq default-directory "H:/Code/")
(defun start-pymacs ()
  (interactive)
  (load-file "~/.emacs.d/pymacs.el")
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-"))

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




(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "~/.ispell")

(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
(electric-indent-mode 1)
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;;(setq jedi:server-args '())

;;(add-hook 'python-mode-hook 'jedi:setup)

(server-start)

;;http://www.johndcook.com/blog/2015/02/01/rare-bigrams/
(key-chord-define-global "sj" 'save-buffer)
(key-chord-define-global "bf" 'beginning-of-buffer)
(key-chord-define-global "xs" 'end-of-buffer)

(key-chord-mode 1)

(require 'midnight)
(setq clean-buffer-list-delay-general 1)
(midnight-delay-set 'midnight-delay "0:01am")
(load-file "~/.emacs.d/macros.el")
(load-file "~/.emacs.d/hooks.el")
(put 'upcase-region 'disabled nil)

(define-generic-mode sim-mode '() '("+" "-" "*" "/" "^" ">" "ge" "<" "le" ">=" "geq" "<=" "leq" "==" "eq" "!=" "<>" "neq" "&&" "and" "||" "or" "!" "not" "true" "false" "if" "lazy_if" "eval_if" "on" "try_except " "exception" "abs_max" "abs_min" "PI" "qNaN" "abs" "acos" "acosh" "asin" "asinh" "atan" "atanh" "ceiling" "cos" "cosh" "delta" "exp" "floor" "int" "integral" "interpolated" "started" "isQNaN" "iteration" "lag" "ln" "log" "log10" "max" "min" "mod" "normsdist" "osc" "prev" "lazy_if" "product" "qf" "sign" "sin" "sinh" "sosc" "sqrt" "sum" "tan" "tanh" "volatility" "weightedemavol" "avg" "ema" "emCor" "emCovar" "emLinest" "emaCovar" "emVol" "emZ" "expWgtMav" "expWgtMavCo" "expWgtNormMavCo" "mavg" "mmin" "mmax" "globalmax" "globalmin" "percentile" "percentrank" "stdema" "median" "top" "top_percent" "variance" "day_of_year" "total_days" "second" "minute" "hour" "day" "month" "year" "weekday" "time" "date" "rand_exponential" "rand_lognormal" "rand_normal" "rand_uniform" "rand_uniform_int" "seed") '(("\@[a-z0-9]+" . font-lock-variable-name-face)) '("\\.cfds\\'") '())

(delete-selection-mode 1)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'python-mode-hook #'flycheck-mode)
