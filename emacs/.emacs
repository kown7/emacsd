;;--------------------------------------------------------------------------------
;;   Modify exec/load-path
;;--------------------------------------------------------------------------------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)
(straight-use-package 'use-package)

(setq load-path (cons "~/.emacs.d/vhdl-mode-3.38.1/" load-path))

(load "~/.emacs.d/req-mode.el")

;; Restart frozen server: https://www.emacswiki.org/emacs/EmacsAsDaemon#h5o-16
(defun signal-restart-server ()
  "Handler for SIGUSR1 signal, to (re)start an emacs server.
or from the command line with:
$ kill -USR1 <emacs-pid>
$ emacsclient -c
"
  (interactive)
  "(server-force-delete)
  (server-start :inhibit-prompt t)"
  (server-start)
  )
(define-key special-event-map [sigusr1] 'signal-restart-server)

;;--------------------------------------------------------------------------------
;; co-pilot specific
;;--------------------------------------------------------------------------------
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)

;; complete by copilot first, then auto-complete
(straight-use-package 'auto-complete)
(defun my-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (ac-expand nil)))

(with-eval-after-load 'auto-complete
  ; disable inline preview
  (setq ac-disable-inline t)
  ; show menu if have only one candidate
  (setq ac-candidate-menu-min 0)

  (define-key ac-completing-map (kbd "TAB") 'my-tab)
  (define-key ac-completing-map (kbd "<tab>") 'my-tab))

(define-key global-map [remap indent-for-tab-command] '(lambda ()
                                                         (interactive)
                                                         (or (copilot-accept-completion)
                                                             (indent-for-tab-command))))

;;--------------------------------------------------------------------------------
;;   Customize keybindings
;;--------------------------------------------------------------------------------
;; hippie expand
(global-set-key '[help]    'hippie-expand)
(global-set-key '[S-tab]   'hippie-expand)
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-line))

(global-set-key (kbd "C-c t") 'tile-select)
(straight-use-package 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; misc stuff
(global-set-key '[f1]   'start-kbd-macro)
(global-set-key '[f2]   'end-kbd-macro)
(global-set-key '[f3]   'call-last-kbd-macro)
(global-set-key '[f4]   'goto-line)
(global-set-key '[f5]   'highlight-symbol-at-point)
(global-set-key '[S-f5] 'unhighlight-regexp)
(global-set-key '[f7]   "\C-u\M-xtoggle-truncate-lines")
(global-set-key '[f8]   'other-window)
(global-set-key "\M-t"  'my-swap-buffer)
(global-set-key '[f9]   'helm-projectile-find-file-dwim)
(global-set-key '[f10]  'helm-buffers-list)
;; M-: (read-key-sequence-vector "") RET C-ö
(global-set-key (quote [67109116]) 'backward-paragraph)
(global-set-key (quote [67109092]) 'forward-paragraph)

;; special scrolling
(global-set-key '[S-home]   'my-scroll-right-9999)
(global-set-key '[M-up]     "\C-u1\M-xscroll-down")
(global-set-key '[M-down]   "\C-u1\M-xscroll-up")
(global-set-key '[M-left]   'my-scroll-right-2)
(global-set-key '[M-right]  'my-scroll-left-2)
(global-set-key '[S-up]     'my-pan-down-1)
(global-set-key '[S-down]   'my-pan-up-1)
(global-set-key '[S-left]   'my-pan-right-2)
(global-set-key '[S-right]  'my-pan-left-2)

(global-set-key (kbd "C-x C-M-c") 'save-buffers-kill-emacs)

(defun my-swap-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 'VISIBLE-OK)))

(defun my-pan-up-1 ()
  (interactive)
  (next-line 1)
  (scroll-up 1))

(defun my-pan-down-1 ()
  (interactive)
  (scroll-down 1)
  (previous-line 1))

(defun my-pan-left-2 ()
  (interactive)
  (forward-char 2)
  (scroll-left 2))

(defun my-pan-right-2 ()
  (interactive)
  (backward-char 2)
  (scroll-right 2))

(defun my-scroll-left-2 ()
  (interactive)
  (let ((current-prefix-arg 2))
    (call-interactively 'scroll-left)))

(defun my-scroll-right-2 ()
  (interactive)
  (let ((current-prefix-arg 2))
    (call-interactively 'scroll-right)))

(defun my-scroll-right-9999 ()
  (interactive)
  (let ((current-prefix-arg 9999))
    (call-interactively 'scroll-right)))

;;--------------------------------------------------------------------------------
;;    General settings (optional)
;;--------------------------------------------------------------------------------
;; no toolbar
(tool-bar-mode -1)

;; misc settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(column-number-mode t)
 '(custom-safe-themes
   '("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "8cc64ffacd333b57125c4f504a433cede1dccd04861c4f7297faef772d325a8a" default))
 '(elpy-shell-use-project-root t)
 '(frame-background-mode 'dark)
 '(helm-gtags-auto-update t t)
 '(inhibit-startup-screen t)
 '(minimap-mode nil)
 '(mouse-scroll-delay 0)
 '(package-selected-packages
   '(helm-ag git-timemachine ag editorconfig helm-dash transpose-frame elpy flycheck ac-php php-mode magit ws-butler xr helm-projectile projectile helm-sly sly helm-lsp yasnippet-classic-snippets zones diff-hl groovy-mode jedi json-mode smartscan ac-octave auto-complete-auctex ac-helm helm-cmd-t helm-commandlinefu helm-exwm helm-fuzzier helm-fuzzy-find helm-ls-git helm-navi window-numbering nyan-mode helm-package helm-mode-manager helm-helm-commands helm-gtags helm-grepint helm-git-grep helm-git-files helm-git helm-frame helm-filesets))
 '(show-paren-mode t nil (paren))
 '(tool-bar-mode nil)
 '(vc-handled-backends '(Git SVN SCCS Bzr Hg Mtn Arch))
 '(vhdl-actual-port-name '("\\(.*?\\)$" . "\\1"))
 '(vhdl-array-index-record-field-in-sensitivity-list nil)
 '(vhdl-highlight-case-sensitive t)
 '(vhdl-highlight-special-words t)
 '(vhdl-intelligent-tab nil)
 '(vhdl-reset-kind "Synchronous")
 '(vhdl-special-syntax-alist
   '(("signal-clr" "\\<\\(Clr\\|clr\\|CLR\\|Clear\\|clear\\|CLEAR\\)[A-Za-z0-9_]*" "Tomato" "orange")
     ("signal-clock" "\\<\\(Clk\\|CLK\\|clk\\|Clock\\|clock\\|CLOCK\\)\\(\\>\\|[_A-Za-z0-9_]*\\>\\)+" "LimeGreen" "lightseagreen")
     ("signal-reset" "\\<\\(Rst\\|RST\\|rst\\|Reset\\|RESET\\|reset\\)[A-Za-z0-9_]*" "Tomato" "red3")
     ("type-definition" "\\<\\w+_[tT]\\>" "aquamarine3" "mediumaquamarine")
     ("record-definition" "\\<\\w+_[rR]\\>" "magenta2" "magenta2")
     ("constant" "\\<\\w+_[cC]\\>" "DodgerBlue3" "dodgerblue3")
     ("generic" "\\<\\w+_[gG]\\>" "DarkOrange" "darkorange")
     ("instance" "\\<[iI]_\\w+\\>" "Grey50" "gray30")
     ("process-name" "\\<[p]_\\w+\\>" "Grey50" "gray50")
     ("Enable" "\\<\\w+\\(En\\|_EN\\|[WR]en\\|Ena\\||_WE\\)\\>" "brightblue" "chartreuse2")
     ("Valid" "\\<\\w+\\(Vld\\|VLD\\|_vld\\|Vld_[A-z]+\\)\\>" "brightblue" "chartreuse2")
     ("Ready" "\\<\\w+\\(Rdy\\|RDY\\|_rdy\\)\\>" "brightblue" "chartreuse2")))
 '(vhdl-underscore-is-part-of-word t))


;;--------------------------------------------------------------------------------
;;    Other customizations
;;--------------------------------------------------------------------------------

(setq make-backup-files nil)
(setq auto-save-default t)
(server-start)

(setq ring-bell-function 'ignore)
(setq visible-bell t)

(straight-use-package 'diff-hl)
(straight-use-package 'flycheck)

;;--------------------------------------------------------------------------------
;; extra face to make tristate-signals bold (not supported by XEmacs)
;;--------------------------------------------------------------------------------
(add-hook 'vhdl-mode-hook
	  (function (lambda ()
		      (defvar vhdl-font-lock-signal-tristate-face   'vhdl-font-lock-signal-tristate-face
			"Face name to use for tristate signals.")
		      (defface vhdl-font-lock-signal-tristate-face
			'((t (:bold t)))
			"Font lock mode face used to highlight tristate signals."
			:group 'vhdl-highlight-faces
			:group 'font-lock-highlighting-faces)
		      (font-lock-add-keywords 'vhdl-mode '(("\\<\\([A-Z][A-Za-z0-9]*x[A-Z]*Z[A-Za-z0-9]*\\)\\>" 1 vhdl-font-lock-signal-tristate-face append)) t)
		      )))

;;--------------------------------------------------------------------------------
;;    emacs VHDL autocomplete
;;--------------------------------------------------------------------------------
(straight-use-package 'auto-complete)
(add-hook 'vhdl-mode-hook 'auto-complete-mode)
(add-hook 'vhdl-mode-hook 'diff-hl-mode)
(add-hook 'vhdl-mode-hook 'diff-hl-flydiff-mode)
(add-hook 'vhdl-mode-hook 'hl-line-mode)

;;--------------------------------------------------------------------------------
;;    Custom and Colours
;;--------------------------------------------------------------------------------
(delete-selection-mode 1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'solarized t)

(set-face-attribute 'region nil :background "#666")
(straight-use-package 'magit)
(require 'magit)
(set-face-attribute 'magit-header-line nil :background "#073642")

(straight-use-package 'nyan-mode)
(nyan-mode)
(nyan-toggle-wavy-trail)
(nyan-start-animation)

(hl-line-mode)
(set-face-background hl-line-face "gray13")

;; set changed-colour to orange
(defface diff-hl-change
  '((default :foreground "orange3")
    (((class color) (min-colors 88) (background light))
     :background "#ddddff")
    (((class color) (min-colors 88) (background dark))
     :background "orange3"))
  "Face used to highlight changed lines."
:group 'diff-hl)


;;--------------------------------------------------------------------------------
;;    HELM
;;--------------------------------------------------------------------------------
(straight-use-package 'helm)
(straight-use-package 'helm-gtags)

(global-set-key (kbd "M-,") 'helm-gtags-pop-stack)
(global-set-key (kbd "M-.") 'helm-gtags-dwim)
;(global-set-key (kbd "M--") 'helm-gtags-find-symbol)
(global-set-key (kbd "M-:") 'helm-gtags-find-tag-other-window)
(global-set-key (kbd "C-M-:") 'helm-git-grep-at-point)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key "\M-y"      'helm-show-kill-ring)
(global-set-key "\C-x\C-f"  'helm-find-files)
(setq helm-completion-style 'emacs)
(setq completion-styles     '(helm-flex))
(global-set-key "\M-x"      'helm-M-x)
(setq helm-buffers-fuzzy-matching  t)
(global-set-key "\C-xb"     'helm-buffers-list)

(setq helm-gtags-auto-update t)
(setq helm-gtags-path-style (quote relative))

(straight-use-package 'ac-helm)  ;; Not necessary if using ELPA package
(require 'ac-helm)  ;; Not necessary if using ELPA package
(global-set-key (kbd "C-:") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)
(global-set-key (kbd "C-c p f") 'helm-projectile-find-files)

(set-face-attribute 'helm-selection nil :background "#441100")

;;--------------------------------------------------------------------------------
;;    C/C++-Mode
;;--------------------------------------------------------------------------------
(straight-use-package 'ws-butler)
(require 'ws-butler)
(add-hook 'c-mode-hook 'ws-butler-mode)
(add-hook 'c-mode-hook 'auto-complete-mode)

;;--------------------------------------------------------------------------------
;;    Python Mode
;;--------------------------------------------------------------------------------

;; https://github.com/jorgenschaefer/elpy/issues/1749
;; (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
;; (set-language-environment "UTF-8")

(setq py-python-command "python3")
(setq python-shell-interpreter "ipython3")
(setq python-shell-interpreter-args "--simple-prompt -i")

(straight-use-package 'elpy)
(elpy-enable)
;; (setq elpy-rpc-backend "jedi")
;; (setq elpy-rpc-python-command "python3")

(add-hook 'python-mode-hook 'ws-butler-mode)
(add-hook 'python-mode-hook 'diff-hl-mode)
(add-hook 'python-mode-hook 'diff-hl-flydiff-mode)
(add-hook 'python-mode-hook 'hl-line-mode)

(add-hook 'python-mode-hook 'flycheck-mode)

(defun my-python-config ()
  (local-set-key (kbd "M-.") 'elpy-goto-definition)
  (local-set-key (kbd "M-:") 'elpy-goto-definition-other-window)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  (local-set-key (kbd "C->") 'python-nav-end-of-block)
  (local-set-key (kbd "C-<") 'python-nav-beginning-of-block)
  )
(add-hook 'python-mode-hook 'my-python-config)

;; No auto-complete --> there are others
; (add-hook 'python-mode-hook 'auto-complete-mode)
; https://emacs.stackexchange.com/a/15244
(add-hook 'python-mode-hook
        (lambda () (setq forward-sexp-function nil)))

;;--------------------------------------------------------------------------------
;;    Other customizations
;;--------------------------------------------------------------------------------
;; no toolbar/scrollbar
(tool-bar-mode -1)
(scroll-bar-mode -1)
;;    Magit / diff-hl
(diff-hl-mode)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(straight-use-package 'tile)
(require 'tile)

(straight-use-package 'window-numbering)
;;(require 'window-numbering)
(window-numbering-mode)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(straight-use-package 'smartscan)
;;(require 'smartscan)
(global-smartscan-mode 1)

(straight-use-package 'button-lock)
(require 'button-lock)
(straight-use-package 'fixmee)
(global-fixmee-mode 1)

(setq make-backup-files nil)
(setq auto-save-default t)
(server-start)

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
	(revert-buffer t t t) )))
  (message "Refreshed open files.") )

;; move lines up and down
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)


(global-set-key (kbd "M-<left>") 'enlarge-window)
(global-set-key (kbd "M-<right>") 'shrink-window)

(defun reverse-letters-region (beg end)
 "Reverse characters between BEG and END."
 (interactive "r")
 (let ((region (buffer-substring beg end)))
   (delete-region beg end)
   (insert (nreverse region))))

(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)

;; Smooth scrolling
;; scroll one line at a time (less "jumpy" than defaults)

(set-mouse-color "white")
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed 't) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(desktop-save-mode 1)

(windmove-default-keybindings)

;;(setq load-path (cons "~/.emacs.d/" load-path))
;;(require 'sr-speedbar)
;;(sr-speedbar-open)
;(setq speedbar-use-images nil)
;(with-current-buffer sr-speedbar-buffer-name
; (setq window-size-fixed 'width))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 83 :width normal)))))
