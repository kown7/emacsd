;;--------------------------------------------------------------------------------
;;   Modify exec/load-path
;;--------------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(setq load-path (cons "~/.emacs.d/vhdl-mode-3.38.1/" load-path))
(setq load-path (cons "~/.emacs.d/Pymacs/" load-path))

(load "~/.emacs.d/req-mode.el")

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

;; misc stuff
(global-set-key '[f1]   'start-kbd-macro)
(global-set-key '[f2]   'end-kbd-macro)
(global-set-key '[f3]   'call-last-kbd-macro)
(global-set-key '[f4]   'goto-line)
(global-set-key '[f5]   'highlight-symbol-at-point)
(global-set-key '[S-f5] 'unhighlight-regexp)
(global-set-key '[f7]   "\C-u\M-xtoggle-truncate-lines")
(global-set-key '[f8]   'other-window)
(global-set-key '[f9]   'my-swap-buffer)
(global-set-key "\M-t"  'my-swap-buffer)
(global-set-key '[f10]  'buffer-menu)
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
 '(helm-gtags-auto-update t)
 '(inhibit-startup-screen t)
 '(minimap-mode nil)
 '(mouse-scroll-delay 0)
 '(package-selected-packages
   (quote
    (zones diff-hl groovy-mode jedi json-mode smartscan ac-octave auto-complete-auctex ac-helm helm-cmd-t helm-commandlinefu helm-exwm helm-fuzzier helm-fuzzy-find helm-ls-git helm-navi window-numbering nyan-mode helm-package helm-mode-manager helm-helm-commands helm-gtags helm-grepint helm-git-grep helm-git-files helm-git helm-frame helm-filesets)))
 '(show-paren-mode t nil (paren))
 '(tool-bar-mode nil)
 '(vc-handled-backends (quote (Git SVN SCCS Bzr Hg Mtn Arch)))
 '(vhdl-actual-port-name (quote ("\\(.*?\\)$" . "\\1")))
 '(vhdl-array-index-record-field-in-sensitivity-list nil)
 '(vhdl-highlight-case-sensitive t)
 '(vhdl-highlight-special-words t)
 '(vhdl-reset-kind "Synchronous")
 '(vhdl-special-syntax-alist
   (quote
    (("signal-clr" "\\<\\(Clr\\|clr\\|CLR\\|Clear\\|clear\\|CLEAR\\)[A-Za-z0-9_]*" "Tomato" "orange")
     ("signal-clock" "\\<\\(Clk\\|CLK\\|clk\\|Clock\\|clock\\|CLOCK\\)\\(\\>\\|_[A-Za-z0-9_]*\\>\\)+" "LimeGreen" "lightseagreen")
     ("signal-reset" "\\<\\(Rst\\|RST\\|rst\\|Reset\\|RESET\\|reset\\)[A-Za-z0-9_]*" "Tomato" "red3")
     ("type-definition" "\\<\\w+_[tT]\\>" "aquamarine3" "mediumaquamarine")
     ("record-definition" "\\<\\w+_[rR]\\>" "magenta2" "magenta2")
     ("constant" "\\<\\w+_[cC]\\>" "DodgerBlue3" "dodgerblue3")
     ("generic" "\\<\\w+_[gG]\\>" "DarkOrange" "darkorange")
     ("instance" "\\<[iI]_\\w+\\>" "Grey50" "gray30")
     ("Enable" "\\<\\w+\\(En\\|EN\\|_[WR]?en\\|Ena\\||WE\\)\\>" "brightblue" "chartreuse2")
     ("Valid" "\\<\\w+\\(Vld\\|VLD\\|_vld\\|Vld_[A-z]+\\)\\>" "brightblue" "chartreuse2")
     ("Ready" "\\<\\w+\\(Rdy\\|RDY\\|_rdy\\)\\>" "brightblue" "chartreuse2"))))
 '(vhdl-underscore-is-part-of-word t))


;;--------------------------------------------------------------------------------
;;    Other customizations
;;--------------------------------------------------------------------------------
(require 'window-numbering)
(window-numbering-mode)

(setq make-backup-files nil)
(setq auto-save-default t)
(server-start)

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
(require 'auto-complete)
(add-hook 'vhdl-mode-hook 'auto-complete-mode)
(add-hook 'vhdl-mode-hook 'diff-hl-mode)
(add-hook 'vhdl-mode-hook 'diff-hl-flydiff-mode)
(add-hook 'vhdl-mode-hook 'hl-line-mode)

;;--------------------------------------------------------------------------------
;;    Custom and Colours
;;--------------------------------------------------------------------------------
(delete-selection-mode 1)

(add-to-list 'load-path "~/.emacs.d/color-theme/")
(require 'color-theme)
(load "~/.emacs.d/color-theme-solarized.el")
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-solarized-dark)))

(set-face-attribute 'region nil :background "#666")

(require 'nyan-mode)
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
(require 'helm)
(require 'helm-config)
(require 'helm-gtags)

(global-set-key (kbd "M-,") 'helm-gtags-pop-stack)
(global-set-key (kbd "M-.") 'helm-gtags-dwim)
;(global-set-key (kbd "M--") 'helm-gtags-find-symbol)
(global-set-key (kbd "M-:") 'helm-gtags-find-tag-other-window)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key "\M-y"      'helm-show-kill-ring)
(global-set-key "\C-x\C-f"  'helm-find-files)
(setq helm-M-x-fuzzy-match  t)
(global-set-key "\M-x"      'helm-M-x)
(setq helm-buffers-fuzzy-matching  t)
(global-set-key "\C-xb"     'helm-buffers-list)

(setq helm-gtags-auto-update t)
(setq helm-gtags-path-style (quote relative))

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

;;--------------------------------------------------------------------------------
;;    C/C++-Mode
;;--------------------------------------------------------------------------------
(require 'ws-butler)
(add-hook 'c-mode-hook 'ws-butler-mode)
(add-hook 'c-mode-hook 'auto-complete-mode)

;;--------------------------------------------------------------------------------
;;    Python Mode
;;--------------------------------------------------------------------------------
(setq py-python-command "python3")
(setq python-shell-interpreter "ipython3")

(elpy-enable)
(setq elpy-rpc-backend "jedi")
(setq elpy-rpc-python-command "python3")
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i")

(add-hook 'python-mode-hook 'ws-butler-mode)
(add-hook 'python-mode-hook 'diff-hl-mode)
(add-hook 'python-mode-hook 'diff-hl-flydiff-mode)
(add-hook 'python-mode-hook 'hl-line-mode)

(defun my-python-config ()
  (local-set-key (kbd "M-.") 'elpy-goto-definition)
  (local-set-key (kbd "M-:") 'elpy-goto-definition-other-window)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  )
(add-hook 'python-mode-hook 'my-python-config)

;;--------------------------------------------------------------------------------
;;    Other customizations
;;--------------------------------------------------------------------------------
;; no toolbar
(tool-bar-mode -1)

(require 'window-numbering)
(window-numbering-mode)

(require 'smartscan)
(global-smartscan-mode 1)

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


(global-set-key (kbd "M-<left>") 'enlarge-window)
(global-set-key (kbd "M-<right>") 'shrink-window)


;; Smooth scrolling
;; scroll one line at a time (less "jumpy" than defaults)

(set-mouse-color "white")
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed 't) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(set-mouse-color "white")

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
