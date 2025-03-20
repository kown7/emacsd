;; Initialize straight.el
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
;; Use straight.el for package management
(straight-use-package 'use-package)

(setq load-path (cons "~/.emacs.d/vhdl-mode-3.39.3/" load-path))

;; Configure Helm
(use-package helm
  :straight t
  :diminish
  :config
  (helm-mode 1)
  (setq helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line t
        helm-autoresize-max-height 0
        helm-autoresize-min-height 20)
  (helm-autoresize-mode 1))

;; Configure LSP for TypeScript
(use-package typescript-mode
  :straight t
  :hook (typescript-mode . lsp-deferred))

;; Configure LSP for Python
(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; Configure LSP for Vue
(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook (vue-mode . lsp-deferred))

;; Additional configuration for Vue
(use-package vue-mode
  :straight t
  :config
  (add-hook 'vue-mode-hook #'lsp-deferred))

;; General LSP configuration
(use-package lsp-mode
  :straight t
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (setq lsp-enable-indentation nil) ;; Disable lsp indentation, use editorconfig/prettier/eslint
  (setq lsp-prefer-capf t)         ;; Use lsp-capf for completion
  (setq lsp-headerline-breadcrumb-enable nil) ;; Disable breadcrumb in the header line
  (setq lsp-modeline-diagnostics-enable t)    ;; Enable diagnostics in the modeline
  (setq lsp-modeline-diagnostics-scope :project) ;; Set the scope of diagnostics to the project
  (setq lsp-idle-delay 0.5) ;; Delay before starting LSP
  (setq lsp-log-io nil) ;; Disable LSP I/O logging
  )

;; Configure company-mode for completion
(use-package company
  :straight t
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0))

;; Configure which-key for keybindings
(use-package which-key
  :straight t
  :config
  (which-key-mode))

;; Configure Helm for completion and navigation
(use-package helm-lsp
  :straight t
  :commands helm-lsp-workspace-symbol)

;; Helm integration with LSP
(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
(define-key lsp-mode-map [remap xref-find-references] #'helm-lsp-workspace-references)
(define-key lsp-mode-map [remap xref-find-definition] #'helm-lsp-find-definition)
(define-key lsp-mode-map [remap xref-find-type-definition] #'helm-lsp-find-type-definition)
(define-key lsp-mode-map [remap xref-find-implementations] #'helm-lsp-find-implementations)

;; Configure Projectile
(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t))

;; magit open other window
(use-package magit
  :bind (:map magit-file-section-map
              ("RET" . magit-diff-visit-file-other-window)
              :map magit-hunk-section-map
              ("RET" . magit-diff-visit-file-other-window))
  )

;; ws-butler
(straight-use-package 'ws-butler)
(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)
(add-hook 'c-mode-hook 'ws-butler-mode)
(add-hook 'c-mode-hook 'auto-complete-mode)

;; Configure LSP for YAML
(use-package lsp-mode
  :straight t
  :hook (yaml-mode . lsp-deferred))

;; YAML mode
(use-package yaml-mode
  :straight t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(straight-use-package 'window-numbering)
;;(require 'window-numbering)
(window-numbering-mode)

(straight-use-package 'smartscan)
(global-smartscan-mode 1)

;;-------------------------------------------------------
;; Custom functions
;;-------------------------------------------------------
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

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
	(revert-buffer t t t) )))
  (message "Refreshed open files.") )

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

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-c t") 'tile-select)
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
(global-set-key (kbd "C-$") 'diff-hl-next-hunk)
(global-set-key (kbd "C-£") 'diff-hl-previous-hunk)

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

;; helm stuff
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key "\M-y"      'helm-show-kill-ring)

;;--------------------------------------------------------------------------------
;;    Customize looks
;;--------------------------------------------------------------------------------
;; no toolbar
(tool-bar-mode -1)
;; highlight changes in margin
(straight-use-package 'diff-hl)

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

 '(frame-background-mode 'dark)
 '(inhibit-startup-screen t)
 '(mouse-scroll-delay 0)

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

;; Custom and Colours
(delete-selection-mode 1)
(desktop-save-mode 1)

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

(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)

(set-mouse-color "white")
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed 't) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 83 :width normal)))))
