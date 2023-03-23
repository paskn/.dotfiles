;; init.el --- Emacs configuration

;; straight.el to manage packages
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

;; make straight.el aware of use-package
(straight-use-package 'use-package)

;; make sure use-package always uses staight.el
(setq straight-use-package-by-default t)

;; iTerm2 config
;; ITERM2 MOUSE SUPPORT
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e)) 
  (setq mouse-sel-mode t))

(setq inhibit-startup-message t) ;; hide the startup message
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(column-number-mode)
;; this boy drives me nuts on pdf-view-mode
;; activate line-numbers in specific modes as necessary
;;(global-display-line-numbers-mode t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(global-hl-line-mode t)

;; disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		pdf-view-mode
		pdf-tools))
  (add-hook mode(lambda () (setq display-line-numbers nil))))

(setq visible-bell nil)

;; set the default font 
;;(set-face-attribute 'default nil :font "Ubuntu Mono")
(set-face-attribute 'default nil :font "IBM Plex Mono")
;; enlarge the defaul font size
(set-face-attribute 'default nil :height 130)

;; use ESC to cancel promt
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

;; Start maximised (cross-platf)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; stop emacs expecting that a sentence ends with double space
(setq sentence-end-double-space nil)
;; stretch cursor to full char.-width, i.e. make tabs visible
(setq x-stretch-cursor t)
;; show recent files with M-x recentf-open-file
(recentf-mode 1)
;; automatically update buffer with changed files
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; INSTALL PACKAGES
;; --------------------------------------

(use-package guru-mode
  :straight t
  :init (guru-global-mode +1))

;; amx to help with ivy-m-x suggestions 
(use-package amx
  :straight t
  :custom
  (amx-backend 'auto)
  (amx-save-file "~/Documents/personal/emacs/amx-items")
  (amx-history-length 50)
  :config
  (amx-mode 1))

;; track changes in the buffer in a tree-like structure
(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode 1))

;; Doom-modeline
(use-package all-the-icons
  :straight t)

(use-package all-the-icons-dired
  :straight t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package modus-themes
  :straight t
  :config
  (load-theme 'modus-vivendi :no-confirm))

(use-package ef-themes
  :straight (ef-themes :type git :host github :repo "protesilaos/ef-themes")
  :config
;  (ef-themes-select 'ef-autumn)
;  (load-theme 'ef-light t)
  )

(use-package popper
  :straight t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
	  "\\*Org Select\\*"
	  "\\*Agenda Commands\\*"
	  "\\*Org Agenda\\*"
	  "\\*Buffer List\\*"
     	  "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
          "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
          "^\\*term.*\\*$"   term-mode   ;term as a popup
          "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
		))
  (popper-mode +1)
  (popper-echo-mode +1)
  (setq popper-group-function #'popper-group-by-project) ; project.el projects
  )

(use-package dashboard
  :straight t
  :after org
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-week-agenda t)
  (setq dashboard-items '((agenda . 10)
			  (bookmarks . 5)
;			  (recents  . 5)
                          ))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-footer 1)
  (setq dashboard-agenda-sort-strategy '(time-up))
  (setq dashboard-agenda-prefix-format " %i %-10s ")
  (setq dashboard-item-names '(("Agenda for the coming week:" . "Schedule:"))
  ))

;; BASIC CUSTOMIZATION
;; --------------------------------------

;; Config to keep the cursor at the center of the screen
;; source: https://protesilaos.com/codelog/2020-07-16-emacs-focused-editing/
(use-package emacs
  :straight nil
  :config
  (setq-default scroll-preserve-screen-position t)
  (setq-default scroll-conservatively 1) ; affects `scroll-step'
  (setq-default scroll-margin 0)
  :init
   ;; TAB ycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  (define-minor-mode sp/scroll-centre-cursor-mode
    "Toggle centred cursor scrolling behaviour."
    :init-value nil
    :lighter " S="
    :global nil
    (if sp/scroll-centre-cursor-mode
        (setq-local scroll-margin (* (frame-height) 2)
                    scroll-conservatively 0
                    maximum-scroll-margin 0.5)
      (dolist (local '(scroll-preserve-screen-position
                       scroll-conservatively
                       maximum-scroll-margin
                       scroll-margin))
        (kill-local-variable `,local))))
  ;; C-c l is used for `org-store-link'.  The mnemonic for this is to
  ;; focus the Line and also works as a variant of C-l.
  :bind
  ("C-c L" . sp/scroll-centre-cursor-mode)
  ("s-/" . set-mark-command)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  (repeat-mode 1)
  )

(use-package aweshell
  :straight (abc-mode :type git :host github :repo "manateelazycat/aweshell")
  :ensure t)

;; Example configuration for Consult
(use-package consult
  :straight (consult :source (melpa gnu-elpa-mirror))
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

(use-package consult-yasnippet
  :straight t)

(use-package consult-project-extra
  :straight t)

(use-package consult-eglot
  :straight t)

(use-package consult-flyspell
  :straight (consult-flyspell :type git :host gitlab :repo "OlMon/consult-flyspell" :branch "master")
  :config
  ;; default settings
  (setq consult-flyspell-select-function nil
        consult-flyspell-set-point-after-word t
        consult-flyspell-always-check-buffer nil))

(use-package marginalia
  :straight t
  :ensure t
  :after consult
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode 1))

(use-package embark
  :straight t
  :bind
  (("s-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :straight t
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package all-the-icons-completion
  :straight t
  :after (marginalia all-the-icons)
  :ensure t
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; cycle selection regions
(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

;; store and manage window configuration
;; C-c <left> and C-c <right>
(use-package winner
  :straight nil
  :config
 (winner-mode 1))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package avy
  :straight t
  :init
  (global-set-key (kbd "s-,") 'avy-goto-word-1))

(use-package which-key
  :straight t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq wich-key-idle-delay 0.3))

;; Enable vertico
(use-package vertico
  :straight t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; history of minibuffer
(use-package savehist
  :straight t
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (savehist-mode))

;; help with automatic parenthesis input
(use-package smartparens
  :straight t
  :config
  (progn (show-smartparens-global-mode t))
  (sp-use-paredit-bindings)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'markdown-mode-hook 'smartparens-mode)
  )

(use-package helpful
  :straight t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;; Language Tool
;;(setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/5.6/libexec/languagetool-commandline.jar")

(use-package langtool
  :straight t
  :config
;;  (setq langtool-java-bin "/usr/bin/java")
;;  (setq langtool-language-tool-server-jar "/Users/sp/languagetool/languagetool-server/target/languagetool-server-6.0-SNAPSHOT.jar")
;;  (setq langtool-language-tool-jar "/Users/sp/languagetool/languagetool-core/target/languagetool-core-6.0-SNAPSHOT.jar")
;;  (setq langtool-java-classpath "/Users/sp/languagetool/languagetool-commandline/target/classes/org/languagetool/commandline")
  (setq langtool-language-tool-jar "/Users/sp/languagetool/languagetool-standalone/target/LanguageTool-6.0-SNAPSHOT/LanguageTool-6.0-SNAPSHOT/languagetool-commandline.jar")
  (setq langtool-default-language "en-US")
  (global-set-key "\C-x4w" 'langtool-check)
  (global-set-key "\C-x4W" 'langtool-check-done)
  (global-set-key "\C-x4l" 'langtool-switch-default-language)
  (global-set-key "\C-x44" 'langtool-show-message-at-point)
  (global-set-key "\C-x4w" 'langtool-check)
  (global-set-key "\C-x4W" 'langtool-check-done)
  (global-set-key "\C-x4l" 'langtool-switch-default-language)
  (global-set-key "\C-x44" 'langtool-show-message-at-point))

  ;; org-mode config
  ;; Do not ask for confirmation when evaluation a block
  ;;  '(org-agenda-files '("~/Documents/personal/emacs/org-agenda.org"))
(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
;;   (python . t)
   (R . t)
   (dot . t)
;   (ledger . t) ; where is ob-ledger??
   ))

(defun sp/org-mode-setup ()
  (org-indent-mode)
;;  (variable-pitch-mode 1)
;;  (auto-fill-mode 1)
  (visual-line-mode 1)
;;    Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)
  (setq display-line-numbers nil))

(use-package volatile-highlights
  :straight t
  :config
  (volatile-highlights-mode t))

;; Let emacs to decide what to do with very long lines
(use-package so-long
;;  :after-call find-file-hook
  :straight t
  :config
  (global-so-long-mode))

;; a mode to work with graphviz
(use-package graphviz-dot-mode
  :straight (graphviz-dot-mode :type git :host github :repo "ppareit/graphviz-dot-mode")
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

;; center org-buffers
(defun sp/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :straight t
  :hook (org-mode . sp/org-mode-visual-fill)
  :hook (markdown-mode . sp/org-mode-visual-fill))

(use-package org
  :straight (:type built-in)
  :hook (org-mode . sp/org-mode-setup)
  :config
  (setq org-use-speed-commands t)
  (setq org-latex-listings 'minted  ;; enable code highlighing and svg in pdf
	org-latex-packages-alist '(("" "minted"))
	org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-image-actual-width nil)
  (set-face-attribute 'fixed-pitch nil :font "IBM Plex Mono")
  (setq org-directory "~/org")
  (setq org-fold-core-style 'overlays)
  (setq org-ellipsis " ▾")
  (setq org-agenda-files
	'("~/org/projects.org"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-les
	'(ol-doi ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww)
	)
  ;; Save the corresponding buffers
  (defun gtd-save-org-buffers ()
    "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
    (interactive)
    (message "Saving org-agenda-files buffers...")
    (save-some-buffers t (lambda () 
			   (when (member (buffer-file-name) org-agenda-files) 
			     t)))
    (message "Saving org-agenda-files buffers... done"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (setq org-capture-templates
	`(;;("i" "Inbox")
	  ("i" "Inbox..." entry (file "inbox.org")
	    ,(concat "* TODO %?\n"
	   	    "/Entered on/ %U"))
	  ;;  ("in" "A note" entry
	  ;;   (file+headline "inbox.org" "Notes")
	  ;;  "* %?\n /Entered on/ %U\n\n %i\n\n %a" )
	   ))
  (setq org-refile-targets
	'(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")
	  ("reference.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")
	  ("someday-maybe.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\))")))
  (setq org-refile-use-outline-path 'file)
  ;; Add it after refile
  (advice-add 'org-refile :after
              (lambda (&rest _)
		(gtd-save-org-buffers)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d!)")
    (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  ;; Configure custom agenda views
(setq org-agenda-custom-commands
      '(("d" "📝 Desk"
	 ((tags-todo "@desk&@next"
		     ((org-agenda-overriding-header "📝 Write & research\n"))
		     ))
	 ((org-agenda-files '("~/org/projects.org"))))
	
	("c" "☎️💬 Comm."
	 ((tags-todo "@communication"
		     ((org-agenda-overriding-header "☎️💬Reach out and convey a message\n"))
		     ))
	 ((org-agenda-files '("~/org/projects.org"))))
	
	("w" "☢️ Waiting"
	 ((tags-todo "@waiting"
		     ((org-agenda-overriding-header "☢️Track progress\n"))
		     ))
	 ((org-agenda-files '("~/org/projects.org"))))
	
	("e" "🚴‍♀️ Errands"
	 ((tags-todo "@errand&@next|@shopping"
		     ((org-agenda-overriding-header "🚴‍♀️Go out\n"))
		     ))
	 ((org-agenda-files '("~/org/projects.org"))))
	
	("h" "🏡 House"
	 ((tags-todo "@house&@next"
		     ((org-agenda-overriding-header "🏡Do it at home\n"))
		     ))
	 ((org-agenda-files '("~/org/projects.org"))))
	
	("r" "📚📝 Reading"
	 ((tags-todo "@reading&@next"
		     ((org-agenda-overriding-header "📚📝Read it and process its value\n"))
		     ))
	 ((org-agenda-files '("~/org/projects.org"))))
	("A" "👌🧘‍♀️ Anywhere"
	 ((tags-todo "@anywhere&@next"
		     ((org-agenda-overriding-header "👌🧘‍♀️Do it wherever is convenient\n"))
		     ))
	 ((org-agenda-files '("~/org/projects.org")))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:height 130 :width normal :family "IBM Plex Mono ")))))

;; (use-package hyperbole
;;   :straight t
;;   :config
;;   (hyperbole-mode 1))

;; working with projects
(use-package project
  :straight (:type built-in)
  :config
  ;; Declare directories with ".project" as a project
  (cl-defmethod project-root ((project (head local)))
    (cdr project))

  (defun my/project-try-local (dir)
    "Determine if DIR is a non-Git project.
DIR must include a .project file to be considered a project."
    (let ((root (locate-dominating-file dir ".project")))
      (and root (cons 'local root))))

  (defun project-todo ()
    "Edit the TODO.org file at the root of the current project."
    (interactive)
    (let* ((base (ignore-errors (project-root (project-current))))
           (todo (file-name-concat base "TODO.org")))
      (cond ((and base (file-exists-p todo)) (find-file todo))
            ((not base) (error "Not in a project"))
            (t (error "Project does not contain a TODO.org file.")))))

  (global-set-key (kbd "C-x p t") 'project-todo)

  (add-to-list 'project-find-functions 'my/project-try-local)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m))
  (add-to-list 'project-switch-commands '(project-todo "Todo" "t")))

;; use ledger mode to keep track of money
(use-package ledger-mode
  :straight t
  :init
  (setq ledger-clear-whole-transactions 1)
  :config
  (setq ledger-binary-path "/usr/local/bin/ledger")
  (setq ledger-reconcile-default-commodity "€")
  :mode "\\.dat\\'")

(use-package citeproc
  :straight t
  :config
  (setq org-cite-csl-styles-dir "~/Zotero/styles"))

(use-package oc-csl
  :straight (:type built-in))

(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode))

;; help org and markdown to align tables
(use-package valign
  :straight t
;;  :after-call org-mode-hook
  :hook (org-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))

(setq org-latex-pdf-process '("texi2dvi -p -b -V %f"))
;; (setq org-latex-pdf-process
;;       '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
;; (add-to-list 'org-latex-classes
;;                '("apa6"
;;                  "\\documentclass{apa6}"
;;                  ("\\section{%s}" . "\\section*{%s}")
;;                  ("\\subsection{%s}" . "\\subsection*{%s}")
;;                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;; (require 'ox-latex)
;; (unless (boundp 'org-latex-classes)
;;   (setq org-latex-classes nil))
;; (add-to-list 'org-latex-classes
;;              '("article"
;;                "\\documentclass{article}"
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/personal/RoamNotes")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 :map org-mode-map
	 ("C-M-c" . completion-at-point))
  :config
  (org-roam-setup))

(use-package mastodon
  :straight t
  :ensure t
  :config
  (setq mastodon-instance-url "https://mastodon.social"
          mastodon-active-user "pashakhin"))

(use-package elfeed
  :straight t
  :config
  (global-set-key (kbd "C-x w") 'elfeed)
  (add-hook 'elfeed-search-mode-hook 'elfeed-update)
  (defun concatenate-authors (authors-list)
    "Given AUTHORS-LIST, list of plists; return string of all authors concatenated."
    (if (> (length authors-list) 1)
        (format "%s et al." (plist-get (nth 0 authors-list) :name))
      (plist-get (nth 0 authors-list) :name)))
  (defun my-search-print-fn (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (title (or (elfeed-meta entry :title)
                      (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (entry-authors (concatenate-authors
                           (elfeed-meta entry :authors)))
           (title-width (- (window-width) 10
                           elfeed-search-trailing-width))
           (title-column (elfeed-format-column
                          title 100
                          :left))
           (entry-score (elfeed-format-column (number-to-string (elfeed-score-scoring-get-score-from-entry entry)) 10 :left))
           (authors-column (elfeed-format-column entry-authors 40 :left)))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")

      (insert (propertize title-column
                          'face title-faces 'kbd-help title) " ")
      (insert (propertize authors-column
                          'kbd-help entry-authors) " ")
      (insert entry-score " ")))
  (setq elfeed-search-print-entry-function #'my-search-print-fn)
  (setq elfeed-search-date-format '("%y-%m-%d" 10 :left))
  (setq elfeed-search-title-max-width 110)
  (setq elfeed-feeds
	'("https://arxiv.org/rss/cs.SI" "https://arxiv.org/rss/cs.IR" "https://arxiv.org/rss/cs.HC" "https://arxiv.org/rss/cs.CY" "https://ijoc.org/index.php/ijoc/gateway/plugin/WebFeedGatewayPlugin/rss2" "https://journals.sagepub.com/action/showFeed?ui=0&mi=ehikzz&ai=2b4&jc=hijb&type=etoc&feed=rss" "https://share.osf.io/api/v2/feeds/atom/?elasticQuery=%7B%22bool%22%3A%7B%22must%22%3A%7B%22query_string%22%3A%7B%22query%22%3A%22*%22%7D%7D%2C%22filter%22%3A%5B%7B%22term%22%3A%7B%22sources%22%3A%22SocArXiv%22%7D%7D%5D%7D%7D" "https://osf.io/preprints/socarxiv/discover?subject=SocArXiv%7CSocial%20and%20Behavioral%20Sciences" "https://academic.oup.com/rss/site_6088/OpenAccess.xml" "https://academic.oup.com/rss/site_6088/advanceAccess_3963.xml" "https://academic.oup.com/rss/site_6088/3963.xml" "https://journals.sagepub.com/action/showFeed?ui=0&mi=ehikzz&ai=2b4&jc=nmsa&type=etoc&feed=rss" "https://journals.sagepub.com/connected/NMS#rss-feeds" "https://www.tandfonline.com/feed/rss/rica20" "https://www.tandfonline.com/feed/rss/upcp20" "https://www.tandfonline.com/journals/upcp20"))
(define-advice elfeed-search--header (:around (oldfun &rest args))
  (if elfeed-db
      (apply oldfun args)
    "No database loaded yet"))
)

(use-package elfeed-score
  :straight t
  :ensure t
  :after elfeed
  :config
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map)))

(use-package telega
  :straight t
  :commands (telega)
  :defer t
  :config
  (setq telega-server-libs-prefix "/opt/homebrew/Cellar/tdlib/1.8.0")
  (setq telega-use-docker t)
  (define-key global-map (kbd "C-c t") telega-prefix-map))

(use-package atomic-chrome
  :straight t
  :ensure t
  :config
  (atomic-chrome-start-server))

;; setup markdown-mode
(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init 
 ;(setq markdown-command "")
  (setq markdown-disable-tooltip-prompt 1)
  (setq markdown-code-block-braces t))

;; set-up bibligraphy work-flow
(use-package citar
  :straight t
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
  :config
  (setq citar-symbols
      `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
        (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
        (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (setq citar-symbol-separator "  "))

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(setq bibtex-completion-bibliography
      ;;      '("/Users/psd/Documents/linis/teach/digital social studies/DSS-course.bib")
;;      '("/Users/psd/Documents/personal/docs/Germany/SMIIA/references.bib")
      '("/Users/psd/Documents/linis/pfi22-proj2/references.bib"))
;;      '("/Users/psd/Documents/personal/docs/HEL-PhD/publics.bib"))

(setq bibtex-completion-pdf-field "File")

(setq bibtex-completion-pdf-open-function
  (lambda (fpath)
    (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath)))

;; Conda configuration
;(use-package conda
;  :ensure t
;  :init
;  (setq conda-anaconda-home (expand-file-name "/usr/local/Caskroom/miniforge/base/"))
;  (setq conda-env-home-directory (expand-file-name "/usr/local/Caskroom/miniforge/base/envs"))

;; Magit config
(use-package magit
  :straight t)

;; support for Graphviz and DOT
(use-package graphviz-dot-mode
  :straight t
  :config
  (setq graphviz-dot-indent-width 4))

;; R and S-family languages
(use-package ess
  :straight t
  :ensure ess
  :init
  (require 'ess-site) 
  :bind (:map ess-r-mode-map
	      ("M-P" . my/add-pipe)
	 :map inferior-ess-r-mode-map
	 ("M-P" . " |> "))
  :config
  (with-eval-after-load 'eglot
    (setq completion-category-defaults nil))
  ;; enable skeleton-pair insert globally
  (setq skeleton-pair t)
  ;;(setq skeleton-pair-on-word t)
  ;; Uncomment if curly braces won't close in .R files
  ;; https://github.com/emacs-ess/ESS/issues/296#issuecomment-189614821
  ;;(define-key ess-mode-map (kbd "{") nil)
  ;;(define-key ess-mode-map (kbd "}") nil) 
   (global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
   (global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
   (global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
   (global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
   (global-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
   (global-set-key (kbd "\`") 'skeleton-pair-insert-maybe)
   (global-set-key (kbd "<") 'skeleton-pair-insert-maybe)
   ;; syntax highlighting
   (setq ess-R-font-lock-keywords '((ess-R-fl-keyword:keywords . t)
				    (ess-R-fl-keyword:constants . t)
				    (ess-R-fl-keyword:modifiers . t)
				    (ess-R-fl-keyword:fun-defs . t)
				    (ess-R-fl-keyword:assign-ops . t)
				    (ess-R-fl-keyword:%op% . t)
				    (ess-fl-keyword:fun-calls . t)
				    (ess-fl-keyword:numbers . t)
				    (ess-fl-keyword:operators . t)
				    (ess-fl-keyword:delimiters . t)
				    (ess-fl-keyword:= . t)
				    (ess-R-fl-keyword:F&T . t)))
   (setq ess-nuke-trailing-whitespace-p t
	 ess-use-ido nil
	 ess-use-R-completion nil
	 ess-use-auto-complete nil
	 ess-use-company t)
   (setq ess-use-flymake nil)
   (defun my/add-pipe ()
     "Adds a pipe operator %>% with one space to the left and right"
     (interactive)
     (just-one-space 1)
     (insert "|>")
     (just-one-space 1))
;; An example of window configuration:
   (setq display-buffer-alist '(("*R Dired"
				 (display-buffer-reuse-window display-buffer-at-bottom)
				 (window-width . 0.5)
				 (window-height . 0.25)
				 (reusable-frames . nil))
				("*R"
				 (display-buffer-reuse-window display-buffer-in-side-window)
				 (side . right)
				 (slot . -1)
				 (window-width . 0.5)
				 (reusable-frames . nil))
				("*Help"
				 (display-buffer-reuse-window display-buffer-in-side-window)
				 (side . right)
				 (slot . 1)
				 (window-width . 0.5)
				 (reusable-frames . nil))))
  )


;; help with RMarkdown
(use-package poly-markdown
  :straight t
  :config
    (define-innermode poly-text-R-innermode
    :indent-offset 2
    :head-matcher (cons "^[ \t]*\\(```[ \t]*{?[[:alpha:]].*\n\\)" 1)
    :tail-matcher (cons "^[ \t]*\\(```\\)[ \t]*$" 1)
    :mode 'ess-r-mode
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-text-R-mode
    :hostmode 'pm-host/text
    :innermodes '(poly-text-R-innermode))
)
;;(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-R))
;; some help with Rmd files
 (use-package poly-R
   :straight t)

;; support for Julia
(use-package julia-mode
  :straight t)

(use-package python
  :straight (:type built-in)
  :interpreter ("python3" . python-mode)
  :config
  (add-hook 'python-hook (lambda ()
                                (setq
                                 python-indent-guess-indent-offset-verbose
                                 nil)
                                )
            )
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'python-mode-hook #'yas-minor-mode)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook (lambda () (setq eglot-connect-timeout 120)))
  (add-hook 'python-mode-hook (lambda () (setq eglot-autoshutdown t)))
  )

(use-package ein
  :straight t)

(use-package cider
  :straight t)

(use-package exec-path-from-shell
  :straight t)

(when (memq window-system '(mac ns))
  (setenv "SHELL" "/bin/zsh")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;; configure PATH for latex
;(when (memq window-system '(mac ns x))
;  (exec-path-from-shell-initialize))
(setq-default TeX-master nil)

;; pdf-tools test
(use-package pdf-tools
  :straight t
  :defer t
  :commands (pdf-view-mode pdf-tools-install)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic (("%PDF" . pdf-view-mode))
  :config
  (custom-set-variables          
    '(pdf-tools-handle-upgrades t))
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-annot-activate-created-annotations t)
  :hook ((pdf-view-mode-hook . pdf-tools-enable-minor-modes)))

(defun markdown-preview-file ()
  "use Marked 2 to preview the current file"
  (interactive)
  (shell-command 
   (format "open -a 'Marked 2.app' %s" 
       (shell-quote-argument (buffer-file-name))))
)
(global-set-key "\C-cm" 'markdown-preview-file)

;; completion with hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; completion in prog-mode
(use-package corfu
  :straight '( corfu :files (:defaults "extensions/*")
                         :includes (corfu-popupinfo))
  :custom
  (add-to-list 'corfu-margin-formatters #'+corfu-icons-margin-formatter)
  (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect-first t) ;; Disable candidate preselection
  (corfu-separator ?\s)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode t)
  :config
  (setq corfu-quit-at-boundary separator)
  (setq corfu-quit-no-match t)
  (setq corfu-popupinfo-delay t)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
	("M-SPC" . corfu-insert-separator))
  )

(straight-use-package
 '(corfu-terminal
   :type git
   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))

;; (use-package corfu-doc
;;   :straight t
;;   :init
;;   (global-corfu-mode)
;;   :hook
;;   (corfu-mode . corfu-doc-mode))

;; Use Dabbrev with Corfu!
;; (use-package dabbrev
;;   ;; Swap M-/ and C-M-/
;;   :bind (("M-/" . dabbrev-completion)
;;          ("C-M-/" . dabbrev-expand))
;;   ;; Other useful Dabbrev configurations.
;;   :custom
;;   (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; fuzzy search for corfu
(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult Wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic substring)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; Add extensions
(use-package cape
  :straight t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . incompletion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :hook ((ess-r-mode . sp/cape-capf-setup-ess)
	 (inferior-ess-mode . sp/cape-capf-setup-ess)
	 (ess-mode . sp/cape-capf-setup-ess)
	 (ess-roxy-mode . sp/cape-capf-setup-ess)
	 (ess-mode . sp/cape-capf-setup-ess)
	 (Poly-Markdown+R . sp/cape-capf-setup-ess)
	 (eglot-managed-mode-hook . sp/cape-capf-setup-ess))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line)
;;  (advice-add #'company-R-objects :around #'cape-wrap-noninterruptible)
;;  (add-hook 'prog-mode-hook #'+cape-set-up-programming-capfs)
;;  (add-hook 'text-mode-hook #'+cape-set-up-writing-capfs)
  ;;  (add-hook 'conf-mode-hook #'+cape-set-up-conf-caps)
 (defun sp/cape-capf-setup-ess ()
   (setq-local completion-at-point-functions
	       (list (cape-super-capf
		      (cape-company-to-capf #'company-R-objects)
		      (cape-company-to-capf #'company-R-library)
		      (cape-company-to-capf #'company-R-args)
				      #'cape-dabbrev
				      #'ess-r-object-completion
				      #'ess-r-package-completion
				      )
		     #'eglot-completion-at-point
		     ))
    ))

(use-package svg-lib
  :straight (svg-lib :type git :host github :repo "rougier/svg-lib"))

(use-package kind-icon
  :straight t
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (add-hook 'my-completion-ui-mode-hook
   	    (lambda ()
   	      (setq completion-in-region-function
   		    (kind-icon-enhance-completion
   		     completion-in-region-function))))
  (setq kind-icon-default-style 
   '(:padding 1.6 :stroke 0 :margin 0 :radius 1.6 :height 1.6 :scale 1.1))
  )

;; Language Servers
(use-package eglot
  :straight t
  :config
  (setq completion-category-overrides '((eglot (styles orderless))))
  (add-hook 'ess-r-mode-hook #'eglot-ensure)
  (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))      
  (add-hook 'python-mode #'eglot-ensure)
  (setq-default eglot-workspace-configuration
      '((haskell
         (maxCompletions . 200))))
  (setq eglot-stay-out-of '(company cape corfu))
  :preface
  (defun mp-eglot-eldoc ()
    (setq eldoc-documentation-strategy
            'eldoc-documentation-compose-eagerly))
  :hook ((eglot-managed-mode . mp-eglot-eldoc)
	 (ess-mode . eglot-ensure)
	 (inferior-ess-mode . eglot-ensure)
	 )
  )

(use-package eldoc
  :straight (:type built-in)
  :preface
   (add-to-list 'display-buffer-alist
               '("^\\*eldoc for" display-buffer-at-bottom
                 (window-height . 4)))
   (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :config
   (eldoc-add-command-completions "paredit-")
   (eldoc-add-command-completions "combobulate-"))

(use-package flycheck
  :straight (:type built-in)
  :preface
  (defun mp-flycheck-eldoc (callback &rest _ignored)
    "Print flycheck messages at point by calling CALLBACK."
    (when-let ((flycheck-errors
		(and flycheck-mode (flycheck-overlay-errors-at (point)))))
      (mapc
       (lambda (err)
         (funcall callback
		  (format "%s: %s"
			  (let ((level (flycheck-error-level err)))
			    (pcase level
			      ('info (propertize "I" 'face 'flycheck-error-list-info))
			      ('error (propertize "E" 'face 'flycheck-error-list-error))
			      ('warning (propertize "W" 'face 'flycheck-error-list-warning))
			      (_ level)))
			  (flycheck-error-message err))
		  :thing (or (flycheck-error-id err)
			     (flycheck-error-group err))
		  :face 'font-lock-doc-face))
       flycheck-errors)))
  (defun mp-flycheck-prefer-eldoc ()
    (add-hook 'eldoc-documentation-functions #'mp-flycheck-eldoc nil t)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    (setq flycheck-display-errors-function nil)
    (setq flycheck-help-echo-function nil))
  :hook
  ((flycheck-mode . mp-flycheck-prefer-eldoc))
  )

;; Configure terminals

(use-package term
  :straight (:type built-in)
  :config
  (setq explicit-shell-file-name "bash")
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :straight t
  :hook (term-mode . eterm-256color-mode))

;; Dired Configuration
(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (setq insert-directory-program "/opt/homebrew/opt/coreutils/libexec/gnubin/ls")
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches "-algho --group-directories-first"))

;; not sure how to configure it but mb useful
;;(use-package dired-single)

;; hide dot files by default
(use-package dired-hide-dotfiles
  :straight t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (define-key dired-mode-map "." 'dired-hide-dotfiles-mode))

(use-package flymake-proselint
  :straight t
  :config
  (add-hook 'text-mode-hook (lambda ()
                            (flymake-mode)
                            (flymake-proselint-setup)))
  (add-hook 'markdown-mode-hook #'flymake-proselint-setup)
  (add-hook 'org-mode-hook #'flymake-proselint-setup))

;; Spelling and Writing
(use-package flycheck
  :straight t
;  :init (global-flychek-mode)
  :config
  (flycheck-add-mode 'proselint 'org-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package beacon
  :straight t
  :config
  (beacon-mode 1))

(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs
	'("~/Documents/personal/snippets"))  ;;my snippets are here
  (yas-global-mode 1))

;; (defalias 'fm 'fly-spell-mode)
;; (defalias 'ss 'ispell-buffer)

(use-package ispell  ;; use aspell instead of ispell which is no longer maintained
  :straight t
  :after flyspell
;  :no-require t
  :config
  (setq-default ispell-program-name "/opt/homebrew/bin/aspell")  ;; testing if it will hell to restore spellchecking
  (setq ispell-dictionary "american")
  (setq highlight-face (quote flyspell-incorrect))
  (setq ispell-silently-savaep t))

(use-package flyspell
  :straight (:type built-in)
  :defer t
  ;; :init
  ;; (progn
  ;;   (add-hook 'message-mode-hook 'turn-on-flyspell)
  ;;   (add-hook 'org-mode-hook 'flyspell-mode)
  ;;   (defalias 'fm flyspell-mode))
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'org-mode-hook 'flyspell-mode))

(use-package flyspell-correct
  :straight t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package auto-dictionary
  :straight t
  :after flyspell
  :config
  (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;; Base dir
(cd "~/")


(put 'dired-find-alternate-file 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(corfu-quit-at-boundary 'separator)
 '(custom-file nil)
 '(custom-safe-themes
   '("0f220ea77c6355c411508e71225680ecb3e308b4858ef6c8326089d9ea94b86f" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "aee6debe7b326de2968d8b023fdc9ee7e6c9996a80532186674f2e1376ad1782" default))
 '(ein:output-area-inlined-images t)
 '(elfeed-feeds
   '("https://export.arxiv.org/rss/cs.SI" "https://export.arxiv.org/rss/cs.IR" "https://export.arxiv.org/rss/cs.HC" "https://export.arxiv.org/rss/cs.CY" "https://ijoc.org/index.php/ijoc/gateway/plugin/WebFeedGatewayPlugin/rss2" "https://journals.sagepub.com/action/showFeed?ui=0&mi=ehikzz&ai=2b4&jc=hijb&type=etoc&feed=rss" "https://share.osf.io/api/v2/feeds/atom/?elasticQuery=%7B%22bool%22%3A%7B%22must%22%3A%7B%22query_string%22%3A%7B%22query%22%3A%22*%22%7D%7D%2C%22filter%22%3A%5B%7B%22term%22%3A%7B%22sources%22%3A%22SocArXiv%22%7D%7D%5D%7D%7D" "https://osf.io/preprints/socarxiv/discover?subject=SocArXiv%7CSocial%20and%20Behavioral%20Sciences" "https://academic.oup.com/rss/site_6088/OpenAccess.xml" "https://academic.oup.com/rss/site_6088/advanceAccess_3963.xml" "https://academic.oup.com/rss/site_6088/3963.xml" "https://journals.sagepub.com/action/showFeed?ui=0&mi=ehikzz&ai=2b4&jc=nmsa&type=etoc&feed=rss" "https://journals.sagepub.com/connected/NMS#rss-feeds" "https://www.tandfonline.com/feed/rss/rica20" "https://www.tandfonline.com/feed/rss/upcp20" "https://www.tandfonline.com/journals/upcp20"))
 '(ess-R-font-lock-keywords
   '((ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:constants . t)
     (ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:%op% . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters . t)
     (ess-fl-keyword:= . t)
     (ess-R-fl-keyword:F&T . t)))
 '(exec-path-from-shell-arguments '("-l"))
 '(exec-path-from-shell-check-startup-files nil)
 '(inferior-ess-r-font-lock-keywords
   '((ess-S-fl-keyword:prompt . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:constants . t)
     (ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:messages . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-fl-keyword:matrix-labels . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters . t)
     (ess-fl-keyword:= . t)
     (ess-R-fl-keyword:F&T . t)))
 '(warning-suppress-log-types '((use-package) (comp) (corfu-doc) (corfu-doc)))
 '(warning-suppress-types '((comp) (corfu-doc) (corfu-doc))))

