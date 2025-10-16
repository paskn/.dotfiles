;; -*- lexical-binding: t; -*-
;; init.el --- Emacs configuration

;; make sure use-package always uses staight.el
(setq straight-use-package-by-default t)

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
(setq use-package-enable-imenu-support t
      use-package-compute-statistics 1)

;; change default garbage collection behavior
;; see https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns))
    (setenv "SHELL" "/bin/zsh")
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs
     '("PATH" "PKG_CONFIG_PATH"))))

;; Default shell in term
(unless
    (or (eq system-type 'windows-nt)
        (not (file-exists-p "/bin/zsh")))
  (setq-default shell-file-name "/bin/zsh")
  (setq explicit-shell-file-name "/bin/zsh"))

;; No cursor in inactive windows make sure to change it customize
;; buffer to enable it globally
(setq cursor-in-non-selected-windows nil)

;; make cursor hollow
(setq-default cursor-type 'hollow)

;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Tab.space equivalence
(setq-default tab-width 4)

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; Minimum window height
(setq window-min-height 1)

;; Buffer encoding
(set-language-environment "English")
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)
(add-to-list 'file-coding-system-alist '("\\.org" utf-8))

;; speed up emacs by deferring font locking
;; see https://teddit.hostux.net/r/emacs/comments/14c4l8j/way_to_make_emacs_feel_smoother/
;; and https://codeberg.org/ideasman42/emacs-jit-lock-stealth-progress
;; (setq jit-lock-stealth-time 1.25)
;; (setq jit-lock-stealth-nice 0.5) ;; Seconds between font locking.
;; (setq jit-lock-chunk-size 4096)

;; smooth scrolling
;; (pixel-scroll-mode 1)

;; more convenient order of cycling with C-l
(setq recenter-positions '(top middle bottom))

;; speed up emacs by disabling some behaviours of mode-line
;; see https://teddit.hostux.net/r/emacs/comments/14c4l8j/way_to_make_emacs_feel_smoother/
;; see also https://codeberg.org/ideasman42/emacs-mode-line-idle
(with-eval-after-load 'time
  ;; Donot show system load in mode line
  (setq display-time-default-load-average nil)
  ;; By default, the file in environment variable MAIL is checked
  ;; It's "/var/mail/my-username"
  ;; I set `display-time-mail-function' to display NO mail notification in mode line
  (setq display-time-mail-function (lambda () nil)))

;; Move backup files
(setq-default backup-directory-alist
              `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
              backup-by-copying t    ; Don't delink hardlinks
              version-control t      ; Use version numbers on backups
              delete-old-versions t  ; Automatically delete excess backups
              kept-new-versions 5    ; how many of the newest versions to keep
              kept-old-versions 5    ; and how many of the old version
              create-lockfiles nil)  ; No lock files

;; Speed up line movement
;; see https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)

;; should guard against possible lags on typing
;; (setq redisplay-skip-fontification-on-input t)

;; Cursor blinking is not necessary
(blink-cursor-mode 0)

;; better line spacing
;; see http://xahlee.info/emacs/emacs/emacs_toggle_line_spacing.html
(setq-default line-spacing 0.15)

;; Default mode is ‘text-mode’.  The actual default,
;; ‘fundamental-mode’ is rather useless.
(setq-default major-mode 'text-mode)

;; Underline at descent position
;; rougier/elegant-emacs
(setq x-underline-at-descent-line t)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

(global-hl-line-mode t)

;; set the default font
;;(set-face-attribute 'default nil :font "Ubuntu Mono")
;;(set-face-attribute 'default nil :font "IBM Plex Mono")
;;(setq default-frame-alist '((font . "IBM Plex Mono" )))
;; enlarge the default font size
;;(set-face-attribute 'default nil :height 130)
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :height 130 :family "CommitMono"))))
;;  '(fixed-pitch ((t (:height 130 :width normal :family "CommitMono")))))

;; prettify how line truncation is expressed
;; https://github.com/rougier/elegant-emacs/blob/d901cf9456b030707ee39ce7cc35e9b988040cf0/elegance.el#L67
(defface fallback '((t :family "IBM Plex Mono")) "Fallback")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'fallback))
(set-display-table-slot standard-display-table 'selective-display
                        (string-to-vector " …"))

;; use ESC to cancel prompt
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
;; save position of a cursor across sessions
(save-place-mode 1)
;; keep history and useful data across sections
(setq bookmark-save-flag 1)
(setq savehist-additional-variables '(register-alist kill-ring))
(savehist-mode 1)
;; Open read-only files in view-mode.
(setq-default view-read-only t)
;; Show keystrokes in the minibuffer area faster than the default.
(setq-default echo-keystrokes 0.1)
;; No cursor in inactive windows
(setq cursor-in-non-selected-windows nil)
;; ibuffer
(global-set-key [remap list-buffers] 'ibuffer)

(defvar ibuffer-group-buffers-by 'modes
  "If non nil ibuffer will group the buffers according to the passed symbol.
The supported values are `modes' to group by major-modes and `projects' to
group by projectile projects.")

(defun sp/ibuffer-group-by-modes ()
        "Group buffers by modes."
        (when (eq 'modes ibuffer-group-buffers-by)
          (sp/ibuffer-create-buffs-group)))
(add-hook 'ibuffer-hook 'sp/ibuffer-group-by-modes)

(defun sp/ibuffer-get-major-modes-ibuff-rules-list (mm-list result-list)
  (if mm-list
      (let* ((cur-mm (car mm-list))
             (next-res-list-el `(,(symbol-name cur-mm) (mode . ,cur-mm))))
        (sp/ibuffer-get-major-modes-ibuff-rules-list
         (cdr mm-list) (cons next-res-list-el result-list)))
    result-list))

(defun sp/ibuffer-get-major-modes-list ()
  (mapcar
   (function (lambda (buffer)
               (buffer-local-value 'major-mode (get-buffer buffer))))
   (buffer-list (selected-frame))))

(defun sp/ibuffer-create-buffs-group ()
  (interactive)
  (let* ((ignore-modes '(Buffer-menu-mode
                         compilation-mode
                         minibuffer-inactive-mode
                         ibuffer-mode
                         magit-process-mode
                         messages-buffer-mode
                         fundamental-mode
                         completion-list-mode
                         help-mode
                         Info-mode))
         (cur-bufs
          (list (cons "Home"
                      (sp/ibuffer-get-major-modes-ibuff-rules-list
                       (cl-set-difference
                        (cl-remove-duplicates
                         (sp/ibuffer-get-major-modes-list))
                        ignore-modes) '())))))
    (setq ibuffer-saved-filter-groups cur-bufs)
    (ibuffer-switch-to-saved-filter-groups "Home")))

;; INSTALL PACKAGES
;; --------------------------------------

;; fix navigation habits: use avy with Super-,
;; (use-package hardtime
;;   :straight (hardtime
;;              :type git
;;              :host github
;;              :repo "ichernyshovvv/hardtime.el")
;;   :config
;;   (hardtime-mode))


(use-package mwim
  :straight t
  :defer 1
  :init
  ;; "first stroke of C-a will move the cursor to the beginning of code.
  ;; Subsequent strokes will toggle between beginning of line and beginning of code."
  ;; "first stroke of C-e will move the cursor to the end of code (before comments).
  ;; Subsequent strokes will toggle between end of line and end of code."
  (progn
    (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
    (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)))

(use-package unfill
  :straight t
  :defer 1
  :commands (unfill-region unfill-paragraph unfill-toggle)
  :init
  ;; M-q will toggle between fill/unfill -paragraphs when
  ;; a major mode allows
  (global-set-key [remap fill-paragraph] #'unfill-toggle))

(defun sp/backward-kill-word-or-region (&optional arg)
  "Calls `kill-region' when a region is active and
`backward-kill-word' otherwise. ARG is passed to
`backward-kill-word' if no region is active."
  (interactive "p")
  (if (region-active-p)
      ;; call interactively so kill-region handles rectangular selection
      ;; correctly (see https://github.com/syl20bnr/spacemacs/issues/3278)
      (call-interactively #'kill-region)
    (backward-kill-word arg)))

(global-set-key (kbd "C-w") 'sp/backward-kill-word-or-region)

(use-package recentf
  :straight (:type built-in)
  :config
  (setq-default recentf-max-saved-items 50)
  (recentf-mode 1))

(use-package crux
  :straight t
  ;; :bind
  ;; ("C-k" . crux-kill-and-join-forward)
  :config
  (global-set-key (kbd "C-x 4 t") #'crux-transpose-windows)
  (global-set-key (kbd "C-c B") #'crux-cleanup-buffer-or-region)
  (global-set-key (kbd "C-c f") #'crux-recentf-find-file)
  (global-set-key (kbd "C-c F") #'crux-recentf-find-directory)
  (keymap-global-set "C-k" 'crux-kill-and-join-forward)
  (keymap-global-set "C-c d" 'duplicate-dwim)
  (keymap-global-set "C-c M-d" 'crux-duplicate-and-comment-current-line-or-region)
  (keymap-global-set "s-<return>" 'crux-smart-open-line)
  (keymap-global-set "s-o" 'crux-smart-open-line-above)
  (keymap-global-set "C-q" 'crux-other-window-or-switch-buffer)

  (defun toggle-window-split ()
    "Switch between horizontal and vertical split window layout."
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))
  (global-set-key (kbd "C-c j") #'toggle-window-split)
  )

;; Make C-v and M-v finally useful
(use-package golden-ratio-scroll-screen
  :straight t
  :config
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))

;; adjust M-> and M-< behavior to fit better certain modes
;; including dired, org-agenda and notmuch
(use-package beginend 
  :straight t
  :config  
  (beginend-global-mode))

;; track changes in the buffer in a tree-like structure
(use-package vundo
  :defer 1
  :straight t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;; declutter tab-bar looks
(use-package tab-bar
  :straight (:type built-in)
  :custom
  (tab-bar-new-button-show . nil)
  (tab-bar-close-button-show . nil))

;; Doom-modeline
(use-package all-the-icons
  :straight t)

(use-package all-the-icons-dired
  :straight t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(use-package ef-themes
  :straight (ef-themes :type git :host github :repo "protesilaos/ef-themes")
  :config
  :hook (emacs-startup . (lambda () (load-theme 'ef-reverie :no-confirm)))
  )

;; colorize color names
(use-package rainbow-mode
  :straight t
  :config
  (rainbow-mode))

(use-package spacious-padding
  :straight t
  :config
  (spacious-padding-mode 1))

(use-package svg-lib
  :straight (svg-lib :type git :host github :repo "rougier/svg-lib"))

;; (use-package oblique-strategies
;;   :straight (oblique-strategies :type git :host github :repo "zzkt/oblique-strategies")
;;   :config
;;   (setq oblique-edition
;;         "strategies/oblique-strategies-condensed.txt")
;;   (defalias 'insert-oblique-strategy #'oblique-strategy-at-point))

;; (use-package dashboard
;;   :straight t
;;   :init
;;   (defun sp/generate-strats ()
;;     (let (result)
;;       (dotimes (i 5 result)
;;         (setq result (cons (oblique-strategy) result)))))
;;   (setq dashboard-center-content t)
;;   (setq dashboard-week-agenda t)
;;   (setq dashboard-items '((agenda . 10)
;;                           (projects . 5)
;;                           (recents  . 5)))
;;   (setq dashboard-set-heading-icons t)
;;   (setq dashboard-set-file-icons t)
;;   (setq dashboard-set-footer 1)
;;   (setq dashboard-agenda-sort-strategy '(time-up))
;;   (setq dashboard-agenda-prefix-format " %i %-10s ")
;;   (setq dashboard-item-names '(("Agenda for the coming week:" . "Schedule:")))
;;   (setq dashboard-footer-messages (sp/generate-strats))
;;   (setq dashboard-footer-icon (all-the-icons-faicon "cogs"
;;                                                     :height 1.1
;;                                                     :v-adjust -0.05
;;                                                     :face 'font-lock-keyword-face))
;;   (set-face-attribute 'dashboard-items-face nil :weight 'normal)
;;   :config
;;   (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
;;   (dashboard-setup-startup-hook))

;; BASIC CUSTOMIZATION
;; --------------------------------------

;; Config to keep the cursor at the center of the screen
;; source: https://protesilaos.com/codelog/2020-07-16-emacs-focused-editing/
(use-package emacs
  :straight nil
  :config
  ;; (pixel-scroll-precision-mode 1)
  (setq show-paren-delay 0)
  (setq-default scroll-preserve-screen-position t)
  (setq-default scroll-conservatively 1) ; affects `scroll-step'
  (setq-default scroll-margin 0)
  (setq undo-outer-limit 24844840 * 5)
  (electric-pair-mode 1)
  (keymap-global-set "C-c v" 'view-mode)
  ;; make cursor red when repeat-mode is active
  ;; https://gist.github.com/jdtsmith/a169362879388bc1bdf2bbb977782d4f
  (add-hook 'post-command-hook
            (defalias 'my/repeat-change-cursor ; change cursor to bar during repeat
              (let (repeat-p ccol)
                (lambda ()
                  (unless (eq repeat-p repeat-in-progress)
                    (if repeat-in-progress ; turning on
		                (setq ccol (face-background 'cursor)))
                    (setq repeat-p repeat-in-progress)
                    (set-cursor-color
		             (if repeat-in-progress
		                 (face-foreground 'error)
		               ccol))))))
            90)

  :init
  ;; TAB ycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  :bind
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
  ;; Show dictionary definition on the left
  ;; https://github.com/SystemCrafters/crafted-emacs/blob/f7c6abae32726e4f38e4cf103a931a9af605a76f/modules/crafted-mastering-emacs.el#L63C42-L63C42
  (add-to-list 'display-buffer-alist
               '("^\\*Dictionary\\*"
                 (display-buffer-in-side-window)
                 (side . left)
                 (window-width . 70)))
  ;; define a key to define the word at point.
  (define-key global-map (kbd "C-c D") #'dictionary-lookup-definition)
  ;; hide windows
  (add-to-list 'display-buffer-alist
               '("\\`\\*Async Shell Command\\*\\'"
                 (display-buffer-no-window))
               '("\\`\\*\\(Warnings\\|Compile-Log\\|Org Links\\)\\*\\'"
                 (display-buffer-no-window)
                 (allow-no-window . t)))
  )

;; isearch -- https://emacsredux.com/blog/2025/03/18/you-have-no-idea-how-powerful-isearch-is/
(use-package isearch
  :straight (:type built-in)
  :config
  ;; When isearching, enable M-<, M->, C-v and M-v to skip between matches
  ;; in an intuitive fashion.  Note that the `cua-selection-mode' bindings
  ;; for C-v and M-v bindings are not supported.
  (setq isearch-allow-motion t
        isearch-motion-changes-direction t)
  ;; Make regular Isearch interpret the empty space as a regular
  ;; expression that matches any character between the words you give
  ;; it.
  (setq search-whitespace-regexp ".*?"))

;; improve scrolling
(use-package ultra-scroll
  :straight (ultra-scroll :type git :host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mode 1))

(use-package vterm
  :straight t
  :defer t
  :config
  (add-hook 'vterm-mode (lambda () (setq-local global-hl-line-mode nil)))
  (add-hook 'vterm-copy-mode (lambda () (call-interactively 'hl-line-mode))))

;; more precision in movement across windows
;; act on windows during C-x o:
;; x -- delete etc.
(use-package ace-window
  :straight t
  :bind ("C-x o" . ace-window))

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
         ;; ("M-g f" . consult-flymake)            ;; Alternative: consult-flycheck
         ("M-g f" . consult-flycheck)
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
  ;; M-s . with consult
  ;; see here for details https://arialdomartini.github.io/consult-line-at-point
  (defun consult-line-symbol-at-point ()
    "Search for a line matching the symbol found near point."
    (interactive)
    (consult-line
     (or (thing-at-point 'symbol))))
  
  (global-set-key (kbd "M-s .") #'consult-line-symbol-at-point)
  (global-set-key (kbd "M-s M-s .") #'isearch-forward-symbol-at-point)


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

;; (use-package consult-flycheck
;;   :straight t)

(use-package consult-yasnippet
  :straight t)

(use-package consult-project-extra
  :straight t)

(use-package consult-eglot
  :straight t
  :after eglot)

(use-package marginalia
  :straight t
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
   ;; ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
    (which-key--show-keymap
     (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
     (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
     nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
    '(embark-which-key-indicator
      embark-highlight-indicator
      embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package all-the-icons-completion
  :straight t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; testing a gentler version of smartparens
(use-package paredit
  :straight t)

(use-package paredit-everywhere
  :straight t
  :hook (prog-mode-hook . paredit-everywhere-mode))

;; cycle selection regions
;; (use-package expand-region
;;   :straight t
;;   :bind ("C-=" . er/expand-region))

(use-package expreg
  :straight t
  :bind ("C-=" . expreg-expand))

;; store and manage window configuration
;; C-c <left> and C-c <right>
(use-package winner
  :straight nil
  :config
  (winner-mode 1))

;; (use-package centered-window
;;   :straight t
;;   :config
;;   (centered-window-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; use C-u 0 C-c M-, to give a description
;; of the change you made at a particular stop
(use-package goto-chg
  :straight t
  :config
  (global-set-key (kbd "C-c M-,") 'goto-last-change)
  (global-set-key (kbd "C-c M-.") 'goto-last-change-reverse)
  (defvar change-navigation-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "," #'goto-last-change)
      (define-key map "." #'goto-last-change-reverse)
      map)
    "Keymap to repeat buffer change navigation key sequences.  Used in `repeat-mode'.")

  (put 'goto-last-change 'repeat-map 'change-navigation-repeat-map)
  (put 'goto-last-change-reverse 'repeat-map 'page-navigation-repeat-map)
  )

;; set-mark-command: repeat map for C-u C-SPACE

(defun sp/jump-previous-location ()
  "Jump to previous set mark; equivalen of C-u C-SPACE."
  (interactive)
  (set-mark-command 4))

;; bind the command to M-o and enable it in repeat mode
(global-set-key (kbd "M-o") 'sp/jump-previous-location)

(defvar mark-jump-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "o" #'sp/jump-previous-location)
      map)
    "Keymap to repeat jumping to previous mark in the buffer key sequences.  Used in `repeat-mode'.")
(put 'sp/jump-previous-location 'repeat-map 'mark-jump-repeat-map)

(use-package avy
  :straight t
  :init
  ;; (global-set-key (kbd "s-,") 'avy-goto-word-1)
  (global-set-key (kbd "s-,") 'avy-goto-char-timer)
)

;; jump by links avy-style
;; https://github.com/abo-abo/ace-link supports less links
(use-package link-hint
  :straight t
  :bind
  ("s-\\" . link-hint-open-link))

(use-package which-key
  :straight (:type built-in)
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq wich-key-idle-delay 0.3))

;; (use-package prescient
;;   :straight t
;;   :config
;;   (prescient-persist-mode t))

;; (use-package corfu-prescient
;;   :straight t
;;   ;; :defer t
;;   :after corfu
;;   :config
;;   (corfu-prescient-mode 1))

;; Enable vertico
(use-package vertico
  ;;  :straight t
  ;; :defer t
  :straight (:files (:defaults "extensions/*"))
  ;; :straight (vertico
  ;;         :type git
  ;;         :host github
  ;;         :repo "emacs-straight/vertico"
  ;;         :files ("*" (:exclude ".git") (:defaults "extensions/*")))
  :hook (after-init . vertico-mode)
  :init
  ;; (vertico-mode)
  ;; (vertico-prescient-mode t)
  ;; hide prompted path when entering file name shadowing
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0))
  ;; Show more candidates
  ;; (setq vertico-count 20)
  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; (use-package vertico-prescient
;;   :straight t
;;   :after vertico)

;; replaces ^L with lines
(use-package page-break-lines
  :defer 1
  :straight t
  :config
  (global-page-break-lines-mode))

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

;; put the command dispatchet on the center of the screen
(use-package vertico-posframe
  :straight t
  :after vertico
  :init
  (vertico-posframe-mode))

(use-package which-key-posframe
  :straight t
  :after which-key
  :init
  (which-key-posframe-mode))

;; ;; see here why its not working in 30.1 https://github.com/emacsorphanage/transient-posframe/wiki
;; (use-package transient-posframe
;;   :straight (transient-posframe :type git :host github :repo "emacsorphanage/transient-posframe")
;;   :init
;;   (transient-posframe-mode))

(use-package powerthesaurus
  :defer t
  :straight t)

(use-package langtool
  :defer t
  :straight t
  :config
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

(use-package auto-fill-mode
  :straight nil
  :hook
  (text-mode . auto-fill-mode)
  (org-mode . auto-fill-mode)
  :config
  (setq-default fill-column 70) ;; Set the desired line width
  )

(use-package volatile-highlights
  :defer 1
  :straight t
  :config
  (volatile-highlights-mode t))

;; teach emacs the difference betwee selecting and highlighing
;; improves hl-line-mode
(use-package lin
  :demand t
  :straight t
  :config
  (lin-global-mode 1))

;; Let emacs to decide what to do with very long lines
(use-package so-long
  :defer 1
;;  :after-call find-file-hook
  :straight t
  :config
  (global-so-long-mode))

;; a mode to work with graphviz
(use-package graphviz-dot-mode
  :defer t
  :straight (graphviz-dot-mode
         :type git
         :host github
         :repo "ppareit/graphviz-dot-mode")
  :config
  (setq graphviz-dot-indent-width 4))

(use-package d2-mode
  :defer t
  :straight t
  )

(use-package ob-d2
  :defer t
  :straight t)

;;Add German holidays
(add-hook 'calendar-load-hook
              (lambda ()
                (calendar-set-date-style 'iso)))
(setq calendar-date-style 'iso)

(use-package org
  :straight (:type built-in)
  :init
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

  (setq calendar-holidays
        '(;; German public holidays during 2023
          ;; https://de.wikipedia.org/wiki/Gesetzliche_Feiertage_in_Deutschland
          (holiday-fixed 1 1 "New Year’s Day (Neujahrstag)")
          (holiday-fixed 4 7 "Good Friday (Karfreitag)")
          (holiday-fixed 4 10 "Easter Monday (Ostermontag)")
          (holiday-fixed 5 1 "Labor Day (Maifeiertag)")
          (holiday-fixed 5 18 "Ascension Day (Christi Himmelfahrt, 40 days after Easter)")
          (holiday-fixed 5 29 "Whit Monday (Pfingstmontag) – seventh Monday after Easter, also called Pentecost Monday")
          (holiday-fixed 10 3 "Day of German Unity (Tag der Deutschen Einheit) ")
          (holiday-fixed 12 25 "Christmas Day (Weihnachtstag)")
          (holiday-fixed 12 26 "Saint Stephen’s Day (Stephanstag) – also known as the second day of Christmas")
          ;; German regional holidays 2023
          (holiday-fixed 1 6 "Epiphany (Heilige Drei Könige) – Baden-Württemberg, Bavaria, and Saxony-Anhalt")
          (holiday-fixed 3 8 "International Women’s Day – Berlin")
          (holiday-fixed 4 9 "Easter Sunday – Brandenburg")
          (holiday-fixed 5 28 "Whit Sunday – Brandenburg")
          (holiday-fixed 6 8 "Corpus Christi (Fronleichnam) Bavaria")
          (holiday-fixed 8 8 "Peace Festival (Freidenfest) – Bavaria (Augsburg)")
          (holiday-fixed 8 15 "Assumption Day (Maria Himmelfahrt) – Saarland and some local authorities in Bavaria")
          (holiday-fixed 10 31 "Reformation Day (Reformationstag) – generally a regional holiday in Brandenburg")
          (holiday-fixed 11 1 "All Saints’ Day (Allerheiligen) – Baden-Württemberg, Bavaria, North Rhine-Westphalia, Rhineland-Palatinate, and Saarland")
          (holiday-fixed 11 22 "Day of Prayer and Repentance (Buß-und Bettag, Wednesday before 23 November) – Saxony")
          ;; Important dates in Germany during 2023
          (holiday-fixed 2 20 "Shrove Monday")
          (holiday-fixed 2 21 "Shrove Tuesday, also known as Carnival")
          (holiday-fixed 2 22 "Ash Wednesday, also known as Carnival")
          (holiday-fixed 3 26 "Clocks go forward one hour as a result of daylight saving time starting")
          (holiday-fixed 5 14 "Mother’s Day (second Sunday of May)")
          (holiday-fixed 5 18 "Father’s Day (Vatertag, also known as Männertag/Herrentag, Men’s Day) – coincides with Ascension Day and can be a family celebration or celebrated by an outing with male friends")
          (holiday-fixed 9 9 "German Language Day")
          (holiday-fixed 9 10 "European Heritage Days – when monument buildings are opened to the public")
          (holiday-fixed 9 16 "Oktoberfest starts")
          (holiday-fixed 9 20 "German World Children’s Day")
          (holiday-fixed 10 29 "Clocks go back one hour as a result of daylight saving time ending")
          (holiday-fixed 11 9 "Fall of the Berlin Wall")
          (holiday-fixed 11 11 "St Martin’s Day – a religious observance where children take part in lantern processions")
          (holiday-fixed 11 19 "National Day of Mourning – victims of war are remembered and in some regions, music or dance events are illegal")
          (holiday-fixed 12 6 "Saint Nicholas Day")
          ))

  :demand t
  :hook (org-mode . sp/org-mode-setup)
  :custom
  (setq org-latex-listings 'minted  ;; enable code highlighing and svg in pdf
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"))
  (org-agenda-include-diary 1)
  (org-list-allow-alphabetical t)
  :config
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-insert-heading-respect-content t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)")))
  ;; after opening jump straight to the first heading
  (add-hook 'org-mode-hook (lambda () (org-next-visible-heading 1)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     ;;   (python . t)
     (R . t)
     (dot . t)
     (d2 . t)
     ;;   (ledger . t) ; where is ob-ledger??
     ))
  
  (add-to-list 'display-buffer-alist
               '("^\\*Org Agenda\\*"
                 (display-buffer-reuse-mode-window
                  display-buffer-in-side-window)
                 (side . right)
                 (window-width . 90)
                 (mode-line-format . none)
                 ))

  (setq org-use-speed-commands t)

  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-image-actual-width nil)
  (set-face-attribute 'fixed-pitch nil :font "CommitMono")
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

(use-package org-modern
  :after org
  :straight t
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

;; improve compile look and make close it on success
(use-package fancy-compilation
  :defer 1
  :straight t
  :custom
  (fancy-compilation-override-colors nil)
  :config
  (fancy-compilation-mode))

;; has side-effect on the content of the buffer
;; (use-package bury-successful-compilation
;;   :straight t
;;   :bind ("C-c C-m" . recompile)
;;   :config
;;   (bury-successful-compilation))

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

;; live by timer instead of clock
;; pomodoro and third time
(use-package pomm
  :defer t
  :straight t
  :commands (pomm pomm-third-time)
  :config
  ;; use macos for alerts
  (setq alert-default-style 'osx-notifier))

(use-package citeproc
  :straight t
  :defer t
  :config
  (setq org-cite-csl-styles-dir "~/Zotero/styles"))

(use-package oc-csl
  :defer t
  :straight (:type built-in))

;; help org and markdown to align tables
(use-package valign
  :defer t
  :straight t
  ;;  :after-call org-mode-hook
  :hook (org-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))

(use-package sicp
  :straight t)

(use-package denote
  :straight t
  :custom
  (denote-directory "~/org/denote")
  )

(use-package denote-journal
  :straight t
  :hook (calendar-mode . denote-journal-calendar-mode)
  :config
  ;; Use the "journal" subdirectory of the `denote-directory'.  Set this
  ;; to nil to use the `denote-directory' instead.
  (setq denote-journal-directory
        (expand-file-name "journal" denote-directory))
  ;; Default keyword for new journal entries. It can also be a list of
  ;; strings.
  (setq denote-journal-keyword "journal")
  ;; Read the doc string of `denote-journal-title-format'.
  (setq denote-journal-title-format 'day-date-month-year))

(use-package consult-notes
  :straight t
  :after denote
  :bind (("C-c n f" . consult-notes))
  :config
  (consult-notes-denote-mode))

(use-package consult-denote
  :straight t
  :config
  (consult-denote-mode 1))

(use-package mastodon
  :straight t
  :defer t
  :config
  (setq mastodon-instance-url "https://mastodon.social"
        mastodon-active-user "pashakhin"))


(use-package notmuch
  :straight t
  :bind
  (("C-c m" . notmuch-hello))
  :config
  ;; (global-set-key (kbd "C-c m") 'notmuch-hello)
  (setq
   notmuch-show-all-tags-list t
   notmuch-column-control 1.0)
  (setq message-sendmail-envelope-from 'header)
  (setq notmuch-always-prompt-for-sender t)
  ;; set up sent mail mailbox
  (setq notmuch-fcc-dirs '(("saig.tk@gmail.com" . "saigtk-gmail/sent")
                           ("pashakhin@gmail.com" . "pashakhin-gmail/[Gmail]/Sent Mail")
                           ("sergei.pashakhin@uni-bamberg.de" . "bamberg/Sent Items")))
  
  ;; use the right port for gmail
  ;; (setq message-send-mail-function 'smtpmail-send-it
  ;;       smtpmail-starttls-credentials
  ;;       '(("smtp.gmail.com" 465 nil nil))
  ;;       smtpmail-default-smtp-server "smtp.gmail.com"
  ;;       smtpmail-smtp-server "smtp.gmail.com"
  ;;       smtpmail-smtp-service 465
  ;;       smtpmail-debug-info t)
  ;; set up with the smtpmail.el
  ;; credentials for sending mail
  (setq smtpmail-servers-requiring-authorization "[.*gmail.com]|[.*uni-bamberg.de]")
  )

;; add to org support for linking to notmuch
(use-package ol-notmuch
  :straight (ol-notmuch :type git :host github :repo "tarsius/ol-notmuch"))

(use-package smtpmail-multi
  :straight t
  :config
  ;; not sure what this is but everyone says it should be default
  (setf epg-pinentry-mode 'loopback)
  ;; fix hanging when 29.1 and gpg > 2.4.0
  (fset 'epg-wait-for-status 'ignore)
  (setenv "GPG_AGENT_INFO" nil)
  (setq smtpmail-auth-credentials "~/.authinfo.gpg")
  (setq smtpmail-multi-accounts '((bamberg . ("sergei.pashakhin@uni-bamberg.de"
                                              "exhub.uni-bamberg.de"
                                              587
                                              t
                                              starttls
                                              nil
                                              nil
                                              "uni-bamberg.de"
                                              ;; nil
                                              ))
                                  (pashakhin . ("pashakhin@gmail.com"
                                                "smtp.gmail.com"
                                                465
                                                nil
                                                ssl
                                                nil
                                                nil
                                                "gmail.com"))
                                  (saig.tk . ("saig.tk@gmail.com"
                                              "smtp.gmail.com"
                                              465
                                              nil
                                              ssl
                                              nil
                                              nil
                                              "gmail.com"))
                                  ))
  (setq smtpmail-multi-associations '(("saig.tk@gmail.com" saig.tk)
                                      ("pashakhin@gmail.com" pashakhin)
                                      ("sergei.pashakhin@uni-bamberg.de" bamberg)))
  (setq smtpmail-multi-default-account 'pashakhin)
  (setq send-mail-function 'smtpmail-multi-send-it)
  (setq message-send-mail-function 'smtpmail-multi-send-it)
  )

(use-package elfeed
  :straight t
  :defer t
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
  ;; :defer t
  :straight t
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
  (define-key global-map (kbd "C-c t") telega-prefix-map)
  (setq telega-server-libs-prefix "~/td/tdlib/")
  (setq telega-use-docker nil))

;; setup markdown-mode
(use-package markdown-mode
  :straight t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
;(setq markdown-command "")
  (setq markdown-disable-tooltip-prompt 1)
  (setq markdown-code-block-braces t)
  (setq markdown-fontify-code-blocks-natively t)
)

;; set-up bibligraphy work-flow
(use-package citar
  :after org
  :straight t
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-notes-paths '("~/Documents/personal/RoamNotes/citar/"))
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
  :config
  (citar-embark-mode)
  (setq citar-org-roam-note-title-template "${author} - ${title}")
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :target
           (file+head
            "%<%Y%m%d%H%M%S>-${slug}.org"
            "#+title: ${note-title}\n")
           :unnarrowed t)
          ("n" "literature note" plain
           "%?"
           :target
           (file+head
            "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${citar-citekey}.org"
            "#+title: ${citar-citekey} (${citar-date}). ${note-title}.\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t)))
  (setq citar-org-roam-capture-template-key "n")
  :custom
  (setq citar-notes-paths '("~/Documents/personal/RoamNotes/citar/"))
  )

(use-package citar-denote
  :straight t
  :config
  :custom (citar-denote-subdir "~/org/denote/literature/")) 

;; (use-package citar-org-roam
;;   :straight t
;;   :after (citar org-roam)
;;   :config (citar-org-roam-mode))

;; (use-package citar-org
;;   :straight t
;;   :after citar
;;   :custom
;;   (org-cite-insert-processor 'citar)
;;   (org-cite-follow-processor 'citar)
;;   (org-cite-activate-processor 'citar))

;; navigate code semantically with ripgrep
(use-package dumb-jump
  :straight t
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  (setq dumb-jump-force-searcher 'rg))

;; Magit config
;; (use-package transient
;;   :straight (transient :type git :host github :repo "magit/transient")
;; )

(use-package direnv
  :straight t
  :config
  (direnv-mode))

(use-package magit
  :straight t
  ;; :defer 1
  ;; :after transient
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-diff-hide-trailing-cr-characters t)
  ;; :custom
  ;; (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  ;; (magit-diff-hide-trailing-cr-characters t)
  )

;; https://github.com/armindarvish/consult-gh
(use-package consult-gh
  :straight (consult-gh :type git :host github :repo "armindarvish/consult-gh")
  :after consult
  :config
  (require 'consult-gh-transient)
  (require 'consult-gh-embark)
  (require 'consult-gh-forge)
  (consult-gh-embark-mode +1)
  (consult-gh-forge-mode +1))

(use-package treesit-auto
  :defer 1
  :straight t
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

;; Docker config
(use-package dockerfile-mode
  :straight t
  :config
  ;; hacky way to remap major a mode to its -ts-analog
  ;; https://github.com/camdencheek/tree-sitter-dockerfile
  (setq major-mode-remap-alist
        '((dockerfile-mode . dockerfile-ts-mode)))
  ;; for docker-compose files, we can use the built-in treesit mode
  ;; https://github.com/ikatyang/tree-sitter-yaml
  ;; or the dedicated mode as is the case below
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode)))

(use-package docker-compose-mode
  :straight t
  :after dockerfile-mode)

(use-package docker
  :straight t
  :defer t
  :bind ("C-c C-d" . docker))

;; R and S-family languages
(use-package ess
  :straight t
  :defer t
  :init
  (require 'ess-site)
  :bind (:map ess-r-mode-map
              ("M-P" . my/add-pipe)
              :map inferior-ess-r-mode-map
              ("M-P" . " |> "))
  :custom
  (ess-style 'RStudio)
  :config
  ;; (with-eval-after-load 'eglot
  ;;   (setq completion-category-defaults nil))
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
        ess-use-R-completion t
        ess-use-auto-complete 'script-only
        ess-use-company nil)
  ;; fix code highlighting in repl
  ;; https://github.com/emacs-ess/ESS/issues/1199
  (defun my-inferior-ess-init ()
    (setq-local ansi-color-for-comint-mode 'filter))
  (add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)
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
  (ess-set-style 'RStudio)
  )

;; help with RMarkdown
(use-package poly-markdown
  :defer t
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
;; some help with Rmd files
(use-package poly-R
  :defer t
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-R)))

(use-package quarto-mode
  :defer t
  :mode (("\\.qmd" . poly-quarto-mode)))

(use-package conda
  :defer t
  :config
  (setq conda-env-home-directory
        (expand-file-name "~/.conda/")))

;; ;; support for Julia
;; (use-package julia-mode
;;   :defer t
;;   :straight t
;;   :hook (eglot-lj-init))

;; (use-package julia-snail
;;   :defer t
;;   :straight t
;;   :hook (julia-mode . julia-snail-mode))

;; (use-package eglot-jl
;;   :after eglot
;;   :straight t
;;   :config
;;   (eglot-jl-init))

(use-package python
  :straight (:type built-in)
  :interpreter ("python3" . python-mode)
  :init
  ;; enable treesit
  (setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))
  :config
  ;; (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'python-mode-hook #'yas-minor-mode)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook (lambda () (setq eglot-connect-timeout 120)))
  (add-hook 'python-mode-hook (lambda () (setq eglot-autoshutdown t)))
  ;; clean completions from trash
  (defun python-eldoc-filter (orig-fun string)
    (let ((clean (if (stringp string) (replace-regexp-in-string "\n__PYTHON_EL_eval.+" "" string) string)))
      (funcall orig-fun clean)))
  (advice-add 'eldoc--message :around #'python-eldoc-filter)

  (defun my-python-completion-filter (completions)
    "Filter out the unwanted '__PYTHON_EL' from COMPLETIONS."
    (cl-remove-if (lambda (completion) (string-match-p "__PYTHON_EL" completion))
                  completions))
  (advice-add 'python-shell-completion-get-completions :filter-return #'my-python-completion-filter)
  )


(use-package pyvenv
  :straight t)

;; * blacken       -- buffer formatting on save using black
;;                    (need to pip install black)
(use-package blacken
  :defer t
  :straight t
  :init
  (add-hook 'python-mode-hook #'blacken-mode))

;; * anaconda      -- code navigation, documentation and completion
;; * pythonic      -- utility packages for running python in different
;;                    environments (dependency of anaconda)
(use-package anaconda-mode
  :defer t
  :straight t
  :init
  (add-hook 'python-mode-hook #'anaconda-mode))

(use-package ein
  :defer t
  :straight t)

(use-package cider
  :straight t
  :defer t)

;; pdf-tools
(use-package pdf-tools
  :straight t
  :defer t
  :init
  ;; (setenv "PKG_CONFIG_PATH" 
  ;;         (concat "/opt/homebrew/opt/poppler/lib/pkgconfig:" (getenv "PKG_CONFIG_PATH")))
  ;; (setenv "PKG_CONFIG_PATH" 
  ;;         (concat "/opt/homebrew/opt/glib/lib/pkgconfig/:" (getenv "PKG_CONFIG_PATH")))
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

;; Epub support
(use-package nov
  :defer t
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width 70))

;; completion in prog-mode
(use-package corfu
  ;; :straight (:files (:defaults "extensions/*"))
  :defer 1
  :straight (corfu
         :type git
         :host github
         :repo "emacs-straight/corfu"
         :files ("*" "extensions/*.el" (:exclude ".git")))
  :custom
  (corfu-auto-delay 0.0)                ;No delay for completion
  (corfu-quit-at-boundary 'separator)
  (add-to-list 'corfu-margin-formatters #'+corfu-icons-margin-formatter)
  (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect-first 'prompt) ;; Disable candidate preselection
  (corfu-separator ?\s)
  (corfu-popupinfo-delay '(0 . 0))
  :hook ((corfu-mode . corfu-popupinfo-mode))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode t)
  (corfu-history-mode t)
  :config
  (setq corfu-quit-no-match t)
  ;; (corfu-prescient-mode 1)
  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode)))
  ;; :bind
  ;; (:map corfu-map
  ;;       ("TAB" . corfu-next)
  ;;       ([tab] . corfu-next)
  ;;       ("S-TAB" . corfu-previous)
  ;;       ([backtab] . corfu-previous)
  ;;       ("M-SPC" . corfu-insert-separator)
  ;;       ("M-p" . corfu-popupinfo-scroll-down)
  ;;       ("M-n" . corfu-popupinfo-scroll-up)
  ;;       ("M-d" . corfu-popupinfo-toggle))
)

;; (use-package corfu-doc
;;   :straight t
;;   :init
;;   (global-corfu-mode)
;;   :hook
;;   (corfu-mode . corfu-doc-mode))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . cape-dabbrev)
         ("C-M-/" . hippie-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package hippie-expand
  :straight (:type built-in)
  :custom
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs 
     try-expand-list 
     try-expand-dabbrev-all-buffers
     try-expand-line
     try-expand-dabbrev-from-kill 
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol)))

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
;;   :hook (
;; (ess-r-mode . sp/cape-capf-setup-ess)
;;      (inferior-ess-mode . sp/cape-capf-setup-ess)
;;      (ess-mode . sp/cape-capf-setup-ess)
;;      (ess-roxy-mode . sp/cape-capf-setup-ess)
;;      (ess-mode . sp/cape-capf-setup-ess)
;;      (Poly-Markdown+R . sp/cape-capf-setup-ess)
;;      (eglot-managed-mode-hook . sp/cape-capf-setup-ess))
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

(use-package kind-icon
  :straight t
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
  ;; (setq kind-icon-default-style
  ;;  '(:padding 1.6 :stroke 0 :margin 0 :radius 1.6 :height 1.6 :scale 1.1))
  )

(use-package tramp
  :straight (:type built-in)
  :custom
  (tramp-allow-unsafe-temporary-files t)
  :config
  ;; see here https://old.reddit.com/r/emacs/comments/1g1bz04/lspdocker_with_pylsppyright_because_my_new_job/
  ;; it should ensure that tramp uses the remote path ??
  (add-to-list ‘tramp-remote-path ‘tramp-own-remote-path)
  )

;; Language Servers
(use-package eglot
  :defer t
  :straight t
  :custom
  (eglot-sync-connect nil)
  ;; (eglot-sync-connect 1)
  (eglot-events-buffer-size 0)
  (eglot-connect-timeout nil)
  :config
  (setq eglot-confirm-server-initiated-edits nil)
  (fset #'jsonrpc--log-event #'ignore)
  (setq completion-category-overrides '((eglot (styles orderless))))
  ;; (add-hook 'ess-r-mode-hook #'eglot-ensure)
  (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
  (add-hook 'python-mode #'eglot-ensure)
  (setq eglot-stay-out-of '(company cape corfu))
  :preface
  (defun mp-eglot-eldoc ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))
  :bind (:map
         eglot-mode-map
         ("C-c c a" . eglot-code-actions)
         ("C-c c o" . eglot-code-actions-organize-imports)
         ("C-c c r" . eglot-rename)
         ("C-c c f" . eglot-format))
  :hook ((eglot-managed-mode . mp-eglot-eldoc)
         ;; not reliable over TRAMP
         ;; (ess-mode . eglot-ensure)
         ;; (inferior-ess-mode . eglot-ensure)
         (julia-mode . eglot-ensure)
         (python-mode . eglot-ensure)
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
   (eldoc-add-command-completions "combobulate-")
   (eldoc-add-command #'corfu-insert))

(use-package eldoc-box
  :straight t
  :after eldoc
  :hook (eldoc-mode . eldoc-box-hover-mode)
  :config
  ;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
  (setq eldoc-box-max-pixel-width 300))

;; (use-package flycheck
;;   :straight t
;;   :defer 80
;;   :preface
;;   (defun mp-flycheck-eldoc (callback &rest _ignored)
;;     "Print flycheck messages at point by calling CALLBACK."
;;     (when-let ((flycheck-errors
;;         (and flycheck-mode (flycheck-overlay-errors-at (point)))))
;;       (mapc
;;        (lambda (err)
;;          (funcall callback
;;           (format "%s: %s"
;;               (let ((level (flycheck-error-level err)))
;;                 (pcase level
;;                   ('info (propertize "I" 'face 'flycheck-error-list-info))
;;                   ('error (propertize "E" 'face 'flycheck-error-list-error))
;;                   ('warning (propertize "W" 'face 'flycheck-error-list-warning))
;;                   (_ level)))
;;               (flycheck-error-message err))
;;           :thing (or (flycheck-error-id err)
;;                  (flycheck-error-group err))
;;           :face 'font-lock-doc-face))
;;        flycheck-errors)))
;;   (defun mp-flycheck-prefer-eldoc ()
;;     (add-hook 'eldoc-documentation-functions #'mp-flycheck-eldoc nil t)
;;     (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
;;     (setq flycheck-display-errors-function nil)
;;     (setq flycheck-help-echo-function nil))
;;   :hook
;;   ((flycheck-mode . mp-flycheck-prefer-eldoc))
;;   :custom
;;   ;; Override default flycheck triggers
;;   (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
;;   (flycheck-idle-change-delay 0.8)
;;   :config
;;   ;; (flycheck-add-mode 'proselint 'org-mode)
;;   (add-hook 'after-init-hook #'global-flycheck-mode)
;;   )

;; Dired Configuration
(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (setq insert-directory-program "/opt/homebrew/opt/coreutils/libexec/gnubin/ls")
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches "-algho --group-directories-first")
  (setq dired-dwim-target t)
  ;; Automatically hide the detailed listing when visiting a Dired
  ;; buffer.  This can always be toggled on/off by calling the
  ;; `dired-hide-details-mode' interactively with M-x or its keybindings
  ;; (the left parenthesis by default).
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  ;; Teach Dired to use a specific external program with either the
  ;; `dired-do-shell-command' or `dired-do-async-shell-command' command
  ;; (with the default keys, those are bound to `!' `&', respectively).
  ;; The first string is a pattern match against file names.  The
  ;; remaining strings are external programs that Dired will provide as
  ;; suggestions.  Of course, you can always type an arbitrary program
  ;; despite these defaults.
  (setq dired-guess-shell-alist-user (list
                                      (list ".*" "open")))
  )

(use-package dired-x
  :straight (:type built-in)
  :after dired)

;; HIDE dot files by default
(use-package dired-hide-dotfiles
  :straight t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (define-key dired-mode-map "." 'dired-hide-dotfiles-mode))

;; Spelling and Writing

;; help for writing papers
(use-package academic-phrases
  :straight t
  :defer t)

;; ;; use vale for prose
;; (use-package flycheck-vale
;;   :straight t
;;   :config
;;   (flycheck-vale-setup))

(use-package beacon
  :straight t
  :config
  (beacon-mode 1))

(use-package yasnippet
  :defer 1
  :straight t
  :config
  (yas-global-mode 1))

;; better behavior for comment-dwim
(use-package smart-comment
  :straight t
  :bind ("M-;" . smart-comment))

(use-package jinx
  :straight t
  :init
  ;; https://github.com/minad/jinx/discussions/160#discussioncomment-9111820
  (setenv "PKG_CONFIG_PATH"
          (concat "/opt/homebrew/opt/glib/lib/pkgconfig/:" (getenv "PKG_CONFIG_PATH"))
  ;; ;;         (concat "/opt/homebrew/Cellar/enchant/2.8.1/lib/pkgconfig/:" (getenv "PKG_CONFIG_PATH"))
          )
  :bind (("C-;" . jinx-correct))
  :config
  (add-hook 'emacs-startup-hook #'global-jinx-mode))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;;;; Fix emacs-mac info
;; For some reason emacs-mac doesn't generate a dir file in
;; /opt/homebrew/Cellar/emacs-mac/emacs-28.1-mac-9.0/share/info/emacs
;;
;; Fix this with
;;   for F in `echo *.info.gz`; do
;;       install-info $F dir
;;   done
;;
;; It also appears this nonsense is required to actually READ that
;; list (even though we don't change the info path at all?).
(use-package info
  :config
  (info-initialize))

;; Base dir
(cd "~/")

;; store and use custom-* separately
(defconst custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (with-temp-file custom-file
    (insert ";; Autogenerated Emacs custom.el file")))
(load custom-file)

(use-package envrc
  :straight t
  :hook (after-init . envrc-global-mode))

(provide 'init)
;;; init.el ends here
