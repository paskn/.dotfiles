;;; early-init.el --- Emacs pre package.el & GUI configuration -*- lexical-binding: t; -*-
;;; Code:
(setq package-enable-at-startup nil)
(setq inhibit-default-init nil)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (defun reset-gc-cons-threshold ()
            (setq gc-cons-threshold 100000000 gc-cons-percentage 0.1)))

;; Prefer loading newest compiled .el filep
(customize-set-variable 'load-prefer-newer t)

;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)
   ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t))

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq
 frame-inhibit-implied-resize t ; Inhibit resizing frame
 package-quickstart nil ; Prevent package.el loading packages prior to their init-file
 package-enable-at-startup nil
 fast-but-imprecise-scrolling t ; More performant rapid scrolling over unfontified regions
 idle-update-delay 1.0  ; slow down UI updates down
 inhibit-compacting-font-caches t ; Inhibit frame resizing for performance
 read-process-output-max (* 1024 1024) ; Increase how much is read from processes in a single chunk.
 process-adaptive-read-buffering nil
 redisplay-skip-fontification-on-input t ; Inhibits it for better scrolling performance.
 vc-follow-symlinks t ; Do not ask about symlink following
 use-short-answers t ; y/n for yes/no
 confirm-kill-emacs 'yes-or-no-p        ;Confirm when exiting Emacs
)

;; Remove some unneeded UI elements
(setq inhibit-startup-message t) ;; hide the startup message
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
(setq visible-bell nil)

;; avoids the white screen flash on startup.
;; (load-theme 'ef-elea-dark t)

(add-to-list 'default-frame-alist
	     '((menu-bar-lines . 0)
	       (tool-bar-lines . 0)
	       (internal-border-width . 0)
	       (fullscreen . fullboth)
	       (vertical-scroll-bar . nil)
	       (horizontal-scroll-bars . nil)
	       ;; (ns-transparent-titlebar . t)
	       ;; (ns-appearance . dark)
           ))

(add-to-list 'default-frame-alist '(undecorated . t))

(add-to-list 'default-frame-alist
             '(font . "CommitMono-13"))

;;; early-init.el ends here
