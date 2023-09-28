;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;; Code:

(put 'dired-find-alternate-file 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-detailed t)
 '(copy-region-blink-delay 0)
 '(corfu-quit-at-boundary 'separator nil nil "Customized with use-package corfu")
 '(cursor-in-non-selected-windows nil)
 '(custom-safe-themes
   '("0f220ea77c6355c411508e71225680ecb3e308b4858ef6c8326089d9ea94b86f" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "aee6debe7b326de2968d8b023fdc9ee7e6c9996a80532186674f2e1376ad1782" default))
 '(dashboard-projects-backend 'project-el)
 '(delete-selection-mode t)
 '(dictionary-server "dict.org")
 '(dictionary-use-single-buffer t)
 '(dired-auto-revert-buffer t)
 '(dired-free-space 'separate)
 '(doc-view-mupdf-use-svg t)
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
 '(ess-indent-with-fancy-comments nil)
 '(exec-path-from-shell-arguments '("-l"))
 '(exec-path-from-shell-check-startup-files nil)
 '(flycheck-display-errors-function 'flycheck-pos-tip-error-messages)
 '(flycheck-indication-mode nil)
 '(flycheck-pos-tip-mode t)
 '(fringe-mode 0 nil (fringe))
 '(global-mark-ring-max 8)
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
 '(isearch-allow-motion t)
 '(isearch-lazy-count t)
 '(isearch-yank-on-move t)
 '(large-file-warning-threshold 100000000)
 '(mark-ring-max 5)
 '(org-latex-pdf-process
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f") t)
 '(pdf-tools-handle-upgrades t)
 '(python-indent-guess-indent-offset-verbose nil)
 '(python-shell-completion-native-enable nil)
 '(ring-bell-function 'ignore)
 '(save-interprogram-paste-before-kill t)
 '(set-mark-command-repeat-pop t)
 '(show-paren-when-point-inside-paren t)
 '(visible-bell nil)
 '(warning-suppress-log-types '((use-package) (comp) (corfu-doc) (corfu-doc)))
 '(warning-suppress-types '((straight) (comp) (corfu-doc) (corfu-doc)))
 '(xref-show-definitions-function 'xref-show-definitions-completing-read))

(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;;; custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :height 130 :family "CommitMono"))))
 '(doom-modeline-bar-inactive ((t nil)))
 '(fixed-pitch ((t (:height 130 :width normal :family "CommitMono"))))
 '(fringe ((t :background "#edf4f8")))
 '(window-divider ((t :background "#edf4f8" :foreground "#edf4f8")))
 '(window-divider-first-pixel ((t :background "#edf4f8" :foreground "#edf4f8")))
 '(window-divider-last-pixel ((t :background "#edf4f8" :foreground "#edf4f8"))))
