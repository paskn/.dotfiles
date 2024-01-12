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
   '("3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "0c08a5c3c2a72e3ca806a29302ef942335292a80c2934c1123e8c732bb2ddd77" "2f8eadc12bf60b581674a41ddc319a40ed373dd4a7c577933acaff15d2bf7cc6" "49acd691c89118c0768c4fb9a333af33e3d2dca48e6f79787478757071d64e68" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "2e98350610ed0594dab8c55fcb4b4d781e020bf022dda8e7e23b038208cb6e5e" "9278d5ab312f1e9de3e1470a93c177db9093051955c316e28a6c76c4049c90d9" "6c49dc039a835cc165502093ce3ab2a0d6b06f868dfd04727e88b0d8bc252fa0" "0f220ea77c6355c411508e71225680ecb3e308b4858ef6c8326089d9ea94b86f" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "aee6debe7b326de2968d8b023fdc9ee7e6c9996a80532186674f2e1376ad1782" default))
 '(dashboard-image-banner-max-height 250)
 '(dashboard-projects-backend 'project-el)
 '(delete-selection-mode t)
 '(dictionary-server "dict.org")
 '(dictionary-use-single-buffer t)
 '(dired-auto-revert-buffer t)
 '(dired-free-space 'separate)
 '(doc-view-imenu-enabled t)
 '(doc-view-mupdf-use-svg t)
 '(doc-view-pdf->png-converter-function 'doc-view-pdf->png-converter-mupdf)
 '(doc-view-resolution 100)
 '(doom-modeline-bar-width 3)
 '(doom-modeline-height 10)
 '(doom-modeline-hud nil)
 '(doom-modeline-hud-min-height 0)
 '(doom-modeline-mode t)
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
 '(ess-style 'RStudio)
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
 '(mode-line-compact 'long)
 '(notmuch-hello-sections
   '(notmuch-hello-insert-saved-searches notmuch-hello-insert-alltags))
 '(notmuch-saved-searches
   '((:name "BambergNew" :query "tag:Bamberg and tag:unread" :key
            [98])
     (:name "academic" :query "tag:academic-general and tag:unread" :key
            [97])
     (:name "subscriptions" :query "tag:subscriptions" :key
            [115])
     (:name "PaperTrail" :query "tag:paper-trail and tag:unread" :key
            [112])
     (:name "inbox" :query "tag:inbox" :key
            [105])
     (:name "flagged" :query "tag:flagged" :key
            [102])
     (:name "sent" :query "tag:sent" :key
            [116])))
 '(notmuch-search-oldest-first nil)
 '(notmuch-show-logo nil)
 '(notmuch-wash-wrap-lines-length 68)
 '(org-latex-pdf-process
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"))
 '(pdf-tools-handle-upgrades t)
 '(python-indent-guess-indent-offset-verbose nil)
 '(python-shell-completion-native-enable nil)
 '(register-preview-delay nil)
 '(ring-bell-function 'ignore)
 '(save-interprogram-paste-before-kill t)
 '(set-mark-command-repeat-pop t)
 '(show-paren-when-point-inside-paren t)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(treesit-font-lock-level 4)
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
 '(fringe ((t :background "#ffffff")))
 '(header-line ((t :box (:line-width 4 :color "#f2f2f2" :style nil))))
 '(header-line-highlight ((t :box (:color "#000000"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "#ffffff")))
 '(mode-line ((t :box (:line-width 6 :color "#c8c8c8" :style nil))))
 '(mode-line-active ((t :box (:line-width 6 :color "#c8c8c8" :style nil))))
 '(mode-line-highlight ((t :box (:color "#000000"))))
 '(mode-line-inactive ((t :box (:line-width 6 :color "#e6e6e6" :style nil))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "#ffffff" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "#c2c2c2" :style nil))))
 '(window-divider ((t :background "#ffffff" :foreground "#ffffff")))
 '(window-divider-first-pixel ((t :background "#ffffff" :foreground "#ffffff")))
 '(window-divider-last-pixel ((t :background "#ffffff" :foreground "#ffffff"))))
