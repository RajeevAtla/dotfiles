;;remove warnings

;;---- PACKAGES -------------------------------------
;; straight.el
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



;; use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;; update on startup
(straight-pull-all)
(straight-pull-recipe-repositories)

;;----------------------------------------------------------

;;---- BACKUPS ------------------------------------------------

;; disable backups
(setq make-backup-files nil)
(setq auto-save-default nil)


;;-------------------------------------------------------------

;;---- APPEARANCE -----------------------------------------------

;; use seti theme
(straight-use-package 'seti-theme)
(load-theme 'seti t)

;;use JetBrains Mono font
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-10.5"))
(add-to-list 'default-frame-alist '(line-spacing . 0.2))

;;use font ligatures
(defconst jetbrains-ligature-mode--ligatures
  '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
    "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
    "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
    "|||>" "<|||" ">>" "<<" "::=" "|]" "[|" "{|" "|}"
    "[<" ">]" ":?>" ":?" "/=" "[||]" "!!" "?:" "?." "::"
    "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
    "<:" ":<" ":>" ">:" "<>" "***" ";;" "/==" ".=" ".-" "__"
    "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
    ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
    "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
    "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
    "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
    "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
    "&="))

(sort jetbrains-ligature-mode--ligatures (lambda (x y) (> (length x) (length y))))

(dolist (pat jetbrains-ligature-mode--ligatures)
  (set-char-table-range composition-function-table
                      (aref pat 0)
                      (nconc (char-table-range composition-function-table (aref pat 0))
                             (list (vector (regexp-quote pat)
                                           0
                                    'compose-gstring-for-graphic)))))

;; Visual indentation
(straight-use-package 'indent-guide)
(indent-guide-global-mode)


;; all the icons
(use-package all-the-icons
  :if (display-graphic-p))

;; set buffer name to file name
(setq frame-title-format "%b")

;; doom modeline
(straight-use-package 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-indent-info t)
(setq doom-modeline-enable-word-count t)



;; nyan cat modeline - disabled to improve performance
;;(straight-use-package 'nyan-mode)
;;(setq nyan-animate-nyancat t)
;;(setq nyan-wavy-trail t)
;;(define-globalized-minor-mode my-global-nyan-mode nyan-mode
  ;;(lambda () (nyan-mode 1)))
;;(my-global-nyan-mode 1)

;; parrot modeline - disabled to improve performance
;; (straight-use-package 'parrot)
;; (parrot-mode)
;; (parrot-start-animation)
;; (setq parrot-num-rotations nil)
;; (setq parrot-animate-parrot t)

;; highlight selected buffer
(straight-use-package 'dimmer)
(dimmer-configure-which-key)
(dimmer-configure-helm)
(dimmer-mode t)

;; disable scrollbar
(toggle-scroll-bar -1)

;; line numbers
(global-linum-mode t)

;; remove whitespace
(straight-use-package 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)


;;-------------------------------------------------------


;;---- UNDO -----------------------------------------------
(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

;;---------------------------------------------------------

;;---- STARTUP ---------------------------------------------
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t
      initial-buffer-choice  nil)

;; completely remove scratch buffer
(kill-buffer "*scratch*")

;; dashboard
(straight-use-package 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-center-content t) ;; center
(setq dashboard-startup-banner 'logo)
(setq dashboard-set-file-icons t)
(setq dashboard-set-heading-icons t)
(setq dashboard-projects-backend 'projectile)
(setq dashboard-banner-logo-title "Welcome to Emacs, Rajeev!")
(setq dashboard-show-shortcuts nil)
(setq dashboard-set-navigator t)

(setq dashboard-items '((recents  . 5)))

(view-buffer "*dashboard*")


;; use utf-8 encoding
(set-language-environment "UTF-8")


;;----------------------------------------------------------

;;---- FLYCHECK -------------------------------------------
(setq straight-fix-flycheck t)
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;-------------------------------------------------------


;;----- CLOJURE -----------------------------------------
(use-package cider
  :ensure t)

(setq cider-repl-display-help-banner nil)


;;----------------------------------------------------------


;;----- TREEMACS ------------------------------------------
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-expand-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   4
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
	  treemacs-pulse-on-success              nil
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-litter-directories            '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              t
          treemacs-silent-refresh                t
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :hook (after-init . #'treemacs-find-file)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-all-the-icons
  :after treemacs)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(setq debug-on-error nil)

(defun treemacs-setup-title ()
  (let ((bg (face-attribute 'default :background))
        (fg (face-attribute 'default :foreground)))
    (face-remap-add-relative 'header-line
                             :background bg :foreground fg
                             :box `(:line-width ,(/ (line-pixel-height) 2) :color ,bg)))
  (setq header-line-format
        '((:eval
           (let* ((text (treemacs-workspace->name (treemacs-current-workspace)))
                  (extra-align (+ (/ (length text) 2) 1))
                  (width (- (/ (window-width) 2) extra-align)))
             (concat (make-string width ?\s) text))))))

(add-hook 'treemacs-setup-title 'treemacs-mode-hook)

;;-----------------------------------------------------------

;;--- AUTOCOMPLETE ---------------------------------------
(straight-use-package 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(straight-use-package 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)


(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") #'company-complete-common-or-cycle) ; complete using tab
  (define-key company-active-map (kbd "<backtab>") (lambda () (interactive) (company-complete-common-or-cycle -1)))) ; cycle completions


;; change background
(require 'color)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; company mode icons using all-the-icons
(defface company-icon+ `((t (:inherit company-tooltip)))
  "Face for the margin icon in the `company-mode' tooltip."
  :group 'company-faces)

(defface company-current-icon+ `((t (:inherit company-tooltip-selection)))
  "Face for the margin icon for the current candidate in the `company-mode' tooltip."
  :group 'company-faces)

(defconst all-the-icons-lsp-kinds+
  (apply
   #'cons
   (cl-loop
    for parent-face in '(company-icon+ company-current-icon+)
    collect `((text        . ,(all-the-icons-material "text_fields"                 :face parent-face))
              (method      . ,(all-the-icons-material "functions" :face `(:inherit (all-the-icons-lyellow ,parent-face))))
              (function    . ,(all-the-icons-material "functions" :face `(:inherit (all-the-icons-lyellow ,parent-face))))
              (constructor . ,(all-the-icons-material "create"                    :face parent-face))
              (field       . ,(all-the-icons-material "adjust" :face `(:inherit (all-the-icons-lorange ,parent-face))))
              (variable    . ,(all-the-icons-material "adjust"                      :face parent-face))
              (class       . ,(all-the-icons-material "class" :face `(:inherit (all-the-icons-lred ,parent-face))))
              (interface   . ,(all-the-icons-material "settings_input_component"    :face parent-face))
              (module      . ,(all-the-icons-material "view_module"                 :face parent-face))
              (property    . ,(all-the-icons-material "settings"                    :face parent-face))
              (unit        . ,(all-the-icons-material "straighten"                  :face parent-face))
              (value       . ,(all-the-icons-material "filter_1"                    :face parent-face))
              (enum        . ,(all-the-icons-material "plus_one" :face `(:inherit (all-the-icons-lorange ,parent-face))))
              (keyword     . ,(all-the-icons-material "filter_center_focus"         :face parent-face))
              (snippet     . ,(all-the-icons-material "short_text"                  :face parent-face))
              (template    . ,(all-the-icons-material "short_text"                  :face parent-face))
              (color       . ,(all-the-icons-material "palette"                     :face parent-face))
              (file        . ,(all-the-icons-material "insert_drive_file"           :face parent-face))
              (folder      . ,(all-the-icons-material "folder"                      :face parent-face))
              (reference   . ,(all-the-icons-material "collections_bookmark"        :face parent-face))
              (struct      . ,(all-the-icons-material "streetview"                  :face parent-face)))))
  "Association between `eglot' LSP kinds and annotation icons for `company-mode'.
To reduce the amount of redundant processing in the margin function, this is defined
as a cons cell of icon alists, with the car alist being for regular candidates in the
company popup and the cdr alist for the current-candidate.

This structure means you don't have to do any processing, or propertising to pick an
icon for a candidate. A simple alist lookup is all you need... it might be worth
turning this into a hashset.")

(defconst all-the-icons-default-completion-icon+
  (cons (all-the-icons-faicon "leaf" :face 'company-icon+)
        (all-the-icons-faicon "leaf" :face 'company-current-icon+)))

(setq company-format-margin-function
      (defun company-format-margin-function+ (candidate selected)
        (concat
         (make-string company-tooltip-margin ? )
         (or
          (when-let ((kind (company-call-backend 'kind candidate)))
            (alist-get kind
                       (if selected
                           (cdr all-the-icons-lsp-kinds+)
                         (car all-the-icons-lsp-kinds+))))
          (if selected
              (cdr all-the-icons-default-completion-icon+)
            (car all-the-icons-default-completion-icon+))))))


;; corfu for completion overlay
(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-echo-documentation nil) ;; Do not show documentation in the echo area
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :init
  (corfu-global-mode))

(straight-use-package 'company-statistics)
(add-hook 'after-init-hook 'company-statistics-mode)



;;----------------------------------------------------


;;---- RUST ------------------------------------------------


;;----------------------------------------------------------


;;---- DISCORD ---------------------------------------------
;; rich prescence
(straight-use-package 'elcord)
(elcord-mode)
(setq elcord-quiet t)
(setq elcord-editor-icon "doom_icon")


;;----------------------------------------------------------

;;---- PYTHON --------------------------------------------

;;guess tabs or spaces
(add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)
(add-hook 'python-mode-hook (lambda ()
                              (guess-style-guess-tab-width)))






;;----------------------------------------------------------

;;---- WEB DEV ------------------------------------------

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;;indentation
(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))
(add-hook 'web-mode-hook  'web-mode-init-hook)

;;auto-pairing
(setq web-mode-enable-auto-pairing t)

;;css colorization
(setq web-mode-enable-css-colorization t)





;;-------------------------------------------------------


;;------ LaTeX ------------------------------------
;;enable document parsing (with AUC TeX)
(setq TeX-auto-save t)
(setq TeX-parse-self t)


;;use latexmk
(straight-use-package 'auctex-latexmk)
(auctex-latexmk-setup)

;; autocomplete using company-auctex and company-math
;;(straight-use-package 'company-auctex)
;;(company-auctex-init)

(straight-use-package 'company-math)
(defun my-latex-mode-setup ()
  (setq-local company-backends
              (append '((company-math-symbols-latex company-latex-commands))
                      company-backends)))
(add-hook 'TeX-mode-hook 'my-latex-mode-setup)

;; use flycheck to check syntax
(add-hook 'TeX-mode-hook #'flymake-mode)

;;---------------------------------------------------------

;;----- PROJECTILE ----------------------------------------
;;for project management, etc.
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;---------------------------------------------------------

;;------ SPEED TYPE -----------------------------------------

(straight-use-package 'speed-type)

;;---------------------------------------------------------------


;;------- LSP-MODE -------------------------------------------

(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)

;;-----------------------------------------------------------

;;-------SYMON--------------------------------------------
;; symon is a gui monitor

;;(straight-use-package 'symon)
;;(symon-mode)


;;----------------------------------------------------------


;;----- RESTART --------------------------------------------
(require 'restart-emacs)
(setq restart-emacs-restore-frames t)

;;------------------------------------------------------------

;;------ INIT -----------------------------------------------
(setq gc-cons-threshold 100000000)
;; DO NOT TOUCH!!!
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1f6039038366e50129643d6a0dc67d1c34c70cdbe998e8c30dc4c6889ea7e3db" default))
 '(doc-view-continuous t)
 '(elcord-display-elapsed t)
 '(elcord-refresh-rate 60)
 '(jdee-server-dir "~/jdee-server/target")
 '(package-selected-packages
   '(rainbow-mode undo-fu fireplace lsp-focus focus restart-emacs solaire-mode modus-themes doom-themes dimmer undo-tree orderless corfu aggressive-indent nyan-mode all-the-icons doom-modeline cargo rust-mode add-node-modules-path emmet-mode prettier-js flycheck-color-mode-line web-mode smart-tabs-mode js2-mode helm-lsp helm which-key lsp-ui pdf-tools blacken all-the-icons-ivy all-the-icons-ivy-rich lsp-mode auctex-latexmk auctex auto-complete-auctex ocp-indent ocamlformat merlin-ac smartparens company auto-package-update hackernews emr gh indent-guide f polymode golden-ratio async jedi treemacs dune git-commit tuareg regex-dsl regex-tool merlin python-black elcord use-package seti-theme python-mode py-autopep8 projectile paradox magit flycheck elpy auto-complete))
 '(paradox-github-token t)
 '(symon-delay 0.1)
 '(symon-monitors
   '(symon-windows-memory-monitor symon-windows-cpu-monitor symon-windows-battery-monitor)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#2cf931423366"))))
 '(company-scrollbar-fg ((t (:background "#2107242c25bf"))))
 '(company-tooltip ((t (:inherit default :background "#19dc1c521d8e"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))

;;----------------------------------------------------------

