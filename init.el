;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))


(leaf company
  :bind (("C-M-i" . company-complete)
	 (company-active-map
	  ("C-n" . company-select-next))
	 (company-active-map
	  ("C-p" . company-select-previous))
	 (company-search-map
	  ("C-n" . company-select-next))
	 (company-search-map
	  ("C-p" . company-select-previous))
	 (company-active-map
	  ("C-s" . company-filter-candidates))
	 (company-active-map
	  ("C-i" . company-complete-selection))
	 (company-active-map
	  ([tab]
	   . company-complete-selection))
	 (company-active-map
	  ("C-f" . company-complete-selection))
	 (emacs-lisp-mode-map
	  ("C-M-i" . company-complete)))
  :custom ((company-transformers quote
				 (company-sort-by-backend-importance))
	   (company-idle-delay . 0)
	   (company-minimum-prefix-length . 3)
	   (company-selection-wrap-around . t)
	   (completion-ignore-case . t)
	   (company-dabbrev-downcase))
  :config  
  (leaf-handler-package company company nil)
  (global-company-mode)
  :ensure t
  )

(leaf yasnippet
  :ensure t
  :blackout yas-minor-mode
  :custom ((yas-indent-line . 'fixed)
	   (yas-global-mode . t)
	   )
  :bind ((yas-keymap
	  ("<tab>" . nil))            ; conflict with company
	 (yas-minor-mode-map
	  ("C-c y i" . yas-insert-snippet)
	  ("C-c y n" . yas-new-snippet)
	  ("C-c y v" . yas-visit-snippet-file)
	  ("C-c y l" . yas-describe-tables)
	  ("C-c y g" . yas-reload-all)))
  :config
  (leaf yasnippet-snippets :ensure t)
  (leaf yatemplate
    :ensure t
    :config
    (yatemplate-fill-alist))
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
	      '(:with company-yasnippet))))
  (defun set-yas-as-company-backend ()
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
    )
  :hook
  ((company-mode-hook . set-yas-as-company-backend))
  )



(leaf recentf
  :custom ((recentf-save-file . "~/.emacs.d/.recentf")
	   (recentf-max-saved-items . 200)
	   (recentf-exclude quote
			    (".recentf"))
	   (recentf-auto-cleanup quote never))
  :config
  (run-with-idle-timer 30 t
		       '(lambda nil
			  (with-suppressed-message
			   (recentf-save-list))))
  :ensure t
  )



(leaf ivy
  :when (version<= "24.5" emacs-version)
  :custom ((ivy-use-virtual-buffers . t)
	   (enable-recursive-minibuffers . t)
	   (ivy-height . 30)
	   (ivy-extra-directories)
	   (ivy-re-builders-alist quote
				  ((t . ivy--regex-plus))))
  :config
  (leaf-handler-package ivy ivy nil)
  (ivy-mode 1)
  :ensure t
  )


(leaf dumb-jump
  :when (version<= "24.3" emacs-version)
  :bind (("M-." . dumb-jump-go)
	 ("M-]" . dumb-jump-back))
  :custom ((dumb-jump-mode . t)
	   (dumb-jump-selector quote ivy)
	   (dumb-jump-use-visible-window))
  :config
  (leaf-handler-package dumb-jump dumb-jump nil)
  :ensure t)

(leaf dracula-theme
  :when (version<= "24.3" emacs-version)
  :config
  (leaf-handler-package dracula-theme dracula-theme nil)
  (load-theme 'dracula t)
  (when (version<= "26.0.50" emacs-version)
    (progn
      (global-display-line-numbers-mode)
      (set-face-attribute 'line-number nil :foreground "gray")
      (set-face-attribute 'line-number-current-line nil :foreground "red")))
  :ensure t
  )

(leaf counsel
  :ensure t
  :when (version<= "24.5" emacs-version)
  :config
  (leaf-handler-package counsel counsel nil)
  (with-eval-after-load 'swiper
    (eval-after-load 'ivy
      '(progn
	 (global-set-key
	  (kbd "M-x")
	  'counsel-M-x)
	 (global-set-key
	  (kbd "C-x C-f")
	  'counsel-find-file)
	 (defvar counsel-find-file-ignore-regexp
	   (regexp-opt
	    '("./" "../")))
	 )))
  (counsel-mode)
  )


(leaf recentf-ext
  :bind (([(super r)]
	  . counsel-recentf))
  :config
  (leaf-handler-package recentf-ext recentf-ext nil)
  :ensure counsel)

(leaf irony
  :ensure company
  :config
  (leaf-handler-package irony irony nil)
  (custom-set-variables
   '(irony-additional-clang-options
     '("-std=c++17")))
					;    (add-to-list 'company-backends 'company-irony)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'c-mode-common-hook 'irony-mode))

(leaf flycheck
  :config 
  (leaf flycheck
    :when (version<= "24.3" emacs-version)
    :ensure t
    :config
    (leaf-handler-package flycheck flycheck nil)
    (when (require 'flycheck nil 'noerror)
      (custom-set-variables
       '(flycheck-display-errors-function
	 (lambda (errors)
	   (let ((messages (mapcar #'flycheck-error-message errors)))
	     (popup-tip
	      (mapconcat 'identity messages "\n")))))
       '(flycheck-display-errors-delay 0.5))
      (define-key flycheck-mode-map
	(kbd "C-M-n")
	'flycheck-next-error)
      (define-key flycheck-mode-map
	(kbd "C-M-p")
	'flycheck-previous-error)
      (add-hook 'c-mode-common-hook 'flycheck-mode))
    (global-flycheck-mode))

  (leaf flycheck-irony
    :ensure flycheck irony
    :when (version<= "24.1" emacs-version)
    :config
    (leaf-handler-package flycheck-irony flycheck-irony nil)
    (eval-after-load 'flycheck
      '(progn
	 (eval-after-load "flycheck"
	   '(progn
	      (when (locate-library "flycheck-irony")
		(flycheck-irony-setup)))))))
  )


(leaf python
  :config

  (leaf py-isort :ensure t)


  (leaf elpy
    :ensure t
    :init
    (elpy-enable)
    :config
    (remove-hook 'elpy-modules 'elpy-module-highlight-indentation) ;; インデントハイライトの無効化
    (remove-hook 'elpy-modules 'elpy-module-flymake) ;; flymakeの無効化
    :custom
    (elpy-rpc-python-command . "python3") ;; https://mako-note.com/elpy-rpc-python-version/の問題を回避するための設定
    (flycheck-python-flake8-executable . "flake8")
    :bind (elpy-mode-map
	   ("C-c C-r f" . elpy-format-code))
    :hook ((elpy-mode-hook . flycheck-mode))

    )
  )

(leaf go-mode
  :doc "Major mode for the Go programming language"
  :tag "go" "languages"
  :added "2021-05-26"
  :url "https://github.com/dominikh/go-mode.el"
  :hook ((go-mode-hook . company-go))
  :config
  (add-to-list 'exec-path (expand-file-name "/usr/local/go/bin/"))
  (add-to-list 'exec-path (expand-file-name "/Users/user/dev/go/bin/"))
  (add-hook 'before-save-hook' 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump)
  (set (make-local-variable 'company-backends) '(company-go))
  (setq indent-tabs-mode nil)    ; タブを利用
  (setq c-basic-offset 4)    ; tabサイズを4にする
  (setq tab-width 4)
  :ensure t)

(leaf company-go
  :doc "company-mode backend for Go (using gocode)"
  :req "company-0.8.0" "go-mode-1.0.0"
  :tag "languages"
  :added "2021-05-26"
  :ensure t
  )

(leaf yaml-mode
  :doc "Major mode for editing YAML files"
  :req "emacs-24.1"
  :tag "yaml" "data" "emacs>=24.1"
  :added "2021-05-17"
  :emacs>= 24.1
  :ensure t)

(leaf rust
  :config
  (leaf rust-mode
    :doc "A major emacs mode for editing Rust source code"
    :req "emacs-25.1"
    :tag "languages" "emacs>=25.1"
    :added "2021-04-20"
    :url "https://github.com/rust-lang/rust-mode"
    :emacs>= 25.1
    :ensure t)
  
  (leaf rustic
    :doc "Rust development environment"
    :req "emacs-26.1" "dash-2.13.0" "f-0.18.2" "let-alist-1.0.4" "markdown-mode-2.3" "project-0.3.0" "s-1.10.0" "seq-2.3" "spinner-1.7.3" "xterm-color-1.6"
    :tag "languages" "emacs>=26.1"
    :added "2021-04-20"
    :custom
    (rustic-lsp-server quote rls)
    :emacs>= 26.1
    :ensure t
    :hook rust-mode-hook
    )
  )

(setq visible-bell 1)

(provide 'init)
;;; init.el ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 3)
 '(company-selection-wrap-around t)
 '(company-transformers '(company-sort-by-backend-importance))
 '(completion-ignore-case t t)
 '(custom-safe-themes
   '("81c3de64d684e23455236abde277cda4b66509ef2c28f66e059aa925b8b12534" default))
 '(dumb-jump-mode t)
 '(dumb-jump-selector 'ivy)
 '(dumb-jump-use-visible-window nil)
 '(enable-recursive-minibuffers t)
 '(flycheck-display-errors-delay 0.5)
 '(flycheck-display-errors-function
   (lambda
     (errors)
     (let
	 ((messages
	   (mapcar #'flycheck-error-message errors)))
       (popup-tip
	(mapconcat 'identity messages "
")))))
 '(imenu-list-position 'left t)
 '(imenu-list-size 30 t)
 '(irony-additional-clang-options '("-std=c++17"))
 '(ivy-extra-directories nil)
 '(ivy-height 30)
 '(ivy-re-builders-alist '((t . ivy--regex-plus)) t)
 '(ivy-use-virtual-buffers t)
 '(package-archives
   '(("org" . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")))
 '(package-selected-packages
   '(terraform-mode lsp-mode macrostep leaf-tree leaf-convert leaf-keywords hydra el-get blackout))
 '(recentf-auto-cleanup 'never t)
 '(recentf-exclude '(".recentf") t)
 '(recentf-max-saved-items 200 t)
 '(recentf-save-file "~/.emacs.d/.recentf" t)
 '(yas-global-mode t)
 '(yas-indent-line 'fixed)
 '(yas-snippet-dirs
   '("~/.emacs.d/mysnippets" "~/.emacs.d/yasnippet-snippets/snippets")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
