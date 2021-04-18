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


(leaf recentf-ext
  :bind (([(super r)]
	  . counsel-recentf))
  :config
  (leaf-handler-package recentf-ext recentf-ext nil)
  :ensure t)


(setq visible-bell 1)

(provide 'init)
;;; init.el ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-dabbrev-downcase nil t)
 '(company-idle-delay 0 t)
 '(company-minimum-prefix-length 3 t)
 '(company-selection-wrap-around t t)
 '(company-transformers (quote (company-sort-by-backend-importance)) t)
 '(completion-ignore-case t t)
 '(dumb-jump-mode t)
 '(dumb-jump-selector (quote ivy))
 '(dumb-jump-use-visible-window nil)
 '(enable-recursive-minibuffers t)
 '(imenu-list-position (quote left) t)
 '(imenu-list-size 30 t)
 '(ivy-extra-directories nil)
 '(ivy-height 30)
 '(ivy-re-builders-alist (quote ((t . ivy--regex-plus))) t)
 '(ivy-use-virtual-buffers t)
 '(package-archives
   (quote
    (("org" . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (macrostep leaf-tree leaf-convert leaf-keywords hydra el-get blackout)))
 '(recentf-auto-cleanup (quote never) t)
 '(recentf-exclude (quote (".recentf")) t)
 '(recentf-max-saved-items 200 t)
 '(recentf-save-file "~/.emacs.d/.recentf" t)
 '(yas-global-mode t)
 '(yas-indent-line (quote fixed))
 '(yas-snippet-dirs
   (quote
    ("~/.emacs.d/mysnippets" "~/.emacs.d/yasnippets/snippets"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
