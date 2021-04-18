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
 '(custom-file "~/.emacs.d/custom.el")
 '(dumb-jump-mode t t)
 '(dumb-jump-selector (quote ivy) t)
 '(dumb-jump-use-visible-window nil t)
 '(enable-recursive-minibuffers t)
 '(imenu-list-position (quote left))
 '(imenu-list-size 30)
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
 '(yas-snippet-dirs
   (quote
    ("~/.emacs.d/mysnippets" "~/.emacs.d/yasnippets/snippets")) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
