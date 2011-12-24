;; Add all subdirs in site-lisp to the load-path
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(setq tramp-mode nil)

;; Interactivly Do Things
(require 'ido)
(ido-mode)
(setq ido-enable-flex-matching 1)


;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 50)


;; Integrate Recentf and Ido
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)


;; SmartTab
(require 'smart-tab)
(global-smart-tab-mode 1)


;; Hide the toolbar and the menu bar
; (tool-bar-mode -1)
(menu-bar-mode 0)


;; Color Themes
(require 'color-theme)
(require 'color-theme-solarized)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (if window-system
     (color-theme-blackboard)
     (color-theme-solarized-dark))))


;; Ruby mode auto-indent
(add-hook 'ruby-mode-hook
	  (lambda()
	    (add-hook 'local-write-file-hooks
		      '(lambda()
			 (save-excursion
			   (untabify (point-min) (point-max))
			   (delete-trailing-whitespace)
			   )))
	    (set (make-local-variable 'indent-tabs-mode) 'nil)
	    (set (make-local-variable 'tab-width) 2)
	    (imenu-add-to-menubar "IMENU")
	    (define-key ruby-mode-map "\C-m" 'newline-and-indent) ;Not sure if this line is 100% right!
					;   (require 'ruby-electric)
					;   (ruby-electric-mode t)
	    ))


;; Mode-compile
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)


;; Textile mode
(require 'textile-mode)

;; Mustache mode
(require 'mustache-mode)

;; Long-lines hooks
(add-hook 'org-mode-hook
          '(lambda () (longlines-mode)))

;; Coffee mode
(require 'coffee-mode)
(add-hook 'coffee-mode-hook '(lambda () (coffee-cos-mode t)))
(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

;; Less css mode
(require 'less-css-mode)

;; Scss mode
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; Haml mode
(require 'haml-mode)
(add-hook 'haml-mode-hook
'(lambda ()
   (set (make-local-variable 'tab-width) 2)
   (setq indent-tabs-mode nil)
   (define-key haml-mode-map "\C-m" 'newline-and-indent)))


;; nxhtml (HTML ERB template support)
;; (load "~/.emacs.d/site-lisp/nxhtml/autostart.el")

;; (setq
;;  nxhtml-global-minor-mode t
;;  ;; mumamo-chunk-coloring 'submode-colored
;;  nxhtml-skip-welcome t
;;  indent-region-mode t
;;  rng-nxml-auto-validate-flag nil
;;  nxml-degraded t)
;; (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo))
;; (setq mumamo-background-colors nil)

;; Comment line
(global-set-key (kbd "C-^") 
		'(lambda () 
		   (interactive)
		   (move-beginning-of-line nil) 
		   (set-mark-command nil) 
		   (move-end-of-line nil) 
		   (comment-dwim nil) ))

;; Ctrl-Shift-S to kill word backward
(global-set-key (kbd "C-M-b") 'backward-kill-word)

;; Alt-p and Alt-n to navigate paragraphs (and functions)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; No backup
(setq make-backup-files nil) 
;; No auto-save
(setq auto-save-default nil)

;; Delete selection on paste
(delete-selection-mode 1)

;; Smart home
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
   Move point to the first non-whitespace character on this line.
   If point was already at that position, move point to beginning of line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

;; Goto-line shortcut
(global-set-key (kbd "C-c g") 'goto-line)

(global-set-key (kbd "C-a") 'smart-beginning-of-line)

;; Cygwin
;; (setenv "PATH" (concat "c:/cygwin/bin;/cygdrive/c/cygwin/bin;/cygdrive/c/cygwin/usr/local/bin;/bin;/usr/local/bin;" (getenv "PATH")))
;; (setq exec-path (cons "c:/cygwin/bin/" exec-path))
;; (require 'cygwin-mount)
;; (cygwin-mount-activate)
; (setenv "PS1" "\\[\\e[0;33m\\]othello\\$ \\[\\e[0m\\]")

;; (add-hook 'comint-output-filter-functions
;;     'shell-strip-ctrl-m nil t)
;; (add-hook 'comint-output-filter-functions
;;     'comint-watch-for-password-prompt nil t)
;; (setq explicit-shell-file-name "bash.exe")
;; ;; For subprocesses invoked via the shell
;; ;; (e.g., "shell -c command")
;; (setq shell-file-name explicit-shell-file-name)

