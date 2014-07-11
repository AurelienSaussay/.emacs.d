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


;; Smex: Ido for commands
(require 'smex)
(smex-initialize)
(global-set-key (kbd "C-c x") 'smex)

;; Terminal Path inside Emacs
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; find-files: recursively open files with -r*
(require 'find-files)

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
(add-hook 'coffee-mode-hook
	  '(lambda ()
	     (coffee-cos-mode t)
	     (set (make-local-variable 'tab-width) 2)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2))

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

;; ESS
;; (load "/usr/share/emacs/site-lisp/ess/ess-site")

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

;; Go mode
(require 'go-mode-load)
;;(add-hook 'before-save-hook 'gofmt-before-save)
(if (eq system-type 'windows-nt)
    (progn (setenv "GOPATH" "C:\\Data\\Go")
	   (setenv "PATH" (concat (getenv "PATH") ";" "C:\\Go\\bin"))
	   (setq exec-path (append exec-path '("C:\\Go\\bin")))))
(add-hook 'go-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (setq tab-width 4)))


;; Yaml mode
(require 'yaml-mode)

;; Python mode
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(require 'anything)
(require 'anything-ipython)

(when (require 'anything-show-completion nil t)
  (use-anything-show-completion 'anything-ipython-complete
				'(length initial-pattern)))

(require 'comint)
(define-key comint-mode-map (kbd "M-") 'comint-next-input)
(define-key comint-mode-map (kbd "M-") 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)

(autoload 'pylookup-lookup "pylookup")
(autoload 'pylookup-update "pylookup")
(setq pylookup-program "~/.emacs.d/pylookup/pylookup.py")
(setq pylookup-db-file "~/.emacs.d/pylookup/pylookup.db")
(global-set-key "\C-ch" 'pylookup-lookup)

;; (autoload 'autopair-global-mode "autopair" nil t)
;; (autopair-global-mode)
(add-hook 'lisp-mode-hook
          #'(lambda () (setq autopair-dont-activate t)))
(add-hook 'python-mode-hook
          #'(lambda ()
              (push '(?' . ?')
                    (getf autopair-extra-pairs :code))
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)


(require 'visual-basic-mode)

;; Search all files
;; by offby1 on Stack Overflow
;; http://stackoverflow.com/questions/2641211/emacs-interactively-search-open-buffers

(defcustom search-all-buffers-ignored-files (list (rx-to-string '(and bos (or ".bash_history" "TAGS") eos)))
  "Files to ignore when searching buffers via \\[search-all-buffers]."
  :type 'editable-list)

(require 'grep)
(defun search-all-buffers (regexp prefix)
  "Searches file-visiting buffers for occurence of REGEXP.  With
prefix > 1 (i.e., if you type C-u \\[search-all-buffers]),
searches all buffers."
  (interactive (list (grep-read-regexp)
                     current-prefix-arg))
  (message "Regexp is %s; prefix is %s" regexp prefix)
  (multi-occur
   (if (member prefix '(4 (4)))
       (buffer-list)
     (remove-if
      (lambda (b) (some (lambda (rx) (string-match rx  (file-name-nondirectory (buffer-file-name b)))) search-all-buffers-ignored-files))
      (remove-if-not 'buffer-file-name (buffer-list))))

   regexp))

(global-set-key (kbd "C-c s") 'search-all-buffers)

;; Textmate.el
;; (require 'textmate)
;; (textmate-mode)

;; magit
;; (require 'magit)
;; Auto-revert always on, helps when branching with git-auto-revert-mode)
(global-auto-revert-mode)


;; Set font
(set-default-font "Consolas 12")

;; Eviews major mode
(require 'eviews)
(add-hook 'eviews-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (setq tab-width 4)))

(global-set-key (kbd "C-x s") 'save-buffer)


;; Scilab mode
(load "scilab/scilab-startup")

;; ELPA for Emacs >= 24
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

(tool-bar-mode 0)

;; MoDeL mode
(require 'model-mode)


;; Custom functions
(defun cleanup-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))


(defun add-star-line ()
  (interactive)
  (save-excursion
    (let ((line-len (- (line-end-position) (line-beginning-position))))
      (beginning-of-line)
      (newline-and-indent)
      (forward-line -1)
      (insert-char ?\* line-len))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

;; From http://www.emacswiki.org/emacs/ElispCookbook#toc39
;; Emacs `filter` equivalent
(defun keep-when (pred seq)
  (let ((del (make-symbol "del")))
    (remove del (mapcar (lambda (el)
			  (if (funcall pred el) el del)) seq))))

(defun list-directories-only (path)
  (keep-when (lambda (f) (and (file-directory-p (concat path f))
			      (not (equal f ".."))
			      (not (equal f "."))))
	     (directory-files path)))

(setq python-spec "*python-spec*")

(defun open-python-spec-shell ()
  (interactive)
  (unless (get-buffer python-spec)
    (shell python-spec)
    (pop-to-buffer python-spec)
    (comint-send-string (get-buffer-process python-spec) (if (eq system-type 'windows-nt)
							     ".\\env\\Scripts\\activate\n"
							   ". env/bin/activate\n"))))

(defun run-python-spec ()
  (interactive)
  (open-python-spec-shell)
  (pop-to-buffer python-spec)
  (comint-send-string (get-buffer-process python-spec) "spec\n"))

(add-hook 'python-mode-hook
	  (lambda () (local-set-key (kbd "C-c t") #'run-python-spec)))


;; From http://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame
