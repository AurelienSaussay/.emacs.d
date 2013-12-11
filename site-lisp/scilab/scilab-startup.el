;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file must be inserted or loaded into users' init emacs
;;; file (.emacs usually).  load-path variable must contain the
;;; directory where scilab.el and scilab-startup.el;;; files are
;;; placed For instance the following two command can be added to
;;; the end of the init-emacs
;;; File
;;;
;;(setq load-path  (append (list "<YOUR DIRECTORY>" ) load-path))
;;(load "scilab-startup")

;;;;;;;; START  OF SCILAB STUFFS FOR .EMACS ;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path (append (list (expand-file-name "./") ) load-path))
(defvar running-xemacs (string-match "XEmacs\\|Lucid" (emacs-version)))
(let* (
      (sciel (locate-library "scilab.el"))
      (scilec (concat sciel "c"))
      (scilab-elc-xemacs running-xemacs))

  (if (not (file-newer-than-file-p scilec sciel))
        (byte-compile-file sciel)
        (find-file scilec)
        (goto-line 4)
        (setq scilab-elc-xemacs (looking-at ".*\\(XEmacs\\|Lucid\\)"))
        (kill-buffer "scilab.elc")
        (if (not (eq scilab-elc-xemacs running-xemacs))
	    (byte-compile-file sciel))))

   (autoload 'scilab-mode "scilab" "Enter Scilab editing mode." t)
   (setq auto-mode-alist (cons '("\\(\\.sci$\\|\\.sce$\\)" . scilab-mode)
       auto-mode-alist))
   (autoload 'scilab-shell "scilab" "Interactive Scilab Shell mode." t)
   (autoload 'scilab-mode-setup "scilab" "Scilab modes Setup." t)
   (autoload 'scilab-help "scilab" "Scilab Topic Browser." t)
   (autoload 'scilab-help-function "scilab" "Scilab Help Function." t)
   (autoload 'scilab-apropos-function "scilab" "Scilab Apropos Function." t)

(defun my-scilab-mode-hook ()
  (if running-gnuemacs (show-paren-mode))
     (setq fill-column 76))		; where auto-fill should wrap
(defun my-scilab-shell-mode-hook ()
(if running-gnuemacs (show-paren-mode)))
;;;;
(add-hook 'scilab-mode-hook 'my-scilab-mode-hook)
(add-hook 'scilab-shell-mode-hook 'my-scilab-shell-mode-hook)

(defcustom scilab-shell-global-key "\C-cs"
  "Global key for `scilab-shell' command \"^C\" means Ctrl-c, \"^X\"
means Ctrl-x,etc"
  :group 'scilab-shell
  :group 'scilab-setup
  :type 'string)

(global-set-key  scilab-shell-global-key 'scilab-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;To display start and setup menu of scilab in "Tools" menu (not necessary)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if running-xemacs
  (progn
	(add-menu-button '("Tools") "---" )
	(add-menu-button '("Tools") ["Scilab Start" scilab-shell t] )
	(add-menu-button '("Tools") ["Scilab Setup" scilab-mode-setup t] )
	(add-menu-button '("Help") ["Scilab Topic" scilab-help t] )
	(add-menu-button '("Help") ["Scilab Help" scilab-help-function t] )
	(add-menu-button '("Help") ["Scilab Apropos" scilab-apropos-function t] ))
     (define-key menu-bar-tools-menu [separator-scilab]
    '("--"))
     (define-key menu-bar-tools-menu [scilab-start] '("Scilab Start"  . scilab-shell))
     (define-key menu-bar-tools-menu [scilab-setup] '("Scilab Setup"  . scilab-mode-setup))

    (define-key menu-bar-help-menu [separator-scilab]
    '("--"))
    (define-key menu-bar-help-menu [scilab-apropos] '("Scilab Apropos"  . scilab-apropos-function))
     (define-key menu-bar-help-menu [scilab-help] '("Scilab Help"  . scilab-help-function))
     (define-key menu-bar-help-menu [scilab-topic] '("Scilab Topic"  . scilab-help)))
;;;;;; END OF SCILAB STUFFS FOR .EMACS;;;;;;;;;;;;;;;;;;;;;;;;;
