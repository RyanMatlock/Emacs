;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Magzor's .emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; Contributors: ;;;;;;;;;;;;
;;;;;;;;;;;; Ryan Matlock  ;;;;;;;;;;;;


;;;; Requirements:
;;;; ~/.emacs -- magzor.emacs -> .emacs
;;;; ~/.emacs.d/plugins -- arduino-mode, python-mode, etc.
;;;; ~/emacs -- for snippets and things

;; reload .emacs when C-c <f12> is pressed
;; source: http://stackoverflow.com/questions/24810079/key-binding-to-reload-emacs-after-changing-it
(defun reload-dotemacs ()
  (interactive)
  (load-file "~/.emacs"))
(global-set-key (kbd "C-c <f12>") 'reload-dotemacs)

;;;; load .el files
(add-to-list 'load-path "~/elisp/")

;;;; OS X ;;;;

;; copy & paste to/from clipboard
;; source: https://web.archive.org/web/20110504210857/http://blog.lathi.net/articles/2007/11/07/sharing-the-mac-clipboard-with-emacs
;; (linked from http://stackoverflow.com/questions/9985316/how-to-paste-to-emacs-from-clipboard)
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil)) 
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;;;; General editing ;;;;
;; get block indentation/unindentation working nicely at some point
;; remember how great Command+[ used to be? Try to get C-[ and C-] working, or
;; at least look into how Python-mode's C-c > and C-c < work so well

;;;; insert spaces instead of tabs
(setq-default indent-tabs-mode nil)

;;;; set default tab width to 4 so that untabify works properly
(setq-default tab-width 4)

;;;; turn column-number-mode on by default
(setq column-number-mode t)

;; untabify on save
;; source: http://www.emacswiki.org/emacs/UntabifyUponSave and
;; http://stackoverflow.com/questions/318553/getting-emacs-to-untabify-when-saving-certain-file-types-and-only-those-file-ty
;; and a little help from http://ergoemacs.org/emacs/emacs_avoid_lambda_in_hook.html
;; and help from http://stackoverflow.com/questions/1931784/emacs-is-before-save-hook-a-local-variable
;; (this runs for all modes except makefile-derived modes -- problematic?)
;; source: http://stackoverflow.com/questions/24832699/emacs-24-untabify-on-save-for-everything-except-makefiles/24857101#24857101
(defun untabify-except-makefiles ()
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))

(add-hook 'before-save-hook 'untabify-except-makefiles)

;;;; copy selection without killing it
;;;; see: http://stackoverflow.com/questions/3158484/emacs-copying-text-without-killing-it and http://www.emacswiki.org/emacs/KeyboardMacros
(global-set-key (kbd "M-w") 'kill-ring-save)

;; see: http://stackoverflow.com/questions/143072/in-emacs-what-is-the-opposite-function-of-other-window-c-x-o
;; I originally thought it would be 'previous-window
(global-set-key (kbd "C-x O") 'previous-multiframe-window)

;; set columns to 80 characters long (as per PEP 8/good programming practice)
;; !! maybe you need to do this for Fundamental as well ???
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 80)

;; text mode 4 spaces instead of indent
;; source: http://stackoverflow.com/questions/69934/set-4-space-indent-in-emacs-in-text-mode
(add-hook 'text-mode-hook (lambda ()
                            (setq indent-tabs-mode t)
                            (setq tab-stop-list (number-sequence 4 200 4))
                            (setq tab-width 4)))

;; set README, LICENSE.md files to open in text-mode
;; let's set README, LICENSE.md file to text-mode like this:
;; source: http://www.emacswiki.org/emacs/AutoModeAlist
;; (plus slight modification to make things more efficient?)
;; source: http://www.gnu.org/software/emacs/manual/html_node/elisp/List-Variables.html
(add-to-list 'auto-mode-alist '("\\LICENSE.md\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\README*\\'" . text-mode))

;; get auto-indentation to work right for lots of modes
;; source: http://www.emacswiki.org/emacs/AutoIndentation
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'html-mode-hook 'set-newline-and-indent)
(add-hook 'lisp-mode-hook 'set-newline-and-indent)
(add-hook 'LaTeX-mode-hook 'set-newline-and-indent)
(add-hook 'c-mode-common-hook 'set-newline-and-indent)

;;;; C ;;;;

;;;; Allman-style indentation + indentation amount
(setq c-default-style "bsd"
    c-basic-offset 4)

;;;;;;; watch the videos and uncomment lines as necessary
;;;;;;; (you'll need to do stuff with M-x package-install in order for
;;;;;;; everything to work)

;; ;;;; Emacs as a C/C++ IDE: auto-complete, yasnippet, auto-complete c/c++
;; ;;;; headers
;; ;;;; source: https://www.youtube.com/watch?v=HTUE03LnaXA

;; ;; start package.el with Emacs
;; (require 'package)
;; ;; add MELPA to repository list
;; ;; also need to add marmalade for Clojure packages (and other stuff eventually,
;; ;; probably)
;; (add-to-list  'package-archives 
;;               '("marmalade" . "http://marmalade-repo.org/packages/")
;;               '("melpa" . "http://melpa.milkbox.net/packages/"))
;;               ;; '("gnu" . "http://elpa.gnu.org/packages/"))
;; ;; initialize package.el
;; (package-initialize)

;; ;; start auto-complete with Emacs
;; (require 'auto-complete)
;; ;; default config for auto-complete
;; (require 'auto-complete-config)
;; (ac-config-default)

;; ;; start yasnippet with Emacs
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; ;; I'm now looking at https://groups.google.com/forum/#!topic/smart-snippet/Cf1jjx_xZRw
;; (setq yas-snippet-dirs (append yas-snippet-dirs
;;                                '("~/emacs/yasnippets")))
;; (yas-global-mode 1)

;; ;; initialize auto-complete-c-headers and gets called for C/C++ hooks
;; (defun my:ac-c-header-init ()
;;   (require 'auto-complete-c-headers)
;;   (add-to-list 'ac-sources 'ac-source-c-headers)
;;   (add-to-list 'achead:include-directories
;;                '"/usr/local/Cellar/gcc/4.8.2_1/lib/gcc/x86_64-apple-darwin13.2.0/4.8.2/include/c++"))
;; ;; call this from C/C++ hooks
;; (add-hook 'c++-mode-hook 'my:ac-c-header-init)
;; (add-hook 'c-mode-hook 'my:ac-c-header-init)

;; ;; iedit
;; ;; (part 2 of making Emacs a good C/C++ editor/IDE,
;; ;; source: https://www.youtube.com/watch?v=r_HW0EB67eY)

;; ;; fix iedit keybinding bug for Macs
;; (define-key global-map (kbd "C-c ;") 'iedit-mode)


;;;;;;; again, you'll have to download some stuff in order for this to work
;;;;;;; (i.e. get python-mode.el, then uncomment what you need to)

;; ;;;; Python ;;;;

;; ;;;; python-mode.el
;; ;;;; taken from https://courses.csail.mit.edu/6.01/spring08/software/installing-software-emacs.html
;; ;;;;;(load "~/python-mode.el") ;; this line didn't work-- python-mode.el is
;; ;;;;; elsewhere
;; (load "~/.emacs.d/plugins/python-mode.el")
;; ;;;; python
;; (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;; (setq interpreter-mode-alist (cons '("python" . python-mode)
;;                                       interpreter-mode-alist))
;; (autoload 'python-mode "python-mode" "Python editing mode." t)
;; (global-font-lock-mode t)
;; (font-lock-mode +1)
;; ;;;; end of borrowed-from-MIT configuration code
;; (put 'upcase-region 'disabled nil)


;;;;;;;; HideShow is really cool
;;;;;;;; C-c @ C-h to hide a block of code
;;;;;;;; C-c @ C-s to show a block of code

;; ;;;; HideShow
;; ;; source: http://www.emacswiki.org/emacs/HideShow
;; (add-hook 'c-mode-common-hook '(lambda () (hs-minor-mode)))
;; (add-hook 'clojure-mode-hook '(lambda () (hs-minor-mode)))
;; (add-hook 'latex-mode-hook '(lambda () (hs-minor-mode)))
;; (add-hook 'python-mode-hook '(lambda () (hs-minor-mode)))
;; (add-hook 'org-mode-hook '(lambda () (hs-minor-mode)))
;; (add-hook 'css-mode-hook '(lambda () (hs-minor-mode)))
;; (add-hook 'html-mode-hook '(lambda () (hs-minor-mode)))
;; (add-hook 'emacs-lisp-mode-hook '(lambda () (hs-minor-mode)))

;;;; Arduino ;;;;
;; make sure your arduino-mode is in the right place!!
(add-to-list 'load-path "~/.emacs.d/plugins/arduino-mode")
(setq auto-mode-alist
      (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode)
            auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode" t)


;;;; Org Mode ;;;;

;; C-c . isn't working for org-time-stamp (shadowed by another mode, yet 
;; attempting to find which mode has been pretty fruitless), so I'm going to
;; try using C-c q
(defun my:org-time-stamp-key ()
  (local-set-key (kbd "C-c q") 'org-time-stamp))
(add-hook 'org-mode-hook 'my:org-time-stamp-key)


;; let C-c S insert a symbol sign (easier than typing "C-x 8 <RET> section 
;; sign")
;; never mind, you're dumb
;; (defun my:section-sign ()
;;   "easier than M-x insert char <RET> section sign"
;;   (interactive)
;;   (insert-char "SECTION SIGN"))
;; (defun my:insert-section-sign ()
;;   "bind my:section-sign to C-c S"
;;   (local-set-key (kbd "C-c s") 'my:section-sign))
;; (add-hook 'org-mode-hook 'my:insert-section-sign)
;; how to really do it:  http://ergoemacs.org/emacs/emacs_n_unicode.html
(defun my:insert-section-sign ()
  "easier than M-x insert char <RET> section sign"
  (local-set-key (kbd "C-c S")
                 (lambda ()
                   (interactive)
                   (insert "§"))))
(add-hook 'org-mode-hook 'my:insert-section-sign)

;; TODO list intermediate state colors
;; source: http://cjohansen.no/en/emacs/emacs_org_mode_todo_colors
(setq org-todo-keyword-faces
      '(("IN-PROGRESS" . (:foreground "yellow" :weight bold))
        ("ON-HOLD" . (:foreground "yellow" :weight bold))))

;; set org-mode keybinding C-c C-v C-b to the string "- [ ] " and
;; C-c C-v C-v to "[ ] " and
;; C-c C-b C-b to "\n- [ ] "
;; source: http://stackoverflow.com/questions/5500035/set-custom-keybinding-for-specific-emacs-mode
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-v C-b") "- [ ] ")))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-)")
                           "[ ] ")))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-|")
                           "\n- [ ] ")))

;; syntax highlighting in BEGIN_SRC ... END_SRC blocks
;; source: http://stackoverflow.com/questions/10642888/syntax-highlighting-within-begin-src-block-in-emacs-orgmode-not-working
(setq org-src-fontify-natively t)
;; have to load org babel languages
;; source: http://superuser.com/questions/429981/org-mode-is-there-a-way-i-can-make-emacs-treat-a-region-to-be-of-a-given-mode
(org-babel-do-load-languages 'org-babel-load-languages
                             '((sh . t)
                               (python . t)
                               (clojure . t)
                               (C . t)
                               ;; (C++ . t)
                               (emacs-lisp . t)
                               (js . t)
                               (latex . t)
                               (gnuplot . t)
                               (haskell . t)
                               (ditaa . t)
                               (org . t)))
;; not sure why C++ isn't working
;; see http://orgmode.org/manual/Languages.html#Languages

;; org-mode fancy HTML5 export
;; source: http://orgmode.org/manual/HTML-doctypes.html
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)
;; this HTML5 thing isn't working nicely yet

;; turn off Org Mode babel evaluation on C-c C-c
(setq org-babel-no-eval-on-ctrl-c-ctrl-c t)

;; org-extra-yas-mode
(define-minor-mode org-extra-yas-mode
  "org-extra-yas-mode adds snippets in
   such a way that it's unlikely to
   conflict with other modes"
  :init-value nil
  :lighter " org-xtra-yas")

(add-hook 'org-extra-yas-mode-hook
          '(lambda ()
             (yas-activate-extra-mode 'org-extra-yas-mode)))


;;;; Makefile mode ;;;;
;; source: http://www.emacswiki.org/emacs/MakefileMode
(require 'make-mode)
  
(defconst makefile-nmake-statements
  `("!IF" "!ELSEIF" "!ELSE" "!ENDIF" "!MESSAGE" "!ERROR" "!INCLUDE" 
    ,@makefile-statements)
  "List (or  )f keywords understood by nmake.")
  
(defconst makefile-nmake-font-lock-keywords
  (makefile-make-font-lock-keywords
   makefile-var-use-regex
   makefile-nmake-statements
   t))

(define-derived-mode makefile-nmake-mode makefile-mode "nMakefile"
  "An adapted `makefile-mode' that knows about nmake."
  (setq font-lock-defaults
        `(makefile-nmake-font-lock-keywords ,@(cdr font-lock-defaults))))

(add-hook 'makefile-mode-hook '(lambda () (auto-complete-mode)))

;; bracket-mode minor mode
(define-minor-mode bracket-mode
  "bracket-mode allows slightly more ergonomic
   entry of brackets and parentheses -- useful
   for a number of major modes"
  :init-value nil
  :lighter " brkt")

;; entering minor mode with major mode
;; source: http://stackoverflow.com/questions/7421445/emacs-entering-minor-mode-with-major-mode
(add-hook 'bracket-mode-hook
          '(lambda ()
             (yas-activate-extra-mode 'bracket-mode)))
;; (defun c-commmon-bracket-hook ()
;;    (bracket-mode 1) nil)
;; (add-hook 'c-mode-common-hook 'c-common-bracket-hook)
;; see also http://stackoverflow.com/questions/4253473/emacs-how-do-i-automatically-enter-minor-modes-when-i-enter-a-major-mode
(add-hook 'c-mode-common-hook '(lambda () (bracket-mode)))
(add-hook 'org-mode-hook '(lambda () (bracket-mode)))
(add-hook 'text-mode-hook '(lambda () (bracket-mode)))
(add-hook 'latex-mode-hook '(lambda () (bracket-mode)))
(add-hook 'python-mode-hook '(lambda () (bracket-mode)))
(add-hook 'makefile-mode-hook '(lambda() (bracket-mode)))


;; allman-c-mode minor mode
(define-minor-mode allman-c-mode
  "allman-c-mode allows the use of Allman-
   style friendly snippets."
  :init-value nil
  :lighter " AlmnC")
(add-hook 'allman-c-mode-hook
          '(lambda ()
             (yas-activate-extra-mode 'allman-c-mode)))
(add-hook 'c-mode-common-hook '(lambda () (allman-c-mode)))

;;;; Flycheck ;;;;
;; cool syntax checker for many languages (relies on third party syntax
;; checkers, e.g. flake8 for Python)
;; installed flycheck using M-x package-install <RET> flycheck <RET>
;; configure it to open up with python-mode
(add-hook 'python-mode-hook 'flycheck-mode)
