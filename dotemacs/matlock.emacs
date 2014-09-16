;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Matlock's .emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; good for when you've added something new, but doesn't need to be perpetually
;; enabled
;; (setq debug-on-error t)

;; reload .emacs when C-c <f12> is pressed
;; source: http://stackoverflow.com/questions/24810079/key-binding-to-reload-emacs-after-changing-it
(defun reload-dotemacs ()
  (interactive)
  (load-file "~/.emacs"))
(global-set-key (kbd "C-c <f12>") 'reload-dotemacs)

;;;; load .el files
(add-to-list 'load-path "~/elisp/")

;;;; OS X ;;;;

;; There's actually a bit of a problem when using this through Tramp -- it
;; expects the local device to have pbcopy and pbpaste, which I can't
;; guarantee.  So that makes this more trouble than it's worth.  Oh well.
;; copy & paste to/from clipboard
;; source: https://web.archive.org/web/20110504210857/http://blog.lathi.net/articles/2007/11/07/sharing-the-mac-clipboard-with-emacs
;; (linked from http://stackoverflow.com/questions/9985316/how-to-paste-to-emacs-from-clipboard)
;; (defun copy-from-osx ()
;;   (shell-command-to-string "pbpaste"))

;; (defun paste-to-osx (text &optional push)
;;   (let ((process-connection-type nil)) 
;;     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;;       (process-send-string proc text)
;;       (process-send-eof proc))))

;; (setq interprogram-cut-function 'paste-to-osx)
;; (setq interprogram-paste-function 'copy-from-osx)


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
;; (defun untabify-everything ()
;;   (untabify (point-min) (point-max)))
;; (defun untabify-everything-on-save ()
;;   (add-hook 'write-contents-functions 'untabify-everything)
;;   nil)

;; this runs for all modes except makefile-derived modes
;; source: http://stackoverflow.com/questions/24832699/emacs-24-untabify-on-save-for-everything-except-makefiles/24857101#24857101
(defun untabify-except-makefiles ()
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))

(add-hook 'before-save-hook 'untabify-except-makefiles)

;; I think the c-mode-common-hook includes the makefile-modes, so it's untabifying those
;; maybe not?
;; (add-hook 'c-mode-common-hook 'untabify-everything-on-save)
;; ;; (add-hook 'c-mode-hook 'untabify-everything-on-save)
;; ;; (add-hook 'c++-mode-hook 'untabify-everything-on-save)
;; ;; (add-hook 'java-mode-hook 'untabify-everything-on-save)
;; (add-hook 'python-mode-hook 'untabify-everything-on-save)
;; (add-hook 'latex-mode-hook 'untabify-everything-on-save)
;; (add-hook 'org-mode-hook 'untabify-everything-on-save)
;; (add-hook 'css-mode-hook 'untabify-everything-on-save)
;; (add-hook 'html-mode-hook 'untabify-everything-on-save)
;; (add-hook 'emacs-lisp-mode-hook 'untabify-everything-on-save)
;; save?

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
;; (add-hook 'text-mode-hook (lambda ()
;;                             ;; (setq tab-stop-list (number-sequence 4 200 4))
;;                             (setq tab-width 4)
;;                             (setq indent-tabs-mode nil)))

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
;;(add-hook 'css-mode 'set-newline-and-indent)
(add-hook 'c-mode-common-hook 'set-newline-and-indent)

;;;; C ;;;;

;;;; auto-indent on newline
;; (add-hook 'c-mode-common-hook '(lambda ()
;;     (local-set-key (kbd "RET") 'newline-and-indent)))

;;;; Allman-style indentation + indentation amount
(setq c-default-style "bsd"
    c-basic-offset 4)

;;;; Emacs as a C/C++ IDE: auto-complete, yasnippet, auto-complete c/c++
;;;; headers
;;;; source: https://www.youtube.com/watch?v=HTUE03LnaXA

;; start package.el with Emacs
(require 'package)
;; add MELPA to repository list
;; also need to add marmalade for Clojure packages (and other stuff eventually,
;; probably)
(add-to-list  'package-archives 
              '("marmalade" . "http://marmalade-repo.org/packages/")
              '("melpa" . "http://melpa.milkbox.net/packages/"))
              ;; '("gnu" . "http://elpa.gnu.org/packages/"))
;; initialize package.el
(package-initialize)

;; start auto-complete with Emacs
(require 'auto-complete)
;; default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; start yasnippet with Emacs
(require 'yasnippet)
(yas-global-mode 1)

;; I'm now looking at https://groups.google.com/forum/#!topic/smart-snippet/Cf1jjx_xZRw
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/emacs/yasnippets")))
(yas-global-mode 1)

;; initialize auto-complete-c-headers and gets called for C/C++ hooks
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories
               '"/usr/local/Cellar/gcc/4.8.2_1/lib/gcc/x86_64-apple-darwin13.2.0/4.8.2/include/c++"))
;; call this from C/C++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;; iedit
;; (part 2 of making Emacs a good C/C++ editor/IDE,
;; source: https://www.youtube.com/watch?v=r_HW0EB67eY)

;; fix iedit keybinding bug for Macs
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;; CEDET mode
;; (part 3 of making Emacs a good C/C++ editor/IDE,
;; source: https://www.youtube.com/watch?v=Ib914gNr0ys)

;; turn on Semantic mode
(semantic-mode 1)
;; define a function to add  Semantic as a suggestion backend to auto-complete
;; and hook said function into c-mode-common-hook
(defun my:add-semantic-to-auto-complete ()
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'my:add-semantic-to-auto-complete)

;; make Auto Complete and yasnippet play nicely together
;; source: http://stackoverflow.com/questions/15774807/emacs-24-autocomplete-yasnippet
;;seems to work in LaTeX, except auto complete tries to take precedence
(setq ac-source-yasnippet nil)

;; bind ac-expand to RET
;; source: http://stackoverflow.com/questions/19900949/how-to-make-auto-complete-work-with-yasnippet-and-abbrev
;; (add-hook 'auto-complete-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "RET")
;;                            'ac-expand)))
;; this had the unfortunate effect of preventing me from entering a newline

;; turn on EDE mode
(global-ede-mode 1)
;;;; template for creating projects for a program
;; (ede-cpp-root-project "my project"
;;                       :file "/path/to/src/main.cpp"
;;                       :include-path '("/../include_folder"))
;;;; you can use system-include-path for setting up system header file
;;;; locations

;; turn on automatic reparsing of open buffers in Semantic
(global-semantic-idle-scheduler-mode 1)

;;;; Python ;;;;

;;;; python-mode.el
;;;; taken from https://courses.csail.mit.edu/6.01/spring08/software/installing-software-emacs.html
;;;;;(load "~/python-mode.el") ;; this line didn't work-- python-mode.el is
;;;;; elsewhere
(load "~/.emacs.d/plugins/python-mode.el")
;;;; python
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                      interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(global-font-lock-mode t)
(font-lock-mode +1)
;;;; end of borrowed-from-MIT configuration code
(put 'upcase-region 'disabled nil)

;; ignore this; use flycheck with flake8 instead
;;;; python-pep8 minor mode
;; checks that syntax follows http://legacy.python.org/dev/peps/pep-0008/
;; (load "~/emacs/python-pep8/python-pep8.el")
;; (autoload 'python-pep8 "python-pep8")
;; (autoload 'pep8 "python-pep8"
;; this doesn't seem to be working well yet

;;;; LaTeX ;;;;

;;;; get Emacs to read the PATH variable
;;;; source: http://tex.stackexchange.com/questions/83594/problems-installing-auctex-over-emacs-24-osx
(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))

;;;;;; source: http://www.stefanom.org/setting-up-a-nice-auctex-environment-on-mac-os-x/
;;;; AucTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

;;;; getting latexmk working nicely with Skim
;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
 
;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

;; hopefully getting the default view to work well
;; source: http://alexkrispin.wordpress.com/2010/10/25/writing-with-emacs-and-auctex-part-1/
(setq TeX-output-view-style (quote (("^pdf$" "." "vince %o")
                                    ("^ps$" "." "gv %o")
                                    ("^dvi$" "." "xdvi %o"))))
(setq tex-dvi-view-command "xdvi")
(setq tex-dvi-print-command "dvips")
(setq tex-alt-dvi-print-command "dvips")

;;;; yet another folding mode
;; ok, ignore yafolding -- see alternative below
;;;; source: http://www.emacswiki.org/emacs/FoldingMode
;; (load "~/elisp/yafolding.el")

;;;; yafolding example config
;; these key bindings didn't work
;; (define-key global-map (kbd "C-'") 'yafolding)
;; ;;(define-key global-map (kbd "C-c C-f") 'yafolding-toggle-all)
;; (define-key global-map (kbd "C-c C-f") 'yafolding-toggle-all-by-current-level)

;; following yafolding instructions from https://github.com/zenozeng/yafolding.el as of 2014-07-07
;; hook into prog-mode-hook
;; (add-hook 'prog-mode-hook
;;           (lambda () (yafolding-mode)))
;; modify keybindings
;(require 'yafolding)
;; (define-key yafolding-mode-map
;;   (kbd "<C-S-return>") nil)
;; (define-key yafolding-mode-map
;;   (kbd "<C-return>") nil)
;; (define-key yafolding-mode-map
;;   (kbd "C-c <C-S-return>") 'yafolding-toggle-all)
;; (define-key yafolding-mode-map
;;   (kbd "C-c <C-return>") 'yafolding-toggle-element)
;; ok, that didn't work; let's try this:
;; (add-hook 'yafolding-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c <C-return>") 'yafolding-toggle-element)))
;; that didn't work either.  Hmm!

;;;; HideShow
;; source: http://www.emacswiki.org/emacs/HideShow
(add-hook 'c-mode-common-hook '(lambda () (hs-minor-mode)))
(add-hook 'clojure-mode-hook '(lambda () (hs-minor-mode)))
(add-hook 'latex-mode-hook '(lambda () (hs-minor-mode)))
(add-hook 'python-mode-hook '(lambda () (hs-minor-mode)))
(add-hook 'org-mode-hook '(lambda () (hs-minor-mode)))
(add-hook 'css-mode-hook '(lambda () (hs-minor-mode)))
(add-hook 'html-mode-hook '(lambda () (hs-minor-mode)))
(add-hook 'emacs-lisp-mode-hook '(lambda () (hs-minor-mode)))

;;;; LaTeX/Cocktails ;;;;

;;;; define minor mode for LaTeX'd cocktail recipes -- totally pesonal
(define-minor-mode cocktail-mode
  "cocktail-mode provides a minor mode for 
   yasnippet to hook onto in order to make
   cocktail creation easier."
  :init-value nil
  :lighter " cktl")

;; in fact, I think this is how you bind special yasnippets to minor modes:
;; source: http://capitaomorte.github.io/yasnippet/snippet-expansion.html
;; Controlling Expansion -> Eligible snippets -> yas-activate-extra-mode
(add-hook 'cocktail-mode-hook
          '(lambda () ;; this line started with a # before -- pretty sure I can
                      ;; remove that
             (yas-activate-extra-mode 'cocktail-mode)))


;;;; LaTeX/listings mode ;;;;
(define-minor-mode listings-mode
  "listings-mode makes it a little easier
   to use LaTeX's listings package"
  :init-value nil
  :lighter " listings")

(add-hook 'listings-mode-hook
          '(lambda ()
             (yas-activate-extra-mode 'listings-mode)))

;;;; Arduino ;;;;

;; Add further minor-modes to be enabled by semantic-mode.
;; See doc-string of `semantic-default-submodes' for other things
;; you can use here.
(add-to-list 'semantic-default-submodes 
             'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 
             'global-semantic-idle-completions-mode t)
;; having an issue with this line
;; (add-to-list 'semantic-default-submodes 
;;              'global-cedet-m3-minor-mode t)

;; Enable Semantic -- already did that
;; (semantic-mode 1)

;; Enable EDE (Project Management) features -- already did that, too
;; (global-ede-mode 1)

;; Configure arduino OS X dirs.
(setq ede-arduino-appdir "/Applications/Arduino.app/Contents/Resources/Java")

;; configure Emacs to use arduino-mode
(add-to-list 'load-path "~/.emacs.d/plugins/arduino-mode")
(setq auto-mode-alist
      (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode)
            auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode" t)

;; load auto-complete with arduino-mode
;; (add-hook 'arduino-mode-hook '(lambda () (auto-complete-mode 1)))
;; that's now how you do it
;; source: http://stackoverflow.com/questions/8095715/emacs-auto-complete-mode-at-startup
(add-to-list 'ac-modes 'arduino-mode)

;; set comment character to "//" instead of "/* ... */"
;; source: http://ergoemacs.org/emacs/elisp_comment_handling.html
;; command to comment/uncomment text
;; (defun my:arduino-mode-comment-dwim (arg)
;;   "Comment or uncomment current line or region in a smart way.
;;    For detail, see `comment-dwim'."
;;   (interactive "*P")
;;   (require 'newcomment)
;;   (let ((comment-start "//") (comment-end ""))
;;     (comment-dwim arg)))
;; (add-hook 'arduino-mode-hook 'my:arduino-mode-comment-dwim)
;; or try it like this
;; source: http://stackoverflow.com/questions/15120346/emacs-setting-comment-character-by-file-extension
;; (this works!)
(defun my:arduino-mode-comment-delimiters ()
  "Redefines comment-start and comment-end for Arduino mode"
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) ""))
(add-hook 'arduino-mode-hook 'my:arduino-mode-comment-delimiters)

;;;; JavaScript ;;;;

;; followed instructions at
;; https://code.google.com/p/js2-mode/wiki/InstallationInstructions
(add-to-list 'load-path "~/.emacs.d/plugins/js2-mode")
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;;; EAGLE UL mode ;;;;
(add-to-list 'load-path "~/.emacs.d/plugins/eagle-ul-mode")
(require 'eagle-ul-mode)

;;;; Org Mode ;;;;

;; C-c . isn't working for org-time-stamp (shadowed by another mode, yet 
;; attempting to find which mode has been pretty fruitless), so I'm going to
;; try using C-c q
(defun my:org-time-stamp-key ()
  (local-set-key (kbd "C-c q") 'org-time-stamp))
(add-hook 'org-mode-hook 'my:org-time-stamp-key)

;; ok, I'm just going to try overriding whatever is shadowing these key
;; bindings just to see what happens (also encountered this issue with the org
;; mode priority key)
;; that didn't actually work, so try postfixing it with <RET>
;; ok, it looks like C-c , is a prefix command, and C-c . seems to bind to
;; C-c C-<stuff>, so I guess I'll go with my original C-c q for time stamp and
;; C-c p for priority
(defun my:org-priority-key ()
  (local-set-key (kbd "C-c p") 'org-priority))
(add-hook 'org-mode-hook 'my:org-priority-key)

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
(defun my:insert-micro-sign ()
  "easier than C-x 8 <RET> micro sign"
  (local-set-key (kbd "C-c u")
                 (lambda ()
                   (interactive)
                   (insert "µ"))))
(add-hook 'org-mode-hook 'my:insert-micro-sign)
(defun my:insert-left-corner-bracket ()
  "easier than C-x 8 <RET> left corner bracket"
  (local-set-key (kbd "C-c l")
                 (lambda ()
                   (interactive)
                   (insert "「"))))
(defun my:insert-right-corner-bracket ()
  "easier than C-x 8 <RET> right corner bracket"
  (local-set-key (kbd "C-c l")
                 (lambda ()
                   (interactive)
                   (insert "」"))))
(add-hook 'org-mode-hook my:insert-left-corner-bracket)
(add-hook 'org-mode-hook my:insert-right-corner-bracket)


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
                               (dot . t)
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

;; scale inline images
;; source: http://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg01388.html
(setq org-image-actual-width '(400))
;; doesn't really seem to work, but ok

;; I guess a lot of stuff is going to come from my cell phone, and in any case,
;; 600px is a reasonable width, so having a macro for setting the width would
;; be cool
;; (defun my:org-html-image-width ()
;;   (local-set-key (kbd "C-c C-w w") "#+ATTR_HTML: width=\"600px\""))
;; (add-hook 'org-mode-hook 'my:org-html-image-width)
;; this works as you would expect it to (!!):
;; (defun qux (r-arg)
;;   "Print the raw prefix value (note capital P vs lowercase p)"
;;   (interactive "P")
;;   (if r-arg
;;       (message "raw prefix arg is '%S'" r-arg)
;;       (message "it was nil!")))
;; take advantage of that!!
;; Oh my god, this actually works!!!
(defun my:insert-attr-html-width (arg)
  (insert (format"#+ATTR_HTML: width=\"%Spx\"" arg)))
(setq my:default-attr-html-width 600)
(defun my:org-html-image-width (arg)
  "set the ATTR_HTML width of an image to arg or default"
  (interactive "P")
   (if arg
       (my:insert-attr-html-width arg)
     (my:insert-attr-html-width my:default-attr-html-width)))
(defun hookify:my:org-html-image-width ()
  (local-set-key (kbd "C-c w") 'my:org-html-image-width))
(add-hook 'org-mode-hook 'hookify:my:org-html-image-width)


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

;;;; MobileOrg ;;;;
;; source: http://orgmode.org/manual/MobileOrg.html#MobileOrg
;; additional source: http://stackoverflow.com/questions/11822353/how-to-make-org-mobile-work-in-android
(setq org-directory "~/org-mode")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-files '("~/org-mode/magzor.org"
                         "~/org-mode/fantabulon.org"
                         "~/org-mode/shopping-list.org"
                         "~/org-mode/personal-projects.org"
                         "~/org-mode/things-to-learn.org"
                         "~/org-mode/things-ive-learned.org"
                         "~/org-mode/movies.org"
                         "~/org-mode/places-to-go.org"
                         "~/org-mode/books-to-read.org"))
(setq org-mobile-inbox-for-pull "~/org-mode/from-mobile.org")

;;;; Sunrise Commander ;;;;
;; repo source: https://github.com/escherdragon/sunrise-commander
;; documentation source: http://www.emacswiki.org/emacs/Sunrise_Commander
;; noob documentation: http://www.emacswiki.org/emacs/Sunrise_Commander_For_Noobs
;; so, I actually installed it through the github repo and 
;; M-x package-install-file, and now I can bring it up using 
;; M-x sunrise-commmander; still, there's much to learn about it

;;;; Magzor-specific modes for extra yasnippets ;;;;
(define-minor-mode magzor-cpp-mode
  "magzor-cpp-mode allows for extra snippets
   that are helpful when writing Magzor-related
   C++ code"
  :init-value nil
  :lighter " magzor-C++")

(add-hook 'magzor-cpp-mode-hook
          '(lambda ()
             (yas-activate-extra-mode 'magzor-cpp-mode)))

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

;;;; CSS ;;;;
;; get indentation working nicely
;; source: http://superuser.com/questions/381801/emacs-css-modes-most-feature-complete-and-maintained
;; (setq cssm-indent-function #'cssm-c-style-indenter)

;; ;; more attempts at getting it to work right
;; ;; source: http://stackoverflow.com/questions/4006005/how-can-i-set-emacs-tab-settings-by-file-type
;; (add-hook 'css-mode-hook
;;           '(lambda ()
;;              (setq indent-tabs-mode nil)))

;;;; Clojure ;;;;
;; source: http://clojure-doc.org/articles/tutorials/emacs.html
(defvar my:clojure-packages '(better-defaults
                      clojure-mode
                      clojure-test-mode
                      cider))

(dolist (p my:clojure-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; never mind, everything was fine; I just forgot to run 
;; M-x package-refresh-contents after adding this clojure stuff

;; trying it some alternate horrible way (downloaded files from marmalade-repo
;; manually, file names were <something-something>-<version>.el; symlinked them
;; in ~/emacs/clojure to <something-something>.el, but cider seems a little too
;; complicated
;; still, cider-<version> was symlinked to cider
;; (add-to-list 'load-path "~/emacs/clojure/cider/")
;; (add-to-list 'load-path "~/emacs/clojure/")
;; (load "~/emacs/clojure/clojure-mode.el")
;; (load "~/emacs/clojure/clojure-test-mode.el")
;; (load "~/emacs/clojure/better-defaults.el")

;;;; supplemental yasnippet stuff ;;;;
;; define .snip files to be snippet definition templates; have them use
;; snippet-mode
(setq auto-mode-alist
      (cons '("\\.snip$" . snippet-mode)
            auto-mode-alist))

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

;;;; Tramp Mode ;;;;
;; problem with hanging; trying this:
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

;; sudo through Tramp

;; not working exactly right
;; source: http://stackoverflow.com/questions/3465567/how-to-use-ssh-and-sudo-together-with-tramp-in-emacs
;; usage: C-x C-f /sudo:root@host[#port]:/path/to/file
;; It will ask you for your password to access remote shell and then your 
;; password again for sudo access.
;; (set-default 'tramp-default-proxies-alist 
;;              (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;; this doesn't seem right either
;; trying this instead: http://irreal.org/blog/?p=895
;; (add-to-list 'tramp-default-proxies-alist
;;              '(nil "\\`root\\'" "/ssh:%h:"))
;; (add-to-list 'tramp-default-proxies-alist
;;              '((regexp-quote (system-name)) nil nil))
;; usage: C-x C-f /sudo:root@<remote_host>:/path/to/file


;;;; Color themes for GUI Emacs ;;;;
;; this looks really bizarre when Terminal is already Solarized, although it
;; works well in the GUI Emacs
;; (load-theme 'solarized-dark t)


;;;; Dot Mode (graphviz)
(load "~/.emacs.d/plugins/dot-mode/dot-mode.el")

;;;; align.el
;; http://www.emacswiki.org/emacs/AlignColumn
(add-to-list 'load-path "~/.emacs.d/plugins/align")
(autoload 'align-cols "align" "Align text in the region." t)
