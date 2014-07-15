;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Matlock's .emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; load .el files
(add-to-list 'load-path "~/elisp")

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

;;;; copy selection without killing it
;;;; see: http://stackoverflow.com/questions/3158484/emacs-copying-text-without-killing-it and http://www.emacswiki.org/emacs/KeyboardMacros
(global-set-key (kbd "M-w") 'kill-ring-save)

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
(add-hook 'css-mode 'set-newline-and-indent)
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
(add-to-list  'package-archives 
              '("melpa" ."http://melpa.milkbox.net/packages/"))
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
;;;; source: http://www.emacswiki.org/emacs/FoldingMode
(load "~/elisp/yafolding.el")

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
(add-to-list 'semantic-default-submodes 
             'global-cedet-m3-minor-mode t)

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

;;;; MobileOrg ;;;;
;; source: http://orgmode.org/manual/MobileOrg.html#MobileOrg
;; additional source: http://stackoverflow.com/questions/11822353/how-to-make-org-mobile-work-in-android
(setq org-directory "~/org-mode")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-files '("~/org-mode/magzor.org"
                         "~/org-mode/fantabulon.org"
                         "~/org-mode/shopping-list.org"))
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
