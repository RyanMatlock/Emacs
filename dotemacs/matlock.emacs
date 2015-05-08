;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Matlock's .emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; TODOs ;;;;;;
;; [ ] figure out differences between shell and GUI Emacs
;; (For example, shell-mode in emacs -nw doesn't allow for 「C-<UP>」 to get
;; the last command, but it does work in GUI Emacs for some reason. I bet a lot
;; of other issues you've had in the past (e.g. 「C-.」 not working in Org mode
;; (?) could be resolved by figuring this out). I guess what you really want is
;; the same nice shell Emacs behavior from the GUI (i.e. familiar colors, no
;; mouse, and maybe some other stuff.)
;;
;; [x] get bash (i.e. 「M-x shell」) to work like it does in terminal
;; (including stuff like extended globs)---did this; it was only *slightly*
;; involved. Basically, I had to symlink /bin/bash to my latest version of the
;; actual bash binary (as of 2015-03-15, that was
;; /usr/local/Cellar/bash/4.3.27/bin/bash) to /bin/bash and then add a
;; ~/.bashrc file (which just said "source /etc/.bashrc"), and now it seems
;; that I (mostly?) get the behavior I want (to be fair, I haven't tried any
;; really serious scripts yet; I just wanted to have a decent Julia lang REPL
;; going on for me)
;;;;; /TODOs ;;;;;;

;; turn off welcome screen
(setq inhibit-startup-message t)

;; good for when you've added something new, but doesn't need to be perpetually
;; enabled
;; (setq debug-on-error t)

;; reload .emacs when C-c <f12> is pressed
;; source: http://stackoverflow.com/questions/24810079/key-binding-to-reload-emacs-after-changing-it
(defun reload-dotemacs ()
  (interactive)
  (load-file "~/.emacs"))
(global-set-key (kbd "C-c <f12>") 'reload-dotemacs)

;; open .emacs files in Emacs Lisp mode
;; (helpful since you store your different .emacs files as <hostname>.emacs)
;; (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;; also, do it with add-to-list instead
(add-to-list 'auto-mode-alist '("\\.emacs$" . emacs-lisp-mode))

;; start package.el with Emacs
(require 'package)
;; add MELPA to repository list
;; also need to add marmalade for Clojure packages (and other stuff eventually,
;; probably)
;; this is the old, bad way:
;; (add-to-list  'package-archives 
;;               '("marmalade" . "http://marmalade-repo.org/packages/")
;;               '("melpa" . "http://melpa.milkbox.net/packages/"))
;;               ;; '("gnu" . "http://elpa.gnu.org/packages/"))
;; this is the new, right way from elematlock:
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; initialize package.el
(package-initialize)

;; get rid of that annoying 「」 (which you can make with 「C-q C-m」) at the
;; end of lines created on some Windows machines
;; this code worked in *scratch*:
;;     (defun my:rep-str (target replacement)
;;       "I want to see if I can define a command for common find/replace operations"
;;       (interactive)
;;       (replace-string target replacement))
;;     (defun my:foo-bar ()
;;       (interactive)
;;       (my:rep-str "foo" "bar"))
;; but if I wanted to be really cool, I'd have it only do the replacement from
;; current cursor position, but if it were called with the C-u prefix, it would
;; start from the beginning of the document (and we'd also save the previous
;; cursor position)
;; see http://ergoemacs.org/emacs/elisp_universal_argument.html
;; and http://www.delorie.com/gnu/docs/emacs/emacs_28.html
;; and https://www.gnu.org/software/emacs/manual/html_node/eintr/save_002dexcursion.html#save_002dexcursion
;; and http://www.chemie.fu-berlin.de/chemnet/use/info/elisp/elisp_28.html
;; this works
;;     (defun my:replace-foo-with-bar (&optional whole-document-p)
;;       (interactive "P")
;;       (if (equal whole-document-p nil)
;;           (replace-string "foo" "bar")
;;         (message "Soon...")))
;; this totally works
;;     (defun my:replace-foo-with-bar-helper ()
;;       (replace-string "foo" "bar"))
;;     (defun my:replace-foo-with-bar (&optional whole-document-p)
;;       (interactive "P")
;;       (if (equal whole-document-p nil)
;;           (my:replace-foo-with-bar-helper)
;;         (save-excursion
;;           (goto-char (point-min))
;;           (my:replace-foo-with-bar-helper))
;;         (message "%s" (point))))
(defun my:delete-carriage-returns-helper ()
  (replace-string "" ""))
(defun delete-carriage-returns (&optional whole-document-p)
  "If called with C-u, it gets rid of all carriage returns in the document;
   otherwise, it gets rid of all carriage returns following the cursor
   position"
  (interactive "P")
  (if (equal whole-document-p nil)
      (my:delete-carriage-returns-helper)
    (save-excursion
      (goto-char (point-min))
      (my:delete-carriage-returns-helper))))
;; enable this globally for C-c R
(global-set-key (kbd "C-c R") 'delete-carriage-returns)

;; highlight lines over 80 chars long
;; see http://emacsredux.com/blog/2013/05/31/highlight-lines-that-exceed-a-certain-length-limit/
;; and http://stackoverflow.com/questions/6344474/how-can-i-make-emacs-highlight-lines-that-go-over-80-chars
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
;; only turn on 80 char rule for programming modes
;; (add-hook 'prog-mode-hook 'whitespace-mode)
;; turn on 80 char rule for everything
(global-whitespace-mode +1)

;;;; Windowed Emacs ;;;;
;; adding Windowed Emacs stuff from elematlock

;; set default frame size to 80w x 45h
;; source: http://www.emacswiki.org/FrameSize
;; ...and do other things
(when window-system
  ;; make the window semi-transparent like my OS X terminal (which is at 96%
  ;; opacity)
  ;; see http://stackoverflow.com/questions/21946382/how-to-get-transparent-window-in-gnu-emacs-on-osx-mavericks
  ;; (defvar my:default-opacity 96)
  ;; (set-frame-parameter (selected-frame) 'alpha
  ;;                      '(my:default-opacity my:default-opacity))
  ;; this is giving my a wrong-type-argument numberp error, which is confusing
  ;; (set-frame-parameter (selected-frame) 'alpha '(96 96))
  ;; even at 99, it's too transparent
  ;; (add-to-list 'default-frame-alist
  ;;              '(alpha my:default-opacity my:default-opacity))
  (set-frame-parameter (selected-frame) 'alpha '(96 96))
  (add-to-list 'default-frame-alist '(alpha 96 96))
  ;; ok, that works (for some reason, reloading .emacs didn't actually reset
  ;; the opacity), although I wish I could have one single variable I need to
  ;; change
  ;; set frame size
  (defvar my:frame-width 80)
  (defvar my:frame-height 45)
  (set-frame-size (selected-frame) my:frame-width my:frame-height)
  ;; I bet I can define functions to resize the frame for side-by-side windows
  ;; and another to revert to the default size
  ;; by the way, I checked, and neither of these work inside of the terminal
  (defun side-by-side ()
  "resizes the frame to accommodate two windows side-by-side"
  (interactive)
  (set-frame-size (selected-frame)
                  (+ (* my:frame-width 2) 2)
                  my:frame-height))

  (defun std-frame ()
    "reverts framesize to standard"
    (interactive)
    (set-frame-size (selected-frame)
                    my:frame-width
                    my:frame-height))

  ;; set your font
  (defvar my:font-face "Inconsolata")
  (defvar my:font-size 15)
  (set-default-font (concat my:font-face
                            "-"
                            (number-to-string my:font-size)))
  ;; already did 「M-x package-install <RET> solarized-theme <RET>」
  ;; installation documented here:
  ;; https://github.com/sellout/emacs-color-theme-solarized
  ;; (load-theme 'solarized-dark t)
  ;; hmm, solarized-dark doesn't work quite right
  ;; according to http://www.emacswiki.org/emacs?action=browse;oldid=ColorTheme;id=ColorAndCustomThemes
  ;; I should try
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-solarized-dark)
  ;; hooray! that worked
  ;; turn on mouse avoidance mode (you can toggle this off with
  ;; 「M-x mouse-avoidance-mode」
  ;; references: http://ergoemacs.org/emacs/emacs-tip_mode_on_off_toggle.html
  ;; and https://www.gnu.org/software/emacs/manual/html_node/emacs/Mouse-Avoidance.html
  ;; (mouse-avoidance-mode t)
  ;; ok, that's not working -- guess you'll have to figure out something else
  ;; mouse avoidance is something else; in order to turn off mouse clicks so
  ;; you don't accidentally change the cursor position when clicking back into
  ;; the emacs window (although one wonders why you're not ⌘-tabbing back into
  ;; the window) anyway,
  ;; http://stackoverflow.com/questions/4906534/disable-mouse-clicks-in-emacs
  ;; looks to have you covered
  (dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1]
               [triple-mouse-1] [mouse-2] [down-mouse-2] [drag-mouse-2]
               [double-mouse-2] [triple-mouse-2] [mouse-3] [down-mouse-3]
               [drag-mouse-3] [double-mouse-3] [triple-mouse-3] [mouse-4]
               [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
               [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5]
               [triple-mouse-5]))
    (global-unset-key k))
  ;; 「C-z」 has the annoying effect of minimizing Emacs in windowed mode, so
  ;; I'm going to disable that
  (global-unset-key (kbd "C-z"))
  )


;;;; load .el files
(add-to-list 'load-path "~/elisp/")

;;;; OS X ;;;;

;; There's actually a bit of a problem when using this through Tramp -- it
;; expects the local device to have pbcopy and pbpaste, which I can't
;; guarantee.  So that makes this more trouble than it's worth.  Oh well.
;; copy & paste to/from clipboard
;; source: https://web.archive.org/web/20110504210857/http://blog.lathi.net/articles/2007/11/07/sharing-the-mac-clipboard-with-emacs
;; (linked from http://stackoverflow.com/questions/9985316/how-to-paste-to-emacs-from-clipboard)
;; it looks like I can check what my operating system is
;; source: http://stackoverflow.com/questions/1817257/how-to-determine-operating-system-in-elisp and
;; http://stackoverflow.com/questions/10088168/how-to-check-whether-a-minor-mode-e-g-flymake-mode-is-on
;; so I think if I just do a
;; (when (string-equal system-type "darwin")
;;   (unless (bound-and-true-p tramp-mode)
;;     ;; os x stuff
;;     ))
;; will work
;; (when (string-equal system-type "darwin")
;;   (unless (bound-and-true-p tramp-mode)
;;     (message "os x sans tramp")))
;; hmm
;; (if (bound-and-true-p tramp-mode)
;;     (message "tramp-mode is defined and active")
;;   (message "tramp-mode is undefined and/or disabled"))
;; see http://stackoverflow.com/questions/26579205/emacs-check-if-buffer-is-being-edited-through-tramp
;; which was answered in
;; http://stackoverflow.com/questions/3415830/does-tramp-offer-any-api-for-interrogating-information-from-the-buffer-file-name
;; you can check if buffer is being managed by Tramp by checking truth value of
;; variable 「tramp-tramp-file-p」, e.g.:
;; (if (bound-and-true-p tramp-tramp-file-p)
;;     (message "I'm a tramp")
;;   (message "I ain't no tramp"))
;; which, when I'm editing .emacs on my local machine, produces the output:
;; "I ain't no tramp"

;; (defun copy-from-osx ()
;;   (shell-command-to-string "pbpaste"))

;; (defun paste-to-osx (text &optional push)
;;   (let ((process-connection-type nil)) 
;;     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;;       (process-send-string proc text)
;;       (process-send-eof proc))))

;; (when (string-equal system-type "darwin")
;;   (unless (bound-and-true-p tramp-tramp-file-p)
;;     (setq interprogram-cut-function 'paste-to-osx)
;;     (setq interprogram-paste-function 'copy-from-osx)))

;; cool, it seems to work!
;; actually, I didn't test it out with Tramp, and all I'm getting is 
;; /bin/bash: pbpaste: command not found
;; when editing a file through Tramp

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
(setq-default fill-column 79)
;; actually, it needs to be set at 79 as per pyflakes (just trust me on this)

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

;; package-initialize/ELPA/MELPA/Marmalade stuff moved to the top as per
;; http://stackoverflow.com/questions/15555309/emacs-for-windows-error-loading-color-theme

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

;; Predictive Mode -- http://www.emacswiki.org/emacs/PredictiveMode
;; mentioned in AUCTeX emacswiki page --
;; http://www.emacswiki.org/emacs/AUCTeX#toc12 which is why it's here in my
;; .emacs file (it looks like it'll work a little better than autocomplete)
;; apparently there's no package-install compatibility at the moment, so you
;; actually need to clone it into your .emacs.d directory, cd into it, run
;; make, add the directory to your load path, and require predictive
;;
;; (add-to-list 'load-path "~/.emacs.d/predictive")
;; (require 'predictive)
;;
;; weird:
;; Warning (emacs): Predictive major-mode setup function predictive-setup-latex
;; failed; latex-mode support disabled
;; so I guess I'll remove this for now
;; (although I'd run make while the folder was in ~/.emacs.d/predictive and
;; then I moved it to ~/.emacs.d/plugins/predictive-mode, which could explain
;; why it wasn't working)
;;
;; that didn't seem to help


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
  (set (make-local-variable 'comment-end) "")
  ;; this stopped working properly for some reason, so I'll see if setting
  ;; comment-multi-line to nil fixes anything see
  ;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Multi_002dLine-Comments.html#Multi_002dLine-Comments
  ;; hmm, it only seems to be that way for a particular file I'm editing; oh
  ;; well, not important now
  (set (make-local-variable 'comment-multi-line) nil))
(add-hook 'arduino-mode-hook 'my:arduino-mode-comment-delimiters)

;;;; JavaScript ;;;;

;; Er, commenting this out seems to be breaking things, but I think I'll see if
;; I can do a package-install of js2-mode
;;
;; ok, 「M-x package-install RET js2-mode RET」 appeared to work, so let's
;; quit, restart, and see what happens
;;
;; looks like everything's cool now
;;
;; I'm going to skip js2-mode for now to see of context-coloring works without
;; it
;; followed instructions at
;; https://code.google.com/p/js2-mode/wiki/InstallationInstructions
;; (add-to-list 'load-path "~/.emacs.d/plugins/js2-mode")
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Douglas Crockford-style context coloring
;; https://github.com/jacksonrayhamilton/context-coloring/
;; 「M-x package-refresh-contents RET」
;; 「M-x package-install RET context-coloring RET」
;; actually, this didn't work yet, but definitely come back to it!!
;; ok, so I can't get the package-install method to work, so I just cloned the
;; git repo to ~/.emacs.d and compiled from there
(add-to-list 'load-path "~/.emacs.d/context-coloring")
(require 'context-coloring)
(add-hook 'js-mode-hook 'context-coloring-mode)
(context-coloring-load-theme 'solarized)

;;;; Eagle UL mode ;;;;
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
  (local-set-key (kbd "C-c r")
                 (lambda ()
                   (interactive)
                   (insert "」"))))
(add-hook 'org-mode-hook 'my:insert-left-corner-bracket)
(add-hook 'org-mode-hook 'my:insert-right-corner-bracket)
;; also add these two to Emacs Lisp mode
(add-hook 'emacs-lisp-mode-hook 'my:insert-left-corner-bracket)
(add-hook 'emacs-lisp-mode-hook 'my:insert-right-corner-bracket)

;; TODO list intermediate state colors
;; source: http://cjohansen.no/en/emacs/emacs_org_mode_todo_colors
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "red" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("IN-PROGRESS" . (:foreground "yellow" :weight bold))
        ("ON-HOLD" . (:foreground "yellow" :weight bold))))

;; set org-mode keybinding C-c C-v C-b to the string "- [ ] " and
;; C-c C-v C-v to "[ ] " and
;; C-c C-b C-b to "\n- [ ] "
;; source: http://stackoverflow.com/questions/5500035/set-custom-keybinding-for-specific-emacs-mode
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c C-v C-b") (insert "\n- [ ] "))))
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "M-)")
;;                            (insert "[ ] "))))
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "M-|")
;;                            (insert "\n- [ ] "))))
;; ignore the above key bindings and look at the docstrings/code below
;; I think (insert <string>) was the key here
(defun my:org-insert-checkbox-item ()
  "「C-c i」 inserts '\n- [ ] '"
  (interactive)
  (insert "\n- [ ] "))
(defun hookify:my:org-insert-checkbox-item ()
  (local-set-key (kbd "C-c i") 'my:org-insert-checkbox-item))
(add-hook 'org-mode-hook 'hookify:my:org-insert-checkbox-item)

;; I forgot that originally, 「C-c <SPC>」 is used to clear a table entry
;; 「C-c M-<SPC>」 inserts \"␣\" (unicode open box/visible space char)"
(defun my:org-insert-visible-space-char ()
  "insert a visible space character (unicode u2423/open box '␣')"
  (interactive)
  (insert "␣"))
(defun hookify:my:org-insert-visible-space-char ()
  (local-set-key (kbd "C-c M-<SPC>") 'my:org-insert-visible-space-char))
(add-hook 'org-mode-hook 'hookify:my:org-insert-visible-space-char)

;; (defun my:org-insert-just-checkbox ()
;;   "「M-)」 inserts '[ ] '"
;;   (interactive)
;;   (insert "[ ] "))
;; (defun hookify:my:org-insert-just-checkbox ()
;;   (local-set-key (kbd "M-)") (insert "[ ] ")))
;; (add-hook 'org-mode-hook 'hookify:my:org-insert-just-checkbox)
;; I seem to be getting a lot of weird "[ ] " floating around in my documents
;; these days, and I think this might be to blame

;; syntax highlighting in BEGIN_SRC ... END_SRC blocks
;; source: http://stackoverflow.com/questions/10642888/syntax-highlighting-within-begin-src-block-in-emacs-orgmode-not-working
;; have to load org babel languages
;; source: http://superuser.com/questions/429981/org-mode-is-there-a-way-i-can-make-emacs-treat-a-region-to-be-of-a-given-mode
(org-babel-do-load-languages 'org-babel-load-languages
                             '((sh . t)
                               (python . t)
                               (clojure . t)
                               (C . t) ;; note that C is capitalized
                               ;; (c++ . t) ;; but c++ is lowercase
                               ;; see http://emacs-fu.blogspot.com/2011/02/executable-source-code-blocks-with-org.html
                               ;; hmm, that didn't work; try cpp?
                               ;; source: http://orgmode.org/worg/org-contrib/babel/languages.html
                               ;; (cpp . t)
                               ;; maybe C is sufficient
                               ;; see http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-C.html
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
;; maybe C++ will work now that I've upgrade Org mode to v8.2.10 -- eh
(setq org-src-fontify-natively t)

;; to get syntax highlighting working with HTML output, htmlize may need to be
;; installed separately
;; see http://stackoverflow.com/questions/24082430/org-mode-no-syntax-highlighting-in-exported-html-page
;; 「M-x package-install <RET> htmlize <RET>」

;; Org 8.2.10 no longer supports $\implies$, it seems I need to include the 
;; amsmath and amssymb packages
;; see http://orgmode.org/worg/org-contrib/babel/examples/article-class.html#latex-proglang
;; (require 'org-latex)
;; (setq org-export-latex-listings t)
;; (add-to-list 'org-export-latex-packages-alist
;;              '(("AUTO" "amsmath" t)))
;; (add-to-list 'org-export-latex-packages-alist
;;              '(("AUTO" "amssymb" t)))
;; (add-to-list 'org-export-latex-packages-alist
;;              '(("AUTO" "inputenc" t)))
;; hmm, that's not working right
;; maybe stackoverflow will find the solution:
;; http://stackoverflow.com/questions/26517510/org-mode-8-2-10-no-longer-supporting-certain-special-characters

;; org-mode fancy HTML5 export
;; source: http://orgmode.org/manual/HTML-doctypes.html
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)
;; this HTML5 thing isn't working nicely yet
;; with org upgrade (7.9 -> 8.2.10), this works!

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

;; allow alphabetical lists
;; no idea why this isn't working
;; source: http://comments.gmane.org/gmane.emacs.orgmode/72865 and
;; http://orgmode.org/manual/Plain-lists.html
;; actually, since upgrading org mode from 7.9.something to 8.2.10, this works!
(setq org-list-allow-alphabetical t)

;; convert PDFs to images to include in Org files (for HTML output)
;; see http://emacs.stackexchange.com/questions/390/display-pdf-images-in-org-mode
;; Execute the `modi/org-include-img-from-pdf' function just before saving the file
(add-hook 'before-save-hook #'modi/org-include-img-from-pdf)
;; Execute the `modi/org-include-img-from-pdf' function before processing the
;; file for export
(add-hook 'org-export-before-processing-hook #'modi/org-include-img-from-pdf)

(defun modi/org-include-img-from-pdf (&rest ignore)
    "Convert the pdf files to image files.

Only looks at #HEADER: lines that have \":convertfrompdf t\".
This function does nothing if not in org-mode, so you can safely
add it to `before-save-hook'."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp
                "^\\s-*#\\+HEADER:.*\\s-:convertfrompdf\\s-+t"
                nil 'noerror)
          (let* (filenoext imgext imgfile pdffile cmd)
            ;; Keep on going on to the next line till it finds a line with
            ;; `[[FILE]]'
            (while (progn
                     (forward-line 1)
                     (not (looking-at "\\[\\[\\(.*\\)\\.\\(.*\\)\\]\\]"))))
            (when (looking-at "\\[\\[\\(.*\\)\\.\\(.*\\)\\]\\]")
              (setq filenoext (match-string-no-properties 1))
              (setq imgext (match-string-no-properties 2))
              (setq imgfile (expand-file-name (concat filenoext "." imgext)))
              (setq pdffile (expand-file-name (concat filenoext "." "pdf")))
              (setq cmd (concat "convert -density 96 -quality 85 "
                                pdffile " " imgfile))
              (when (file-newer-than-file-p pdffile imgfile)
                ;; This block is executed only if pdffile is newer than imgfile
                ;; or if imgfile does not exist
                ;; Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Testing-Accessibility.html
                (message "%s" cmd)
                              (shell-command cmd))))))))

;; org-extra-yas-mode
(define-minor-mode org-extra-yas-mode
  "org-extra-yas-mode adds snippets in
   such a way that it's unlikely to
   conflict with other modes"
  :init-value nil
  :lighter " OXY")

;; wait, this doesn't make sense
;; wait, actually it does -- it means that yasnippet activates this mode when
;; it's activated (but you probably only want it active when Org mode is
;; active
(add-hook 'org-extra-yas-mode-hook
          '(lambda ()
             (yas-activate-extra-mode 'org-extra-yas-mode)))
;; see http://stackoverflow.com/questions/7421445/emacs-entering-minor-mode-with-major-mode
;; for how you should do this
;; (defun org-extra-yas-mode-hook ()
;;   (lambda ()
;;     (require 'yasnippet)
;;     (yas-activate-extra-mode 'org-extra-yas-mode)))
;; or just do it the way suggested in the link
;; (defun org-extra-yas-mode-hook ()
;;   (org-extra-yas-mode 1))
;; (add-hook 'org-mode-hook 'org-extra-yas-mode-hook)
;; maybe try it in interactive mode because the documentation in yasnippet.el
;; says the following:
;;
;;   M-x yas-activate-extra-mode
;;
;;     Prompts you for an extra mode to add snippets for in the
;;     current buffer.
;; hmm, this isn't working
;; (defun org-extra-yas-mode-hook ()
;;   (lambda ()
;;     (require 'yasnippet)
;;     (interactive)
;;     (yas-activate-extra-mode 'org-extra-yas-mode)))
(defun org-extra-yas-mode-activation-kludge ()
  (org-extra-yas-mode 1))
(add-hook 'org-mode-hook 'org-extra-yas-mode-activation-kludge)
;; ok, that works, as long as you have the hook thing above working

;; subscript and superscript behavior -- turn it off without curly braces
;; source: http://orgmode.org/manual/Subscripts-and-superscripts.html
;; (setq 'org-use-sub-superscripts '{})
;; I'm not really sure what I'm supposed to set that to, but I guess on a
;; per-file basis I can add
;; #+OPTIONS: ^:{}
;; source: 
;; http://stackoverflow.com/questions/698562/disabling-underscore-to-subscript-in-emacs-org-mode-export
;; not ideal, but better than nothing
;; now that I'm using a newer Org (8.2.10 instead of 7.9.x), let's try this
;; again
;; (setq org-use-sub-superscripts "{}")
;; hmm, maybe the export is different, so let's try that
;; see http://lists.gnu.org/archive/html/emacs-orgmode/2013-11/msg00624.html
;; (setq org-export-with-sub-superscripts "{}")
;; (setq org-use-sub-superscripts "{}")
;; answer from stackoverflow:
;; You must write instead:
;;     (setq org-use-sub-superscripts '{})
(setq org-use-sub-superscripts '{})
;; you actually need the following for the HTML (and LaTeX?) exporting to work
;; as you'd like, too
(setq org-export-with-sub-superscripts '{})

;; org-mode:  make 「C-c t」 prompt for text and insert "\n- <text>:" (good for
;; taking notes)
;; source: http://ergoemacs.org/emacs/elisp_idioms_prompting_input.html
(defun my:org-list-note-prompt(note-prefix)
  "prompts user for text, inserts '\n- <text>:'"
  ;; the 's' means the input will be processed as a string"
  (interactive 
   "sEnter a list heading (e.g. time if you're watching a video): ")
  (insert (format "\n- %s: " note-prefix)))
(defun hookify:my:org-list-note-prompt ()
  (local-set-key (kbd "C-c t") 'my:org-list-note-prompt))
(add-hook 'org-mode-hook 'hookify:my:org-list-note-prompt)

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
                         "~/org-mode/books-to-read.org"
                         "~/org-mode/scratch.org"))
(setq org-mobile-inbox-for-pull "~/org-mode/from-mobile.org")

(setq org-html-head-extra
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://RyanMatlock.github.io/org-style/org-style.css\" />")

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

;; this never ended up being useful
;; ;; bracket-mode minor mode
;; (define-minor-mode bracket-mode
;;   "bracket-mode allows slightly more ergonomic
;;    entry of brackets and parentheses -- useful
;;    for a number of major modes"
;;   :init-value nil
;;   :lighter " brkt")

;; ;; entering minor mode with major mode
;; ;; source: http://stackoverflow.com/questions/7421445/emacs-entering-minor-mode-with-major-mode
;; (add-hook 'bracket-mode-hook
;;           '(lambda ()
;;              (yas-activate-extra-mode 'bracket-mode)))
;; ;; (defun c-commmon-bracket-hook ()
;; ;;    (bracket-mode 1) nil)
;; ;; (add-hook 'c-mode-common-hook 'c-common-bracket-hook)
;; ;; see also http://stackoverflow.com/questions/4253473/emacs-how-do-i-automatically-enter-minor-modes-when-i-enter-a-major-mode
;; (add-hook 'c-mode-common-hook '(lambda () (bracket-mode)))
;; (add-hook 'org-mode-hook '(lambda () (bracket-mode)))
;; (add-hook 'text-mode-hook '(lambda () (bracket-mode)))
;; (add-hook 'latex-mode-hook '(lambda () (bracket-mode)))
;; (add-hook 'python-mode-hook '(lambda () (bracket-mode)))
;; (add-hook 'makefile-mode-hook '(lambda() (bracket-mode)))


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

;; use ssh as default for Tramp
;; see
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Default-Method.html
(setq tramp-default-method "ssh")

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

;;;; Scheme, Common Lisp, Racket, etc. ;;;;
;; installed geiser with 「M-x package install <RET> geiser <RET>」
;; see: http://nongnu.org/geiser/geiser_2.html#Installation
;; to install guile and racket, I've just done a simple
;; $ brew install guile
;; $ guile --version
;; guile (GNU Guile) 2.0.11
;; $ brew install plt-racket
;; $ racket --version
;; Welcome to Racket v6.1.
;; (current as of 2014-10-08)
;; as per http://www.nongnu.org/geiser/geiser_2.html#Friends, I'll also install
;; Paredit and ac-geiser

;; pretty sure
;; (require 'geiser)
;; is *not* necessary

;; Paredit
;; source: http://www.emacswiki.org/emacs/ParEdit
(autoload 'enable-paredit-mode 
  "paredit" 
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'geiser-repl-mode-hook #'enable-paredit-mode) ;; my addition
;; after 30s of use: this is the greatest thing

;; ac-geiser
;; source: https://github.com/xiaohanyu/ac-geiser/
;; 「M-x package install <RET> ac-geiser <RET>」
(require 'ac-geiser)
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))

;;;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'paredit-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; interactive stuff (from
;; https://github.com/haskell/haskell-mode/wiki/Haskell-Interactive-Mode-Setup)
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

;; handy commands that for some reason don't seem to be set up yet
;; http://www.cis.syr.edu/courses/cis252/emacs.html
;; actually, it turns out some of these key combos don't work, so I'm just
;; going to remove the second C- part and go ahead with that
(defun my-key:haskell-indent-insert-equal ()
  (local-set-key (kbd "C-c =") 'haskell-indent-insert-equal))
(add-hook 'haskell-mode-hook 'my-key:haskell-indent-insert-equal)

(defun my-key:haskell-indent-insert-guard ()
  (local-set-key (kbd "C-c |") 'haskell-indent-insert-guard))
(add-hook 'haskell-mode-hook 'my-key:haskell-indent-insert-guard)

(defun my-key:haskell-indent-insert-otherwise ()
  (local-set-key (kbd "C-c o") 'haskell-indent-insert-otherwise))
(add-hook 'haskell-mode-hook 'my-key:haskell-indent-insert-otherwise)

(defun my-key:haskell-indent-insert-where ()
  (local-set-key (kbd "C-c w") 'haskell-indent-insert-where))
(add-hook 'haskell-mode-hook 'my-key:haskell-indent-insert-where)

(defun my-key:haskell-indent-align-guards-and-rhs ()
  (local-set-key (kbd "C-c a") 'haskell-indent-align-guards-and-rhs))
;; for some reason, C-c . wasn't working well, so C-c a it is!
(add-hook 'haskell-mode-hook 'my-key:haskell-indent-align-guards-and-rhs)

;; tried to install Helm with
;; 「M-x package-install <RET> helm <RET>」
;; but MELPA couldn't find the tarball, so I guess I'll try
;; 「M-x package-list-packages」 instead; ok, that worked

;;;; Io language
;; 「M-x package-install <RET> io-mode <RET>」 worked
;; see https://github.com/superbobry/io-mode

;;;; Julia language
;; 「M-x package-install <RET> julia-mode <RET>」 worked

;;;; (ANSI) Term stuff
;; tab completion not working? try this
;; source: http://stackoverflow.com/questions/18278310/emacs-ansi-term-not-tab-completing
(add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))
