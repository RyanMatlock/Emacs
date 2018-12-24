;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Matlock's .emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; TODOs ;;;;;;
;; [ ] make .emacs resilient to a missing package even in the event of being
;; unable to connect to the internet; see
;; http://camdez.com/blog/2015/04/03/switching-to-melpa-stable/ point #3
;; example:
;;     (when (require 'keyfreq nil 'no-error)
;;       (keyfreq-mode 1)           ; configuration of keyfreq
;;       (keyfreq-autosave-mode 1)) ; more configuration of keyfreq
;;
;; [ ] break .emacs into manageable chunks a la
;; http://ergoemacs.org/emacs/organize_your_dot_emacs.html (org-dotemacs.el
;; doesn't necessarily seem like a long-term stable way to store your
;; configuration, so avoid for now)
;;
;; [ ] move stuff out of ~/elisp; maybe keep ~/emacs for dotemacs stuff, but
;; otherwise reorganize it a bit
;;
;; [ ] switch to MELPA-stable releases
;;
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

;;;; installed packages -- last updated 2015-07-25

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
(add-to-list 'auto-mode-alist '("\\.emacs$" . emacs-lisp-mode))

;; start package.el with Emacs
(require 'package)
;; this is the new, right way from elematlock:
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; initialize package.el
(package-initialize)

;; load local $PATH
;; http://emacs.stackexchange.com/questions/14159/why-emacs-overrides-my-path-when-it-runs-bash
;; requries exec-path-from-shell
(exec-path-from-shell-initialize)

;;;; Ido mode (interactively do things)
(require 'ido)
(ido-mode 1)

;;;; (ANSI) Term stuff
;; tab completion not working? try this
;; source: http://stackoverflow.com/questions/18278310/emacs-ansi-term-not-tab-completing
(add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))

;; evaluate your .bashrc and stuff?
;; see comments on http://stackoverflow.com/a/4393645/2677392
(setq shell-command-switch "-ic")

;; get rid of that annoying 「」 (which you can make with 「C-q C-m」) at the
;; end of lines created on some Windows machines
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

;; exec-path and $PATH behave differently apparently. As a result,
;; cider-jack-in for clojure doesn't seem to be working right because it can't
;; find lein
(setq exec-path (append exec-path '("/usr/local/bin")))
;; wait
;; http://emacswiki.org/emacs/EmacsApp#toc3 and
;; http://stackoverflow.com/questions/13243048/mac-osx-emacs-24-2-and-nrepl-el-not-working

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
  ;;
  ;; disable menu bar
  ;; see http://emacswiki.org/emacs/MenuBar#toc1
  (menu-bar-mode -1)
  (set-frame-parameter (selected-frame) 'alpha '(96 96))
  (add-to-list 'default-frame-alist '(alpha 96 96))
  ;; ok, that works (for some reason, reloading .emacs didn't actually reset
  ;; the opacity), although I wish I could have one single variable I need to
  ;; change
  ;; set frame size
  (defvar my:frame-width 80)
  (defvar my:frame-height 45)
  (set-frame-size (selected-frame) my:frame-width my:frame-height)
  (defun side-by-side ()
  "resizes the frame to accommodate two windows side-by-side"
  (interactive)
  (set-frame-size (selected-frame)
                  (+ (* my:frame-width 2) 3)
                  my:frame-height))

  (defun std-frame ()
    "reverts framesize to standard"
    (interactive)
    (set-frame-size (selected-frame)
                    (+ 1 my:frame-width)
                    my:frame-height))

  (defun my:calculate-frame-width (num-windows)
    "calculate how wide the frame should be for a number of windows"
    (let ((inter-window-space 2))
      (+ (* my:frame-width num-windows)
         (* inter-window-space (- num-windows 1)))))

  (defun lg-frame ()
    "resize frame for 2 side-by-side windows (same as side-by-side function,
     which is being kept for now for the sake of legacy)"
    (interactive)
    (let ((num-windows 2))
      (set-frame-size (selected-frame)
                      (my:calculate-frame-width num-windows)
                      my:frame-height)))

  (defun xl-frame ()
    "resize frame for 3 side-by-side-by-side windows + extra height"
    (interactive)
    (let ((num-windows 3)
          (height-multiplier 1.3))
      (set-frame-size (selected-frame)
                      (my:calculate-frame-width num-windows)
                      (floor (* my:frame-height height-multiplier)))))

  ;; set your font
  (defvar my:font-face "Inconsolata")
  (defvar my:font-size 15)
  (set-default-font (concat my:font-face
                            "-"
                            (number-to-string my:font-size)))
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
  ;; get rid of annoying visual bell/weird little square in the middle of the
  ;; display
  ;; http://stackoverflow.com/questions/36805713/emacs-blank-square-on-mac-os-x/36813418#36813418
  (setq visible-bell nil)
  ;; Ok, now I get an audio bell instead, but I'll see if I can live with that.
  ;; If not, look into creating a custom ring-bell-function
  )

;; unset C-[ from being bound to ESC (too close to C-p) and bind C-] to ESC
;; (global-unset-key (kbd "C-["))
;; https://stackoverflow.com/questions/10452532/make-a-key-behave-as-another-key
;; (define-key key-translation-map (kbd "C-c {") (kbd ESC))
;; not sure why that isn't working
;; (define-key key-translation-map (kbd "C-c {") (kbd "ESC"))
;; ok, that's weird ESC ESC ESC is now undefined, but it has the unintended
;; consequence of not killing my window setup now at least

;;;; Enable use of magic 8-ball Python script within Emacs
;; 「C-c 8」 calls the 8-ball
;; 「C-u C-c 8」 calls the 8-ball, prints the response, and forgets it happened
;; 「C-c *」 yanks the last 8-ball question response
;; 「C-u C-c *」 calls the 8-ball and yanks question and response
;; question and response are now timestamped when yanked
;;
;; [2018-12-07 11:39:52] What if I asked a question with "quotes" in it? Ask a question of the virtual magic 8-ball:
;; Traceback (most recent call last):
;;   File "/usr/local/bin/8-ball", line 41, in <module>
;;     question = input("Ask a question of the virtual magic 8-ball:\n")
;; EOFError: EOF when reading a line
;;
;; Ok, looks like I need to escape quotes in 8-ball-input
;; per https://stackoverflow.com/a/23218390/2677392 I'm going to use a function
;; from s.el, but in case that ever breaks:
;; (defun replace-in-string (what with in)
;;   (replace-regexp-in-string (regexp-quote what) with in nil 'literal))
;; from https://stackoverflow.com/a/17325791/2677392 would also work
;;
;; this test works:
;; (defun my:temp-formatter (&optional dont-save)
;;   (interactive "P")
;;   (setq my:temp-input
;;         (read-from-minibuffer "Say it: "))
;;   (message (format "output: %s"
;;                    (s-replace "\"" "\\\"" my:temp-input))))
;;
;; [2018-12-08 10:21:54] Am I "living my best life?" As I see it, yes
;; success!

(defun 8-ball (&optional dont-save)
  (interactive "P")
  (setq 8-ball-input
        (read-from-minibuffer "Ask the 8-ball a question: "))
  (setq 8-ball-output
        (substring 
         (shell-command-to-string
          (format "8-ball \"%s\""
                  (s-replace "\"" "\\\"" 8-ball-input)))
         0 -1))
  ;; see https://www.emacswiki.org/emacs/InsertingTodaysDate
  (setq 8-ball-timestamp
        (shell-command-to-string "echo -n $(date +\"%F %H:%M:%S\")"))
  (message "%s %s" 8-ball-input 8-ball-output)
  (if (equal dont-save nil)
      nil
    (progn
      (setq 8-ball-timestamp "1969-12-31 11:59:59")
      (setq 8-ball-input "(Last question forgotten)")
      (setq 8-ball-output ""))))
;; store last 8 ball question and answer to kill ring -- it's cleaner this way
(defun 8-ball-recall-last-q-and-a ()
  (interactive)
  (let ((formatted-timestamp (format "[%s]" 8-ball-timestamp)))
    (kill-new (format "%s %s %s"
                      formatted-timestamp
                      8-ball-input
                      8-ball-output))))
(defun 8-ball-yank (&optional call-and-yank-p)
  (interactive "P")
  (if (equal call-and-yank-p nil)
      (progn
       (8-ball-recall-last-q-and-a)
       (yank))
    (progn
     (8-ball)
     (8-ball-recall-last-q-and-a)
     (yank))))
(global-set-key (kbd "C-c 8") '8-ball)
(global-set-key (kbd "C-c *") '8-ball-yank)

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

;; this runs for all modes except makefile-derived modes
;; source: http://stackoverflow.com/questions/24832699/emacs-24-untabify-on-save-for-everything-except-makefiles/24857101#24857101
(defun untabify-except-makefiles ()
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))

(add-hook 'before-save-hook 'untabify-except-makefiles)

;; insert ISO 8601 format date
(defun insert-current-date-iso-8601-format ()
  (interactive)
  (insert
   ;; the substring part is necessary otherwise there's an unwanted newline
   ;; inserted
   (substring (shell-command-to-string "date +\"%F\"") 0 -1)))
;; shortcut only enabled for YAML mode so far
;; looks like this is how you need to add a keybinding to a specific mode
;; (add-hook 'yaml-mode-hook
;;           (lambda ()
;;             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
;; source: https://www.emacswiki.org/emacs/YamlMode
(add-hook 'yaml-mode-hook
          (lambda () 
            (define-key yaml-mode-map
              (kbd "C-c !") 'insert-current-date-iso-8601-format)))

;;;; copy selection without killing it
;;;; see: http://stackoverflow.com/questions/3158484/emacs-copying-text-without-killing-it and http://www.emacswiki.org/emacs/KeyboardMacros
(global-set-key (kbd "M-w") 'kill-ring-save)

;; see: http://stackoverflow.com/questions/143072/in-emacs-what-is-the-opposite-function-of-other-window-c-x-o
;; I originally thought it would be 'previous-window
(global-set-key (kbd "C-x p") 'previous-multiframe-window)

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

;; Allman-style indentation + indentation amount
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

;; iedit
;; part 2 of making Emacs a good C/C++ editor/IDE,
;; source: https://www.youtube.com/watch?v=r_HW0EB67eY

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

;; turn on EDE mode
(global-ede-mode 1)

;; turn on automatic reparsing of open buffers in Semantic
(global-semantic-idle-scheduler-mode 1)

;;;; Python ;;;;
(add-to-list 'load-path "~/.emacs.d/plugins/python-mode/")
(setq py-install-directory "~/.emacs.d/plugins/python-mode/")
(require 'python-mode)

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
;; improve indentation?
;; source http://stackoverflow.com/questions/2477195/latex-indentation-formatting-in-emacs
;; and http://www.gnu.org/software/auctex/manual/auctex/Indenting.html
(setq LaTeX-item-indent 0)
(setq LaTeX-indent-level 2)

;; LaTeX word count using TeXcount
;; see the various answers in
;; http://superuser.com/questions/125027/word-count-for-latex-within-emacs
(defun latex-word-count ()
  (interactive)
  (shell-command (concat "texcount "
                         ;; options
                         "-brief "
                         ;; use shell-quote-argument to handle buffer names
                         ;; with spaces or other weirdness
                         (shell-quote-argument buffer-file-name))))
;; see also
;; http://stackoverflow.com/questions/8507695/using-texcount-in-emacs-to-determine-word-count-of-latex-or-tex-file-wanting-op
;; if you want to make it a little fancier

(add-hook 'LaTeX-mode-hook 'latex-word-count)
(eval-after-load 'latex
  '(define-key LaTeX-mode-map (kbd "C-c w") 'latex-word-count))


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
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (push
             '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
               :help "Run latexmk on file")
             TeX-command-list)))
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (push
             '("xelatexmk" "latexmk -xelatex -pdf %s" TeX-run-TeX nil t
               :help "Run latexmk -xelatex on file")
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

(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
                '("pdftex" "pdftex %s" TeX-run-command t t
                  :help "Run pdftex on file") t))

;;;; LaTeX/Cocktails ;;;;
;; define minor mode for LaTeX'd cocktail recipes -- totally pesonal
(define-minor-mode cocktail-mode
  "cocktail-mode provides a minor mode for 
   yasnippet to hook onto in order to make
   cocktail creation easier."
  :init-value nil
  :lighter " cktl")

;;;; define minor mode for LaTeX'd cocktail menus, too
(define-minor-mode drink-menu-mode
  "drink-menu-mode provides a minor mode for 
   yasnippet to hook onto in order to make
   drink menu creation easier."
  :init-value nil
  :lighter " dmm")

;; in fact, I think this is how you bind special yasnippets to minor modes:
;; source: http://capitaomorte.github.io/yasnippet/snippet-expansion.html
;; Controlling Expansion -> Eligible snippets -> yas-activate-extra-mode
(add-hook 'cocktail-mode-hook
          '(lambda () ;; this line started with a # before -- pretty sure I can
                      ;; remove that
             (yas-activate-extra-mode 'cocktail-mode)))

(add-hook 'drink-menu-mode-hook
          '(lambda () (yas-activate-extra-mode 'drink-menu-mode)))

(define-minor-mode yaml-cocktail-mode
  "cocktail-mode provides a minor mode for 
   yasnippet to hook onto in order to make
   cocktail creation easier."
  :init-value nil
  :lighter " yacm")

(add-hook 'yaml-cocktail-mode-hook
          '(lambda () (yas-activate-extra-mode 'yaml-cocktail-mode)))

(add-hook 'yaml-cocktail-mode-hook 'auto-fill-mode)
(add-hook 'yaml-cocktail-mode-hook 'yas-minor-mode)
(add-hook 'yaml-cocktail-mode-hook 'yaml-mode)

(add-to-list 'auto-mode-alist '("\\.ctl\\.yml\\'" . yaml-cocktail-mode))


;; see above once this actually works right (APAenumerate, aenum, etc.)
;; (defun my:cktl-add-latex-environments ()
;;   (LaTeX-add-environments
;;    '("Ingredients" LaTeX-env-item)
;;    ))
;; (add-hook 'cocktail-mode-hook 'my:cktl-add-latex-environments)

;;;; LaTeX/listings mode ;;;;
(define-minor-mode listings-mode
  "listings-mode makes it a little easier
   to use LaTeX's listings package"
  :init-value nil
  :lighter " listings")

(add-hook 'listings-mode-hook
          '(lambda ()
             (yas-activate-extra-mode 'listings-mode)))

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
;; (add-to-list 'load-path "~/.emacs.d/plugins/eagle-ul-mode")
;; (require 'eagle-ul-mode)
;;;; disabling EAGLE UL mode because:
;;;; (started with upgrade to Emacs 25)
;; Debugger entered--Lisp error: (wrong-number-of-arguments setq 3)
;;   (setq eagle-ul-all-constants eagle-ul-single-valued-constants eagle-ul-array-constants)
;;   eval-buffer(#<buffer  *load*-259146> nil "/Users/matlock/.emacs.d/plugins/eagle-ul-mode/eagle-ul-mode.el" nil t)  ; Reading at buffer position 3155
;;   load-with-code-conversion("/Users/matlock/.emacs.d/plugins/eagle-ul-mode/eagle-ul-mode.el" "/Users/matlock/.emacs.d/plugins/eagle-ul-mode/eagle-ul-mode.el" nil t)
;;   require(eagle-ul-mode)
;;   eval-buffer(#<buffer  *load*> nil "/Users/matlock/.emacs" nil t)  ; Reading at buffer position 37218
;;   load-with-code-conversion("/Users/matlock/.emacs" "/Users/matlock/.emacs" nil nil)
;;   load("/Users/matlock/.emacs" nil nil t)
;;   load-file("~/.emacs")
;;   reload-dotemacs()
;;   funcall-interactively(reload-dotemacs)
;;   call-interactively(reload-dotemacs nil nil)
;;   command-execute(reload-dotemacs)


;;;; Org Mode ;;;;

;; pdf processing to enable BiBTeX usage
;; see http://tex.stackexchange.com/questions/197707/using-bibtex-from-org-mode-bbl-and-aux-files-are-incorrectly-generated
(setq org-latex-pdf-process
      '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))

;; don't prompt for LaTeX code block evaluation
;; see http://orgmode.org/manual/Code-evaluation-security.html
(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "latex")))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; turn on spell check by default
(add-hook 'org-mode-hook 'flyspell-mode)

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
(defun my:insert-corner-brackets ()
  "easier than C-c l (whatever you were going to write) C-c r, but not as good
   as a snippet"
  (local-set-key (kbd "C-c e")
                 (lambda ()
                   (interactive)
                   (insert "「」")
                   ; need to move cursor back a space
                   (left-char))))
(defun my:org-insert-bullet ()
  "easier than C-x 8 <RET> bullet <RET>"
  (local-set-key (kbd "C-c b")
                 (lambda ()
                   (interactive)
                   (insert "• "))))
(add-hook 'org-mode-hook 'my:insert-left-corner-bracket)
(add-hook 'org-mode-hook 'my:insert-right-corner-bracket)
(add-hook 'org-mode-hook 'my:insert-corner-brackets)
(add-hook 'org-mode-hook 'my:org-insert-bullet)
;; also add these ~two~ three to Emacs Lisp mode
(add-hook 'emacs-lisp-mode-hook 'my:insert-left-corner-bracket)
(add-hook 'emacs-lisp-mode-hook 'my:insert-right-corner-bracket)
(add-hook 'emacs-lisp-mode-hook 'my:insert-corner-brackets)
;; useful in text mode too because that's where you edit git commit messages
(add-hook 'text-mode-hook 'my:insert-left-corner-bracket)
(add-hook 'text-mode-hook 'my:insert-right-corner-bracket)
(add-hook 'text-mode-hook 'my:insert-corner-brackets)
(add-hook 'text-mode-hook 'my:org-insert-bullet)

;; TODO list intermediate state colors
;; source: http://cjohansen.no/en/emacs/emacs_org_mode_todo_colors
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "red" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("IN-PROGRESS" . (:foreground "yellow" :weight bold))
        ("ON-HOLD" . (:foreground "yellow" :weight bold))))

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

(require 'ob-ipython)

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
                               (org . t)
                               (ipython . t)))
;; not sure why C++ isn't working
;; see http://orgmode.org/manual/Languages.html#Languages
;; maybe C++ will work now that I've upgrade Org mode to v8.2.10 -- eh
(setq org-src-fontify-natively t)

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
             (yas-activate-extra-mode 'org-extra-yas-mode)
             (yas-minor-mode 1)))

(defun org-extra-yas-mode-activation-kludge ()
  (org-extra-yas-mode 1))
(add-hook 'org-mode-hook 'org-extra-yas-mode-activation-kludge)
;; ok, that works, as long as you have the hook thing above working

;; subscript and superscript behavior -- turn it off without curly braces
;; source: http://orgmode.org/manual/Subscripts-and-superscripts.html
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

;;;; Clojure ;;;;
;; source: http://clojure-doc.org/articles/tutorials/emacs.html
(defvar my:clojure-packages '(better-defaults
                      clojure-mode
                      ;; clojure-test-mode ;; legacy mode
                      cider))

(dolist (p my:clojure-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;; supplemental yasnippet stuff ;;;;
;; define .snip files to be snippet definition templates; have them use
;; snippet-mode
(setq auto-mode-alist
      (cons '("\\.snip$" . snippet-mode)
            auto-mode-alist))

;; bind M-<TAB> to yas-expand in addition to tab so you can have a snippet
;; within a snippet
;; see https://joaotavora.github.io/yasnippet/snippet-expansion.html
(define-key yas-minor-mode-map (kbd "M-TAB") 'yas-expand)

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

;;;; Shell (ansi-term)
;; https://stackoverflow.com/a/12679864
(setq explicit-shell-file-name "/usr/local/bin/bash")

;;;; Tramp Mode ;;;;
;; problem with hanging; trying this:
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
;; maybe this should be updated to /usr/local/bin/bash or I should figure out
;; how to symlink /usr/local/bin/bash to /bin/bash; if so, the above
;; explicit-shell-file-name should be updated

;; use ssh as default for Tramp
;; see
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Default-Method.html
(setq tramp-default-method "ssh")

;;;; Dot Mode (graphviz)
(load "~/.emacs.d/plugins/dot-mode/dot-mode.el")

;;;; align.el
;; http://www.emacswiki.org/emacs/AlignColumn
(add-to-list 'load-path "~/.emacs.d/plugins/align")
(autoload 'align-cols "align" "Align text in the region." t)

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
(add-hook 'geiser-repl-mode-hook #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook #'enable-paredit-mode)

;;;; SLIME, SBCL, & quicklisp

(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy
                       slime-tramp
                       slime-asdf))
;; (slime-require :swank-listener-hooks)

;; add paredit to slime-repl-mode
(add-hook 'slime-repl-mode-hook 'paredit-mode)

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

;; Haskell REPL stuff
;;https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(package-selected-packages
   (quote
    (s ob-ipython ein electric-case electric-operator electric-spacing elein eldoc-overlay eldoc-eval ac-slime elisp-slime-nav slime auto-complete emojify yaml-mode wgrep-ack wget web-beautify tagedit sx scion pytest paredit pandoc-mode org-bullets org-ac org nodejs-repl magithub json-mode js3-mode iedit help-mode+ help-fns+ help+ helm-ghc helm-bibtex gist flycheck-pyflakes exec-path-from-shell d-mode company-auctex color-theme-solarized cider blank-mode bison-mode better-defaults awk-it auto-complete-chunk auto-complete-c-headers auctex-latexmk arduino-mode ac-python ac-js2 ac-geiser))))


(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
(eval-after-load 'haskell-cabal
  '(progn
     (define-key haskell-cabal-mode-map (kbd "C-c C-z")
       'haskell-interactive-switch)
     (define-key haskell-cabal-mode-map (kbd "C-c C-k")
       'haskell-interactive-mode-clear)
     (define-key haskell-cabal-mode-map (kbd "C-c C-c")
       'haskell-process-cabal-build)
     (define-key haskell-cabal-mode-map (kbd "C-c c")
       'haskell-process-cabal)))

(eval-after-load 'haskell-mode
          '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

;;;; Flyspell
;; path to ispell
;; source: http://unix.stackexchange.com/questions/38916/how-do-i-configure-emacs-to-use-ispell-on-mac-os-x
(setq ispell-program-name "/usr/local/bin/ispell")

;; actually, it sounds like you should use aspell instead?
;; see http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
;; and http://emacs-fu.blogspot.com/2009/12/automatically-checking-your-spelling.html
;; and http://blog.binchen.org/posts/effective-spell-check-in-emacs.html
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

;;;; Markdown mode
;; 「M-x package-list-packages」 and installed markdown-mode, markdown-mode+,
;; and gh-md (GitHub markdown)

;;;; Magit
(global-set-key (kbd "C-c 0") 'magit-status)

;;;; ANSI term
(global-set-key (kbd "C-c +") 'ansi-term)

;;;; daily-todo script
(defun daily-todo (&optional overwrite?)
  "call daily-todo script and open the newly-created file
   
   optional overwrite? arg will overwrite existing file without complaint, so
   be careful"
  (interactive "P")
  (let ((command-string "daily-todo --print_path"))
    (if overwrite?
        (setq command-string (concat command-string " --overwrite"))
      (setq command-string (concat command-string " --no_prompts")))
    (let ((new-daily-todo-path
           (replace-regexp-in-string "\n" "" (shell-command-to-string
                                              command-string))))
      (find-file new-daily-todo-path))))

;;;; Markdown mode
(defun markdown-set-markdown-preview-key ()
  (local-set-key (kbd "C-c p") 'markdown-preview))
(add-hook 'markdown-mode-hook 'markdown-set-markdown-preview-key)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;; IPython/EIN
;; (require 'ein)
;; ;; ;; https://stackoverflow.com/a/32086985/2677392
;; ;; (require 'python)
;; ;; (setq python-shell-interpreter "ipython")
;; ;; (setq python-shell-interpreter-args "--pylab")
;; ;; ;; Emacs froze when I tried to open a new .py file with that in effect
;; ;; this seems reasonable:
;; ;; https://stackoverflow.com/a/17822336/2677392
;; (when (executable-find "ipython")
;;   (setq python-shell-interpreter "ipython"))
;; Emacs froze again when I tried to open a new .py file (but in a different
;; way--it was after loading yasnippets this time)
;; I'm just going to try using IPython through the shell for now
