;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Matlock's .emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; load .el files
(add-to-list 'load-path "~/elisp")

;;;; General editing ;;;;
;;;; reload .emacs with M-1
;;(global-set-key (kbd "<f12>") (load-file "~/.emacs"))

;;;; indent region using (select region) + > and unindent region using <
;; (defun my-indent-region (N)
;;   (interactive "p")
;;   (if (use-region-p)
;;       (progn (indent-rigidly (region-beginning) (region-end) (* N 4))
;;              (setq deactivate-mark nil))
;;     (self-insert-command N)))

;; (defun my-unindent-region (N)
;;   (interactive "p")
;;   (if (use-region-p)
;;       (progn (indent-rigidly (region-beginning) (region-end) (* N -4))
;;              (setq deactivate-mark nil))
;;     (self-insert-command N)))

;; (global-set-key ">" 'my-indent-region)
;; (global-set-key "<" 'my-unindent-region)

;;(defun un-indent-region ('indent-rigidly -4))

;;(global-set-key (kbd "M-[") 'indent-region)
;;(global-set-key (kbd "M-]") 'un-indent-region)

;;;; indent region with C->, unindent with C-<
;;;; source: http://www.emacswiki.org/emacs-ja/IndentRigidlyN
(defun indent-rigidly-n (n)
      "Indent the region, or otherwise the current line, by N spaces."
      (let* ((use-region (and transient-mark-mode mark-active))
             (rstart (if use-region (region-beginning) (point-at-bol)))
             (rend   (if use-region (region-end)       (point-at-eol)))
             (deactivate-mark "irrelevant")) ; avoid deactivating mark
        (indent-rigidly rstart rend n)))
    ;;;; stock implementation is as follows:
    ;; (defun indent-rigidly-4 ()
    ;;   "Indent the region, or otherwise the current line, by 4 spaces."
    ;;   (interactive)
    ;;   (indent-rigidly-n 4))
    ;; (defun outdent-rigidly-4 ()
    ;;   "Indent the region, or otherwise the current line, by -4 spaces."
    ;;   (interactive)
    ;;   (indent-rigidly-n -4))
    ;;;; I think it would be better to use c-basic-offset (4 spaces) instead
    (defun indent-rigidly-4 ()
      "Indent the region, or otherwise the current line, by 4 spaces."
      (interactive)
      (indent-rigidly-n 'c-basic-offset))
    (defun outdent-rigidly-4 ()
      "Indent the region, or otherwise the current line, by -4 spaces."
      (interactive)
      (indent-rigidly-n (- 'c-basic-offset)))
    ;;;; stock value was [(control->)] and [(control-<)], respectively
    ;;;; but (kbd "C->") seems like the way it's done
    (global-set-key (kbd "C->") 'indent-rigidly-4)
    (global-set-key (kbd "C-<") 'outdent-rigidly-4)

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
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 80)

;; set README, LICENSE.md files to open in text-mode
;; source: http://www.gnu.org/software/emacs/manual/html_node/elisp/Auto-Major-Mode.html
;; ok, that isn't working
;; (setq auto-mode-alist
;;       (append (;;("README*'" . text-mode)
;;                ("\\.md\\'" . text-mode))
;;               auto-mode-alist))

;; let's set README, LICENSE.md file to text-mode like this:
;; source: http://www.emacswiki.org/emacs/AutoModeAlist
;; (plus slight modification to make things more efficient?)
;; source: http://www.gnu.org/software/emacs/manual/html_node/elisp/List-Variables.html
(add-to-list 'auto-mode-alist '("\\LICENSE.md\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\README*\\'" . text-mode))

;;;; C ;;;;

;;;; auto-indent on newline
(add-hook 'c-mode-common-hook '(lambda ()
    (local-set-key (kbd "RET") 'newline-and-indent)))

;;;; set default indentation
;(setq-default c-basic-offset 4)

;;;; Allman/ANSI/BSD-style indentation
;(setq-default c-default-style '(other . "bsd"))

;;;; Allman-style indentation + indentation amount
(setq c-default-style "bsd"
    c-basic-offset 4)

;;;; Emacs as a C/C++ IDE: auto-complete, yasnippet, auto-complete ce headers
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

;; I was looking in here: https://groups.google.com/forum/#!topic/smart-snippet/igXJ9RsFFdg for the next two lines -- update: I modified this
;; (yas/initialize) ;; I guess I don't need this?

;; I'm now looking at https://groups.google.com/forum/#!topic/smart-snippet/Cf1jjx_xZRw
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/emacs/yasnippets")))
(yas-global-mode 1)

;; going to see if this fix works: https://github.com/capitaomorte/yasnippet/issues/231 -- actually, I don't think this is what I need
;; (add-hook 'cocktail-mode-hook
;;           '(lambda()
;;              (local-set-key [tab] 'yas/expand)))

;; in fact, I think this is how you bind special yasnippets to minor modes:
;; source: http://capitaomorte.github.io/yasnippet/snippet-expansion.html
;; Controlling Expansion -> Eligible snippets -> yas-activate-extra-mode
(add-hook 'cocktail-mode-hook
          #'(lambda ()
              (yas-activate-extra-mode 'cocktail-mode)))

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

;;;; none of these lines seemed to work especially well, so I'm trying a new
;;;; approach
;; ;;;; use pdflatex instead of latex
;; (setq latex-run-command "pdflatex")

;; ;;;; set AUCTeX to pdf mode
;; (setq TeX-PDF-mode t)

;; ;;;; continuously use latexmk (for compilation & previewing)
;; ;;;; source: http://stackoverflow.com/questions/15892486/how-to-have-latexmk-work-with-emacs-and-okular
;; (add-hook 'LaTeX-mode-hook (lambda ()
;;   (push
;;     '("Latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
;;       :help "Run Latexmk on file")
;;     Tex-command-list)))

;; ;;;; needed to get MELPA working (for LaTeXPreviewPane)
;; ;;;; see: http://www.emacswiki.org/emacs/LaTeXPreviewPane
;; ;;(require 'package)
;; ;;(add-to-list 'package-archives
;; ;;  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; ;;;; load latex-preview-pane
;; (load "~/elisp/latex-preview-pane.el")
;; ;;(load "~/elisp/latex-preview-pane-pkg.el")

;; ;;(latex-preview-pane-enable)

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
;; I don't know why this didn't cause an error, because copy/pasting yielded:
;; (setq TeX-output-view-style (quote (
;; (“^pdf$” “.” “evince %o”)
;; (“^ps$” “.” “gv %o”)
;; (“^dvi$” “.” “xdvi %o”)
;; )))
;; originally
(setq TeX-output-view-style (quote (("^pdf$" "." "vince %o")
                                    ("^ps$" "." "gv %o")
                                    ("^dvi$" "." "xdvi %o"))))
;; got an error: Symbol's value as variable is void: “xdvi”
;; I think the issue was how the quotes got copied and pasted
;; here's how it looked originally:
;; (setq tex-dvi-view-command “xdvi”)
(setq tex-dvi-view-command "xdvi")
(setq tex-dvi-print-command "dvips")
(setq tex-alt-dvi-print-command "dvips")

;;;; yet another folding mode
;;;; source: http://www.emacswiki.org/emacs/FoldingMode
(load "~/elisp/yafolding.el")

;;;; yafolding example config
(define-key global-map (kbd "C-'") 'yafolding)
;;(define-key global-map (kbd "C-c C-f") 'yafolding-toggle-all)
(define-key global-map (kbd "C-c C-f") 'yafolding-toggle-all-by-current-level)
;;(add-hook 'indent-buffer-before-hook
;;    (lambda ()
;;      (yafolding-temp-toggle nil)))
;;(add-hook 'indent-buffer-after-hook
;;    (lambda ()
;;      (yafolding-temp-toggle t)))



;;;; define minor mode for LaTeX'd cocktail recipes -- totally pesonal
(define-minor-mode cocktail-mode
  "cocktail-mode provides a minor mode for 
   yasnippet to hook onto in order to make
   cocktail creation easier."
  :init-value nil
  :lighter " cktl")
