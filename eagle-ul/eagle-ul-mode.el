;; eagle-ul-mode.el
;; Ryan Matlock, 2014

;; This was first started with EAGLE-6.6.0, although the User Language Manual
;; that I'm using dates back to 2009.  Things may change in the future.

;; source for how to do this: http://ergoemacs.org/emacs/elisp_syntax_coloring.html
;; actually, I'm starting this over again using http://www.emacswiki.org/emacs/ModeTutorial

(defvar eagle-ul-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.ulp$" . eagle-ul-mode))


;; syntax table
(defvar eagle-ul-mode-syntax-table
  (let ((st (make-syntax-table))))) ;; uh??

(setq eagle-ul-directives
      '("#include" "#require" "#usage"))

(setq eagle-ul-single-valued-constants
      '("EAGLE_VERSION" "EAGLE_RELEASE" "EAGLE_SIGNATURE" "EAGLE_DIR"
        "EAGLE_PATH" "EAGLE_HOME" "OS_SIGNATURE" "REAL_EPSILON" "REAL_MAX"
        "REAL_MIN" "INT_MAX" "INT_MIN" "PI" "usage"))

(setq eagle-ul-array-constants
      '("path_lbr" "path_dru" "path_ulp" "path_scr" "path_cam" "path_epf"
        "used_libraries"))

(setq eagle-ul-character-functions
      '("isalnum" "isalpha" "iscntrl" "isdigit" "isgraph" "islower" "isprint"
        "ispunct" "isspace" "isxdigit" "tolower" "toupper"))

(setq eagle-ul-file-handling-functions
      '("fileerror" "fileglob" "filedir" "fileext" "filename" "fileread"
        "filesetext" "filesize" "filetime"))

(setq eagle-ul-mathematical-functions
      '("abs" "acos" "asin" "atan" "ceil" "cos" "exp" "floor" "frac" "log"
        "log10" "max" "min" "pow" "round" "sin" "sqrt" "trunc" "tan"))

(setq eagle-ul-unit-conversion-functions
      '("u2inch" "u2mic" "u2mil" "u2mm" "inch2u" "mic2u" "mil2u" "mm2u"))

(setq eagle-ul-misc-functions
      '("exit" "language" "lookup" "palette" "sort" "status" "system"))

(setq eagle-ul-string-functions
      '("strchr" "strjoin" "strlen" "strlwr" "strrchr" "strrstr" "strsplit"
        "strsub" "strtod" "strtol" "strupr"))

(setq eagle-ul-time-functions
      '("t2day" "t2dayofweek" "t2hour" "t2minute" "t2month" "t2second"
        "t2string" "t2year" "time" "timems"))

(setq eagle-ul-object-functions
      '("ingroup"))

(setq eagle-ul-builtin-statements
      '("board" "deviceset" "library" "output" "package" "schematic" "sheet"
        "symbol"))

(setq eagle-ul-predefined-dialogs
      '("dlgDirectory" "dlgFileOpen" "dlgFileSave" "dlgMessageBox"))

(setq eagle-ul-dialog-objects
      '("dlgCell" "dlgCheckBox" "dlgComboBox" "dlgDialog" "dlgGridLayout"
        "dlgGroup" "dlgHBoxLayout" "dlgIntEdit" "dlgLabel" "dlgListView"
        "dlgPushButton" "dlgRadioButton" "dlgRealEdit" "dlgSpacing"
        "dlgSpinBox" "dlgStretch" "dlgStringEdit" "dlgTabPage" "dlgTabWidget"
        "dlgTextEdit" "dlgTextView" "dlgVBoxLayout"))

(setq eagle-ul-dialog-functions
      '("dlgAccept" "dlgRedisplay" "dlgReset" "dlgReject"))

(setq eagle-ul-html-tags
      '("html" "h1" "h2" "h3" "p" "center" "blockquote" "ul" "ol" "li" "pre"
        "a" "em" "strong" "i" "b" "u" "big" "small" "code" "tt" "font" "img"
        "hr" "br" "nobr" "table" "tr" "td" "th" "author" "dl" "dt" "dd"
        "&lt;" "&gt;" "&amp;" "&nbsp;" "&auml;" "&ouml;" "&uuml;" "&Auml;"
        "&Ouml;" "&Uuml;" "&szlig;" "&copy;" "&deg;" "&micro;" "&plusmn;"))

;; (setq eagle-ul-schematic-data-members
;;       '("alwaysvectorfont" "description" "grid" "headline" "name" 
;;         "verticaltext" "xreflabel"))

;; (setq eagle-ul-schematic-loop-members
;;       '("attributes" "classes" "layers" "libraries" "nets" "parts" "sheets"
;;         "variantdefs"))

;; concatenate all kinds into single keyword class (directives aren't included 
;; because they'd be the only members of their class)
(setq eagle-ul-all-constants
      eagle-ul-single-valued-constants
      eagle-ul-array-constants)

(setq eagle-ul-all-functions
      eagle-ul-character-functions
      eagle-ul-file-handling-functions
      eagle-ul-mathematical-functions
      eagle-ul-unit-conversion-functions
      eagle-ul-misc-functions
      eagle-ul-string-functions
      eagle-ul-time-functions
      eagle-ul-object-functions)

;; curious (2014/06/09): the builtin statements keywords are being highlighted
;; curiouser (2014/06/09): tried writing first word of each group up there in
;; a .ulp file, and only EAGLE_VERSION and board had any change in text color
(setq eagle-ul-all-builtins
      eagle-ul-builtin-statements
      eagle-ul-predefined-dialogs
      eagle-ul-dialog-objects
      eagle-ul-dialog-functions
      eagle-ul-html-tags)

(setq eagle-ul-directives-regexp
      (regexp-opt eagle-ul-directives 'words))
(setq eagle-ul-constants-regexp
      (regexp-opt eagle-ul-all-constants 'words))
(setq eagle-ul-functions-regexp
      (regexp-opt eagle-ul-all-functions 'words))
(setq eagle-ul-builtins-regexp
      (regexp-opt eagle-ul-all-builtins 'words))

;;;; now we can free up lots of memory
;;;; directives
(setq eagle-ul-directives nil)
;;;; constants
(setq eagle-ul-single-valued-constants nil)
(setq eagle-ul-array-constants nil)
(setq eagle-ul-all-constants)
;;;; functions
(setq eagle-ul-character-functions nil)
(setq eagle-ul-file-handling-functions nil)
(setq eagle-ul-mathematical-functions nil)
(setq eagle-ul-unit-conversion-functions nil)
(setq eagle-ul-misc-functions nil)
(setq eagle-ul-string-functions nil)
(setq eagle-ul-time-functions nil)
(setq eagle-ul-object-functions nil)
(setq eagle-ul-all-functions nil)
;;;; builtins
(setq eagle-ul-builtin-statements nil)
(setq eagle-ul-predefined-dialogs nil)
(setq eagle-ul-dialog-objects nil)
(setq eagle-ul-dialog-functions nil)
(setq eagle-ul-html-tags nil)
(setq eagle-ul-all-builtins nil)

;; create the list for font-lock
(setq eagle-ul-font-lock-keywords
      `(
        (,eagle-ul-directives-regexp . font-lock-preprocessor-face)
        (,eagle-ul-constants-regexp . font-lock-constant-face)
        (,eagle-ul-functions-regexp . font-lock-function)
        (,eagle-ul-builtins-regexp . font-lock-builtin-face)))

(define-derived-mode eagle-ul-mode c-mode
  "EAGLE UL mode"
  "Major mode for writing EAGLE CAD ULPs"

  ;; code for syntax highlighting
  (font-lock-add-keywords 'eagle-ul-mode-hook
                          '((eagle-ul-font-lock-keywords)))

  ;; clear memory
  ;; (setq eagle-ul-directives-regexp nil)
  ;; (setq eagle-ul-constants-regexp nil)
  ;; (setq eagle-ul-functions-regexp nil)
  ;; (setq eagle-ul-builtins-regexp nil)

  ;; you have to run eagle-ul-mode-hook to allow for outside customization
  (run-hooks 'eagle-ul-mode-hook))

;; (setq auto-mode-alist
;;       (cons '("\\.ulp$" . eagle-ul-mode)
;;             auto-mode-alist))


(provide 'eagle-ul-mode)
