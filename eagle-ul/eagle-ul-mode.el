;; source for how to do this: http://ergoemacs.org/emacs/elisp_syntax_coloring.html

;; (setq eagle-ul-keywords
;;       '(("schematic" . font-lock-function-name-face)
;;         ("name\\|value" . font-lock-constant-face)))

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
      eagle-ul-time-functions)

(define-derived-mode eagle-ul-mode c-mode
  (setq font-lock-defaults '(eagle-ul-keywords))
  (setq mode-name "EAGLE UL"))

(provide 'eagle-ul-mode)
