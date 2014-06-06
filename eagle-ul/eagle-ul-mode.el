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

(setq eagle-ul-unit-conversion-functions
      '("u2inch" "u2mic" "u2mil" "u2mm" "inch2u" "mic2u" "mil2u" "mm2u"))

(setq eagle-ul-misc-functions
      '("output" "schematic"))

(define-derived-mode eagle-ul-mode c-mode
  (setq font-lock-defaults '(eagle-ul-keywords))
  (setq mode-name "EAGLE UL"))

(provide 'eagle-ul-mode)
