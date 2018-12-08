(defun foo ()
  "curious about calling functions defined within functions in LISP

   calling 「M-x foo」 has the desired effect of messaging 'Hello, world!'"
  (interactive)
  (defun bar ()
            "a nested function"
            (message "Hello, world!"))
  (bar))


(defun plp-find-installed ()
  "search package-list-packages for installed packages and store their names in
   a list"
  (interactive)
  (goto-char 1)
  (setq plp-installed-packages '())
  (while (search-forward-regexp
          ;; tested this
          "^[[:space:]]\{2\}\([-A-Za-z0-9]+\)[[:space:]]+[.0-9]+[[:space:]]+installed"
          ;; "^[[:space:]]\{2\}\([-A-Za-z0-9]+\)[[:space:]]+[.0-9]+[[:space:]]+installed"
          nil t)
    (setq plp-installed-packages (cons (match-string 1)
                                       plp-installed-packages)))
  (message (match-string 1))
  (sleep-for 1))

(defun print-list-elements (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))
  (print-list-elements plp-installed-packages)

  0blayout           1.0.2         available  melpa-s... Layout grouping with ease
  0blayout           20161007.2307 available  melpa      Layout grouping with ease
  0xc                20170125.1953 available  melpa      Base conversion made easy
  2048-game          20151026.1233 available  melpa      play 2048 in Emacs
  4clojure           20131014.1507 available  melpa      Open and evaluate 4clojure.com questions
  @                  20140707.520  available  melpa      multiple-inheritance prototype-based objects DSL
  aa-edit-mode       0.0.2         available  melpa-s... Major mode for editing AA(S_JIS Art) and .mlt file
  aa-edit-mode       20170118.1920 available  melpa      Major mode for editing AA(S_JIS Art) and .mlt file
  abc-mode           20140225.944  available  melpa      Major mode for editing abc music files
  abc-mode           20140225.944  available  melpa-s... Major mode for editing abc music files
  abl-mode           0.9.0         available  marmalade  Python TDD minor mode
  abl-mode           20170211.1328 available  melpa      Python TDD minor mode
  abyss-theme        0.5           available  melpa-s... A dark theme with contrasting colours.
  ac-geiser          20130928.2347 installed             Auto-complete backend for geiser
  ac-python          20110519      installed             Simple Python Completion Source for Auto-Complete
  arduino-mode       20150503.758  installed             Major mode for the Arduino language
  async              20150724.2211 installed             Asynchronous processing in Emacs
  auctex             11.88.6       installed             Integrated environment for *TeX*
  auctex-latexmk     20140904.1918 installed             Add LatexMk support to AUCTeX
  auto-complete      20150618.1949 installed             Auto Completion for GNU Emacs
  auto-complete-c... 20140325.835  installed             An auto-complete source for C/C++ header files
  auto-complete-c... 20140225.146  installed             Auto-completion for dot.separated.words.
  auto-complete-pcmp 20140226.2251 installed             Provide auto-complete sources using pcomplete results
  awk-it             20130917.1848 installed             Run AWK interactively on region!
  better-defaults    20150404.223  installed             Fixing weird quirks and poor defaults
  blank-mode         20130824.1159 installed             minor mode to visualize TAB, (HARD) SPACE, NEWLINE
  cider              20150725.1319 installed             Clojure Interactive Development Environment that Rocks
  clojure-mode       20150723.1038 installed             Major mode for Clojure code
  color-theme        20080305.34   installed             install color themes
  color-theme-sol... 20150619.1734 installed             Solarized themes for Emacs
  company            20150706.347  installed             Modular text completion framework
  company-auctex     20150620.1421 installed             Company-mode auto-completion for AUCTeX
  d-mode             20150621.614  installed             D Programming Language major mode for (X)Emacs

