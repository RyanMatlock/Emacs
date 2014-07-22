#!/usr/local/bin/python3
"""
snippet-generator.py

Reads in ;; separated value file of name, key, and snippet pattern i.e.
<name>;;<key>;;<snippet>
to auto-generate yasnippet-compatible files.  If snippet definition file is
named <foo-mode>.<ext>, it places the resulting snippets in the ./<foo-mode>/,
which will be created if it does not exist.  Any files with the same name as
the target files will be overwritten.

* TODO features:
- [ ] use the argparse module (https://docs.python.org/3.4/library/argparse.html)
  to allow arbitrary comment and split characters
"""
import re
import sys
import os

COMMENT_CHAR = "#"
SPLIT_STR = ";;"

NAME_PREFIX = "# name: "
KEY_PREFIX = "# key: "
CONTRIBUTOR_PREFIX = "# contributor: "
CONTRIBUTOR = "Ryan Matlock <ryan.matlock@gmail.com>"
SEPARATOR = "# --"

snippet_defs = []
while True:
    if len(sys.argv) > 1:
        source = sys.argv[1]
    else:
        while True:
            source = input("Enter filename to process: ")
            if source:
                break
    try:
        source = os.path.expanduser(source)
        with open(source) as f:
            for line in f:
                snippet_defs.append(line)
        break
    except IOError as e:
        print("{}".format(e))
        stop = input("Would you like to try again? (y/n): ")
        if stop.lower() == "y" or stop.lower() == "yes":
            pass
        else:
            exit()





snippet_templates = []
with open(raw_snippet_templates) as raw_snippets:
    for line in raw_snippets:
        if line[0] != COMMENT_CHAR:
            snippet_templates.append(line.split(SPLIT_STR))
        

OUTPUT_SUBFOLDER = "generated/"
LATEX_OUTPUT_FILE = "add_to_latex.tex"

# this should overwrite old versions of the output file
# note that "a" = append, "w" = (over)write, "r" = read
with open(LATEX_OUTPUT_FILE, "w"):
    pass

for snippet_template in snippet_templates:
    name = snippet_template[0]
    key = snippet_template[1]
    snippet = snippet_template[2]

    # I'd prefer to explicity add a newline when I want it, so I'll get rid of
    # it if it's there                      
    if snippet[-1] == "\n":
        snippet = snippet[:-1]


out_dir = os.path.dirname(fname)
out_fname, ext = os.path.splitext(os.path.basename(fname))
out_fname += "-allman" + ext

with open(os.path.join(out_dir, out_fname), "w") as out:
    for line in lines:
        out.write(line)

with open(os.path.join(OUTPUT_SUBFOLDER + name), "w") as snippet_file:
    snippet_file.write(NAME_PREFIX + name + "\n")
    snippet_file.write(KEY_PREFIX + key + "\n")
    snippet_file.write(CONTRIBUTOR_PREFIX + CONTRIBUTOR + "\n")
    snippet_file.write(SEPARATOR + "\n")
    snippet_file.write(snippet)
with open(LATEX_OUTPUT_FILE, "a") as latex_file:
    latex_file.write("%% " + key + ": " + snippet + "\n")

print("Snippet writing finished!")

