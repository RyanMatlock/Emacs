#!/usr/local/bin/python3
"""
snippet-maker.py

Reads in ;; separated value file of name, key, and snippet pattern i.e.
<name>;;<key>;;<snippet>
to auto-generate yasnippet-compatible files.
"""
import os.path

COMMENT_CHAR = "#"
SPLIT_STR = ";;"
raw_snippet_templates = "snippets.dat"

snippet_templates = []
with open(raw_snippet_templates) as raw_snippets:
    for line in raw_snippets:
        if line[0] != COMMENT_CHAR:
            snippet_templates.append(line.split(SPLIT_STR))
        
NAME_PREFIX = "# name: "
KEY_PREFIX = "# key: "
CONTRIBUTOR_PREFIX = "# contributor: "
CONTRIBUTOR = "Ryan Matlock <ryan.matlock@gmail.com>"
SEPARATOR = "# --"

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

    with open(os.path.join(OUTPUT_SUBFOLDER + name), "w") as snippet_file:
        snippet_file.write(NAME_PREFIX + name + "\n")
        snippet_file.write(KEY_PREFIX + key + "\n")
        snippet_file.write(CONTRIBUTOR_PREFIX + CONTRIBUTOR + "\n")
        snippet_file.write(SEPARATOR + "\n")
        snippet_file.write(snippet)
    with open(LATEX_OUTPUT_FILE, "a") as latex_file:
        latex_file.write("%% " + key + ": " + snippet + "\n")

print("Snippet writing finished!")
