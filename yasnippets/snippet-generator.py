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
- [ ] allow for extra fields in the snippet so you can set stuff like expand
  environment and stuff like that (see
  http://capitaomorte.github.io/yasnippet/snippet-development.html#sec-2-5)
  (in order to do this, you'll want to check the length of your split string)
"""

import re
import sys
import os

COMMENT_CHAR = "#"
SPLIT_STR = ";;"

NAME_PREFIX = "# name: "
KEY_PREFIX = "# key: "
CONTRIBUTOR_PREFIX = "# contributor: "
contributor = "Ryan Matlock <ryan.matlock@gmail.com>"
SEPARATOR = "# --"

snippet_defs = []
while True:
    if len(sys.argv) > 1:
        source = sys.argv[1]
    else:
        source = input("Enter filename to process (or nothing to exit): ")
        if source:
            pass
        else:
            exit()
            
    try:
        source = os.path.expanduser(source)
        with open(source) as f:
            for line in f:
                if line:
                    if line[0] == COMMENT_CHAR:
                        pass
                    else:
                        # I want to explicity add newline characters
                        if line[-1] == "\n":
                            line = line[:-1]
                        snippet_defs.append(line)
        break
    except IOError as e:
        print("{}".format(e))
        stop = input("Would you like to try again? (y/n): ")
        if stop.lower() == "y" or stop.lower() == "yes":
            pass
        else:
            exit()


source_path = os.path.dirname(source)
source_base_name, source_ext = os.path.splitext(os.path.basename(source))

target_path = os.path.join(source_path, source_base_name)

try:
    if not os.path.isdir(target_path):
        os.mkdir(target_path)
except OSError as e:
    print("{}".format(e))
    print("Unrecoverable error; terminating.")


for snippet_def in snippet_defs:
    try:
        name, key, snippet = snippet_def.split(SPLIT_STR)
        try:
            with open(os.path.join(target_path, name), "w") as out:
                out.write(NAME_PREFIX + name + "\n")
                out.write(KEY_PREFIX + key + "\n")
                out.write(CONTRIBUTOR_PREFIX + contributor + "\n")
                out.write(SEPARATOR + "\n")
                out.write(snippet + "\n")
        except OSError as e:
            print("{}".format(e))
            print("Writing snippet '{}' failed; skipping".format(name))
    except (ValueError, IndexError) as e:
        print("{}".format(e))
        print("Line '{}' incorrectly formatted; skipping.".format(snippet_def))

print("Snippet writing finished!")

