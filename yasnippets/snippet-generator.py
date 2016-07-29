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
- [ ] option to clear target folder contents
- [ ] option to process/not process escape characters in source file
"""

import re
import sys
import os
import logging

logging.basicConfig(level=logging.DEBUG)

COMMENT_CHAR = "#"
SPLIT_STR = ";;"

EMACS_SNIPPET_MODE = "# -*- mode: snippet -*-"
NAME_PREFIX = "# name: "
KEY_PREFIX = "# key: "
CONTRIBUTOR_PREFIX = "# contributor: "
contributor = "Ryan Matlock <ryan.matlock@gmail.com>"
SEPARATOR = "# --"

# this was causing your unicode bugs I think
# # make this an option eventually
# # the idea is that you can put the string "\n" in your snippet definition file
# # and get a newline out of it
# parse_escapes = True
# make tab-related stuff parameters you can pass, too
TAB_WIDTH = 4
TAB = " " * TAB_WIDTH

# see http://stackoverflow.com/questions/24917942/python-unexpected-behavior-with-printing-writing-escape-characters
# for advice on how to proceed
# def escape_parser(snippet):
#     # the only ones you need to escape are backslash, tab, and newline
#     # i.e. \\, \t, and \n
#     # this is probably not what actually happens now, though
#     # print("pre-escaping: '{}'".format(snippet))

#     # snippet = re.sub(r"\\", r"\\", snippet)
#     # snippet = re.sub(r"\t", "\t", snippet)
#     # snippet = re.sub(r"\n", "\n", snippet)
#     snippet = bytes(snippet, "utf-8").decode("unicode_escape")

#     # print("post-escaping: '{}'".format(snippet))

#     return snippet

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
        with open(source, "r", encoding="utf8") as f:
            for line in f:
                logging.debug("line: '{}'".format(line))
                if line:
                    if line[0] == COMMENT_CHAR or line[0] == '\n':
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
    logging.debug("snippet_def: '{}'".format(snippet_def))
    try:
        name, key, snippet = snippet_def.split(SPLIT_STR)
        logging

        # if parse_escapes:
        #     snippet = escape_parser(snippet)
            
        try:
            with open(os.path.join(target_path, name),
                      "w",
                      encoding="utf8") as out:
                out.write(EMACS_SNIPPET_MODE + "\n")
                out.write(NAME_PREFIX + name + "\n")
                out.write(KEY_PREFIX + key + "\n")
                out.write(CONTRIBUTOR_PREFIX + contributor + "\n")
                out.write(SEPARATOR + "\n")
                out.write(snippet)
        except OSError as e:
            print("{}".format(e))
            print("Writing snippet '{}' failed; skipping".format(name))
    except (ValueError, IndexError) as e:
        print("{}".format(e))
        print("Line '{}' incorrectly formatted; skipping.".format(snippet_def))

print("Snippet writing finished!")
