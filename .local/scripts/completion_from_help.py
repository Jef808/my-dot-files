#!/usr/bin/env python3

import subprocess
import re
import sys
from pathlib import Path


file_header = "#compdef free\n_arguments -s \\"

help_pat = re.compile(r"""
 (?P<short>(?<=-)[a-zA-Z](?:\s\w)?)      # Short form of option
 \s*                                     # Discard optional comma and whitespace
 (?P<long>(?<=--)[\w=]+(?:\s\w)?)        # Long form of option
""", re.VERBOSE)

pat_optshort = re.compile(r"""
 (?:\s|^)(-|+             # Prefix of '-' or '+' after a space or at start of string
 (?:[^\s,]|[\s,](?=\S))+) # Anything until double space or comma+space
""", re.VERBOSE)

pat_optlong = re.compile(r"""
 (?:^|\s{2,}|,\s)--     # Prefix of '--' starting string, after multiple space or after comma+space
 (\S)[,\s{2,}]          # Anything until double space or comma+space
""", re.VERBOSE)

# \s*                                 # Discard whitespace
# ,?\s*                         # Discard whitespace, any comma
# (?P<desc>\b\w+\b)$         # Description of option

def cli_help(cmd):
    return subprocess.run([cmd, "--help"], capture_output=True, text=True).stdout

def parsed(option):
    opts_desc = list(map(lambda s: s.strip(), option.strip().split('  ')))
    short_aliases = [opt for opt in opts_desc[0] if opt.startswith('-')]
    long_aliases = [opt for opt in opts_desc[0] if opt.startswith('--')]
    return {'shorts': short_aliases, 'longs': long_aliases, 'desc': opts_desc[1]}
    #return f"  '({m['short']} {m['long']})'{{{m['short']},{m['long']}}}'[{m['desc']}]'"

def is_single_line_opt(line):
    print("\nTesting", line, " ...")
    if line.isspace():
        print("  DISCARDING:", "Only whitespace")
        return False
    linestrip = line.strip()
    opts_desc = re.findall(r"[(?:\S(?!\s{2,}))]+", line)   # Segments with at most one consecutive space in them
    #ss = linestrip.find('  ')
    #opts_desc = list(map(lambda part: part.strip(), (linestrip[:ss], linestrip[ss:])))
    if (len(opts_desc) != 2):
        print("  DISCARDING:", f"found {len(opts_desc)} substrings containing " )
    re.findall(pat_optshort, opts_desc[0])
    # short_opts = re.findall("(?:^-|\s-)[a-zA-Z](?:\s\S+)?", opts_desc[0])
    # long_opts = re.findall("(?:^--|\s--)\S+(?:\s\S+)?", opts_desc[0])
    print("Short:", short_opts)
    print("Long:", long_opts)
    # res = True
    # for opt in opts:
    #     if not re.match("^-|^--", opt.strip()):
    #         print("  DISCARDING:", f"{opt} does not start with '-' or '--'")
    #         res = False
    # return res


def formatted_cli_help(cmd):
    for line in (l for l in cli_help(cmd).split('\n') if is_single_line_opt(l)):
        print(parsed(line))
        #yield formatted(line)

def line_sep():
    return " \\\n"

def make_output_path(cmd : str, path : Path):
    return Path(path) / f"_{cmd.capitalize()}"

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"USAGE: {sys.argv[0]} COMMAND", file=sys.stderr)
        sys.exit(1)
    command = sys.argv[1]
    output_dir = Path.cwd() if len(sys.argv) == 2 else Path(sys.argv[2])
    print(f"output_dir: {output_dir}")
    formatted_cli_help(command)
    # with open(make_output_path(command, output_dir), "w") as writer:
    #     writer.write(file_header)
    #     for formatted_opt in formatted_cli_help(command):
    #         writer.write(line_sep())
    #         writer.write(formatted_opt)
    #     writer.write('\n')
