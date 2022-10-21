#!/usr/bin/env python3

import subprocess
from pathlib import Path


def find_git_repos(root_dir):
    return (str(p.parent) for p in Path(root_dir).glob("*/**/.git"))


def list_dot_files():
    return [p.name for p in Path.home().glob(".*")]


def list_raw_repo_files(repo_directory):
    s_ret = subprocess.run(("git", "--git-dir=/home/jfa/.cfg", "ls-files"),
                           stdout=subprocess.PIPE,
                           text=True)
    return [f for f in s_ret.stdout.split('\n')]


def main():
    print('\n'.join(find_git_repos(Path.home())))


if __name__ == '__main__':
    main()
