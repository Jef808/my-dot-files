#!/usr/bin/env python

import time
import subprocess
from pathlib import Path
import notify2


def open_json_file(filepath):
    subprocess.Popen([
        "emacsclient",
        "-e",
        f"'(find-file {str(filepath)}) (mark-whole-buffer) (json-mode-beutify)'"
    ])

def poll_directory(directory, filename, interval_s=1, timeout_s=30):
    notify2.init('File Watcher')

    now = time.time()

    while True:
        file_path = Path(directory) / filename
        if file_path.exists():
            notification = notify2.Notification(
                "File found",
                f"The file '{filename}' has been created in '{directory}'."
            )
            notification.add_action(
                "open",
                "Open File",
                open_json_file,
                str(file_path)
            )
            notification.show()
            break
        if time.time() - now > timeout_s:
            break

        time.sleep(interval_s)


if __name__ == "__main__":
    import sys
    directory_to_watch = sys.argv[1]
    file_to_watch = sys.argv[2]
    poll_directory(directory_to_watch, file_to_watch)
