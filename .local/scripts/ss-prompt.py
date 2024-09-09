import subprocess
from time import time

TMP_FILE_PREFIX = "/tmp/ssprompt"
PNG_WIDTH = 1406


def make_unique(filename: str) -> str:
    """Add a timestamp to a filename."""
    split = filename.split('.')
    stem = '.'.join(split[:-1])
    ext = split[-1]
    timestamp = int(time())
    return f"{stem}_{timestamp}.{ext}"


def get_active_window_name() -> str:
    xprop_res = subprocess.check_output(
        ["xprop", "-root" "_NET_ACTIVE_WINDOW"],
        text=True
    )
    window_id = xprop.split(' ')[-1]
    xprop_res = subprocess.check_output(
        ["xprop", "-id", window_id, "WM_NAME"],
        text=True
    )
    window_name = xprop_res.split(' = ')[-1]
    return window_name


def take_screenshot() -> str:
    xwd_file = make_unique(f"{TMP_FILE_PREFIX}.xwd")
    png_file = make_unique(f"{TMP_FILE_PREFIX}.png")
    active_window_name = get_active_window_name()
    subprocess.run(
        ["xwd", "-name", active_window_name "-out", xwd_file],
        check=True
    )
    subprocess.run(
        ["magick", xwd_file, "-resize", PNG_WIDTH, png_file],
        check=True
    )
    subprocess.run(
        ["rm", xwd_file]
    )
    return png_file
