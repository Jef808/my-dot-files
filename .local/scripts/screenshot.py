#!/usr/bin/env python

"""Take a screenshot of currently active window."""

import subprocess
from uuid import uuid4

TMP_DIRECTORY = "/tmp/"
MAX_NUM_PIXELS = 1_200_000


def get_active_window_id() -> str:
    """Use `xprop` to get the id of the active window."""
    xprop_res = subprocess.check_output(
        ["xprop", "-root", "_NET_ACTIVE_WINDOW"],
        text=True
    )
    window_id = xprop_res.rsplit(' ', maxsplit=1)[-1]
    return window_id


def get_screenshot_geometry(filepath: str):
    """Use ImageMagick to get the image's dimensions."""
    identify_res = subprocess.check_output(
        ["identify", filepath],
        text=True
    )
    geometry = identify_res.split(' ', maxsplit=3)[2]
    return list(map(int, geometry.split('x')))


def format_screenshot(xwd_file: str) -> str:
    """Use ImageMagick to convert the image to png.

    The image is also resized, preserving the aspect ratio,
    so that the resulting png image has at most `MAX_NUM_PIXELS`
    pixels in total.
    Return the path to the png file.
    """
    png_file = xwd_file.rsplit('.', maxsplit=1)[0] + ".png"
    width, height = get_screenshot_geometry(xwd_file)
    num_pixels = width * height
    resize_params = []
    if num_pixels > MAX_NUM_PIXELS:
        ratio = (MAX_NUM_PIXELS / num_pixels) ** 0.5
        resize_params.extend(["-resize", str(int(ratio * width))])
    subprocess.run(
        ["magick", xwd_file] + resize_params + [png_file],
        check=True
    )
    return png_file


def unique_filename(filename: str, extension: str) -> str:
    """Add a timestamp to a filename."""
    uuid = uuid4()
    return f"{filename}_{uuid.hex}.{extension}"


def take_screenshot() -> str:
    """Take a screenshot of the active window.

    Return the path to the png file created.
    """
    xwd_file = TMP_DIRECTORY + unique_filename("ss", "xwd")
    active_window_id = get_active_window_id()
    subprocess.run(
        ["xwd", "-id", active_window_id, "-out", xwd_file],
        check=True
    )
    png_file = format_screenshot(xwd_file)
    subprocess.run(
        ["rm", xwd_file],
        check=False
    )
    return png_file


if __name__ == "__main__":
    print(take_screenshot())
