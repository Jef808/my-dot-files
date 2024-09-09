#!/usr/bin/env python

"""Take a screenshot of currently active window."""

import json
import base64
import anthropic
import subprocess
from uuid import uuid4
import anthropic

TMP_DIRECTORY = "/tmp/"
MAX_NUM_PIXELS = 1_200_000

INSTRUCTIONS = """You are world-renowned programming assistant.
You will be given a the screenshot of a user's active window with a written task or question.
Use the screenshot to deduce the user's current activity and intent for best executing their task or answering their question.
"""


def get_active_window_name() -> str:
    """Use `xprop` to get the name of the active window."""
    xprop_res = subprocess.check_output(
        ["xprop", "-root", "_NET_ACTIVE_WINDOW"],
        text=True
    )
    window_id = xprop_res.rsplit(' ', maxsplit=1)[-1]
    xprop_res = subprocess.check_output(
        ["xprop", "-id", window_id, "WM_NAME"],
        text=True
    )
    window_name = xprop_res.split(' = ', maxsplit=1)[-1].replace('"', '').rstrip()
    return window_name


def get_active_window_id() -> str:
    """Use `xprop` to get the name of the active window."""
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
    """Take a screenshot of the """
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


def to_base64(binary_file) -> str:
    with open(binary_file, 'rb') as f:
        data = f.read()
    b64 = base64.b64encode(data).decode("utf-8")
    return b64


def anthropic_api_key() -> str:
    """Retrieve the anthropic api key from the password store."""
    pass_output = subprocess.check_output(
        ["pass", "anthropic/api_key"],
        text=True
    ).rstrip()
    if not pass_output:
        raise Exception("Failed to retrieve api key")

    return pass_output


def make_prompt(
        instructions: str,
        image_data: str,
        image_media_type: str,
        text_content: str
) -> dict:
    client = anthropic.Anthropic(api_key=anthropic_api_key())
    message = client.messages.create(
        model="claude-3-5-sonnet-20240620",
        max_tokens=1024,
        temperature=0.7,
        system=instructions,
        messages=[
            {
                "role": "user",
                "content": [
                    {
                        "type": "image",
                        "source": {
                            "type": "base64",
                            "media_type": image_media_type,
                            "data": image_data
                        }
                    },
                    {
                        "type": "text",
                        "text": text_content
                    }
                ]
            }
        ]
    )
    return message


if __name__ == "__main__":
    import sys

    prompt = ' '.join(sys.argv[1:])

    ss_png = take_screenshot()
    subprocess.Popen(["rm", ss_png])

    ss_b64 = to_base64(ss_png)
    response = make_prompt(
        instructions=INSTRUCTIONS,
        image_data=ss_b64,
        image_media_type="image/png",
        text_content=prompt,
    )

    out_json = f"{TMP_DIRECTORY}response_{uuid4()}.json"

    with open(out_json, 'w+') as f:
        f.write(response.json())

    print(response.content[0].text)
