#!/usr/bin/env python3


from sys import stderr
from typing import List
import time
import sys
import datetime

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options as ChromeOptions

from rules import Tile, Board, all_correct
from runner import Runner
from web import WebInteract


SCREENSHOT_DIR = 'screenshots'


def get_date() -> str:
    now = datetime.datetime.now()
    return now.strftime('%F')  # ISO 8601 date format


def autosolve(weber: WebInteract, runner: Runner) -> Board:

    tiles: Board = []
    banned: List[str] = []

    i: int = 0
    while i < 6:

        print(f'{runner.get_cli_command(tiles)}')

        word: str = runner.next_guess(tiles, banned)
        ret = weber.send_word(i, word)

        if ret is None:
            banned.append(word)
            weber.delete_word()
            continue

        # show(ret)

        tiles.append(ret)

        if all_correct(ret):
            break

        i += 1

    weber.screenshot(f'{SCREENSHOT_DIR}/screenshot-{get_date()}')
    return tiles


def main() -> int:

    if len(sys.argv) < 3:
        print(f'usage: {sys.argv[0]} DRIVER_PATH SOLVER_PATH', file=stderr)
        return 1

    DRIVER_PATH   = sys.argv[1]
    SOLVER_PATH   = sys.argv[2]
    URL           = "https://www.nytimes.com/games/wordle/index.html"

    # “Weber” is a german name, apparently
    weber = WebInteract(URL, DRIVER_PATH)

    runner = Runner(SOLVER_PATH)

    print(weber.get_title())

    weber.close_overlays()

    tiles = autosolve(weber, runner)

    time.sleep(1.5)

    print()
    print(weber.get_score())

    weber.quit()

    return 0


if __name__ == '__main__':
    sys.exit(main())



