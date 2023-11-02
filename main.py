#!/usr/bin/env python3


from sys import stderr
from typing import List
import time
import sys
import os
import datetime

from rules import Board, all_correct, show
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

        print(f'{runner.get_cli_command(tiles)}', file=stderr)

        word: str = runner.next_guess(tiles, banned)
        ret = weber.send_word(i, word)

        if ret is None:
            banned.append(word)
            weber.delete_word()
            continue

#         show(ret)

        tiles.append(ret)

        if all_correct(ret):
            break

        i += 1

    weber.screenshot(f'{SCREENSHOT_DIR}/screenshot-{get_date()}')
    return tiles


def run() -> int:
    if len(sys.argv) < 3:
        print(f'usage: {sys.argv[0]} SOLVER_PATH DATABASE_PATH',
                file=stderr)
        return 1

    start = time.time()

    SOLVER_PATH   = sys.argv[1]
    DATABASE_PATH = sys.argv[2]
    URL           = "https://www.nytimes.com/games/wordle/index.html"

    # “Weber” is a german name, apparently
    if os.getenv("DRIVER") == "chrome":
        weber = WebInteract(URL, WebInteract.CHROME, os.getenv("CHROME_DRIVER"))
    else:
        weber = WebInteract(URL, WebInteract.FIREFOX, os.getenv("FIREFOX_DRIVER"))

    cache = None
    try:
        with open(".solver_cache", "rb") as f:
            cache = f.readlines()
    except IOError:
        cache = None

    if len(cache) == 0:
        raise RuntimeError("cache is empty")

    runner = Runner(SOLVER_PATH, DATABASE_PATH, cache)

#     print(weber.get_title())

    weber.close_overlays()

    tiles = autosolve(weber, runner)

    end = time.time()

    time.sleep(1.5)

    print('\nBotle {}/6 (time {:.2f}s)\n'
            .format(len(tiles), end - start))
    for row in tiles:
        show(row)

    weber.quit()

    return 0


def main() -> int:

    try:
        return run()
    except Exception as e:
        print(f'ERROR: ({type(e)}) {e}', file=stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())
