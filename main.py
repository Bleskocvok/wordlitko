#!/usr/bin/env python3


from sys import stderr
from typing import List
import time
import sys
import os
import datetime

from rules import Board, all_correct, show, word
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
        print(f'usage: {sys.argv[0]} SOLVER_PATH DATABASE_PATH [-v]',
                file=stderr)
        return 1

    start = time.time()

    SOLVER_PATH   = sys.argv[1]
    DATABASE_PATH = sys.argv[2]
    SHOW_GUESSES  = len(sys.argv) > 3 and sys.argv[3] == '-v'
    URL           = "https://www.nytimes.com/games/wordle/index.html"

    DRIVER = os.getenv("DRIVER")
    CHROME_DRIVER = os.getenv("CHROME_DRIVER")
    FIREFOX_DRIVER = os.getenv("FIREFOX_DRIVER")

    print(f'DRIVER = {DRIVER}', file=stderr)
    print(f'CHROME_DRIVER = {CHROME_DRIVER}', file=stderr)
    print(f'FIREFOX_DRIVER = {FIREFOX_DRIVER}', file=stderr)

    # “Weber” is a german name, apparently
    if DRIVER == "chrome".casefold():
        weber = WebInteract(URL, WebInteract.CHROME, CHROME_DRIVER)
    else:
        weber = WebInteract(URL, WebInteract.FIREFOX, FIREFOX_DRIVER)

    runner = Runner(SOLVER_PATH, DATABASE_PATH)

#     print(weber.get_title())

    try:
        weber.close_overlays()
    except Exception as e:
            print(f'Warning: {e}', file=stderr)

    tiles = autosolve(weber, runner)

    end = time.time()

    time.sleep(1.5)

    print('\nBotle {}/6 (time {:.2f}s)\n'
            .format(len(tiles), end - start))

    for row in tiles:
        if SHOW_GUESSES:
            print(f'{show(row)} ||{word(row)}||')
        else:
            print(f'{show(row)}')

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
