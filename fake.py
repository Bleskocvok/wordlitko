#!/usr/bin/env python3

from sys import stderr
import http.client
import sys
import time

from evaluation import solve, get_file
from rules import Simulator, show, word as show_word
from runner import Runner


def make_request(root: str, path: str) -> str:
    con = http.client.HTTPSConnection(root)
    con.request("GET", path)
    resp = con.getresponse()
    if resp.status != 200:
        con.close()
        raise RuntimeError(f"{resp.status} {resp.reason}")
    res = resp.read()
    con.close()
    return res


def run(url: str, solver: str, words_path: str,
        show_guesses: bool = False) -> int:
    try:
        delim = url.find("/")
        root = url[:delim]
        path = url[delim:]
        content = make_request(root, path)
    except Exception as ex:
        raise RuntimeError("request", ex)

    word = content.decode("utf-8")

    if len(word) != 5:
        raise RuntimeError(f"word not five characters '{word}'")

    word = word.lower()

    start = time.time()

    possible = get_file(words_path)
    SHOW_GUESSES = show_guesses

    runner = Runner(solver, words_path)
    sim = Simulator(possible)

    tries, tiles = solve(word, sim, runner)

    end = time.time()

    print('Fakele {}/6 (time {:.3f}s)\n'
            .format(len(tiles), end - start))

    for row in tiles:
        if SHOW_GUESSES:
            print(f'{show(row)} ||{show_word(row)}||')
        else:
            print(f'{show(row)}')


def main(args) -> int:

    try:
        run(args[1], args[2], args[3], len(args) > 4 and args[4] == "v")
        return 0
    except Exception as e:
        print(f'ERROR: ({type(e)}) {e}', file=stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main(sys.argv))
