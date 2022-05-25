
import subprocess
import random
from typing import List, Optional

from rules import Board, to_argument


SOLVER_PATH = None


def set_solver_path(path: str) -> None:
    global SOLVER_PATH
    SOLVER_PATH = path


def next_guess(tiles: Board,
               banned: Optional[List[str]] = None) -> str:
    if banned is None:
        banned = []

    arg: str = to_argument(tiles)

    print(f"{SOLVER_PATH} {arg}")

    solve = subprocess.Popen([f"{SOLVER_PATH}", arg], stdout=subprocess.PIPE)

    decode = lambda s: s.decode('utf-8')    \
                        .replace('\n', '')  \
                        .replace('\r', '')  # cause of CRLF

    nxt = lambda: decode(solve.stdout.readline())

    word  = nxt()
    while word in banned:
        if solve.stdout:
            word = nxt()
        else:
            word = ''
    solve.kill()

    if len(word) != 5:
        solve = subprocess.Popen([f"{SOLVER_PATH}", ''], stdout=subprocess.PIPE)
        words = solve.stdout.readlines()
        word = decode(random.choice(words))
        solve.kill()

    return word

