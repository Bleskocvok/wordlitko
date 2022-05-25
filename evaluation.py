#!/usr/bin/env python3


from sys import stderr
from typing import List, Optional, Tuple
import time
import sys
import datetime

from rules import Simulator, Tile, Board, all_correct, show
from runner import Runner



def solve(word: str, sim: Simulator, runner: Runner) -> Tuple[int, Board]:

    sim.reset(word)

    tiles: Board = []
    banned: List[str] = []

    i: int = 0
    while i < 6:

        # print(f'{runner.get_cli_command(tiles)}')

        guess: str = runner.next_guess(tiles, banned)

        # print(f'guess: {guess}')

        ret: Optional[List[Tile]] = sim.guess(guess)

        if ret is None:
            banned.append(guess)
            continue

        tiles.append(ret)

        if all_correct(ret):
            return (i, tiles)

        i += 1

    return (-1, tiles)


def get_file(filename) -> List[str]:
    lines = []
    with open(filename, mode='r') as file:
        lines = file.readlines()

    lines = map(lambda line: line.replace('\n', '')  \
                                 .replace('\r', '')  \
                                 .lower(), lines)
    return list(filter(lambda l: len(l) == 5, lines))


class Stats:
    def __init__(self):
        self.data = [0 for _ in range(7)]

    def record(self, i: int) -> None:
        self.data[i + 1] += 1
    
    def __str__(self) -> str:
        res = ''
        for i in range(len(self.data)):
            idx = (i + 1) % len(self.data)
            count = self.data[idx]
            cat = idx if idx != 0 else 'N'
            res += f'{cat} / 6: {str(count).rjust(5)}\n'
        return res


def main() -> int:

    if len(sys.argv) < 4:
        print(f'usage: {sys.argv[0]} ANSWERS_PATH WORDS_PATH SOLVER_PATH',
              file=stderr)
        return 1

    ANSWERS_PATH = sys.argv[1]
    WORDS_PATH = sys.argv[2]
    SOLVER_PATH = sys.argv[3]

    runner = Runner(SOLVER_PATH)

    answers = get_file(ANSWERS_PATH)
    possible = get_file(WORDS_PATH)

    sim = Simulator(possible)

    stats = Stats()

    print('...\n' * 16)

    for i, word in enumerate(answers):
        tries, tiles = solve(word, sim, runner)
        stats.record(tries)

        print('\033[18A')
        print(f'ROUND: {i + 1} / {len(answers)}')

        print(f'WORD:  {word}')
        for row in range(6):
            print('\033[K', end='')
            if row < len(tiles):
                for elem in tiles[row]:
                    print(f'{elem.clue.value}', end='')
            print(' ')

        print(f'STATS:')
        print(stats)

    return 0


if __name__ == '__main__':
    sys.exit(main())



