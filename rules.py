
from enum import Enum
from dataclasses import dataclass
from typing import List, Optional, Dict
from functools import reduce



class Clue(Enum):
    GRAY   = '⬛'
    YELLOW = '🟨'
    GREEN  = '🟩'



@dataclass
class Tile:
    char: str
    clue: Clue



Board = List[List[Tile]]



WORD_LENGTH: int = 5


def clue_bg(clue: Clue) -> str:
    if clue == Clue.GRAY:   return "\033[100m"
    if clue == Clue.YELLOW: return "\033[43m"
    if clue == Clue.GREEN:  return "\033[42m"


def print_term_tile(tile: Tile):
    print(end='\033[30;1m')
    print(end=clue_bg(tile.clue))
    print(end=f' {tile.char.upper()} ')
    print(end='\033[0m')


class Simulator:

    def __init__(self, allowed: List[str], word: Optional[str] = None):
        self.word = word
        self.allowed = allowed
        self.counts = {}
        if word is not None:
            self.reset(word)


    def reset(self, word: str) -> None:
        self.word = word
        self.counts: Dict[str, int] = {}
        for c in self.word:
            self.counts[c] = self.counts.get(c, 0) + 1


    def guess(self, guessed: str) -> Optional[List[Tile]]:

        if guessed not in self.allowed:
            return None

        res: List[Tile] = []

        for c, g in zip(self.word, guessed):

            clue = None

            if c == g:                      clue = Clue.GREEN
            elif self.word.find(g) != -1:   clue = Clue.YELLOW
            else:                           clue = Clue.GRAY

            res.append(Tile(g, clue))

        greens = lambda c: \
            reduce(lambda s, x: s + (x.char == c and x.clue == Clue.GREEN), res, 0)

        for c in guessed:
            count = self.counts.get(c, 0)
            gs = greens(c)
            ith = 0
            for tile in filter(lambda x: x.char == c and x.clue == Clue.YELLOW, res):
                ith += 1
                if ith > count - gs:
                    tile.clue = Clue.GRAY

        return res


def show(result: List[Tile]) -> None:
    for el in result:
        print(el.clue.value, end='')
    print()


def all_correct(result: List[Tile]) -> bool:
    return all(map(lambda x: x.clue == Clue.GREEN, result))


def to_argument(tiles: Board) -> str:
    arg: str = ''
    for i in range(5):
        for tile in tiles:
            if tile[i].clue == Clue.YELLOW:
                arg += '^'
            elif tile[i].clue == Clue.GRAY:
                arg += '!'
            arg += tile[i].char
        arg += '.'
    return arg

