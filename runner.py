
import subprocess
import random
from typing import List, Optional

from rules import Board, to_argument



class Runner:

    def __init__(self,
                 solver_path: str,
                 data_path: str,
                 cache: Optional[List[str]] = None):

        self.solver_path = solver_path
        self.data_path = data_path

        # temporary workaround to make repeated execution much faster
        self.cache = cache


    def get_cli_command(self, tiles: Board) -> str:
        arg: str = to_argument(tiles)
        return f"{self.solver_path} '{arg}'"


    def popen(self, arg: str = '') -> subprocess.Popen:
        return subprocess.Popen([self.solver_path, arg, self.data_path],
                                 stdout=subprocess.PIPE)


    def read_all_lines(self) -> List[str]:
        if self.cache is not None:
            return self.cache
        solver = self.popen()
        words = solver.stdout.readlines()
        solver.kill()
        self.cache = words
        return words


    def next_guess(self,
                   tiles: Board,
                   banned: Optional[List[str]] = None) -> str:

        decode = lambda s: s.decode('utf-8')    \
                            .replace('\n', '')  \
                            .replace('\r', '')  # cause of CRLF

        if banned is None:
            banned = []

        if len(banned) == 0 and len(tiles) == 0:
            return decode(self.read_all_lines()[0])

        arg: str = to_argument(tiles)

        solver = self.popen(arg)

        nxt = lambda: decode(solver.stdout.readline())

        word = nxt()
        while word in banned:
            if solver.stdout:
                word = nxt()
            else:
                word = ''
        solver.kill()

        if len(word) != 5:
            word = decode(random.choice(self.read_all_lines()))

        return word

