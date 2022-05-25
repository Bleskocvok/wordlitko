
import subprocess
import random
from typing import List, Optional

from rules import Board, to_argument



class Runner:

    def __init__(self, solver_path: str):
        self.solver_path = solver_path


    def get_cli_command(self, tiles: Board) -> str:
        arg: str = to_argument(tiles)
        return f'{self.solver_path} {arg}'


    def popen(self, arg: str) -> subprocess.Popen:
        return subprocess.Popen([f"{self.solver_path}", arg],
                                  stdout=subprocess.PIPE)


    def read_all_lines(self) -> List[str]:
        solver = self.popen('')
        words = solver.stdout.readlines()
        solver.kill()
        return words


    def next_guess(self,
                tiles: Board,
                banned: Optional[List[str]] = None) -> str:

        if banned is None:
            banned = []

        arg: str = to_argument(tiles)

        solver = self.popen(arg)

        decode = lambda s: s.decode('utf-8')    \
                            .replace('\n', '')  \
                            .replace('\r', '')  # cause of CRLF

        nxt = lambda: decode(solver.stdout.readline())

        word = nxt()
        while word in banned:
            if solver.stdout:
                word = nxt()
            else:
                word = ''
        solver.kill()

        if len(word) != 5:
            word = decode(self.read_all_lines())

        return word

