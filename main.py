#!/usr/bin/env python3


from sys import stderr
from typing import List, Optional
from enum import Enum
import subprocess
import time


from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options as ChromeOptions


class Clue(Enum):
    GRAY   = '⬛'
    YELLOW = '🟨'
    GREEN  = '🟩'


class Tile:
    def __init__(self, char: str, clue: Clue):
        self.char = char
        self.clue = clue


def send_word(driver, body, idx: int, word: str) -> Optional[List[Tile]]:
    body.send_keys(word + "\n")

    driver.implicitly_wait(2.5)

    time.sleep(2.4)

    tiles = driver.find_element(By.XPATH, '/html/body/game-app')  \
                  .shadow_root  \
                  .find_elements(By.CSS_SELECTOR, 'game-theme-manager div div div game-row')[idx]  \
                  .shadow_root  \
                  .find_element(By.CSS_SELECTOR, 'div')  \
                  .find_elements(By.CSS_SELECTOR, 'game-tile')

    res: List[Tile] = []

    for tile in tiles:
        letter = tile.get_attribute('letter')
        value  = tile.get_attribute('evaluation')

        if value is None:
            return None
        elif value == 'absent':
            res.append(Tile(letter, Clue.GRAY))
        elif value == 'present':
            res.append(Tile(letter, Clue.YELLOW))
        elif value == 'correct':
            res.append(Tile(letter, Clue.GREEN))
        else:
            print("invalid value", file=stderr)
            return None

        # match tile:
        #     case ['absent']:
        #         pass
        #     case ['present']:
        #         pass
        #     case ['correct']:
        #         pass
        #     case [None]:
        #         return None
        #     case other:
        #         return None

    return res


def show(result: List[Tile]) -> None:
    for el in result:
        print(el.clue.value, end='')
    print()


def all_correct(result: List[Tile]) -> bool:
    return all(map(lambda x: x.clue == Clue.GREEN, result))


def next_guess(tiles: List[List[Tile]],
               banned: Optional[List[str]] = None) -> str:
    if banned is None:
        banned = []

    arg: str = ''
    for i in range(5):
        for tile in tiles:
            if tile[i].clue == Clue.YELLOW:
                arg += '^'
            elif tile[i].clue == Clue.GRAY:
                arg += '!'
            arg += tile[i].char
        arg += '.'

    solve = subprocess.Popen(['./solver.hs', arg], stdout=subprocess.PIPE)
    word =  solve.stdout.readline().decode('utf-8').replace('\n', '')
    solve.kill()
    return word


service = Service(executable_path="./chromedriver")
options = ChromeOptions()
driver = webdriver.Chrome(service=service, options=options)


driver.get("https://www.nytimes.com/games/wordle/index.html")

print(driver.title)

# accept cockies
cock = driver.find_element(By.XPATH, '//*[@id="pz-gdpr-btn-accept"]')
cock.click()

driver.implicitly_wait(0.5)

body = driver.find_element(By.TAG_NAME, "body")
# close help overlay
body.click()


tiles: List[List[Tile]] = []
i: int = 0
while i < 5:

    word: str = next_guess(tiles)
    ret = send_word(driver, body, i, word)

    if ret is None:
        i -= 1
    else:
        tiles.append(ret)

    show(ret)

    if all_correct(ret):
        break

    i += 1


driver.save_screenshot('screen.png')

time.sleep(2.4)


driver.quit()

