#!/usr/bin/env python3


from sys import stderr
import time
from typing import List, Optional
from enum import Enum


from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options as ChromeOptions


class Tile(Enum):
    GRAY   = '⬛'
    YELLOW = '🟨'
    GREEN  = '🟩'


def send_word(driver, body, word: str) -> Optional[List[Tile]]:
    body.send_keys(word + "\n")

    driver.implicitly_wait(2.5)

    time.sleep(2.4)

    tiles = driver.find_element(By.XPATH, '/html/body/game-app')  \
                  .shadow_root  \
                  .find_element(By.CSS_SELECTOR, 'div div div game-row')  \
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
            res.append(Tile.GRAY)
        elif value == 'present':
            res.append(Tile.YELLOW)
        elif value == 'correct':
            res.append(Tile.GREEN)
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


def show(result: Optional[List[Tile]]) -> None:
    for el in result:
        print(el.value, end='')
    print()


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

a = send_word(driver, body, "aeons")
show(a)

b = send_word(driver, body, "weary")
show(b)


time.sleep(2.4)

driver.quit()

