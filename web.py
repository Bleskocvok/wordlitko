
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options as ChromeOptions

from typing import List, Optional
import tkinter as tk
import time

from rules import Clue, Tile, WORD_LENGTH


class WebInteract:


    def __init__(self, url: str, driver_path: str):

        service = Service(executable_path=driver_path)
        options = ChromeOptions()
        self.driver = webdriver.Chrome(service=service, options=options)

        self.driver.get(url)

        self.body = self.driver.find_element(By.TAG_NAME, "body")


    def quit(self) -> None:
        self.driver.quit()


    def get_title(self) -> str:
        return self.driver.title


    def close_overlays(self) -> None:

        # accept cookies
        cock = self.driver  \
               .find_element(By.XPATH, '//*[@id="pz-gdpr-btn-accept"]')
        cock.click()

        self.driver.implicitly_wait(0.5)

        # close help overlay
        self.body.click()


    def delete_word(self) -> None:
        for _ in range(5):
            self.body.send_keys(Keys.BACKSPACE)


    def get_score(self) -> str:
    
        share = self.driver  \
                .find_element(By.XPATH, '//*[@id="share-button"]')
        share.click()

        # save clipboard contents to a variable
        root = tk.Tk()
        root.withdraw()
        value = root.clipboard_get()
        return value


    def screenshot(self, filename: str) -> None:
        self.driver.save_screenshot(filename + '.png')


    def send_word(self, idx: int, word: str) -> Optional[List[Tile]]:

        self.body.send_keys(word + "\n")

        self.driver.implicitly_wait(2.5)

        time.sleep(2.4)

        tiles = self.driver  \
                .find_element(By.XPATH, f'/html/body/div/div[1]/div/div[{idx + 1}]')

        if not tiles:
            raise RuntimeError('Tile cells elements not found')

        res: List[Tile] = []

        for i in range(WORD_LENGTH):
            cell   = tiles.find_element(By.XPATH, f'div[{i + 1}]/div')
            value  = cell.get_attribute('data-state')
            letter = cell.text

            if value is None:           return None
            elif value == 'absent':     res.append(Tile(letter, Clue.GRAY))
            elif value == 'present':    res.append(Tile(letter, Clue.YELLOW))
            elif value == 'correct':    res.append(Tile(letter, Clue.GREEN))
            else:                       raise RuntimeError('invalid value')

        return res
