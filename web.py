
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options as ChromeOptions

from typing import List, Optional
import tkinter as tk
import time

from rules import Clue, Tile



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
        cock = self.driver.find_element(By.XPATH, '//*[@id="pz-gdpr-btn-accept"]')
        cock.click()

        self.driver.implicitly_wait(0.5)

        # close help overlay
        self.body.click()


    def delete_word(self) -> None:
        for _ in range(5):
            self.body.send_keys(Keys.BACKSPACE)


    def get_score(self) -> str:
        share = self.driver  \
                .find_element(By.XPATH, '/html/body/game-app')  \
                .shadow_root  \
                .find_element(By.CSS_SELECTOR, 'game-theme-manager div#game game-modal')  \
                .find_element(By.CSS_SELECTOR, 'game-stats')  \
                .shadow_root  \
                .find_element(By.CSS_SELECTOR, 'div.container') \
                .find_element(By.CSS_SELECTOR, 'div.footer')  \
                .find_element(By.CSS_SELECTOR, 'div.share')  \
                .find_element(By.CSS_SELECTOR, 'button')
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

        tiles = self.driver.find_element(By.XPATH, '/html/body/game-app')  \
                    .shadow_root  \
                    .find_elements(By.CSS_SELECTOR,'game-theme-manager '
                                                    + 'div '
                                                    + 'div '
                                                    + 'div '
                                                    + 'game-row')[idx]  \
                    .shadow_root  \
                    .find_element(By.CSS_SELECTOR, 'div')  \
                    .find_elements(By.CSS_SELECTOR, 'game-tile')

        res: List[Tile] = []

        for tile in tiles:
            letter = tile.get_attribute('letter')
            value  = tile.get_attribute('evaluation')

            if value is None:           return None
            elif value == 'absent':     res.append(Tile(letter, Clue.GRAY))
            elif value == 'present':    res.append(Tile(letter, Clue.YELLOW))
            elif value == 'correct':    res.append(Tile(letter, Clue.GREEN))
            else:                       raise RuntimeError('invalid value')

        return res
