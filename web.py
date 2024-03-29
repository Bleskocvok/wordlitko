from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.firefox.service import Service as FirefoxService
from selenium.webdriver.firefox.options import Options as FirefoxOptions
from selenium.webdriver.chrome.service import Service as ChromeService
from selenium.webdriver.chrome.options import Options as ChromeOptions


from typing import List, Optional
from sys import stderr
import time

from rules import Clue, Tile, WORD_LENGTH


class WebInteract:
    FIREFOX: int = 0
    CHROME: int = 0

    def __init__(self, url: str, which: int, driver_path: str ):

        if which == WebInteract.CHROME:
            service = ChromeService(executable_path=driver_path)
            options = ChromeOptions()
            options.add_argument("--headless");
            options.add_argument("--disable-gpu");
            options.add_argument('--no-sandbox')
            options.headless = True
            self.driver = webdriver.Chrome(service=service, options=options)
        elif which == WebInteract.FIREFOX:
            service = FirefoxService(executable_path=driver_path)
            options = FirefoxOptions()
            options.headless = True
            self.driver = webdriver.Firefox(service=service, options=options)
        else:
            raise RuntimeError(f"Unsupported which={which}")

        self.driver.get(url)

        self.body = self.driver.find_element(By.TAG_NAME, "body")


    def quit(self) -> None:
        self.driver.quit()


    def get_title(self) -> str:
        return self.driver.title


    def close_overlays(self) -> None:

        time.sleep(2)

        # first one seems to disappear after the first container clicks it

        # accept cookies and trackers
        # idk
        # idk
        overlays = [
            '//*[@id="pz-gdpr-btn-accept"]',
            '/html/body/div/div/div/div/div/div[2]/button[2]',
            '/html/body/div/div/dialog/div/button'
        ]

        for xpath in overlays:
            self.driver.implicitly_wait(0.5)
            try:
                self.driver .find_element(By.XPATH, xpath).click()
            except Exception as e:
                print(f'Warning: {e}', file=stderr)

        # close help overlay
        self.body.click()


    def delete_word(self) -> None:
        for _ in range(5):
            self.body.send_keys(Keys.BACKSPACE)


    def get_score(self) -> str:
        try:
            import tkinter as tk

            # start the thing for the clipboard thing
            root = tk.Tk()
            root.withdraw()
            root.clipboard_clear()
            root.clipboard_append('[EMPTY CLIPBOARD]')

            share = self.driver  \
                    .find_element(By.XPATH,
                        '/html/body/div/div/dialog/div/div/div[3]/div[2]/div/button')
            share.click()

            # save clipboard contents to a variable
            value = root.clipboard_get()
            return value
        except:
            return None


    def screenshot(self, filename: str) -> None:
        self.driver.save_screenshot(filename + '.png')


    def send_word(self, idx: int, word: str) -> Optional[List[Tile]]:

        self.body.send_keys(word + "\n")
        self.body.send_keys(Keys.RETURN)

        self.driver.implicitly_wait(2.5)

        time.sleep(2.4)

        tiles = self.driver  \
                .find_element(By.XPATH,
                    f'/html/body/div/div/div[4]/main/div[1]/div/div[{idx + 1}]')

        if not tiles:
            raise RuntimeError('Tile cells elements not found')

        self.driver.implicitly_wait(1.0)

        res: List[Tile] = []

        for i in range(WORD_LENGTH):
            cell   = tiles.find_element(By.XPATH, f'div[{i + 1}]/div')
            value  = cell.get_attribute('data-state')
            letter = cell.text

            if value is None:           return None
            elif value == 'absent':     res.append(Tile(letter, Clue.GRAY))
            elif value == 'present':    res.append(Tile(letter, Clue.YELLOW))
            elif value == 'correct':    res.append(Tile(letter, Clue.GREEN))
            elif value == 'empty':      return None
            else:                       raise RuntimeError(f'unexpected value "{value}"')

        return res
