#!/usr/bin/env python3


import time
from typing import List


from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options as ChromeOptions


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
body.send_keys("weary\n")

driver.implicitly_wait(2.5)

time.sleep(2.4)

tiles = driver.find_element(By.XPATH, '/html/body/game-app')  \
              .shadow_root  \
              .find_element(By.CSS_SELECTOR, 'div div div game-row')  \
              .shadow_root \
              .find_element(By.CSS_SELECTOR, 'div')  \
              .find_elements(By.CSS_SELECTOR, 'game-tile')

for tile in tiles:
    letter = tile.get_attribute('letter')
    value  = tile.get_attribute('evaluation')
    print(f"{letter} : {value}")

time.sleep(2.4)

driver.quit()

