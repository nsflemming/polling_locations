# Nathaniel Flemming 20 1 25

#import webdriver
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.wait import WebDriverWait #wait for page to load
from selenium.webdriver.support import expected_conditions as EC #check page has loaded
#import time
import time
import re #regex
import pandas as pd #data frames
import os

#### Functions
def remove_element(driver, xpath):
    blocking_element = driver.find_element(By.XPATH, xpath)
    driver.execute_script("arguments[0].style.visibility='hidden'", blocking_element)

#create browser
browser = webdriver.Firefox()
#control browser
browser.get('https://www.catholicdirectory.com/search_results?location_value=Pennsylvania%2C+USA&radius=20&adm_lvl_1_sn=PA&stateSearchLN=Pennsylvania&country_sn=US&location_type=administrative_area_level_1&stateSearch=PA&swlat=39.71979900602428&nelat=42.51416576510771&swlng=-80.51984850253436&nelng=-74.68956094709924&lat=41.2033216&lng=-77.1945247&faddress=Pennsylvania&place_id=ChIJieUyHiaALYgRPbQiUEchRsI')
time.sleep(1)
WebDriverWait(browser, 60).until(EC.presence_of_element_located((By.ID, 'btnToLoadMorePost')))
time.sleep(1)
#loop to load all churches
for page in range(0, 30):
    # click load more churches
    browser.find_element(By.ID, 'btnToLoadMorePost').click()
    ## wait for more churches to load
    WebDriverWait(browser, 60).until(EC.presence_of_element_located((By.ID, 'btnToLoadMorePost')))
    time.sleep(5)
    #scroll to button
    element = browser.find_element(By.ID, 'btnToLoadMorePost')
    browser.execute_script("return arguments[0].scrollIntoView();", element)
## get number of Churches on page
#WebDriverWait(browser, 60).until(EC.presence_of_element_located((By.XPATH, "//span[contains(@class,'h3 bold inline-block member-search-full-name')]")))
#num_churches = len(browser.find_elements(By.XPATH, "//span[contains(@class,'h3 bold inline-block member-search-full-name')]"))
#get church name
descriptions = [elem.text for elem in browser.find_elements(By.XPATH, "//span[contains(@class,'h3 bold inline-block member-search-full-name')]")]
# get church addresses
addresses = [elem.text for elem in browser.find_elements(By.XPATH, "//span[contains(@class,'small member-search-location rmargin rpad')]")]
# bind churches and addresses together
Churches=pd.DataFrame({'Description':descriptions, 'Address':addresses})
# split address into multiple columns
Churches[['City','State','ZIP']] = Churches['Address'].str.split(', ',expand=True)


# save dataframe to csv
os.chdir('C:\\Users\\natha\\Desktop\\Polling Places DiD\\data')
Churches.to_csv('Cath_Dir_PA_Churches_1_20_2025.csv', index=False)