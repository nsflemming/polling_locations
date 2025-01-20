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
    time.sleep(2)
## get number of Churches on page
#WebDriverWait(browser, 60).until(EC.presence_of_element_located((By.XPATH, "//span[contains(@class,'h3 bold inline-block member-search-full-name')]")))
#num_churches = len(browser.find_elements(By.XPATH, "//span[contains(@class,'h3 bold inline-block member-search-full-name')]"))
#get church name
descriptions = [elem.text for elem in browser.find_elements(By.XPATH, "//span[contains(@class,'h3 bold inline-block member-search-full-name')]")]
# get church addresses
addresses = [elem.text for elem in browser.find_elements(By.XPATH, "//span[contains(@class,'small member-search-location rmargin rpad')]")]
# bind churches and addresses together
Churches=pd.DataFrame({'Description':descriptions, 'Address':addresses})





## Select roman catholic faith
browser.find_element(By.XPATH,"//select[@name='nf-field-794']/option[text()='Roman Catholic']").click()
time.sleep(1)
## Select PA
browser.find_element(By.XPATH,"//select[@name='nf-field-785']/option[text()='Pennsylvania']").click()
time.sleep(1)
# Search, wait to load
browser.find_element(By.ID,'nf-field-774').click()
WebDriverWait(browser, 30).until(EC.presence_of_element_located((By.CLASS_NAME, 'School_Profile_Link__c')))

# create empty dataframe
schools = pd.DataFrame(columns=['Description', 'Address'])

# cycle through pages and links
## predetermined number of pages
for page in range(6, 7):
    print('page: ', page)
    ## get number of schools on page
    WebDriverWait(browser, 60).until(EC.presence_of_element_located((By.CLASS_NAME, 'School_Name__c')))
    num_profiles = len(browser.find_elements(By.CLASS_NAME, 'School_Name__c'))
    #num_profiles = 49 #50 per page max?
    time.sleep(2)
    for profile in range(1,num_profiles):
        print('profile num: ',profile)
        # click a profile
        ## remove elements blocking profile link
        #blocking_element = browser.find_element(By.XPATH, '/html/body/div[1]/div/div[1]/div')
        #browser.execute_script("arguments[0].style.visibility='hidden'", blocking_element)
        remove_element(browser, '/html/body/div[1]/div/div[1]/div')
        #blocking_element = browser.find_element(By.XPATH, '// *[ @ id = "ajax-loading-screen"]')
        #browser.execute_script("arguments[0].style.visibility='hidden'", blocking_element)
        remove_element(browser, '// *[ @ id = "ajax-loading-screen"]')
        #blocking_element = browser.find_element(By.XPATH, '//*[@id="header-outer"]')
        #browser.execute_script("arguments[0].style.visibility='hidden'", blocking_element)
        remove_element(browser, '//*[@id="header-outer"]')
        #blocking_element = browser.find_element(By.XPATH, '/html/body/div[1]/div/div[3]/header/div/div/div[2]')
        #browser.execute_script("arguments[0].style.visibility='hidden'", blocking_element)
        remove_element(browser, '/html/body/div[1]/div/div[3]/header/div/div/div[2]')
        remove_element(browser, '/ html / body / div[1] / div / div[3] / header / div / div')
        remove_element(browser, '/ html / body / div[1] / div / div[3] / div[1] / div')
        #click
        time.sleep(1)
        browser.find_elements(By.CLASS_NAME, 'School_Profile_Link__c')[profile].click()
        WebDriverWait(browser, 60).until(EC.presence_of_element_located((By.XPATH,'/html/body/div[1]/div/div[4]/div[1]/div[3]/div[1]/div/div/div[2]/div/div/div/div[2]/div/div/h3/strong/span')))
        # get school name
        school_name = browser.find_element(By.XPATH,'/html/body/div[1]/div/div[4]/div[1]/div[3]/div[1]/div/div/div[2]/div/div/div/div[2]/div/div/h3/strong/span').text
        # get school address
        address = browser.find_element(By.XPATH,"/html/body/div[1]/div/div[4]/div[1]/div[3]/div[1]/div/div/div[2]/div/div/div/div[2]/div/div/h4[1]/span[1]").text
        time.sleep(2)
        # concatenate onto end of data frame
        row = [school_name, address]
        new_df = pd.DataFrame([row],columns=['Description', 'Address'])
        schools = pd.concat([schools, new_df], axis=0, ignore_index=True)
        # back to list of schools
        browser.back()
        WebDriverWait(browser, 60).until(EC.presence_of_element_located((By.CLASS_NAME, 'School_Profile_Link__c')))
        # go to page
        remove_element(browser, '/html/body/div[1]/div/div[1]/div')
        remove_element(browser, '// *[ @ id = "ajax-loading-screen"]')
        browser.find_element(By.LINK_TEXT, str(3)).click()
        browser.find_element(By.LINK_TEXT, str(5)).click()
        browser.find_element(By.LINK_TEXT, str(page)).click()


#single profile for broken pages (2,)
i=0
# get school name
school_name = browser.find_element(By.XPATH,'/html/body/div[1]/div/div[4]/div[1]/div[3]/div[1]/div/div/div[2]/div/div/div/div[2]/div/div/h3/strong/span').text
        # get school address
address = browser.find_element(By.XPATH,"/html/body/div[1]/div/div[4]/div[1]/div[3]/div[1]/div/div/div[2]/div/div/div/div[2]/div/div/h4[1]/span[1]").text
time.sleep(1)
# concatenate onto end of data frame
row = [school_name, address]
new_df = pd.DataFrame([row],columns=['Description', 'Address'])
schools = pd.concat([schools, new_df], axis=0, ignore_index=True)
# back to list of schools
browser.back()
WebDriverWait(browser, 60).until(EC.presence_of_element_located((By.CLASS_NAME, 'School_Profile_Link__c')))
time.sleep(1)
# go to chosen page
browser.find_element(By.LINK_TEXT, str(3)).click()
browser.find_element(By.LINK_TEXT, str(5)).click()
## remove elements blocking profile link
blocking_element = browser.find_element(By.XPATH, '/html/body/div[1]/div/div[1]/div')
browser.execute_script("arguments[0].style.visibility='hidden'", blocking_element)
blocking_element = browser.find_element(By.XPATH, '// *[ @ id = "ajax-loading-screen"]')
browser.execute_script("arguments[0].style.visibility='hidden'", blocking_element)
blocking_element = browser.find_element(By.XPATH, '//*[@id="header-outer"]')
browser.execute_script("arguments[0].style.visibility='hidden'", blocking_element)
blocking_element = browser.find_element(By.XPATH, '/html/body/div[1]/div/div[3]/header/div/div/div[2]')
browser.execute_script("arguments[0].style.visibility='hidden'", blocking_element)
#
i+=1
browser.find_elements(By.CLASS_NAME, 'School_Profile_Link__c')[i].click()

test=schools
test.columns=['Description','Address','d2','a2']
test.Description.fillna(test.d2, inplace=True)
test.Address.fillna(test.a2, inplace=True)
test=test.drop_duplicates()

# save dataframe to csv
os.chdir('C:\\Users\\natha\\Desktop\\Polling Places DiD\\data')
test.to_csv('MSA_PA_Catholic_Schools_1_18_2025.csv', index=False)