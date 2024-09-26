'''
Nathaniel Flemming
1/4/23 created
Taking the voter rolls and running them through the census geocoder
'''
import numpy as np
#import webdriver
from selenium import webdriver
#import firefox specific webdriver?
from selenium.webdriver.firefox.options import Options
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.support.select import Select
#import time
import time
#import os for file selecting
import os
import requests # to interact with API

#set working directory
os.chdir('C:\\Users\\natha\\OneDrive\\Desktop\\Polling Places\\data\\FVEs_geocoded')

#create county names list
'''counties = ['ADAMS', 'ALLEGHENY', 'ARMSTRONG', 'BEAVER', 'BEDFORD', 'BERKS', 'BLAIR', 'BRADFORD', 'BUCKS', 'BUTLER',
            'CAMBRIA', 'CAMERON', 'CARBON', 'CENTRE', 'CHESTER', 'CLARION', 'CLEARFIELD', 'CLINTON', 'COLUMBIA',
            'CRAWFORD', 'CUMBERLAND', 'DAUPHIN', 'DELAWARE', 'ELK', 'ERIE', 'FAYETTE', 'FOREST', 'FRANKLIN', 'FULTON',
            'GREENE', 'HUNTINGDON', 'INDIANA', 'JEFFERSON', 'JUNIATA', 'LACKAWANNA', 'LANCASTER', 'LAWRENCE', 'LEBANON',
            'LEHIGH', 'LUZERNE', 'LYCOMING', 'MCKEAN', 'MERCER', 'MIFFLIN', 'MONROE', 'MONTGOMERY', 'MONTOUR',
            'NORTHAMPTON', 'NORTHUMBERLAND', 'PERRY', 'PHILADELPHIA', 'PIKE', 'POTTER', 'SCHUYLKILL', 'SNYDER',
            'SOMERSET', 'SULLIVAN', 'SUSQUEHANNA', 'TIOGA', 'UNION', 'VENAGO', 'WARREN', 'WASHINGTON', 'WAYNE',
            'WESTMORELAND', 'WYOMING', 'YORK']'''
# remaining counties
counties = ['BUTLER',
            'CAMBRIA', 'CAMERON', 'CARBON', 'CENTRE', 'CHESTER', 'CLARION', 'CLEARFIELD', 'CLINTON', 'COLUMBIA',
            'CRAWFORD', 'CUMBERLAND', 'DAUPHIN', 'DELAWARE', 'ELK', 'ERIE', 'FAYETTE', 'FOREST', 'FRANKLIN', 'FULTON',
            'GREENE', 'HUNTINGDON', 'INDIANA', 'JEFFERSON', 'JUNIATA', 'LACKAWANNA', 'LANCASTER', 'LAWRENCE', 'LEBANON',
            'LEHIGH', 'LUZERNE', 'LYCOMING', 'MCKEAN', 'MERCER', 'MIFFLIN', 'MONROE', 'MONTGOMERY', 'MONTOUR',
            'NORTHAMPTON', 'NORTHUMBERLAND', 'PERRY', 'PHILADELPHIA', 'PIKE', 'POTTER', 'SCHUYLKILL', 'SNYDER',
            'SOMERSET', 'SULLIVAN', 'SUSQUEHANNA', 'TIOGA', 'UNION', 'VENAGO', 'WARREN', 'WASHINGTON', 'WAYNE',
            'WESTMORELAND', 'WYOMING', 'YORK']

'''######## Simulate browser to run Census Geocoder
#create options object for simulated browser
opts = Options()
# have browser, but won't see the window
opts.add_argument('-headless')
#opts.headless=True
#create browser
service = Service('C:\Program Files\Mozilla Firefox\geckodriver.exe')
browser = webdriver.Firefox(service=service, options=opts)
#control browser
browser.get('https://geocoding.geo.census.gov/geocoder/locations/addressbatch?form')

#read in csvs for geocoding
path = 'C:/Users/natha/OneDrive/Desktop/Polling Places/data/FVEs for geocode/'
files = [i for i in os.listdir(path) if os.path.isfile(os.path.join(path,i)) and
         any(county in i for county in counties)]  # include file if it has a county in its name


# find the file upload element on the page
file_input = browser.find_element(By.ID, 'uploadFile')
#find bechmark menu element on page
Benchmark = browser.find_element(By.ID, '_benchmark_id')
# select the 2020 census for geocoding
select_dropdown = Select(Benchmark)
select_dropdown.select_by_visible_text('Public_AR_Census2020')

for file in files:
    # convert file path to absolute path instead of relative so Selenium can read it correctly
    whole_path = os.path.abspath(os.path.join(path,file))
    # send file to browse/file input element
    file_input.send_keys(whole_path)
    time.sleep(1)
    # press get results button
    browser.find_element(By.ID, 'locationBatchSubmit').click()
    # wait for 5 minutes for results then loop back to top
    time.sleep(300)'''

#use geocoder API to geocode
## path to FVEs for geocoding
path = 'C:/Users/natha/OneDrive/Desktop/Polling Places/data/FVEs_for_geocode/'
## list of all FVE files that have a county in the county name list


'''
files = {
    'addressFile': open('C:\\Users\\natha\\OneDrive\\Desktop\\Polling Places\\data\\FVEs_for_geocode\\CAMBRIA_FVE_20000.csv', 'rb'),
    'benchmark': (None, 'Public_AR_Current'),
}
##execute the API command
response = requests.post('https://geocoding.geo.census.gov/geocoder/locations/addressbatch', files=files)
##write to csv
with open('geocoderesultCAMBRIA_FVE_20000.csv', 'wb') as f:
    f.write(response.content)
'''