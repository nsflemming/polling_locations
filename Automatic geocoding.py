'''
Nathaniel Flemming
1/4/23 originally created
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
os.chdir('C:\\Users\\natha\\Desktop\\Polling Places DiD\\data\\FVEs_geocoded 2020')

#create county names list
counties = ['ADAMS', 'ALLEGHENY', 'ARMSTRONG', 'BEAVER', 'BEDFORD', 'BERKS', 'BLAIR', 'BRADFORD', 'BUCKS', 'BUTLER',
            'CAMBRIA', 'CAMERON', 'CARBON', 'CENTRE', 'CHESTER', 'CLARION', 'CLEARFIELD', 'CLINTON', 'COLUMBIA',
            'CRAWFORD', 'CUMBERLAND', 'DAUPHIN', 'DELAWARE', 'ELK', 'ERIE', 'FAYETTE', 'FOREST', 'FRANKLIN', 'FULTON',
            'GREENE', 'HUNTINGDON', 'INDIANA', 'JEFFERSON', 'JUNIATA', 'LACKAWANNA', 'LANCASTER', 'LAWRENCE', 'LEBANON',
            'LEHIGH', 'LUZERNE', 'LYCOMING', 'MCKEAN', 'MERCER', 'MIFFLIN', 'MONROE', 'MONTGOMERY', 'MONTOUR',
            'NORTHAMPTON', 'NORTHUMBERLAND', 'PERRY', 'PHILADELPHIA', 'PIKE', 'POTTER', 'SCHUYLKILL', 'SNYDER',
            'SOMERSET', 'SULLIVAN', 'SUSQUEHANNA', 'TIOGA', 'UNION', 'VENAGO', 'WARREN', 'WASHINGTON', 'WAYNE',
            'WESTMORELAND', 'WYOMING', 'YORK']
# remaining counties
'''counties = ['ARMSTRONG', 'BEAVER', 'BEDFORD', 'BERKS', 'BLAIR', 'BRADFORD', 'BUCKS', 'BUTLER',
            'CAMBRIA', 'CAMERON', 'CARBON', 'CENTRE', 'CHESTER', 'CLARION', 'CLEARFIELD', 'CLINTON', 'COLUMBIA',
            'CRAWFORD', 'CUMBERLAND', 'DAUPHIN', 'DELAWARE', 'ELK', 'ERIE', 'FAYETTE', 'FOREST', 'FRANKLIN', 'FULTON',
            'GREENE', 'HUNTINGDON', 'INDIANA', 'JEFFERSON', 'JUNIATA', 'LACKAWANNA', 'LANCASTER', 'LAWRENCE', 'LEBANON',
            'LEHIGH', 'LUZERNE', 'LYCOMING', 'MCKEAN', 'MERCER', 'MIFFLIN', 'MONROE', 'MONTGOMERY', 'MONTOUR',
            'NORTHAMPTON', 'NORTHUMBERLAND', 'PERRY', 'PHILADELPHIA', 'PIKE', 'POTTER', 'SCHUYLKILL', 'SNYDER',
            'SOMERSET', 'SULLIVAN', 'SUSQUEHANNA', 'TIOGA', 'UNION', 'VENAGO', 'WARREN', 'WASHINGTON', 'WAYNE',
            'WESTMORELAND', 'WYOMING', 'YORK']'''


#use geocoder API to geocode
## path to FVEs for geocoding
path = 'C:/Users/natha/Desktop/Polling Places DiD/data/FVEs for geocode/'
## list of all FVE files that have a county in the county name list
files = os.listdir(path)

for csv in files:
    filepath = path+csv
    print(filepath)
    file = {
        'addressFile': open(filepath, 'rb'),
        'benchmark': (None, 'Public_AR_Current'),
    }
    ##execute the API command
    response = requests.post('https://geocoding.geo.census.gov/geocoder/locations/addressbatch', files=file)
    ##write to csv
    output_file = 'geocoderesult_'+csv
    print(output_file)
    with open(output_file, 'wb') as f:
        f.write(response.content)
