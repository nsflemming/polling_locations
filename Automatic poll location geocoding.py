'''
Nathaniel Flemming
1/4/23 originally created
Taking the voter rolls and running them through the census geocoder
'''
################### Packages
import os #import os for file selecting
import requests # to interact with API

################## Main
#set working directory
os.chdir('C:\\Users\\natha\\Desktop\\Polling Places DiD\\data\\gov_poll_places geocoded')

#create years names list
years = ['2018','2019','2020','2021','2022','2023']

#use geocoder API to geocode
## path to FVEs for geocoding
path = 'C:/Users/natha/Desktop/Polling Places DiD/data/gov_poll_places for geocode/'
## list of all FVE files that have a county in the county name list
files = os.listdir(path)

for csv in files:
    filepath = path+csv
    print(filepath)
    file = {
        'addressFile': open(filepath, 'rb'),
        'benchmark': (None, 'Public_AR_Census2020'),
    }
    ##execute the API command
    response = requests.post('https://geocoding.geo.census.gov/geocoder/locations/addressbatch', files=file)
    ##write to csv
    output_file = 'geocoderesult_'+csv
    print(output_file)
    with open(output_file, 'wb') as f:
        f.write(response.content)
