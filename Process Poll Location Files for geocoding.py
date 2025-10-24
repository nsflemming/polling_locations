#Nathaniel Flemming
#1/16/25
#import numpy for math
import numpy as np
#import pandas for data frames
import pandas as pd
#import os for file selection
import os

############# Convert FVE files to Census geocoding format
## file names
years = ['2017']
# create list of paths to FVE files
pollpath = 'C:/Users/natha/Desktop/Polling Places DiD/data/gov_poll_places/'
pollfiles = [i for i in os.listdir(pollpath) if os.path.isfile(os.path.join(pollpath,i)) and
         'csv' in i]  # include csv files only
pollfilepaths = [pollpath + file for file in pollfiles]  # add path to file name

## Read in files and reformat for geocoding
# counter to keep track of years
i = 0
# for loop reading and reformating
for file in pollfilepaths[1:2]:
    data = pd.read_csv(file)  # read in file
    #break out address into street number, name, city, state, Zip
    num = data['HouseNum']
    #2016 variable names
    #street = data['PrefixDirectionDesc'].fillna('')+' '+data['Street'].fillna('')+' '+data['StreetTypeDesc'].fillna('')+' '+data['SuffixdirectionDesc'].fillna('')
    # Post 2016 variable names
    street = data['PrefixDirection'].fillna('')+' '+data['Street'].fillna('')+' '+data['StreetType'].fillna('')+' '+data['SuffixDirection'].fillna('')
    city = data['City']
    state = pd.Series('PA', index=np.arange(len(data)))
    ZIP = data['PostalCode']
    #combine number and street
    ## convert numbers to strings
    numstreet = num.str.cat(street, sep=' ')
    #combine lists into data frame
    address = pd.DataFrame(np.column_stack([numstreet, city, state, ZIP]),
                           columns=['numstreet', 'city', 'state', 'zip'])
    # slice at every 10k addresses if needed
    n = 10000
    #list of sliced data frames
    addressdfs = [address[i:i+n] for i in range(0, len(address), n)]
    # save each data frame as a procedurally named file
    ## create path
    savepath = 'C:/Users/natha/Desktop/Polling Places DiD/data/gov_poll_places for geocode/'
    for slice in addressdfs:
        m = str(n) #convert slice number to string
        slice_path = savepath+years[i]+'_poll_locations_'+m+'.csv'  # create incrementing path
        slice.to_csv(slice_path, sep=',', header=False)  # save to csv
        n += 10000  # increment
    i += 1  # increment year