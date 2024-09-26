#Nathaniel Flemming
#10/5/23
#import numpy for math
import numpy as np
#import pandas for data frames
import pandas as pd
#import os for file selection
import os

############# Convert FVE files to Census geocoding format
## county names
'''counties = ['ADAMS', 'ALLEGHENY', 'ARMSTRONG', 'BEAVER', 'BEDFORD', 'BERKS', 'BLAIR', 'BRADFORD', 'BUCKS', 'BUTLER',
            'CAMBRIA', 'CAMERON', 'CARBON', 'CENTRE', 'CHESTER', 'CLARION', 'CLEARFIELD', 'CLINTON', 'COLUMBIA',
            'CRAWFORD', 'CUMBERLAND', 'DAUPHIN', 'DELAWARE', 'ELK', 'ERIE', 'FAYETTE', 'FOREST', 'FRANKLIN', 'FULTON',
            'GREENE', 'HUNTINGDON', 'INDIANA', 'JEFFERSON', 'JUNIATA', 'LACKAWANNA', 'LANCASTER', 'LAWRENCE', 'LEBANON',
            'LEHIGH', 'LUZERNE', 'LYCOMING', 'MCKEAN', 'MERCER', 'MIFFLIN', 'MONROE', 'MONTGOMERY', 'MONTOUR',
            'NORTHAMPTON', 'NORTHUMBERLAND', 'PERRY', 'PHILADELPHIA', 'PIKE', 'POTTER', 'SCHUYLKILL', 'SNYDER',
            'SOMERSET', 'SULLIVAN', 'SUSQUEHANNA', 'TIOGA', 'UNION', 'VENAGO', 'WARREN', 'WASHINGTON', 'WAYNE',
            'WESTMORELAND', 'WYOMING', 'YORK']'''
counties = ['PHILADELPHIA']
# create list of paths to FVE files
FVEpath = 'C:/Users/natha/OneDrive/Desktop/Polling Places/data/Raw FVEs/'
FVEfiles = [i for i in os.listdir(FVEpath) if os.path.isfile(os.path.join(FVEpath,i)) and
         any(sub in i for sub in counties) and # include file if it has a county in its name
         '20191028.txt' in i]  # include file if it has 20191028.txt in its name
FVEfilepaths = [FVEpath + file for file in FVEfiles]  # add path to file name

## Read in raw FVEs and reformat for geocoding
# counter to keep track of counties
i = 0
# for loop reading and reformating
for FVE in FVEfilepaths:
    data = pd.read_csv(FVE, sep="\t", header = None)  # read in FVE
    #break out address into street number, name, city, state, Zip
    num = data.iloc[:,12]
    street = data.iloc[:,14]
    city = data.iloc[:,17]
    state = pd.Series('PA', index=np.arange(len(data)))
    ZIP = data.iloc[:,19]
    #combine number and street
    ## convert numbers to strings
    num = num.astype(str)
    numstreet = num.str.cat(street, sep=' ')
    #combine lists into data frame
    address = pd.DataFrame(np.column_stack([numstreet, city, state, ZIP]),
                           columns=['numstreet', 'city', 'state', 'zip'])
    # slice at every 10k addresses
    n = 10000
    #list of sliced data frames
    addressdfs = [address[i:i+n] for i in range(0, len(address), n)]
    # save each data frame as a procedurally named file
    ## create path
    savepath = 'C:/Users/natha/OneDrive/Desktop/Polling Places/data/FVEs for geocode/'
    for slice in addressdfs:
        m = str(n) #convert to string
        slice_path = savepath+counties[i]+'_FVE_'+m+'.csv'  # create incrementing path
        slice.to_csv(slice_path, sep=',', header=False)  # save to csv
        n += 10000  # increment
    i += 1  # increment county