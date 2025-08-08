#Nathaniel Flemming
#5/11/24

## convert raw PA voter files to useful csv so they can be merged with poll location and L2 files

################# Libraries
library(tidyverse)
library(data.table) #read in data selectively
library(stringr) #string manipulation

################# Functions
### create filepaths
make_FVE_filepath<-function(dir, folder, name_pattern){
  ### get list of FVE files in voter file directory
  files <- list.files(paste0(dir,folder), pattern=name_pattern)
  ### convert file names to file paths and return
  return(paste0(data_dir, folder,'\\',files))
}

### Read in and combine files
read_and_combine<-function(filepaths, columns, countynames){
  datalist<-lapply(filepaths, FUN=fread, sep='\t', header = F, 
                   select = columns)
  ## Add list with county name
  for(i in 1:length(countynames)){
    datalist[[i]][,'County']<-countynames[i]
  }
  ### Convert all lists to characters
  datalist<-rapply(datalist,as.character,how="replace")
  ### bind files together into data frame and return
  return(rbindlist(datalist))
}


################## Main
#### set directories
data_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data'
#### set year
year='2019'

### create vector of county names
counties = c('ADAMS', 'ALLEGHENY', 'ARMSTRONG', 'BEAVER', 'BEDFORD', 'BERKS', 'BLAIR', 'BRADFORD', 'BUCKS', 'BUTLER',
            'CAMBRIA', 'CAMERON', 'CARBON', 'CENTRE', 'CHESTER', 'CLARION', 'CLEARFIELD', 'CLINTON', 'COLUMBIA',
            'CRAWFORD', 'CUMBERLAND', 'DAUPHIN', 'DELAWARE', 'ELK', 'ERIE', 'FAYETTE', 'FOREST', 'FRANKLIN', 'FULTON',
            'GREENE', 'HUNTINGDON', 'INDIANA', 'JEFFERSON', 'JUNIATA', 'LACKAWANNA', 'LANCASTER', 'LAWRENCE', 'LEBANON',
            'LEHIGH', 'LUZERNE', 'LYCOMING', 'MCKEAN', 'MERCER', 'MIFFLIN', 'MONROE', 'MONTGOMERY', 'MONTOUR',
            'NORTHAMPTON', 'NORTHUMBERLAND', 'PERRY', 'PHILADELPHIA', 'PIKE', 'POTTER', 'SCHUYLKILL', 'SNYDER',
            'SOMERSET', 'SULLIVAN', 'SUSQUEHANNA', 'TIOGA', 'UNION', 'VENANGO', 'WARREN', 'WASHINGTON', 'WAYNE',
            'WESTMORELAND', 'WYOMING', 'YORK')

## create list of paths to FVE files
FVE_paths<-make_FVE_filepath(dir=data_dir, folder=paste0('\\Pennsylvania Statewide ',year),
                             name_pattern='FVE')

##read in and combine files
FVE_data<-read_and_combine(filepaths=FVE_paths,columns=c(1, 13:18, 20, 27, 28), 
                           countynames = counties)

#### Rename columns
FVE_data <- FVE_data%>%
  rename(VOTERID = V1,
         House_Num = V13,
         House_Num_suffix = V14,
         StreetName = V15,
         Apartment_Num = V16,
         AddressLine2 = V17,
         City = V18,
         ZIP = V20,
         PrecCode = V27,
         PrecCodeSplit = V28)%>%
  select(c(VOTERID, House_Num, House_Num_suffix, StreetName, Apartment_Num,
           AddressLine2, City, ZIP,County, PrecCode, PrecCodeSplit))
         
### Save data frame as csv
write.csv(FVE_data, paste0(data_dir,'\\FVE_',year,'.csv'))


