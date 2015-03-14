#Does segregation lead to higher occurences of environmental problems?

### Import Census Data
##Census Data on US, State, and County Level
#Data
# census_dataset<-read.table(url('http://quickfacts.census.gov/qfd/download/DataSet.txt'),sep=',',header=T)
# write.csv(census_dataset,'../processed_data/Census/DataSet.csv',row.names=F) #save data to local processed_data directory

## Variable Explanation Metadata
#census_dataset_dict<-read.csv('../processed_data/Census/DataDict.csv',header=T); #url('http://quickfacts.census.gov/qfd/download/DataDict.txt') #variables

## Define rows for desired location level (us,state,county) as binary
# census_dataset_county<-read.csv('../raw_data/Census/FIPS_CountyName.txt',header=F); #url('http://quickfacts.census.gov/qfd/download/FIPS_CountyName.txt
# census_dataset_county$code<-as.numeric(sapply(census_dataset_county[,1], function(x) substr(x,1,5))) #extract county code as number
# census_dataset_county$name<-sapply(census_dataset_county[,1], function(x) gsub("^[0-9]* ","",x)) #extract us/state/county name as character
# census_dataset_county$state<-as.numeric(sapply(census_dataset_county$V1, function(x) substr(x,1,2))) #use number to determine if row is state or county
# census_dataset_county$county<-as.numeric(sapply(census_dataset_county$V1, function(x) substr(x,3,5))) #use number to determine if row is state or county
# census_dataset_county$county_rown<-census_dataset_county$county>0&census_dataset_county$code>0
# census_dataset_county$state_rown<-census_dataset_county$county==0&census_dataset_county$code>0
# census_dataset_county$us_rown<-census_dataset_county$code==0
# write.csv(census_dataset_county,'../processed_data/Census/FIPS_CountyName.csv',row.names=F);
 
#To use data, column dictionary, and row locations from Census
census_dataset<-read.csv('../processed_data/Census/DataSet.csv',header=T) #data on county level
census_dict<-read.csv('../processed_data/Census/DataDict.csv',header=T) #variables of study
census_fip<-read.csv('../processed_data/Census/FIPS_CountyName.csv',header=T) #binary=>column 7: county,8:state,9:us
