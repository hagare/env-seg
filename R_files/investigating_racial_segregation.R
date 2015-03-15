## Initial investigation of racial segregation
#Load Census data
source('Data_Import_Scripts.R')
#generates vars: census_dataset, census_dict, and census_fip

blank=NA*c(1:length(census_dataset[,1])) #generate empty vector length of dataset to initialize other variables later
#Question where are the most Black segregated counties in US?
# % Blacks in US
census_dataset[1,13]

#Descriptive Statistics on county level
summary(census_dataset[census_fip$county_rown,12])

require(Hmisc)
describe(census_dataset[census_fip$county_rown,12])

fivenum(census_dataset[census_fip$county_rown,12])

#Generate census_data vector when race label specified
#just for ease of use
raceCode<-function(race){
if(race=="white") {raceCode=12}
if(race=="black") {raceCode=13}
if(race=="native") {raceCode=14}
if(race=="asian") {raceCode=15}
if(race=="islander") {raceCode=16}
if(race=="multi") {raceCode=17}
if(race=="hispanic") {raceCode=18}
if(race=="white-non") {raceCode=19} #white non hispanic
return(raceCode)
}

#Are more places integrated or segregated? county level
#Initialize and Generate variables and arrays
race_label=c("white","black","native","asian","islander","multi","hispanic","white-non")
race_blank=NA*c(12:19)
perc_race_rng=race_blank
perc_race_med=race_blank
perc_race_iqr=race_blank
state_perc=blank #initialize %state vector
county.state_perc=matrix(rep(blank,dim(census_dataset)[2]),nrow=dim(census_dataset)[1])#initialize county/state race ratio matrix

par(mfrow=c(4,2))
for (i in c(12:19))   hist(census_dataset[census_fip$county_rown,i],main=race_label[i-11],xlab="% of race per county")
for (i in c(12:19))  { 
  for (j in c(3:3195)) {
    state_val=census_fip$state[census_fip$code==census_dataset$fips[j]] #choose state
    state_perc[j]=census_dataset[census_fip$state==state_val&state_rown==T,i] #create vector of state % for specified race
  }
  county.state_perc[census_fip$county_rown,i]=census_dataset[census_fip$county_rown,i]/state_perc[census_fip$county_rown] #matrix where rows are county values normalized by state
  
  hist(county.state_perc[census_fip$county_rown,i],main=race_label[i-11],xlab="% of race per county/%state ") #new way
  #hist(census_dataset[census_fip$county_rown,i]/state_perc[census_fip$county_rown,i],main=race_label[i-11],xlab="% of race per county/%state ") #old way


#perc_race_mean[i-11]=mean(census_dataset[census_fip$county_rown,i]/state_perc[census_fip$county_rown],na.rm=T)
# perc_race_med[i-11]=median(census_dataset[census_fip$county_rown,i]/state_perc[census_fip$county_rown],na.rm=T)
# perc_race_iqr[i-11]=IQR(census_dataset[census_fip$county_rown,i]/state_perc[census_fip$county_rown],na.rm=T)
#perc_race_rng[i-11]=diff(range(census_dataset[census_fip$county_rown,i]/state_perc[census_fip$county_rown],na.rm=T))
}

for (i in c(12:19)) 
{
  hist(census_dataset[census_fip$county_rown,i]/census_dataset[census_fip$us_rown,i],main=race_label[i-11],xlab="% of race per county/%us ")
}

for (i in c(12:19)) hist(census_dataset[census_fip$state_rown,i]/census_dataset[census_fip$us_rown,i],main=race_label[i-11],xlab="% of race per state/%us ")


#What are state rankings for the highest level of racial segregation in terms of black people


#How segregated is the US?
source('indices.R')