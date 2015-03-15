#Index of dissimilarity
#index measures evenness with which two mutually exclusive groups are distributed across teh geographc units taht make up a larger geographic entity
#ex: distribution of blacks and whites across counties that make up a state
#the minimum value is 0 and max is 100

#Source http://enceladus.isr.umich.edu/race/calculate.html
#bi= black small component
#B= black large geography
#wi= white small component
#W=white large geography
#D=(1/2) SUM (bi /B – wi / W )
D=NA*1:max(census_fip$state) # initialize var length=total states
I=NA*1:max(census_fip$state) # initialize var length=total states
bi=blank
B=blank
wi=blank
W=blank
state_pop=blank

#Dissimilarity between two groups x and y representing vector location on census_data set
#black 13, white-non-19, white 12
#ex: use raceCode("black") and raceCode("white-non") or raceCode("white")
# make vector where rows contain state population to be used with Dis.xy function
for (j in c(3:3195)) {
  state_val=census_fip$state[census_fip$code==census_dataset$fips[j]] #choose state
  state_pop[j]=census_dataset[census_fip$state==state_val&state_rown==T,3] #create vector of state population 2013
}

Dis.xy<-function(x=raceCode("black"),y=raceCode("white-non")) {
bi[census_fip$county_rown]=census_dataset[census_fip$county_rown,x]/100*census_dataset[census_fip$county_rown,3] #perc of race in 2013 * tot population 2013= race population

B[census_fip$county_rown]=county.state_perc[census_fip$county_rown,x]/100*state_pop[census_fip$county_rown]
wi[census_fip$county_rown]=census_dataset[census_fip$county_rown,y]/100*census_dataset[census_fip$county_rown,3]
W[census_fip$county_rown]=county.state_perc[census_fip$county_rown,y]/100*state_pop[census_fip$county_rown]

  for (i in 1:max(census_fip$state)){ #scroll through to generate D for each state based on county data
    rown=census_fip$state==i&census_fip$county_rown==T
    D[i]=0.5*sum(abs((bi[rown]/B[rown])-(wi[rown]/W[rown])))
    }
summary(D)
hist(D,main=sprintf("Dissimilarity Index for %s and %s",race_label[x-11],race_label[y-11]))
return(D) 
}

#Execute function for Dissimilarity Index
Dis.black_whitenon=Dis.xy(x=raceCode("black"),y=raceCode("white-non"))
Dis.black_white=Dis.xy(x=raceCode("black"),y=raceCode("white"))
Dis.hispanic_whitenon=Dis.xy(x=raceCode("hispanic"),y=raceCode("white-non"))
Dis.asian_whitenon=Dis.xy(x=raceCode("asian"),y=raceCode("white-non"))
Dis.multi_whitenon=Dis.xy(x=raceCode("multi"),y=raceCode("white-non"))

# Isolation Index
ti=blank
Iso.x<-function(x=raceCode("white-non")) {
  wi[census_fip$county_rown]=(census_dataset[census_fip$county_rown,x]/100)*census_dataset[census_fip$county_rown,3]
  W[census_fip$county_rown]=(county.state_perc[census_fip$county_rown,x]/100)*state_pop[census_fip$county_rown]
  ti[census_fip$county_rown]=census_dataset[census_fip$county_rown,3] #total county population
  for (i in 1:max(census_fip$state)){ #scroll through to generate I for each state based on county data
    rown=census_fip$state==i&census_fip$county_rown==T
    I[i]=sum((wi[rown]/W[rown])*(wi[rown]/ti[rown]))/100 #not sure why but Isolation index should be 0 to 1 so divided answer by 100
    ###Make sure to find error in calculation and fix "/100" is just manual fix
  }
  summary(I)
  hist(I,main=sprintf("Isolation Index for %s",race_label[x-11]))
  return(I) 
}

#Execute function for Dissimilarity Index
Iso_whitenon=Iso.x(raceCode("white-non"))
Iso_white=Iso.x(raceCode("white"))
Iso_black=Iso.x(raceCode("black"))
Iso_hispanic=Iso.x(raceCode("hispanic"))
Iso_asian=Iso.x(raceCode("asian"))
Iso_multi=Iso.x(raceCode("multi"))






