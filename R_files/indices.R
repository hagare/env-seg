#Equations taken from Dimensions of Segregation
#Author: D. Massey
#file: Social Forces-1988-Massey-281-315.pdf
# 1. Evenness
##Index of dissimilarity
#index measures evenness with which two mutually exclusive groups are distributed across teh geographc units taht make up a larger geographic entity
#ex: distribution of blacks and whites across counties that make up a state
#the minimum value is 0 and max is 1

#Dissimiliarity Index is a measure of evenness calculated by :
#D=sum( (ti*abs(pi-P)) / (2*T*P*(1-P)))
#ti non white total population in sub area
#pi proportion of non white in sub area
#T population in big area
#P proportion in big area

### Update with smaller unit level such as zip,census block, or census tract
#Generate Isolation Matrix for races census_dataset col 12-19 (can expand to other factors by changing column number)
for (x in c(12:19)) {
  for (i in 1:56) { #scroll through states or big units
    if (i==7|i==14|i==43){D.matrix[i,x]=NA
    }else{
      #generate county and state rows for each state
      county.rown=census_fip$state==i&census_fip$county>0 #rows for county or small subset
      state.rown=census_fip$state==i&census_fip$county==0 #rows for state or big unit
      
      ti=census_dataset[county.rown,3] #county pop
      pi=(census_dataset[county.rown,x]/100)#*census_dataset[county.rown,3]  #race pop county
      T=sum(ti) #total pop in state by summing county pop
      P=(census_dataset[state.rown,x]/100) #race pop in state=sum race in all counties
      D.matrix[i,x]= sum(ti*abs(pi-P)/(2*T*P*(1-P)))
    }
  }
  
}

#Generate Index of Dissimilarity for Races
mar.4x2=c(4.2, 4.2, 2.5, .75)
par(mfrow=c(4,2),mar =mar.4x2)
for (p in c(12:19)){
  #summary(D)
  hist(D.matrix[,p],xlab="Dissimilarity",main=sprintf("%s (state level)",race_label[p-11]))
}


# 2. Exposure
## Isolation Index
#Measures extent to which minority members are exposed only to one other rather than majority members
#(minority weighted avg)#ti is the total population of smaller unit
#I=sum((wi/W)*(wi/ti))

### Update with smaller unit level such as zip,census block, or census tract
#Generate Isolation Matrix for races census_dataset col 12-19 (can expand to other factors by changing column number)
for (x in c(12:19)) {
  for (i in 1:56) { #scroll through states or big units
    if (i==7|i==14|i==43){I.matrix[i,x]=NA
    }else{
    county.rown=census_fip$state==i&census_fip$county>0 #rows for county or small subset
    state.rown=census_fip$state==i&census_fip$county==0 #rows for state or big unit
    wi.W=(((census_dataset[county.rown,x]/100)*census_dataset[county.rown,3])/(census_dataset[state.rown,x]/100*sum(census_dataset[county.rown,3])))
    wi.ti=(((census_dataset[county.rown,x]/100)*census_dataset[county.rown,3])/census_dataset[county.rown,3])
    
    I.matrix[i,x]=sum(wi.W*wi.ti)
    }
  }
  
}

#Generate Distribution of Isolation of Races
opar=par(mar = c(5, 4, 4, 2) + 0.1)
mar.4x2=c(4.2, 4.2, 2.5, .75)
par(mfrow=c(4,2),mar =mar.4x2)
for (p in c(12:19)){
  #summary(I)
  hist(I.matrix[,p],xlab="Isolation",main=sprintf("%s (state level)",race_label[p-11]))
}

# Interaction Index
#Measures exposure to degree of potential contact or interaction between minority and majority group members
#ti is the total population of smaller unit
#xi minority
#yi majority
#Int=sum((xi/X)*(yi/ti))
Int.matrix=matrix(rep(rep(NA,max(census_fip$state)),dim(census_dataset)[2]),nrow=max(census_fip$state)) #initialize matrix 
### Update with smaller unit level such as zip,census block, or census tract
#Generate Interaction Matrix for races census_dataset col 12-19 (can expand to other factors by changing column number)
for (x in c(12:19)) {
  for (i in 1:56) { #scroll through states or big units
    if (i==7|i==14|i==43){Int.matrix[i,x]=NA
    }else{
      county.rown=census_fip$state==i&census_fip$county>0 #rows for county or small subset
      state.rown=census_fip$state==i&census_fip$county==0 #rows for state or big unit
      xi.X=(((census_dataset[county.rown,x]/100)*census_dataset[county.rown,3])/(census_dataset[state.rown,x]/100*sum(census_dataset[county.rown,3])))
      yi.ti=census_dataset[county.rown,raceCode("white-non")]/100
      
      Int.matrix[i,x]=sum(xi.X*yi.ti)
    }
  }
  
}

#Generate Distribution of Intsolation of Races
opar=par(mar = c(5, 4, 4, 2) + 0.1)
mar.4x2=c(4.2, 4.2, 2.5, .75)
par(mfrow=c(4,2),mar =mar.4x2)
for (p in c(12:19)){
  #summary(Int)
  hist(Int.matrix[,p],xlab="Interaction",main=sprintf("%s (state level)",race_label[p-11]))
}


# 3. Concentration
#Concentration refers to the relative amount of physical space occupied buy a minority group
# Delta is a specific application of dissimilarity
# Delta=0.5*sum(abs(xi/X-ai/A))
# xi = minority pop in subarea (county)
# X = minority pop in geographic area (state)
# ai = land area of sub area square meters 2010
# Ai = land area of geographic area square meters 2010


### Update with smaller unit level such as zip,census block, or census tract
#Generate Delta/Spatial Concentration Index Matrix for races census_dataset col 12-19 (can expand to other factors by changing column number)
Delta.matrix=matrix(rep(rep(NA,max(census_fip$state)),dim(census_dataset)[2]),nrow=max(census_fip$state)) #initialize matrix 

for (x in c(12:19)) {
  for (i in 1:56) { #scroll through states or big units
    if (i==7|i==14|i==43){Delta.matrix[i,x]=NA
    }else{
      county.rown=census_fip$state==i&census_fip$county>0 #rows for county or small subset
      state.rown=census_fip$state==i&census_fip$county==0 #rows for state or big unit
      
      xi.X=(((census_dataset[county.rown,x]/100)*census_dataset[county.rown,3])/(census_dataset[state.rown,x]/100*sum(census_dataset[county.rown,3])))
      ai.A=(census_dataset[county.rown,53]/100)/sum(census_dataset[county.rown,53]/100)
      Delta.matrix[i,x]=0.5*sum(abs(xi.X-ai.A))
    }
  }
  
}

#Generate Distribution of Deltasolation of Races
opar=par(mar = c(5, 4, 4, 2) + 0.1)
mar.4x2=c(4.2, 4.2, 2.5, .75)
par(mfrow=c(4,2),mar =mar.4x2)
for (p in c(12:19)){
  #summary(Delta)
  hist(Delta.matrix[,p],xlab="Delta/Spatial Concentration Index",main=sprintf("%s (state level)",race_label[p-11]))
}
