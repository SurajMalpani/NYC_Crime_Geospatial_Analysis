
library(readr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(data.table)
library(ggmap)


glimpse(dataset)
summary(dataset)
names(dataset)
head(dataset)
#complete.cases(dataset)
#summary(dataset.util)
head(dataset$CMPLNT_FR_DT, 100)
summary(dataset$CMPLNT_FR_DT)
#dataset$CMPLNT_FR_DT <- mdy(dataset$CMPLNT_FR_DT,format="mm/dd/yyyy")
dataset$CRM_ATPT_CPTD_CD <- as.factor(dataset$CRM_ATPT_CPTD_CD)
dataset$OFNS_DESC <- as.factor(dataset$OFNS_DESC)
dataset$LAW_CAT_CD <- as.factor(dataset$LAW_CAT_CD)
dataset$LOC_OF_OCCUR_DESC <- as.factor(dataset$LOC_OF_OCCUR_DESC )
dataset$PREM_TYP_DESC <- as.factor(dataset$PREM_TYP_DESC)
dataset$BORO_NM <- as.factor(dataset$BORO_NM)
dataset$JURIS_DESC <- as.factor(dataset$JURIS_DESC)
dataset$PARKS_NM <- as.factor(dataset$PARKS_NM)
dataset$HADEVELOPT <- as.factor(dataset$HADEVELOPT)

#Visualizing locations --------
library(ggmap)
#summary(dataset$LAW_CAT_CD)
#qmplot(Longitude, Latitude, data=dataset)
# location_map <- get_map(location='new york',zoom=10)
#                       base_layer = ggplot(aes(x=Longitude, y=Latitude), data=crime_location))

# qmplot(Longitude, Latitude, data=crime_location)+
#   geom_point(aes(x=Longitude, y = Latitude), fill= PREM_TYP_DESC, data=PREM_TYP_DESC)
# ggmap(location_map)

#Total Crimes after 2011
crime_violation <- dataset%>%
  select(CMPLNT_FR_DT, Longitude, Latitude, Lat_Lon, OFNS_DESC, BORO_NM, CRM_ATPT_CPTD_CD, LAW_CAT_CD, PREM_TYP_DESC) %>%
  filter(CMPLNT_FR_DT > "12/31/2011" )
crime_violation <- crime_violation[complete.cases(crime_violation),]

#ggmap
ny <- get_map(location='New York',zoom=11, maptype="terrain")
ggmap(ny)
#Plot by Law category
ggmap(ny)+
  geom_point(
    aes(x=Longitude, y=Latitude, colour=LAW_CAT_CD, alpha=1/200),
    data=crime_violation
  )

#plot by ofns_desc
ggmap(ny)+
  geom_point(
    aes(x=Longitude, y=Latitude, colour=OFNS_DESC, shape=CRM_ATPT_CPTD_CD, alpha=1/50),
    data=crime_violation
  )+
  facet_wrap(~LAW_CAT_CD)

#Plots by top boroughs
#Brooklyn
crime_location <- dataset%>%
  select(CMPLNT_FR_DT, Longitude, Latitude, Lat_Lon, OFNS_DESC, BORO_NM, CRM_ATPT_CPTD_CD, LAW_CAT_CD, PREM_TYP_DESC) %>%
  filter(CRM_ATPT_CPTD_CD == 'COMPLETED' & BORO_NM=='BROOKLYN' & LAW_CAT_CD == 'FELONY')
crime_location <- crime_location[complete.cases(crime_location),]
summary(crime_location)

ny <- get_map(location='Brooklyn',zoom=12)
ggmap(ny)+
  geom_point(
    aes(x=Longitude, y=Latitude, colour=OFNS_DESC, alpha=1/100),
    data=crime_location
  )

#Manhattan
crime_location <- dataset%>%
  select(CMPLNT_FR_TM, Longitude, Latitude, OFNS_DESC, BORO_NM, CRM_ATPT_CPTD_CD, LAW_CAT_CD, PREM_TYP_DESC) %>%
  filter(CRM_ATPT_CPTD_CD == 'COMPLETED'& BORO_NM=='MANHATTAN' & LAW_CAT_CD == 'FELONY')
crime_location <- crime_location[complete.cases(crime_location),]

ny <- get_map(location='MANHATTAN',zoom=13)
ggmap(ny)+
  geom_point(
    aes(x=Longitude, y=Latitude, colour=OFNS_DESC),
    data=crime_location
  )

ggmap(ny)+
  stat_bin_2d(
    aes(x=Longitude, y=Latitude, colour=OFNS_DESC),
    size=.5, bins=100, alpha = 0.3,
    data=crime_location
  )


