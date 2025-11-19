#This code had all the dates that start on March 11, which is the latest start date of GPS, This will allow for simultaneous timing. Note, when there are not 2 years, they follow previous tracks


#Load in Libs
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(rgdal)
library(raster)
library(Rcpp)
library(sp)
library(sf)
library(move)
library("dplyr")
library(padr)
library(zoo)
library(conflicted)
conflict_prefer("select", "dplyr")




#binding them all and looping through ----
F1 <- read.csv("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F1_Collar43284_2.csv")
F2 <- read.csv("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F2_Collar43276_2.csv")
F4 <- read.csv("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F4_Collar43289_2.csv")
F5 <- read.csv("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F5_Collar43286_2.csv")
F5[c(11670), c(6:8)] = "NA" #remove negative numbers
F5[c(5729), c(6:8)] = "NA" #remove negative numbers
F6 <- read.csv("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F6_Collar43292_2.csv")
F7 <- read.csv("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F7_Collar43290_2.csv")
F8 <- read.csv("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F8_Collar43285_2.csv")
F9 <- read.csv("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F9_Collar43283_2.csv")
F11 <- read.csv("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F11_46082_All_Deployment_2.csv")
F11[c(3193), c(6:8)] = "NA" #remove negative numbers
F11[c(4541), c(6:8)] = "NA" #remove negative numbers
F12 <- read.csv("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F12_43291_All_Deployment_2.csv")
F12[c(13044), c(6:8)] = "NA" #remove negative numbers
F13 <- read.csv("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F13_43288_All_Deployment_2.csv")
F13[c(1742), c(6:8)] = "NA" #remove outlier
F14 <- read.csv("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F14_Collar47020_2.csv")
F15 <- read.delim("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F15_Collar27898_rightformat.txt")
F16 <- read.delim("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F16_Collar27895_rightformat.txt")
F16<-F16[-c(1), ]
F17 <- read.delim("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F17_Collar22428_rightformat.txt")
F18 <- read.delim("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F18_Collar47977_rightformat.txt")
F19 <- read.delim("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F19_Collar27900_rightformat.txt")
F20 <- read.delim("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F20_Collar47978_rightformat.txt")
F21 <- read.delim("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F21_Collar47979_rightformat.txt")
F21<-F21[-c(1), ]
F21[c(6159), c(5:6)] = "NA" #remove outlier
F22 <- read.delim("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F22_Collar22435_rightformat.txt")
F23 <- read.delim("~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/F23_Collar22422_rightformat.txt")

#futher cleaning
Old_Deer<-rbind(F1, F2, F4, F5, F6, F7, F8, F9, F11, F12, F13, F14)
Old_Deer<-subset(Old_Deer, select = -c(3,4,5,8:14) )
New_Deer<-rbind(F18, F19, F20, F21, F22, F23)
New_Deer<-subset(New_Deer, select = -c(15) )
New_Deer<-rbind(F15, F16, F17, New_Deer)
New_Deer<-subset(New_Deer, select = -c(3, 4, 7:14))
All_Deer<-rbind(Old_Deer, New_Deer)

All_Deer <- All_Deer %>% mutate_at(c('Latitude.deg.'), ~na_if(., 0))
All_Deer <- All_Deer %>% mutate_at(c('Longitude.deg.'), ~na_if(., 0))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


#Padding time -----
All_Deer.Unique_IDs<-unique(All_Deer$Collar.ID)

output<-list()
All_Together_For_Poly<-c()
for (i in 1:length(All_Deer.Unique_IDs)){
 data_1 <- subset(All_Deer, Collar.ID == All_Deer.Unique_IDs[i])
  data_1$DateTime <- data_1$Acq..Time..UTC.
  data_1<- data_1 %>%
    separate(DateTime, sep=" ", into = c("Date", "Time"))
  data_1$Date <- strptime(as.character(data_1$Date), "%d/%m/%Y")
  data_1<- data_1 %>%
    separate(Date, sep="-", into = c("Year", "Month", "Day"))
  data_1<- data_1 %>%
    separate(Time, sep=":", into = c("Hour", "Min"))
  data_1<-  data_1 %>% 
    mutate(DateTime2 = make_datetime(Year, Month, Day, Hour, Min)) 
  data_1<-data_1 %>% thicken('hour')
  data_1 <- data_1[ -c(10) ]
  data_1<-data_1 %>% pad('hour')
  data_1$DateTime2 <- data_1$DateTime2_hour
  data_1$DateTime <- data_1$DateTime2_hour
  data_1<- data_1 %>%
    separate(DateTime, sep=" ", into = c("Date", "Time"))
  data_1<- data_1 %>%
    separate(Date, sep="-", into = c("Year", "Month", "Day"))
  data_1<- data_1 %>%
    separate(Time, sep=":", into = c("Hour", "Min", "Sec"))
  
  data_1 <- data_1[!duplicated(data_1$DateTime2_hour),]
  data_1 <- transform(data_1, 
                      Latitude.deg. = na.approx(Latitude.deg., DateTime2_hour, na.rm=FALSE),
                      Longitude.deg. = na.approx(Longitude.deg., DateTime2_hour, na.rm=FALSE), 
                      Hour = na.approx(Hour, DateTime2_hour, na.rm=FALSE))
  data_1$Collar.ID<-All_Deer.Unique_IDs[i]
  output <-  rbind(output, data_1)
}


All_Deer_Ordered<-output[order(output$DateTime2_hour ),]
  All_Deer_Ordered.sf <- st_as_sf(All_Deer_Ordered, 
                                coords = c("Longitude.deg.", "Latitude.deg."), 
                                crs = 4326) # WGS 84 Coordinate System
st_write(All_Deer_Ordered.sf, dsn = "~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/", layer = "All.Deer.NotPadded", driver = "ESRI Shapefile", overwrite_layer = TRUE)

  

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#Finding the staring and ending points for each----
All.Deer.Individual.Time<-All_Deer_Ordered %>% group_by(Collar.ID) %>% 
    summarise(Begin_Time = first(DateTime2),
              End_Time = last(DateTime2),  .groups = 'drop')

All.Deer.Individual.Time.Output<-cbind(All.Deer.Individual.Time$Collar.ID,  as.character(All.Deer.Individual.Time$Begin_Time), as.character(All.Deer.Individual.Time$End_Time))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#1 2/3 years----
#For deer with > 1 2/3 years worth of data, we create two independent movement tracks. For the second year, each deer repeats where they were on year 1 once they run out of data   
Under.2Years.IDS<-c("43283","43286","43288", "43289", "43290", "43291", "43292", "46082", "22428","27895","27898" )

output<-c()
for (i in 1:length(Under.2Years.IDS)){
  data_1 <- subset(All_Deer_Ordered, Collar.ID == Under.2Years.IDS[i])  
  #  data_1 <- subset(All_Deer_Ordered, Collar.ID == 46082)

  min_date<-min(data_1$DateTime2_hour)
  data_1$MonthDayYear<-format(data_1$DateTime2_hour, format="%y-%m-%d")
  data_1$MonthDayYearHour<-format(data_1$DateTime2_hour, format="%y-%m-%d %H")
 #This is to get rid of start hours ahead of the min day
   min_date2<-min(data_1$MonthDayYear)
   data_1<-subset(data_1, MonthDayYear > min_date2)
#Get rid of lead day 
   data_1$MonthDay<-format(data_1$DateTime2_hour, format="%m-%d")
   data_1<-subset(data_1, MonthDay != "02-29" )
#Subset each year of data 
  t_year1<-as.numeric(min(data_1$Year))
  t_year2<-t_year1 + 1
  Year_1<-subset(data_1, Year == t_year1)
  Year_2<-subset(data_1, Year == t_year2)
#extract and finalize Year 1 data
  Year_1_Start<-subset(Year_1, MonthDay >=  "03-11")
  Year_2_To_Add_Yr1<-subset(Year_2, MonthDay < "03-11")
  Year_1_New<-rbind(Year_1_Start, Year_2_To_Add_Yr1)
  Year_1_New$New_AID<- Under.2Years.IDS[i]
  Year_1_New$New_AID<- paste0(Year_1_New$New_AID, ".1" )
  Year_1_New$New_Datetime <- seq(as_datetime("2020-03-11"), length = 8760, by = "hours")
  Year_1_New.df<-data.frame(Year = as.numeric(format(Year_1_New$New_Datetime, "%Y")), Month = as.numeric(format(Year_1_New$New_Datetime, "%m")), Day = as.numeric(format(Year_1_New$New_Datetime, "%d")),    Hour = as.numeric(format(Year_1_New$New_Datetime, "%H")),  DateTime = Year_1_New$New_Datetime, New_AID = Year_1_New$New_AID )
  listcoords_data_1 <- lapply(split(Year_1_New, Year_1_New$DateTime2_hour), function(x) cbind(x$Longitude.deg., x$Latitude.deg))
  spmdf_Year_1<- SpatialMultiPointsDataFrame(listcoords_data_1, data.frame(Year_1_New.df, count = sapply(listcoords_data_1, nrow), stringsAsFactors = FALSE))
  proj4string(spmdf_Year_1 ) <- CRS("+proj=longlat +datum=WGS84") 
  spmdf_Yr1<- st_as_sf(spmdf_Year_1)
  
  #now, year 2  
  Year_2_Start<-subset(Year_2, MonthDay >= "03-11")
  End_of_Year_2_Data<-max(Year_2$DateTime2_hour)
  year(End_of_Year_2_Data) <- t_year1
  Fill_In_Year_2<-subset(Year_1, DateTime2_hour > End_of_Year_2_Data )
  year(Fill_In_Year_2$DateTime2_hour) <- t_year2
  year(Fill_In_Year_2$DateTime2) <- t_year2
  Fill_In_Year_2$MonthDayYear<-format(Fill_In_Year_2$DateTime2_hour, format="%y-%m-%d")
  Fill_In_Year_2$MonthDayYearHour<-format(Fill_In_Year_2$DateTime2_hour, format="%y-%m-%d %H")
  Fill_In_Year_2$Year<-t_year2
  Add_Year_3_Start<-subset(Year_2, MonthDay < "03-11")
  t_year3 <- t_year2 + 1
  year(Add_Year_3_Start$DateTime2_hour) <- t_year3
  year(Add_Year_3_Start$DateTime2) <- t_year3
 Add_Year_3_Start$MonthDayYear<-format(Add_Year_3_Start$DateTime2_hour, format="%y-%m-%d")
  Add_Year_3_Start$MonthDayYearHour<-format(Add_Year_3_Start$DateTime2_hour, format="%y-%m-%d %H")
  Add_Year_3_Start$Year<-t_year3
  Year_2_New<-rbind(Year_2_Start,Fill_In_Year_2, Add_Year_3_Start)
  Year_2_New$New_Datetime <- seq(as_datetime("2020-03-11"), length = 8760, by = "hours")
   Year_2_New$New_AID<- Under.2Years.IDS[i]
  Year_2_New$New_AID<- paste0(Year_2_New$New_AID, ".2" )
    Year_2_New.df<-data.frame(Year = as.numeric(format(Year_2_New$New_Datetime, "%Y")), Month = as.numeric(format(Year_2_New$New_Datetime, "%m")), Day = as.numeric(format(Year_2_New$New_Datetime, "%d")),    Hour = as.numeric(format(Year_2_New$New_Datetime, "%H")),  DateTime = Year_2_New$New_Datetime, New_AID = Year_2_New$New_AID )
  listcoords_data_2 <- lapply(split(Year_2_New, Year_2_New$DateTime2_hour), function(x) cbind(x$Longitude.deg., x$Latitude.deg))
  spmdf_Year_2<- SpatialMultiPointsDataFrame(listcoords_data_2, data.frame(Year_2_New.df, count = sapply(listcoords_data_2, nrow), stringsAsFactors = FALSE))
  proj4string(spmdf_Year_2 ) <- CRS("+proj=longlat +datum=WGS84") 
  spmdf_Yr2<- st_as_sf(spmdf_Year_2)
  Stacked<-rbind(spmdf_Yr1, spmdf_Yr2 )
  output <-  rbind(output, Stacked)
}

NotQuite.2years<-output[order(output$Year, output$Month,output$Day,output$Hour ),]

#st_write(output, dsn = "~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/", layer = "All.Deer.Output_TEST", driver = "ESRI Shapefile", overwrite_layer = TRUE)



#under 1 year ----
#For the deer with not quite a full year of data, they back track along their movement track to fill out the year
NotQuite.1Year.IDs<-c("47020", "43284", "47977", "27900", "47978", "47979", "22422", "22435")
output<-c()
for (i in 1:length(NotQuite.1Year.IDs)){
  data_1 <- subset(All_Deer_Ordered, Collar.ID == NotQuite.1Year.IDs[i])
 #   data_1 <- subset(All_Deer_Ordered, Collar.ID == 47977)
    
  min_date<-min(data_1$DateTime2_hour)
  data_1$MonthDayYear<-format(data_1$DateTime2_hour, format="%y-%m-%d")
  data_1$MonthDayYearHour<-format(data_1$DateTime2_hour, format="%y-%m-%d %H")
 #This is to get rid of start hours ahead of the min day
   min_date2<-min(data_1$MonthDayYear)
   data_1<-subset(data_1, MonthDayYear > min_date2)
#Get rid of lead day 
   data_1$MonthDay<-format(data_1$DateTime2_hour, format="%m-%d")
   data_1<-subset(data_1, MonthDay != "02-29" )  
#Subset March - End of Data
    data_1<-subset(data_1, MonthDay >=  "03-11")
#Getting Backtrack data
  data_1$New_Datetime <- seq(as_datetime("2020-03-11"), length = nrow(data_1), by = "hours")
  Should_End_Time<-as_datetime("2021-03-10 22:00:00")
  End_of_time<-max(data_1$New_Datetime)
  End_of_time_MonthDay<-max(data_1$MonthDay)
  if(hour(End_of_time) < 23){
    Actual_End_of_Year_1_Data<-as_datetime(End_of_time - hours(hour(End_of_time)),tz = "EST")
    data_1<-subset(data_1, MonthDay < End_of_time_MonthDay )
  }
  #not sure this works because cannot test 
  if(hour(End_of_time) == 23){
    Actual_End_of_Year_1_Data<-max(data_1$New_Datetime)
    data_1<-subset(data_1, MonthDay < End_of_time_MonthDay)
  }
  Actual_End_of_Year_1_Data<-max(data_1$New_Datetime)
  Time_Diff<-difftime(Should_End_Time, Actual_End_of_Year_1_Data, units="hours")
  new.dt <- Actual_End_of_Year_1_Data - as.difftime(Time_Diff, unit="hours")
  Year_3_Filler <- subset(data_1, New_Datetime >= new.dt )
  Start_Filler<- Actual_End_of_Year_1_Data + hours(1)
  Year_3_Filler$New_Datetime <- seq(as_datetime(Start_Filler), length = nrow(Year_3_Filler), by = "hours")
  Year_1_New<-rbind(data_1,  Year_3_Filler)
  Year_1_New$New_Datetime <- seq(as_datetime("2020-03-11"), length = nrow(Year_1_New), by = "hours")
  Year_1_New$New_AID<- NotQuite.1Year.IDs[i]
  Year_1_New$New_AID<- paste0(Year_1_New$New_AID)
  Year_1_New.df<-data.frame(Year = as.numeric(format(Year_1_New$New_Datetime, "%Y")), Month = as.numeric(format(Year_1_New$New_Datetime, "%m")), Day = as.numeric(format(Year_1_New$New_Datetime, "%d")),    Hour = as.numeric(format(Year_1_New$New_Datetime, "%H")),  DateTime = Year_1_New$New_Datetime, New_AID = Year_1_New$New_AID )
  listcoords_Year2 <- lapply(split(Year_1_New, Year_1_New$New_Datetime), function(x) cbind(x$Longitude.deg., x$Latitude.deg))
  spmdf_Year_1 <- SpatialMultiPointsDataFrame(listcoords_Year2, data.frame(Year_1_New.df, count = sapply(listcoords_Year2, nrow), stringsAsFactors = FALSE))
  proj4string(spmdf_Year_1 ) <- CRS("+proj=longlat +datum=WGS84") 
  spmdf_Yr1<- st_as_sf(spmdf_Year_1)
  output <-  rbind(output, spmdf_Yr1)
}


NotQuite.1Year<-output[order(output$Year, output$Month,output$Day,output$Hour ),]


#output----
All.Deer.Output<-rbind(NotQuite.2years, NotQuite.1Year)
All.Deer.Output<-All.Deer.Output[order(All.Deer.Output$Year, All.Deer.Output$Month, All.Deer.Output$Day,All.Deer.Output$Hour, All.Deer.Output$New_AID),]
st_write(All.Deer.Output, dsn = "~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/", layer = "All.Deer.Output_Dec2024_NewDate", driver = "ESRI Shapefile", overwrite_layer = TRUE)


#resident deer -----
Resident.Deer.Output<-All.Deer.Output[All.Deer.Output$New_AID %in% c(22428.1, 22428.2, 27895.1,  27895.2, 27898.1, 27898.2, 43286.1, 43286.2, 43288.1, 43288.2, 43290.1, 43290.2, 43291.1, 43291.2, 22422, 47978, 47979),]
st_write(Resident.Deer.Output, dsn = "~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/", layer = "Resident.Deer_Feb2025", driver = "ESRI Shapefile", overwrite_layer = TRUE)


#Migratory deer -----

Migratory.Deer.Output<-All.Deer.Output[All.Deer.Output$New_AID %in% c(22422, 22435, 27900, 43283.1, 43283.2, 43284, 43289.1, 43289.2, 43292.1,43292.2, 46082.1, 46082.2, 47977),]
st_write(Migratory.Deer.Output, dsn = "~/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Deer Data for Model_GPS and Parameters/", layer = "Migraoty.Deer_Feb2025", driver = "ESRI Shapefile", overwrite_layer = TRUE)

