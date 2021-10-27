### Cyclistic_Exercise_Analysis ###

# This analysis is for case study 1 from the Google Data Analytics Certificate (Cyclistic).  It’s originally based on the case study "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study). We will be using the Divvy dataset for the case study.
# The purpose of this script is to consolidate downloaded Divvy data into a single dataframe and then conduct simple analysis to help answer the key question: “In what ways do members and casual riders use Divvy bikes differently?”

# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# libridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # # 
library("tidyverse")
library("ggplot2")
library("lubridate")
library("geosphere")
library("gridExtra") 
library("ggmap")
library("data.table")
library("XML")
library("httr")
library("stringi")
library("aws.s3")
library("stringr")
library("dplyr")
library("pbapply")
library("xfun")

source("functions.R")

## When working on my mac
dest_path <- "/Users/namankanwar/Case Studys/CyclisticBikeShareAnalysis/Data"

## When working on my PC
## dest_path <- "I:/Work/Datasets/Cyclistic"


###################################################################################################################
####################### Making Zipped files df with filename and DownloadURL ######################################
###################################################################################################################

files_zipped_df <- get_bucket_df("divvy-tripdata") %>% 
  filter(str_detect(Key,".zip")) %>% 
  select(1) 
files_zipped_df <- files_zipped_df %>% 
  mutate(DownloadURL = paste("https://divvy-tripdata.s3.amazonaws.com",Key,sep="/"))
colnames(files_zipped_df) <- c("Filename","DownloadURL")

###################################################################################################################
####################### Downloading all zip files from aws s3 bucket ##############################################
###################################################################################################################

download()

###################################################################################################################
####################### Unzipping files  ##########################################################################
###################################################################################################################

unzip_all()

###################################################################################################################
####################### Making Unzipped files df with filename and path ###########################################
###################################################################################################################

files_unzipped_df <- as.data.frame(list.files(dest_path,recursive = TRUE,full.names = TRUE))
colnames(files_unzipped_df) <- c("FilePath")
files_unzipped_df <- files_unzipped_df %>% 
  mutate(Filename = basename(FilePath))

###################################################################################################################
####################### Deleting unwanted files/folders, adding extensions ########################################
###################################################################################################################


organise_all()


###################################################################################################################
####################### Updating Unzipped files df with filename and path #########################################
###################################################################################################################

files_unzipped_df <- as.data.frame(list.files(dest_path,recursive = TRUE,full.names = TRUE))
colnames(files_unzipped_df) <- c("FilePath")
files_unzipped_df <- files_unzipped_df %>% 
  mutate(Filename = basename(FilePath))

###################################################################################################################
####################### Loading all files to data frames, making of list of all dataframes ########################
###################################################################################################################

for(i in 1:nrow(files_unzipped_df)) {
  name <- paste(files_unzipped_df[i,2],i,sep="_")
  assign(name,fread(files_unzipped_df[i,1]))
}

###################################################################################################################
####################### Shaping each to same structure, naming and class, combining per year ######################
###################################################################################################################

## 2014 -

combine_year(2014)


all_2014 <- all_2014 %>% 
  rename(ride_id = trip_id,
         started_at = starttime,
         ended_at = stoptime,
         start_station_id = from_station_id,
         start_station_name = from_station_name,
         end_station_id = to_station_id,
         end_station_name = to_station_name,
         member_casual = usertype) %>% 
  mutate(started_at = strptime(started_at,"%m/%d/%Y %H:%M"),
         ended_at = strptime(ended_at,"%m/%d/%Y %H:%M"),
         ride_id = as.character(ride_id),
         start_station_id = as.character(start_station_id),
         end_station_id = as.character(end_station_id))

cols_to_del <- c(4,5)

all_2014 <- all_2014[,(cols_to_del):=NULL]

## 2015 -


combine_year(2015)


all_2015 <- all_2015 %>% 
  rename(ride_id = trip_id,
         started_at = starttime,
         ended_at = stoptime,
         start_station_id = from_station_id,
         start_station_name = from_station_name,
         end_station_id = to_station_id,
         end_station_name = to_station_name,
         member_casual = usertype) %>% 
  mutate(started_at = strptime(started_at,"%m/%d/%Y %H:%M"),
         ended_at = strptime(ended_at,"%m/%d/%Y %H:%M"),
         ride_id = as.character(ride_id),
         start_station_id = as.character(start_station_id),
         end_station_id = as.character(end_station_id))

cols_to_del <- c(4,5)

all_2015 <- all_2015[,(cols_to_del):=NULL]

## 2016 -

combine_year(2016)

all_2016 <- all_2016 %>% 
  rename(ride_id = trip_id,
         started_at = starttime,
         ended_at = stoptime,
         start_station_id = from_station_id,
         start_station_name = from_station_name,
         end_station_id = to_station_id,
         end_station_name = to_station_name,
         member_casual = usertype) %>% 
  mutate(started_at = strptime(started_at,"%m/%d/%Y %H:%M"),
         ended_at = strptime(ended_at,"%m/%d/%Y %H:%M"),
         ride_id = as.character(ride_id),
         start_station_id = as.character(start_station_id),
         end_station_id = as.character(end_station_id))

cols_to_del <- c(4,5)

all_2016 <- all_2016[,(cols_to_del):=NULL]

## 2017 -


combine_year(2017)


all_2017 <- all_2017 %>% 
  rename(ride_id = trip_id,
         started_at = start_time,
         ended_at = end_time,
         start_station_id = from_station_id,
         start_station_name = from_station_name,
         end_station_id = to_station_id,
         end_station_name = to_station_name,
         member_casual = usertype) %>% 
  mutate(started_at = strptime(started_at,"%m/%d/%Y %H:%M"),
         ended_at = strptime(ended_at,"%m/%d/%Y %H:%M"),
         ride_id = as.character(ride_id),
         start_station_id = as.character(start_station_id),
         end_station_id = as.character(end_station_id))

cols_to_del <- c(4,5)

all_2017 <- all_2017[,(cols_to_del):=NULL]

## 2018 -
Divvy_Trips_2018_Q1.csv_39 <- Divvy_Trips_2018_Q1.csv_39 %>%
  rename(trip_id = "01 - Rental Details Rental ID",
         bikeid = "01 - Rental Details Bike ID",
         tripduration = "01 - Rental Details Duration In Seconds Uncapped",
         gender = "Member Gender",
         birthyear = "05 - Member Details Member Birthday Year",
         start_time = "01 - Rental Details Local Start Time",
         end_time = "01 - Rental Details Local End Time",
         from_station_id = "03 - Rental Start Station ID",
         to_station_id = "02 - Rental End Station ID",
         usertype = "User Type",
         from_station_name = "03 - Rental Start Station Name",
         to_station_name = "02 - Rental End Station Name")


combine_year(2018)



all_2018 <- all_2018 %>% 
  rename(ride_id = trip_id,
         started_at = start_time,
         ended_at = end_time,
         start_station_id = from_station_id,
         start_station_name = from_station_name,
         end_station_id = to_station_id,
         end_station_name = to_station_name,
         member_casual = usertype) %>% 
  mutate(ride_id = as.character(ride_id),
         start_station_id = as.character(start_station_id),
         end_station_id = as.character(end_station_id))

cols_to_del <- c(4,5)

all_2018 <- all_2018[,(cols_to_del):=NULL]


## 2019 -

Divvy_Trips_2019_Q2.csv_44 <- Divvy_Trips_2019_Q2.csv_44 %>% 
  rename(trip_id = "01 - Rental Details Rental ID",
         bikeid = "01 - Rental Details Bike ID",
         tripduration = "01 - Rental Details Duration In Seconds Uncapped",
         gender = "Member Gender",
         birthyear = "05 - Member Details Member Birthday Year",
         start_time = "01 - Rental Details Local Start Time",
         end_time = "01 - Rental Details Local End Time",
         from_station_id = "03 - Rental Start Station ID",
         to_station_id = "02 - Rental End Station ID",
         usertype = "User Type",
         from_station_name = "03 - Rental Start Station Name",
         to_station_name = "02 - Rental End Station Name")


combine_year(2019)

all_2019 <- all_2019 %>% 
  rename(ride_id = trip_id,
         started_at = start_time,
         ended_at = end_time,
         start_station_id = from_station_id,
         start_station_name = from_station_name,
         end_station_id = to_station_id,
         end_station_name = to_station_name,
         member_casual = usertype) %>% 
  mutate(ride_id = as.character(ride_id),
         start_station_id = as.character(start_station_id),
         end_station_id = as.character(end_station_id))

cols_to_del <- c(4,5)

all_2019 <- all_2019[,(cols_to_del):=NULL]


## 2020 -
Divvy_Trips_2020_Q1.csv_47 <- mutate(Divvy_Trips_2020_Q1.csv_47,start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
`202004-divvy-tripdata.csv_1` <- mutate(`202004-divvy-tripdata.csv_1`,start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
`202005-divvy-tripdata.csv_2` <- mutate(`202005-divvy-tripdata.csv_2`,start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
`202006-divvy-tripdata.csv_3` <- mutate(`202006-divvy-tripdata.csv_3`,start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
`202007-divvy-tripdata.csv_4` <- mutate(`202007-divvy-tripdata.csv_4`,start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
`202008-divvy-tripdata.csv_5` <- mutate(`202008-divvy-tripdata.csv_5`,start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
`202009-divvy-tripdata.csv_6` <- mutate(`202009-divvy-tripdata.csv_6`,start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
`202010-divvy-tripdata.csv_7` <- mutate(`202010-divvy-tripdata.csv_7`,start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
`202011-divvy-tripdata.csv_8` <- mutate(`202011-divvy-tripdata.csv_8`,start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))


combine_year(2020)

cols_to_del <- c(2)

all_2020 <- all_2020[,(cols_to_del):=NULL]


## 2021 -
combine_year(2021)

cols_to_del <- c(2)

all_2021 <- all_2021[,(cols_to_del):=NULL]
###################################################################################################################
####################### Combining into single data frame, Deleting separate data frames ###########################
###################################################################################################################

rm(list = ls()[grepl("csv", ls())])

list_dt <- mget(ls(pattern = glob2rx("all_*")))

MasterDT <- bind_rows(list_dt)

rm(list = ls()[grepl("all_", ls())])

gc()

###################################################################################################################
####################### Cleaning, precpocessing, adding new columns ###############################################
###################################################################################################################

MasterDT <- clean_preprocess_1(MasterDT)

MasterDT <- clean_preprocess_2(MasterDT)

MasterDT <- clean_preprocess_3(MasterDT)

MasterDT <- clean_preprocess_4(MasterDT)

MasterDT <- clean_preprocess_5(MasterDT)

MasterDT <- clean_preprocess_6(MasterDT)

MasterDT <- clean_preprocess_7(MasterDT)

gc()

############################################################

### Analyze 


## Tables/counts

# Count of rides, Average ride duration - per customer type - overall
MasterDT %>% 
  filter(start_lng != end_lng & start_lat != end_lat) %>% 
  group_by(member_casual) %>% 
  summarise(rides = n(), 
            avg_ride_time = mean(Ride_Time, na.rm = TRUE))


# Count of rides, Average ride duration - per customer type - per day of the week

MasterDT %>%
  group_by(member_casual,DayOfWeek) %>%
  summarise(Rides = n(), Avg_Ride_time = mean(Ride_Time)/60) %>%
  arrange(member_casual,DayOfWeek)


# Count of rides, Average ride duration - per customer type - per month

MasterDT %>%
  group_by(member_casual,Month) %>%
  summarise(Rides = n(), Avg_Ride_time = mean(Ride_Time)/60) %>%
  arrange(Month)

# Count of rides, Average ride duration - per customer type - per Year

MasterDT %>%
  group_by(member_casual,Year) %>%
  summarise(Rides = n(), Avg_Ride_time = mean(Ride_Time)/60) %>%
  arrange(Year)


# Count of rides, Average ride duration - per Starting Station - overall
MasterDT %>%
  group_by(start_station_id,start_station_name) %>% 
  summarise(Rides = n(), Avg_Ride_time = mean(Ride_Time)/60) %>% 
  arrange(desc(Rides))


# Casual & Member Share per Day of Week
MasterDT %>% 
  group_by(DayOfWeek) %>% 
  summarise(Member_Ride_Share = round((sum(member_casual=="Member"))/n()*100,0),
            Casual_Ride_Share = round((sum(member_casual=="Casual"))/n()*100,0))%>% 
  arrange(desc(n()))


# Casual & Member share per Station
MasterDT %>% 
  group_by(start_station_id,start_station_name) %>% 
  summarise(Member_Ride_Share = round((sum(member_casual=="Member"))/n()*100,0),
            Casual_Ride_Share = round((sum(member_casual=="Casual"))/n()*100,0))%>% 
  arrange(desc(n()))

# Casual & Member share per year
MasterDT %>% 
  group_by(Year) %>% 
  summarise(MemberShare = (sum(member_casual=="Member")/n())*100,
            CasualShare = (sum(member_casual=="Casual")/n())*100) %>% 
  arrange(desc(n()))


# Casual & Member share per month
MasterDT %>% 
  group_by(Month) %>% 
  summarise(MemberShare = (sum(member_casual=="Member")/n())*100,
            CasualShare = (sum(member_casual=="Casual")/n())*100) %>% 
  arrange(desc(n()))



# Average age of members per gender
MasterDT %>% 
  filter(member_casual!="Casual" & gender != "NA") %>% 
  group_by(gender) %>% 
  summarise(Average_age = mean(2021-birthyear,na.rm=TRUE))

# Plots

# Count of rides per customer type over time - line -- shows seasonality
MasterDT %>% 
  group_by(Date,member_casual) %>% 
  summarise(Rides =  n()) %>% 
  ggplot(aes(x=Date,y=Rides,group=member_casual,color=member_casual)) +
  geom_line() 


# Avg ride time per customer type overall
MasterDT %>% 
  filter(start_lng != end_lng & start_lat != end_lat) %>% 
  group_by(member_casual) %>% 
  summarise(rides = n(), 
            avg_ride_time = mean(Ride_Time, na.rm = TRUE)) %>% 
  ggplot() + 
  geom_col(aes(x=member_casual,y=avg_ride_time/60,fill=member_casual), show.legend = FALSE) +
  labs(title = "Avg ride time by User type",x="User Type",y="Avg time in mins")


# Count of rides - per customer type per day of week - line graph with points
MasterDT %>% 
  group_by(DayOfWeek,member_casual) %>% 
  summarise(Rides=n()) %>% 
  ggplot(aes(x=DayOfWeek,y=Rides,group=member_casual,color=member_casual)) + 
  geom_line() +
  geom_point()

# Avg Ride time - per customer type per day of week - line graph with points
MasterDT %>% 
  group_by(member_casual,DayOfWeek) %>%
  summarise(Rides = n(), Avg_Ride_time = mean(Ride_Time)/60) %>%
  arrange(member_casual,DayOfWeek) %>% 
  ggplot(aes(x=DayOfWeek,y=Avg_Ride_time,group=member_casual,color=member_casual)) + 
  geom_line() +
  geom_point()

# Count of rides - per customer type per month - line graph with points
MasterDT %>% 
  group_by(member_casual,Month) %>% 
  summarise(Rides = n()) %>% 
  ggplot(aes(x=Month,y=Rides,group=member_casual,color=member_casual)) +
  geom_line() +
  geom_point()


# Avg Ride time - per customer type per month - line graph with points
MasterDT %>% 
  group_by(member_casual,Month) %>%
  summarise(Avg_Ride_time = mean(Ride_Time)/60) %>%
  ggplot(aes(x=Month,y=Avg_Ride_time,group=member_casual,color=member_casual)) + 
  geom_line() +
  geom_point()

# Avg Ride time - per customer type per Year - line graph with points
MasterDT %>% 
  group_by(member_casual,Year) %>%
  summarise(Avg_Ride_time = mean(Ride_Time)/60) %>%
  ggplot(aes(x=Year,y=Avg_Ride_time,group=member_casual,color=member_casual)) + 
  geom_line() +
  geom_point()

# Count of rides - per customer type per Year - line graph with points
MasterDT %>% 
  filter(Year!=2021) %>% 
  group_by(member_casual,Year) %>%
  summarise(Rides = n()) %>%
  ggplot(aes(x=Year,y=Rides,group=member_casual,color=member_casual)) + 
  geom_line() +
  geom_point()


# % split of total rides by user type per year
MasterDT %>% 
  group_by(Year) %>% 
  summarise(Member = (sum(member_casual=="Member")/n())*100,
            Casual = (sum(member_casual=="Casual")/n())*100) %>% 
  gather(key = User_type, value = Perc_Share,Member,Casual) %>% 
  ggplot(aes(x=Year,y=Perc_Share,group=User_type,color=User_type)) +
  geom_point() +
  geom_line() +
  labs(title = "% split of rides by user type per year",x="Year",y="(%)")


# % split of total rides by user type per Day of week
MasterDT %>% 
  group_by(DayOfWeek) %>% 
  summarise(Member = (sum(member_casual=="Member")/n())*100,
            Casual = (sum(member_casual=="Casual")/n())*100) %>% 
  gather(key = User_type, value = Perc_Share,Member,Casual) %>% 
  ggplot(aes(x=DayOfWeek,y=Perc_Share,group=User_type,color=User_type)) +
  geom_point() +
  geom_line() +
  labs(title = "% split of rides by user type per day of week",x="Day of Week",y="(%)")


# % split of total rides by user type per month
MasterDT %>% 
  group_by(Month) %>% 
  summarise(Member = (sum(member_casual=="Member")/n())*100,
            Casual = (sum(member_casual=="Casual")/n())*100) %>% 
  gather(key = User_type, value = Perc_Share,Member,Casual) %>% 
  ggplot(aes(x=Month,y=Perc_Share,group=User_type,color=User_type)) +
  geom_point() +
  geom_line() +
  labs(title = "% split of rides by user type per month",x="Month",y="(%)")



# Member to causal Rides ratio per year
MasterDT %>% 
  group_by(Year) %>% 
  summarise(Ratio = sum(member_casual=="Member")/sum(member_casual=="Casual")) %>% 
  ggplot(aes(x=Year,y=Ratio,group=1)) +
  geom_point() +
  geom_line() +
  labs(title = "Member/Causal Rides Ratio per Year",x="Year",y="Member/Causal Rides Ratio")

# Member to causal Rides ratio per month
MasterDT %>% 
  group_by(Month) %>% 
  summarise(Ratio = sum(member_casual=="Member")/sum(member_casual=="Casual")) %>% 
  ggplot(aes(x=Month,y=Ratio,group=1)) +
  geom_point() +
  geom_line() +
  labs(title = "Member/Causal Rides Ratio per Month",x="Month",y="Member/Causal Rides Ratio")


# Member to causal Rides ratio per Day of week
MasterDT %>% 
  group_by(DayOfWeek) %>% 
  summarise(Ratio = sum(member_casual=="Member")/sum(member_casual=="Casual")) %>% 
  ggplot(aes(x=DayOfWeek,y=Ratio,group=1)) +
  geom_point() +
  geom_line() + 
  labs(title = "Member/Causal Rides Ratio per Day of Week",x="DayOfWeek",y="Member/Causal Rides Ratio")


# Average age of members per gender per year
MasterDT %>% 
  filter(member_casual!="Casual" & gender != "NA") %>%
  group_by(Year,gender) %>% 
  summarise(Average_age = mean(2021-birthyear,na.rm=TRUE)) %>% 
  ggplot(aes(x=Year,y=Average_age,group=gender,color=gender)) +
  geom_point() +
  geom_line()

# % split of Member gender per year
MasterDT %>% 
  group_by(Year) %>% 
  filter(member_casual!="Casual" & gender != "NA") %>%
  summarise(Male = (sum(gender=="Male")/n())*100,
            Female = (sum(gender=="Female")/n()*100)) %>% 
  gather(key = gender, value = Perc_Share,Male,Female) %>% 
  ggplot(aes(x=Year,y=Perc_Share,group=gender,color=gender)) +
  geom_point() +
  geom_line() 


###################################################################################
##ROUGH
###################################################################################

# Create new data frame with post popular routes

Fav_stations <- MasterDT %>% 
  filter(start_lng != end_lng & start_lat != end_lat) %>%
  group_by(start_lng, start_lat, end_lng, end_lat, member_casual) %>%
  summarise(Rides = n()) %>% 
  arrange(desc(Rides))

# Splitting this into 2 as per user type

Fav_station_Casual <- Fav_stations %>% 
  filter(member_casual=="Casual") %>% 
  head(250)

Fav_station_Member <- Fav_stations %>% 
  filter(member_casual=="Member") %>% 
  head(250)

#Lets store bounding box coordinates for ggmap:
bb <- c(
  left = -87.700424,
  bottom = 41.790769,
  right = -87.554855,
  top = 41.990119
)

#Here we store the map
map <- get_stamenmap(
  bbox = bb,
  zoom = 15,
  maptype = "toner"
)

#Then we plot the data on the map

ggmap(map,darken = c(0.8, "white")) +
  geom_curve(Fav_station_Casual,mapping=aes(x=start_lng,y=start_lat,xend=end_lng,yend=end_lat,alpha=Rides,color="red"),size=0.5,curvature = 0.2,arrow = arrow(length = unit(0.2,"cm"),ends = "first",type = "closed")) +
  coord_cartesian() +
  labs(title = "Most popular routes by casual users",x=NULL,y=NULL) +
  theme(legend.position="none")

ggmap(map,darken = c(0.8, "white")) +
  geom_curve(Fav_station_Member,mapping=aes(x=start_lng,y=start_lat,xend=end_lng,yend=end_lat,alpha=Rides),size=0.5,curvature = 0.2,arrow = arrow(length = unit(0.2,"cm"),ends = "first",type = "closed")) +
  coord_cartesian() +
  labs(title = "Most popular routes by Members",x=NULL,y=NULL) +
  theme(legend.position="none")
