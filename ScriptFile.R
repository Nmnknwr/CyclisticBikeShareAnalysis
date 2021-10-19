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

source("/Users/namankanwar/Case Studys/CyclisticBikeShareAnalysis/RProject/functions.R")

getwd()
setwd("/Users/namankanwar/Case Studys/CyclisticBikeShareAnalysis/Data/CSVs")
dest_path = getwd()

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

source(download())

###################################################################################################################
####################### Unzipping files  ##########################################################################
###################################################################################################################
source(unzip_all())

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

source(organise_all())

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
  assign(name,read_csv(files_unzipped_df[i,1]))
}

###################################################################################################################
####################### Shaping each to same structure, naming and class, combining per year ######################
###################################################################################################################

## 2014 -
source(combine_year(2014))

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

## 2015 -
source(combine_year(2015))

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

## 2016 -
source(combine_year(2016))

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

## 2017 -
source(combine_year(2017))

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

source(combine_year(2018))

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

source(combine_year(2019))

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

source(combine_year(2020))

## 2021 -
source(combine_year(2021))


###################################################################################################################
####################### Combining into single data frame ##########################################################
###################################################################################################################

list_df <- mget(ls(pattern = glob2rx("all_*")))
MasterDF <- bind_rows(list_df)

###################################################################################################################
####################### Deleting separate data frames #############################################################
###################################################################################################################

rm(list = ls()[grepl("csv", ls())])
rm(list = ls()[grepl("all_", ls())])

###################################################################################################################
####################### Cleaning, precpocessing, adding new columns ###############################################
###################################################################################################################

MasterDF_clean <- clean_preprocess(MasterDF)

