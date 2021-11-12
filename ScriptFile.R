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
library("skimr")

source("functions.R")

## When working on my mac
 dest_path <- "/Users/namankanwar/Case Studys/CyclisticBikeShareAnalysis/Data"

## When working on my PC
# dest_path <- "I:/Work/Datasets/Cyclistic"


###################################################################################################################
####################### Making Zipped files df (Only taking 2019 onwards data) ####################################
###################################################################################################################

files_zipped_df <- get_bucket_df("divvy-tripdata") %>% 
  filter(str_detect(Key,".zip")) %>% 
  select(1) 

files_zipped_df <- files_zipped_df %>% 
  mutate(DownloadURL = paste("https://divvy-tripdata.s3.amazonaws.com",Key,sep="/"),
         FilePath = paste(dest_path,Key,sep = "/"))

         
  

colnames(files_zipped_df) <- c("Filename","DownloadURL","FilePath")

years_to_include <- c("2019","2020","2021")

files_zipped_df <- files_zipped_df %>% 
  filter(str_detect(Filename,paste(years_to_include,collapse = "|")))

 
###################################################################################################################
####################### Downloading all zip files from aws s3 bucket ##############################################
###################################################################################################################

download()


###################################################################################################################
####################### Unzipping All #############################################################################
###################################################################################################################

unzip_files()


###################################################################################################################
####################### Making unzipped files df ##################################################################
###################################################################################################################

files_unzipped_df <- as.data.frame(list.files(dest_path,recursive = TRUE,full.names = TRUE))
colnames(files_unzipped_df) <- c("FilePath")
files_unzipped_df <- files_unzipped_df %>% 
  mutate(Filename = basename(FilePath))

###################################################################################################################
####################### Organise all ##############################################################################
###################################################################################################################

organise()

###################################################################################################################
####################### Updating unzipped files df ################################################################
###################################################################################################################

files_unzipped_df <- as.data.frame(list.files(dest_path,recursive = TRUE,full.names = TRUE))
colnames(files_unzipped_df) <- c("FilePath")
files_unzipped_df <- files_unzipped_df %>% 
  mutate(Filename = basename(FilePath))

###################################################################################################################
####################### Loading all files to data tables ##########################################################
###################################################################################################################

load_2019()

load_2020_2021()

###################################################################################################################
####################### Combining into single data table, Deleting individual ones and other objects ##############
###################################################################################################################

list_dt <- mget(ls(pattern = glob2rx("all_*")))

MasterDT <- bind_rows(list_dt)

rm(list_dt,dest_path,years_to_include)

rm(list = ls()[grepl("all_", ls())])

rm(list = ls()[grepl("files", ls())])

gc()

###################################################################################################################
####################### Adding new columns ########################################################################
###################################################################################################################

MasterDT <- add_new_columns(MasterDT)

###################################################################################################################
####################### Cleaning, pre-processing, filtering #######################################################
###################################################################################################################

MasterDT <- clean_preprocess(MasterDT)
 
###################################################################################################################
###################################################################################################################

### Analyze 


## Tables/counts

# Count of rides, Average ride duration - per customer type - overall


MasterDT[start_lng != end_lng & start_lat != end_lat,.(Rides = .N,Avg_Ride_time = mean(Ride_Time)/60),by=member_casual]


# Count of rides, Average ride duration - per customer type - per day of the week

MasterDT[,.(Rides = .N,Avg_Ride_time = mean(Ride_Time)/60),keyby=.(member_casual,DayOfWeek)]



# Count of rides, Average ride duration - per customer type - per month


MasterDT[,.(Rides = .N,Avg_Ride_time = mean(Ride_Time)/60),keyby=.(member_casual,Month)]


# Count of rides, Average ride duration - per customer type - per Year

MasterDT[,.(Rides = .N,Avg_Ride_time = mean(Ride_Time)/60),keyby=.(member_casual,Year)]



# Count of rides, Average ride duration - per Starting Station - overall


MasterDT[,.(Rides = .N,Avg_Ride_time = mean(Ride_Time)/60),by=.(start_station_id,start_station_name)]


# Casual & Member Share per Day of Week

MasterDT[,.(Member_Ride_Share = round((sum(member_casual=="Member"))/.N*100,0),
            Casual_Ride_Share = round((sum(member_casual=="Casual"))/.N*100,0)),by=.(DayOfWeek)]


# Casual & Member share per Station


MasterDT[,.(Member_Ride_Share = round((sum(member_casual=="Member"))/.N*100,0),
            Casual_Ride_Share = round((sum(member_casual=="Casual"))/.N*100,0)),by=.(start_station_id,start_station_name)]


# Casual & Member share per year

MasterDT[,.(Member_Ride_Share = round((sum(member_casual=="Member"))/.N*100,0),
            Casual_Ride_Share = round((sum(member_casual=="Casual"))/.N*100,0)),by=.(Year)]


# Casual & Member share per month

MasterDT[,.(Member_Ride_Share = round((sum(member_casual=="Member"))/.N*100,0),
            Casual_Ride_Share = round((sum(member_casual=="Casual"))/.N*100,0)),by=.(Month)]


# Plots

# Count of rides per customer type over time - line -- shows seasonality

MasterDT[,.(Rides=.N),by=.(Date,member_casual)] %>% 
  ggplot(aes(x=Date,y=Rides,group=member_casual,color=member_casual)) +
  geom_line() 


# Avg ride time per customer type overall

MasterDT[start_lng != end_lng & start_lat != end_lat,
         .(rides = .N,
           avg_ride_time = mean(Ride_Time, na.rm = TRUE)),
         by=.(member_casual)] %>% 
  ggplot() + 
  geom_col(aes(x=member_casual,y=avg_ride_time/60,fill=member_casual), show.legend = FALSE) +
  labs(title = "Avg ride time by User type",x="User Type",y="Avg time in mins")

# Count of rides - per customer type per day of week - line graph with points

MasterDT[,.(rides = .N),by=.(DayOfWeek,member_casual)] %>%
  ggplot(aes(x=DayOfWeek,y=rides,group=member_casual,color=member_casual)) + 
  geom_line() +
  geom_point()

# Avg Ride time - per customer type per day of week - line graph with points

MasterDT[,.(Rides = .N, Avg_Ride_time = mean(Ride_Time)/60),by=.(member_casual,DayOfWeek)] %>%
  ggplot(aes(x=DayOfWeek,y=Avg_Ride_time,group=member_casual,color=member_casual)) + 
  geom_line() +
  geom_point()

# Count of rides - per customer type per month - line graph with points

MasterDT[,.(Rides = .N),by=.(member_casual,Month)] %>%
  ggplot(aes(x=Month,y=Rides,group=member_casual,color=member_casual)) +
  geom_line() +
  geom_point()

# Avg Ride time - per customer type per month - line graph with points

MasterDT[,.(Avg_Ride_time = mean(Ride_Time)/60),by=.(member_casual,Month)] %>%
  ggplot(aes(x=Month,y=Avg_Ride_time,group=member_casual,color=member_casual)) + 
  geom_line() +
  geom_point()

# Avg Ride time - per customer type per Year - line graph with points

MasterDT[,.(Avg_Ride_time = mean(Ride_Time)/60),by=.(member_casual,Year)] %>%
  ggplot(aes(x=Year,y=Avg_Ride_time,group=member_casual,color=member_casual)) + 
  geom_line() +
  geom_point()

# Count of rides - per customer type per Year - line graph with points

MasterDT[,.(Rides = .N),by=.(member_casual,Year)] %>%
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

MasterDT[,.(Ratio = sum(member_casual=="Member")/sum(member_casual=="Casual")),by=.(Year)] %>% 
  ggplot(aes(x=Year,y=Ratio,group=1)) +
  geom_point() +
  geom_line() +
  labs(title = "Member/Causal Rides Ratio per Year",x="Year",y="Member/Causal Rides Ratio")


# Member to causal Rides ratio per month

MasterDT[,.(Ratio = sum(member_casual=="Member")/sum(member_casual=="Casual")),by=.(Month)] %>% 
  ggplot(aes(x=Month,y=Ratio,group=1)) +
  geom_point() +
  geom_line() +
  labs(title = "Member/Causal Rides Ratio per Month",x="Month",y="Member/Causal Rides Ratio")


# Member to causal Rides ratio per Day of week

MasterDT[,.(Ratio = sum(member_casual=="Member")/sum(member_casual=="Casual")),by=.(DayOfWeek)] %>% 
  ggplot(aes(x=DayOfWeek,y=Ratio,group=1)) +
  geom_point() +
  geom_line() +
  labs(title = "Member/Causal Rides Ratio per Day of Week",x="DayOfWeek",y="Member/Causal Rides Ratio")


###################################################################################
##ROUGH
###################################################################################

# Create new data frame with most popular routes

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

# Lets store bounding box coordinates for ggmap:
bb <- c(
  left = -87.700424,
  bottom = 41.790769, 
  right = -87.554855,
  top = 41.990119
)

# Here we store the map
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
