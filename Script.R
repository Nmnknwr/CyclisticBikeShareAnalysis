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

getwd()
setwd("I:/Work/Datasets/Cyclistic/CSVs")

#=====================
# DATA Loading and collection 
#=====================
# Uploading Raw Divvy datasets (csv files) for 2018 to 2021 March
q1_2021_01 <- read_csv("202101-divvy-tripdata.csv")
q1_2021_02 <- read_csv("202102-divvy-tripdata.csv")
q1_2021_03 <- read_csv("202103-divvy-tripdata.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")
q2_2020_04 <- read_csv("202004-divvy-tripdata.csv")
q2_2020_05 <- read_csv("202005-divvy-tripdata.csv")
q2_2020_06 <- read_csv("202006-divvy-tripdata.csv")
q3_2020_07 <- read_csv("202007-divvy-tripdata.csv")
q3_2020_08 <- read_csv("202008-divvy-tripdata.csv")
q3_2020_09 <- read_csv("202009-divvy-tripdata.csv")
q4_2020_10 <- read_csv("202010-divvy-tripdata.csv")
q4_2020_11 <- read_csv("202011-divvy-tripdata.csv")
q4_2020_12 <- read_csv("202012-divvy-tripdata.csv")
q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2018 <- read_csv("Divvy_Trips_2018_Q1.csv")
q2_2018 <- read_csv("Divvy_Trips_2018_Q2.csv")
q3_2018 <- read_csv("Divvy_Trips_2018_Q3.csv")
q4_2018 <- read_csv("Divvy_Trips_2018_Q4.csv")



### Checking column names and structures of the loaded raw files
### Changing Column names where Necessary

## 2021

All_2021_trips <- bind_rows(q1_2021_01,q1_2021_02,q1_2021_03)


## 2020
colnames(q1_2020)
colnames(q2_2020_04)
colnames(q2_2020_05)
colnames(q2_2020_06)
colnames(q3_2020_07)
colnames(q3_2020_08)
colnames(q3_2020_09)
colnames(q4_2020_10)
colnames(q4_2020_11)
colnames(q4_2020_12)

str(q1_2020)
str(q2_2020_04)
str(q2_2020_05)
str(q2_2020_06)
str(q3_2020_07)
str(q3_2020_08)
str(q3_2020_09)
str(q4_2020_10)
str(q4_2020_11)
str(q4_2020_12) # start_station_id, end_station_id is char here, double in others
# start_station_id, end_station_id should be char everywhere. changing to char datatype
q1_2020 <- mutate(q1_2020,start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
q2_2020_04 <- mutate(q2_2020_04,start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
q2_2020_05 <- mutate(q2_2020_05,start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
q2_2020_06 <- mutate(q2_2020_06,start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
q3_2020_07 <- mutate(q3_2020_07,start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
q3_2020_08 <- mutate(q3_2020_08,start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
q3_2020_09 <- mutate(q3_2020_09,start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
q4_2020_10 <- mutate(q4_2020_10,start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
q4_2020_11 <- mutate(q4_2020_11,start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))

# Combining to single file for 2020
All_2020_trips <- bind_rows(q1_2020, q2_2020_04, q2_2020_05, q2_2020_06, q3_2020_07, q3_2020_08, q3_2020_09, q4_2020_10, q4_2020_11, q4_2020_12)


str(All_2020_trips)

##2019

q1_2019 <- mutate(q1_2019,
                  trip_id = as.character(trip_id),
                  from_station_id = as.character(from_station_id),
                  to_station_id = as.character(to_station_id))

q1_2019 <- rename(q1_2019,
                  ride_id = trip_id,
                  start_station_id = from_station_id,
                  end_station_id = to_station_id,
                  started_at = start_time,
                  ended_at = end_time,
                  member_casual = usertype,
                  start_station_name = from_station_name,
                  end_station_name = to_station_name)


q2_2019 <- rename(q2_2019,
                  ride_id = "01 - Rental Details Rental ID",
                  bikeid = "01 - Rental Details Bike ID",
                  tripduration = "01 - Rental Details Duration In Seconds Uncapped",
                  gender = "Member Gender",
                  birthyear = "05 - Member Details Member Birthday Year",
                  started_at = "01 - Rental Details Local Start Time",
                  ended_at = "01 - Rental Details Local End Time",
                  start_station_id = "03 - Rental Start Station ID",
                  end_station_id = "02 - Rental End Station ID",
                  member_casual = "User Type",
                  start_station_name = "03 - Rental Start Station Name", 
                  end_station_name = "02 - Rental End Station Name")


q2_2019 <- mutate(q2_2019,
                  ride_id = as.character(ride_id),
                  start_station_id = as.character(start_station_id),
                  end_station_id = as.character(end_station_id))


q3_2019 <- rename(q3_2019,
                  ride_id = trip_id,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_id = from_station_id,
                  end_station_id = to_station_id,
                  start_station_name = from_station_name,
                  end_station_name = to_station_name,
                  member_casual = usertype)

q3_2019 <- mutate(q3_2019,
                  ride_id = as.character(ride_id),
                  start_station_id = as.character(start_station_id),
                  end_station_id = as.character(end_station_id))

q4_2019 <- rename(q4_2019,
                  ride_id = trip_id,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_id = from_station_id,
                  end_station_id = to_station_id,
                  start_station_name = from_station_name,
                  end_station_name = to_station_name,
                  member_casual = usertype)

q4_2019 <- mutate(q4_2019,
                  ride_id = as.character(ride_id),
                  start_station_id = as.character(start_station_id),
                  end_station_id = as.character(end_station_id))

All_2019_trips <- bind_rows(q1_2019,q2_2019,q3_2019,q4_2019)

## 2018

q1_2018 <- rename(q1_2018,
                  ride_id = "01 - Rental Details Rental ID",
                  bikeid = "01 - Rental Details Bike ID",
                  tripduration = "01 - Rental Details Duration In Seconds Uncapped",
                  gender = "Member Gender",
                  birthyear = "05 - Member Details Member Birthday Year",
                  started_at = "01 - Rental Details Local Start Time",
                  ended_at = "01 - Rental Details Local End Time",
                  start_station_id = "03 - Rental Start Station ID",
                  end_station_id = "02 - Rental End Station ID",
                  member_casual = "User Type",
                  start_station_name = "03 - Rental Start Station Name", 
                  end_station_name = "02 - Rental End Station Name")


q1_2018 <- mutate(q1_2018,
                  ride_id = as.character(ride_id),
                  start_station_id = as.character(start_station_id),
                  end_station_id = as.character(end_station_id))

q2_2018 <- mutate(q2_2018,
                  trip_id = as.character(trip_id),
                  from_station_id = as.character(from_station_id),
                  to_station_id = as.character(to_station_id))

q2_2018 <- rename(q2_2018,
                  ride_id = trip_id,
                  start_station_id = from_station_id,
                  end_station_id = to_station_id,
                  started_at = start_time,
                  ended_at = end_time,
                  member_casual = usertype,
                  start_station_name = from_station_name,
                  end_station_name = to_station_name)

q3_2018 <- mutate(q3_2018,
                  trip_id = as.character(trip_id),
                  from_station_id = as.character(from_station_id),
                  to_station_id = as.character(to_station_id))

q3_2018 <- rename(q3_2018,
                  ride_id = trip_id,
                  start_station_id = from_station_id,
                  end_station_id = to_station_id,
                  started_at = start_time,
                  ended_at = end_time,
                  member_casual = usertype,
                  start_station_name = from_station_name,
                  end_station_name = to_station_name)

q4_2018 <- mutate(q4_2018,
                  trip_id = as.character(trip_id),
                  from_station_id = as.character(from_station_id),
                  to_station_id = as.character(to_station_id))

q4_2018 <- rename(q4_2018,
                  ride_id = trip_id,
                  start_station_id = from_station_id,
                  end_station_id = to_station_id,
                  started_at = start_time,
                  ended_at = end_time,
                  member_casual = usertype,
                  start_station_name = from_station_name,
                  end_station_name = to_station_name)

All_2018_trips <- bind_rows(q1_2018,q2_2018,q3_2018,q4_2018)

# Combining all years, inspecting Master Dump

MasterTrip_Dump <- bind_rows(All_2018_trips,All_2019_trips,All_2020_trips,All_2021_trips)

# Removing columns bikeid, tripduration, rideable_type

MasterTrip_Dump <- MasterTrip_Dump[-c(4,5,13)]

#### Exploration, Cleaning of Master Dump

table(MasterTrip_Dump$member_casual)


# Need to make casual = Customer and member = Subscriber


MasterTrip_Dump <- MasterTrip_Dump %>%
  mutate(member_casual = recode(member_casual,"Subscriber"="Member","Customer"="Casual")) %>%
  mutate(member_casual = recode(member_casual,"member"="Member","casual"="Casual"))


table(MasterTrip_Dump$member_casual)

# Removing rows with duplicate ride_id 

MasterTrip_Dump <- MasterTrip_Dump %>%
  distinct(ride_id, .keep_all = TRUE)

# Addition of new calculated columns

MasterTrip_Dump$Date <- as.Date(MasterTrip_Dump$started_at) #Date
MasterTrip_Dump$Month <- format(as.Date(MasterTrip_Dump$Date),"%b") #Month
MasterTrip_Dump$Year <- format(as.Date(MasterTrip_Dump$Date),"%Y") #Year
MasterTrip_Dump$Day <- format(as.Date(MasterTrip_Dump$Date),"%d") #Day
MasterTrip_Dump$DayOfWeek <- format(as.Date(MasterTrip_Dump$Date),"%a") #Day of Week
MasterTrip_Dump$Ride_Time <- difftime(MasterTrip_Dump$ended_at,MasterTrip_Dump$started_at) #Ride Time
MasterTrip_Dump <- MasterTrip_Dump %>% 
  mutate(WeekDay_WeekEnd = case_when(
    DayOfWeek == "Sat" | DayOfWeek == "Sun" ~ "Weekend",
    TRUE ~ "Weekday"
  ))

# Changing Ride_time to a numeric field for easy Calculations
MasterTrip_Dump$Ride_Time <- as.numeric(as.character(MasterTrip_Dump$Ride_Time))


# Removing rows where - start_station_id or end_station_id = NA, Ride_time <0, start_station_name is "HQ QR"
MasterTrip_Dump_clean <- MasterTrip_Dump %>% 
  filter(!(start_station_name == "HQ QR" | Ride_Time<0)) %>% 
  filter(!(is.na(start_station_id))) %>% 
  filter(!(is.na(end_station_id)))


MasterTrip_Dump_clean$DayOfWeek <- ordered(MasterTrip_Dump_clean$DayOfWeek, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

MasterTrip_Dump_clean$Month <- ordered(MasterTrip_Dump_clean$Month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sep","Oct","Nov","Dec"))

str(MasterTrip_Dump_clean)


### Analyze 


## Tables/counts

# Count of rides, Average ride duration - per customer type - overall
MasterTrip_Dump_clean %>% 
  filter(start_lng != end_lng & start_lat != end_lat) %>% 
  group_by(member_casual) %>% 
  summarise(rides = n(), 
            avg_ride_time = mean(Ride_Time, na.rm = TRUE))


# Count of rides, Average ride duration - per customer type - per day of the week

MasterTrip_Dump_clean %>%
  group_by(member_casual,DayOfWeek) %>%
  summarise(Rides = n(), Avg_Ride_time = mean(Ride_Time)/60) %>%
  arrange(member_casual,DayOfWeek)


# Count of rides, Average ride duration - per customer type - per month

MasterTrip_Dump_clean %>%
  group_by(member_casual,Month) %>%
  summarise(Rides = n(), Avg_Ride_time = mean(Ride_Time)/60) %>%
  arrange(Month)

# Count of rides, Average ride duration - per customer type - per Year

MasterTrip_Dump_clean %>%
  group_by(member_casual,Year) %>%
  summarise(Rides = n(), Avg_Ride_time = mean(Ride_Time)/60) %>%
  arrange(Year)


# Count of rides, Average ride duration - per Starting Station - overall
MasterTrip_Dump_clean %>%
  group_by(start_station_id,start_station_name) %>% 
  summarise(Rides = n(), Avg_Ride_time = mean(Ride_Time)/60) %>% 
  arrange(desc(Rides))


# Casual & Member Share per Day of Week
MasterTrip_Dump_clean %>% 
  group_by(DayOfWeek) %>% 
  summarise(Member_Ride_Share = round((sum(member_casual=="Member"))/n()*100,0),
            Casual_Ride_Share = round((sum(member_casual=="Casual"))/n()*100,0))%>% 
  arrange(desc(n()))


# Casual & Member share per Station
MasterTrip_Dump_clean %>% 
  group_by(start_station_id,start_station_name) %>% 
  summarise(Member_Ride_Share = round((sum(member_casual=="Member"))/n()*100,0),
            Casual_Ride_Share = round((sum(member_casual=="Casual"))/n()*100,0))%>% 
  arrange(desc(n()))

# Casual & Member share per year
MasterTrip_Dump_clean %>% 
  group_by(Year) %>% 
  summarise(MemberShare = (sum(member_casual=="Member")/n())*100,
            CasualShare = (sum(member_casual=="Casual")/n())*100) %>% 
  arrange(desc(n()))


# Casual & Member share per month
MasterTrip_Dump_clean %>% 
  group_by(Month) %>% 
  summarise(MemberShare = (sum(member_casual=="Member")/n())*100,
            CasualShare = (sum(member_casual=="Casual")/n())*100) %>% 
  arrange(desc(n()))



# Average age of members per gender
MasterTrip_Dump_clean %>% 
  filter(member_casual!="Casual" & gender != "NA") %>% 
  group_by(gender) %>% 
  summarise(Average_age = mean(2021-birthyear,na.rm=TRUE))

# Plots

# Count of rides per customer type over time - line -- shows seasonality
MasterTrip_Dump_clean %>% 
  group_by(Date,member_casual) %>% 
  summarise(Rides =  n()) %>% 
  ggplot(aes(x=Date,y=Rides,group=member_casual,color=member_casual)) +
  geom_line() 


# Avg ride time per customer type overall
MasterTrip_Dump_clean %>% 
  filter(start_lng != end_lng & start_lat != end_lat) %>% 
  group_by(member_casual) %>% 
  summarise(rides = n(), 
            avg_ride_time = mean(Ride_Time, na.rm = TRUE)) %>% 
  ggplot() + 
  geom_col(aes(x=member_casual,y=avg_ride_time/60,fill=member_casual), show.legend = FALSE) +
  labs(title = "Avg ride time by User type",x="User Type",y="Avg time in mins")


# Count of rides - per customer type per day of week - line graph with points
MasterTrip_Dump_clean %>% 
  group_by(DayOfWeek,member_casual) %>% 
  summarise(Rides=n()) %>% 
  ggplot(aes(x=DayOfWeek,y=Rides,group=member_casual,color=member_casual)) + 
  geom_line() +
  geom_point()

# Avg Ride time - per customer type per day of week - line graph with points
MasterTrip_Dump_clean %>% 
  group_by(member_casual,DayOfWeek) %>%
  summarise(Rides = n(), Avg_Ride_time = mean(Ride_Time)/60) %>%
  arrange(member_casual,DayOfWeek) %>% 
  ggplot(aes(x=DayOfWeek,y=Avg_Ride_time,group=member_casual,color=member_casual)) + 
  geom_line() +
  geom_point()

# Count of rides - per customer type per month - line graph with points
MasterTrip_Dump_clean %>% 
  group_by(member_casual,Month) %>% 
  summarise(Rides = n()) %>% 
  ggplot(aes(x=Month,y=Rides,group=member_casual,color=member_casual)) +
  geom_line() +
  geom_point()


# Avg Ride time - per customer type per month - line graph with points
MasterTrip_Dump_clean %>% 
  group_by(member_casual,Month) %>%
  summarise(Avg_Ride_time = mean(Ride_Time)/60) %>%
  ggplot(aes(x=Month,y=Avg_Ride_time,group=member_casual,color=member_casual)) + 
  geom_line() +
  geom_point()

# Avg Ride time - per customer type per Year - line graph with points
MasterTrip_Dump_clean %>% 
  group_by(member_casual,Year) %>%
  summarise(Avg_Ride_time = mean(Ride_Time)/60) %>%
  ggplot(aes(x=Year,y=Avg_Ride_time,group=member_casual,color=member_casual)) + 
  geom_line() +
  geom_point()

# Count of rides - per customer type per Year - line graph with points
MasterTrip_Dump_clean %>% 
  filter(Year!=2021) %>% 
  group_by(member_casual,Year) %>%
  summarise(Rides = n()) %>%
  ggplot(aes(x=Year,y=Rides,group=member_casual,color=member_casual)) + 
  geom_line() +
  geom_point()


# % split of total rides by user type per year
MasterTrip_Dump_clean %>% 
  group_by(Year) %>% 
  summarise(Member = (sum(member_casual=="Member")/n())*100,
            Casual = (sum(member_casual=="Casual")/n())*100) %>% 
  gather(key = User_type, value = Perc_Share,Member,Casual) %>% 
  ggplot(aes(x=Year,y=Perc_Share,group=User_type,color=User_type)) +
  geom_point() +
  geom_line() +
  labs(title = "% split of rides by user type per year",x="Year",y="(%)")


# % split of total rides by user type per Day of week
MasterTrip_Dump_clean %>% 
  group_by(DayOfWeek) %>% 
  summarise(Member = (sum(member_casual=="Member")/n())*100,
            Casual = (sum(member_casual=="Casual")/n())*100) %>% 
  gather(key = User_type, value = Perc_Share,Member,Casual) %>% 
  ggplot(aes(x=DayOfWeek,y=Perc_Share,group=User_type,color=User_type)) +
  geom_point() +
  geom_line() +
  labs(title = "% split of rides by user type per day of week",x="Day of Week",y="(%)")


# % split of total rides by user type per month
MasterTrip_Dump_clean %>% 
  group_by(Month) %>% 
  summarise(Member = (sum(member_casual=="Member")/n())*100,
            Casual = (sum(member_casual=="Casual")/n())*100) %>% 
  gather(key = User_type, value = Perc_Share,Member,Casual) %>% 
  ggplot(aes(x=Month,y=Perc_Share,group=User_type,color=User_type)) +
  geom_point() +
  geom_line() +
  labs(title = "% split of rides by user type per month",x="Month",y="(%)")



# Member to causal Rides ratio per year
MasterTrip_Dump_clean %>% 
  group_by(Year) %>% 
  summarise(Ratio = sum(member_casual=="Member")/sum(member_casual=="Casual")) %>% 
  ggplot(aes(x=Year,y=Ratio,group=1)) +
  geom_point() +
  geom_line() +
  labs(title = "Member/Causal Rides Ratio per Year",x="Year",y="Member/Causal Rides Ratio")

# Member to causal Rides ratio per month
MasterTrip_Dump_clean %>% 
  group_by(Month) %>% 
  summarise(Ratio = sum(member_casual=="Member")/sum(member_casual=="Casual")) %>% 
  ggplot(aes(x=Month,y=Ratio,group=1)) +
  geom_point() +
  geom_line() +
  labs(title = "Member/Causal Rides Ratio per Month",x="Month",y="Member/Causal Rides Ratio")


# Member to causal Rides ratio per Day of week
MasterTrip_Dump_clean %>% 
  group_by(DayOfWeek) %>% 
  summarise(Ratio = sum(member_casual=="Member")/sum(member_casual=="Casual")) %>% 
  ggplot(aes(x=DayOfWeek,y=Ratio,group=1)) +
  geom_point() +
  geom_line() + 
  labs(title = "Member/Causal Rides Ratio per Day of Week",x="DayOfWeek",y="Member/Causal Rides Ratio")


# Average age of members per gender per year
MasterTrip_Dump_clean %>% 
  filter(member_casual!="Casual" & gender != "NA") %>%
  group_by(Year,gender) %>% 
  summarise(Average_age = mean(2021-birthyear,na.rm=TRUE)) %>% 
  ggplot(aes(x=Year,y=Average_age,group=gender,color=gender)) +
  geom_point() +
  geom_line()

# % split of Member gender per year
MasterTrip_Dump_clean %>% 
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

Fav_stations <- MasterTrip_Dump_clean %>% 
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

