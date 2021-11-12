## Function to download all ZIP files from https://divvy-tripdata.s3.amazonaws.com/index.html
download <- function() {
  # Loop over each file in files_zipped_df 
  for(i in 1:nrow(files_zipped_df)) {
    # download it using downloadURL, dest_path, and filename in data frame
  download.file(files_zipped_df[i,2],paste(dest_path,files_zipped_df[i,1],sep="/"))
  }
}

## Unzip

unzip_files <- function() {
  # Loop over each file in files_zipped_df
  for(i in 1:nrow(files_zipped_df)) {
    # unzip it
    unzip(paste(dest_path,files_zipped_df[i,1],sep="/"),exdir = dest_path)
    # delete the original .zip
    file.remove(paste(dest_path,files_zipped_df[i,1],sep="/"))
  }
  
}

## Organize files, extensions

organise <- function() {
  # Loop over each file in files_unzipped_df
  for(i in 1:nrow(files_unzipped_df)) {
    # check for blank extension
    if (file_ext(files_unzipped_df[i,1])=="") {
      # add .csv extension
      file.rename(files_unzipped_df[i,1],paste(files_unzipped_df[i,1],"csv",sep="."))
    }
  }
  # delete "__MACOSX" folder that comes from unzipping
  unlink(paste(dest_path,"__MACOSX",sep="/"),recursive = TRUE)
}
  

## Load into R

load_2019 <- function() {
    files_unzipped_df <- files_unzipped_df %>% 
    # Filter only for Files names containing "2019"
    filter(str_detect(Filename,"2019"))
  
  # Set column headers
  column_names <- c("ride_id","started_at","ended_at","start_station_id","start_station_name","end_station_id","end_station_name","member_casual")
  
  # Loop over each file in files_unzipped_df
  for(i in 1:nrow(files_unzipped_df)) {
    # Assign a name for that files data table once imported
    name <- paste("dt",files_unzipped_df[i,2],i,sep="_")
    # import into data table using fread with arguments for column data types, column headers and dropping some unneeded columns
    assign(name,fread(files_unzipped_df[i,1],drop=c(4,5,11,12),col.names = column_names,colClasses = list(character=c(1,6,8,10))), envir = .GlobalEnv)
  }
  pat <- "*2019*csv*"
  # Making list of all data tables made from loop above for 2019
  list_dt <- mget(ls(pattern = glob2rx(pat),envir = .GlobalEnv),envir = .GlobalEnv) # I know...plz ignore :P
  name <- "all_2019"
  # Using bind_rows to stack all data tables into single one for 2019
  assign(name,bind_rows(list_dt),envir = .GlobalEnv) 
  # Deleting individual data tables
  rm(list = ls(envir = .GlobalEnv)[grepl("csv", ls(envir = .GlobalEnv))],envir = .GlobalEnv)
  
# Removing older objects with "rm()" helped in saving memory, since there were a lot of rows..
}


load_2020_2021 <- function() {
  years_to_include <- c("2020","2021")
  
  # Set column headers
  column_names <- c("ride_id","started_at","ended_at","start_station_name","start_station_id","end_station_name","end_station_id","start_lat","start_lng","end_lat","end_lng", "member_casual")
  
  # Filter only for Files names containing years_to_include
  files_unzipped_df <- files_unzipped_df %>% 
    filter(str_detect(Filename,paste(years_to_include,collapse = "|")))

  # Loop over each file in files_unzipped_df
    for(i in 1:nrow(files_unzipped_df)) {
    # Assign a name for that files data table once imported
    name <- paste("dt",files_unzipped_df[i,2],i,sep="_")
    # import into data table using fread with arguments for column data types, column headers and dropping some unneeded columns
    assign(name,fread(files_unzipped_df[i,1],drop=c(2),col.names = column_names,colClasses = list(character=c(1,6,8,13))),envir = .GlobalEnv)
    }
  pat <- "*202*csv*"
  # Making list of all data tables made from loop above for 2020 and 2021
  list_dt <- mget(ls(pattern = glob2rx(pat),envir = .GlobalEnv),envir = .GlobalEnv) # I know...plz ignore :P
  name <- "all_2020_2021"
  # Using bind_rows to stack all data tables into single one for 2010 and 2021
  assign(name,bind_rows(list_dt),envir = .GlobalEnv)
  # Deleting individual data tables
  rm(list = ls(envir = .GlobalEnv)[grepl("csv", ls(envir = .GlobalEnv))],envir = .GlobalEnv)
  
# Removing older objects with "rm()" helped in saving memory, since there were a lot of rows..
}

## Add new columns

add_new_columns <- function(dt) {
  dt$Date <- as.Date(dt$started_at)
  dt$Month <- format(as.Date(dt$Date),"%b")
  dt$Year <- format(as.Date(dt$Date),"%Y")
  dt$Day <- format(as.Date(dt$Date),"%d")
  dt$DayOfWeek <- format(as.Date(dt$Date),"%a") 
  dt$Ride_Time <- difftime(dt$ended_at,dt$started_at)
  dt <- dt %>% 
    mutate(WeekDay_WeekEnd = case_when(DayOfWeek == "Sat" | DayOfWeek == "Sun" ~ "Weekend",TRUE ~ "Weekday"))
  
  return(dt)
}

## Clean, preprocess, filter
clean_preprocess <- function(dt) {
  dt <- dt[.(member_casual=c("Subscriber","member"), to="Member"), on="member_casual",member_casual := i.to][.(member_casual=c("casual","Customer"), to="Casual"), on="member_casual",member_casual := i.to][member_casual!="Dependent"][, `:=` (Ride_Time = as.numeric(as.character(Ride_Time)))][, `:=` (DayOfWeek=ordered(DayOfWeek, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")))][, `:=` (Month=ordered(Month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sep","Oct","Nov","Dec")))][, `:=` (WeekDay_WeekEnd = as.factor(WeekDay_WeekEnd))][start_station_name != "HQ QR"][!(is.na(start_station_id))][!(is.na(end_station_id))][!(Ride_Time<0)][, `:=` (Day = as.numeric(as.character(Day)),Year = as.numeric(as.character(Year)),member_casual=as.factor(member_casual))]
  return(dt)
}