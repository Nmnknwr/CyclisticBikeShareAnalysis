## Function to download all ZIP files from https://divvy-tripdata.s3.amazonaws.com/index.html
download <- function() {
  for(i in 1:nrow(files_zipped_df)) {
  download.file(files_zipped_df[i,2],paste(dest_path,files_zipped_df[i,1],sep="/"))
  }
}

## Unzip

unzip_all <- function() {
  for(i in 1:nrow(files_zipped_df)) {
    unzip(paste(dest_path,files_zipped_df[i,1],sep="/"),exdir = dest_path)
    file.remove(paste(dest_path,files_zipped_df[i,1],sep="/"))
  }
  
}

## Organize files into same directory + delete 2013 and unwanted files/folders

organise_all <- function() {
  for(i in 1:nrow(files_unzipped_df)) {
    if(file_ext(files_unzipped_df[i,1])=="zip" | file_ext(files_unzipped_df[i,1])=="txt" | str_detect(files_unzipped_df[i,2],"_Stations_")==TRUE) {
      file.remove(files_unzipped_df[i,1])
    } else if (file_ext(files_unzipped_df[i,1])=="") {
      file.rename(files_unzipped_df[i,1],paste(files_unzipped_df[i,1],"csv",sep="."))
    } else if (file_ext(files_unzipped_df[i,1])=="xlsx") {
      newname <- gsub("xlsx","csv",files_unzipped_df[i,1])
      file.rename(files_unzipped_df[i,1],newname)
    } 
  }
  unlink(paste(dest_path,"__MACOSX",sep="/"),recursive = TRUE)
  unlink(paste(dest_path,"Divvy_Stations_Trips_2013",sep="/"),recursive = TRUE)
  }
  
## combine per year

combine_year <- function(fileyear) {
  pat <- paste("*",fileyear,"*",sep = "")
  list_df <- mget(ls(pattern = glob2rx(pat),envir = .GlobalEnv),envir = .GlobalEnv)
  name <- paste("all",fileyear,sep = "_")
  assign(name,bind_rows(list_dt),envir = .GlobalEnv)
}

## Clean, preprocess, add additional columns
clean_preprocess <- function(df) {
  df <- df[-c(4,5,13)]
  df <- df %>% 
    mutate(member_casual = recode(member_casual,"Subscriber"="Member","Customer"="Casual")) %>%
    mutate(member_casual = recode(member_casual,"member"="Member","casual"="Casual")) %>% 
    distinct(ride_id, .keep_all = TRUE) %>% 
    mutate(
      Date = as.Date(started_at),
      Month = format(as.Date(Date),"%b"),
      Year = format(as.Date(Date),"%Y"),
      Day = format(as.Date(Date),"%d"),
      DayOfWeek = format(as.Date(Date),"%a"),
      Ride_Time = difftime(ended_at,started_at),
      WeekDay_WeekEnd = case_when(
        DayOfWeek == "Sat" | DayOfWeek == "Sun" ~ "Weekend",
        TRUE ~ "Weekday"),
      Ride_Time = as.numeric(as.character(Ride_Time))) %>%
    filter(!(start_station_name == "HQ QR" | Ride_Time<0)) %>% 
    filter(!(is.na(start_station_id))) %>% 
    filter(!(is.na(end_station_id)))
  
  df$DayOfWeek <- ordered(df$DayOfWeek, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
  df$Month <- ordered(df$Month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sep","Oct","Nov","Dec"))
  
  return(df)
}