library(stringr)
library(rvest)
library(dplyr)
library(tidyr)

track_res  <- data.frame()
nodata_csv <- data.frame()

setwd('C:/Users/Packr1/Documents/Personal/Track/rawhtml/')
evt_list <- list.files()

for(evt in 4:length(evt_list)){
  # print(paste(evt, ":", evt_list[evt]))
  evt_in <- evt_list[evt]
  # Add .html if not present
  if(!str_detect(evt_in, ".html")){
      file.rename(evt_in, paste0(evt_in, ".html"))
  }
  # manually converted "mmer-track---8-3-201"
  dig_date <- str_replace_all(str_sub(evt_list[evt], -15), "[^0-9]", "")
  if(2000 <= as.numeric(str_sub(dig_date, 1, 4)) & 2050 >= as.numeric(str_sub(dig_date, 1, 4))){
      dtvar <- ymd(dig_date)
  } else {
      dig_date <- paste0("0", dig_date)
      if(nchar(dig_date)==7){
          dig_date <- paste0(str_sub(dig_date, 1, 2), "0", str_sub(dig_date,3))
      }
      dtvar <- mdy(dig_date)
  }
  print(paste0(evt, " : ", dig_date))
  print(paste0(evt, " : ", dtvar))
  
  dat1 <- read_html(evt_list[evt])
  dat1_events <- dat1 %>% html_nodes("strong") %>% html_text() %>% tbl_df(.) %>% 
      mutate(event0 = str_replace_all(toupper(value), " " ,"")) %>%
      dplyr::filter(str_detect(event0,
                     "MILE|1500M|100M|200M|400M|800M|3K|5K|3000M|5000M")) %>%
      dplyr::filter(!str_detect(event0, "RELAY")) %>%
      distinct(value) %>% mutate(ct_evt = row_number() * 1000) %>%
      rename(Name = value)
  # No data found, record date in CSV at end
  if(nrow(dat1_events) == 0){
      nodata_csv <- rbind(nodata_csv, dtvar) 
      next
  }
  dat1_events <- data.frame(lapply(dat1_events, function(x) {str_trim(x)}), stringsAsFactors = FALSE)
  dat1_events <- data.frame(lapply(dat1_events, function(x) {str_replace_all(x, "\\s+", " ")}), stringsAsFactors = FALSE)
 
  print(dat1_events)
  
  dat2 <- dat1 %>% html_nodes("td") %>% html_text() %>%
      str_replace_all("\n", "")
  dat2        <- str_trim(dat2)
  dat2        <- str_replace_all(dat2, "\\s+", " ")
  dat2 <- dat2[which(dat2==str_trim(dat1_events[1,1])):length(dat2)]
  names(dat2) <- rep(c("Name", "Sex", "Age", "Time"), length(dat2) / 4)
  
  test_dat2 <- dat2[5:8]
  test_dat2frm <- as.data.frame(t(as.data.frame(test_dat2)))
  
  rowct <- length(dat2) / 4
  dat3 <- data.frame()
  for(o in 1:rowct){
      col_counter = seq(1:4) + 4* (o-1)
      infrm <- dat2[col_counter]
      dat3 <- bind_rows(dat3, infrm)
  }
  
  dat1_events2 <- dat1_events %>% rename(Event = Name)
  
   dat4 <- left_join(dat3, dat1_events, by = "Name") %>%
       mutate(ct_evt2 = zoo::na.locf(ct_evt)) %>%
      dplyr::filter(is.na(ct_evt)) %>%
      mutate(ct_evt = ct_evt2) %>%
      select(-ct_evt2) %>%
      left_join(., dat1_events2, by = "ct_evt") %>%
      # dplyr::filter(!is.na(track_time)) %>%
      mutate(mins = case_when(
          stringr::str_count(Time, ":") == 1 ~
              str_replace_all(str_extract(Time, "(.*?):"), ":", ""),
          stringr::str_count(Time, "\\.") == 1 ~
              "0"),
          secs = case_when(
              stringr::str_count(Time, ":") == 1 ~
                  str_replace_all(str_extract(Time, ":(.*)$"), ":", ""),
              stringr::str_count(Time, "\\.") == 1 ~
                  Time)) %>%
      dplyr::filter(!is.na(secs) & secs != "?" & !is.na(Age)) %>%
      mutate(track_time = (60 * as.numeric(mins)) + as.numeric(secs),
             temp_dist  = str_extract(Event, "^([^ \t]+)"),
             dist_m    = case_when(
                 str_detect(temp_dist, "k") ~ as.numeric(str_extract(Event, "(\\d)+")),
                 str_detect(temp_dist, "Mile") & !is.na(as.numeric(str_extract(Event, "(\\d)+"))) ~
                     as.numeric(str_extract(Event, "(\\d)+")) * 1609,
                 # No number found for Mile then assume 1 mile
                 str_detect(Event, "Mile") ~ 1609,
                 TRUE ~ as.numeric(temp_dist))) %>%
      # to make Max Hamlyn special 200.2 m into 200m
      mutate(dist_m = round(dist_m)) %>%
      dplyr::filter(!is.na(track_time)) %>%
      group_by(Event, Sex) %>%
      arrange(track_time) %>%
      mutate(Place = dense_rank(track_time),
             Max_Hamlyn_pts = case_when(
                 Place == 1 ~ 5,
                 Place == 2 ~ 4,
                 Place == 3 ~ 3,
                 Place == 4 ~ 2,
                 Place == 5 ~ 1,
                 TRUE ~ 0),
             Date_Meet = dtvar,
             Age = as.numeric(Age)) %>%
      mutate(agegrp = case_when(
          Age < 30 ~ "Age < 30",
          Age < 35 ~ "Age 30 - 34",
          Age < 40 ~ "Age 35 - 39",
          Age < 45 ~ "Age 40 - 44",
          Age < 50 ~ "Age 45 - 50",
          Age < 55 ~ "Age 51 - 55",
          Age < 60 ~ "Age 56 - 60",
          Age < 65 ~ "Age 61 - 64",
          Age < 70 ~ "Age 65 - 69",
          Age < 75 ~ "Age 70 - 74",
          Age < 80 ~ "Age 75 - 79",
          Age < 85 ~ "Age 80 - 85",
          Age < 90 ~ "Age 86 - 89",
          is.na(Age) | Age == 0 ~ NA_character_,
          TRUE ~ "Age 90+"))
  track_res <- bind_rows(track_res, dat4)
}
colnames(track_res)


write.csv(track_res, "C:/Users/Packr1/Documents/Personal/Track/track_res.csv", row.names = FALSE)
write.csv(nodata_csv, "C:/Users/Packr1/Documents/Personal/Track/nodata.csv", row.names = FALSE)
 
