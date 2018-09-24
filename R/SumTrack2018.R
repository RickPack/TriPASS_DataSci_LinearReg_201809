# World Masters Athletics (WMA) Age-graded track tables can be found on
# Howard Grubb's website:
# http://www.howardgrubb.co.uk/athletics/data/wavacalc06.xls

library(rvest)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(patchwork)

setwd('C:/Users/Packr1/Documents/Personal/Track')
WMA_Women <- read_xls('wavacalc06.xls', sheet = 1) %>%
              select(2:ncol(.)) %>%
              mutate(dist_m = case_when(
                  grepl("WALK",  toupper(Event)) ~ (1000 * `dist(km)`) + 0.11,
                  TRUE ~ 1000 * `dist(km)`)) %>%
              mutate(Sex = 'F') %>%
              # dist_m is 0 for field events 
              # Unknown functional difference between Road distinctions
              #   (e.g., 5kmRoad vs. 5km)
              dplyr::filter(dist_m > 0 & 
                            !grepl('Road', Event))
              print(nrow(WMA_Women) - nrow(WMA_Women %>% distinct(dist_m)))
              message("Expecting 0 s=> duplicate distance")
WMA_Men   <- read_xls('wavacalc06.xls', sheet = 2) %>%
              select(2:ncol(.)) %>%
              mutate(dist_m = case_when(
                grepl("WALK",  toupper(Event)) ~ (1000 * `dist(km)`) + 0.11,
                TRUE ~ 1000 * `dist(km)`)) %>%
              mutate(Sex = 'M') %>%
              # dist_m is 0 for field events 
              # Unknown functional difference between Road distinctions
              #   (e.g., 5kmRoad vs. 5km)
              dplyr::filter(dist_m > 0 & 
                            !grepl('Road', Event))
              print(nrow(WMA_Men) - nrow(WMA_Men %>% distinct(dist_m)))
              message("Expecting 0 so no duplicate distance")

# not including results labeled "All Comers Track Meet" (different appearance)
# evt_list <- c('https://www.carolinagodiva.org/raceresults/track2000_05_24.html',
#               'https://www.carolinagodiva.org/raceresults/track2000_05_31.html',
#               'https://www.carolinagodiva.org/raceresults/track2000_06_07.html',
#               'https://www.carolinagodiva.org/raceresults/track2000_06_14.html',
#               'https://www.carolinagodiva.org/raceresults/track2000_06_21.html',
#               'https://www.carolinagodiva.org/raceresults/track2000_06_28.html',
#               'https://www.carolinagodiva.org/raceresults/track2000_07_05.html',
#               'https://www.carolinagodiva.org/raceresults/track2000_07_12.html',
#               'https://www.carolinagodiva.org/raceresults/track2001_05_23.html',
#               'https://www.carolinagodiva.org/raceresults/track2001_06_06.html',
#               'https://www.carolinagodiva.org/raceresults/track2001_06_13.html',
#               'https://www.carolinagodiva.org/raceresults/track2001_06_20.html',
#               'https://www.carolinagodiva.org/raceresults/track2001_06_27.html',
#               'https://www.carolinagodiva.org/raceresults/track2001_07_05.html',
#               'https://www.carolinagodiva.org/raceresults/track2001_07_11.html',
#               'https://www.carolinagodiva.org/raceresults/track2001_07_18.html',
#               'https://www.carolinagodiva.org/raceresults/track2001_07_25.html',
#               'https://www.carolinagodiva.org/raceresults/track2001_08_01.html',
#               'https://www.carolinagodiva.org/raceresults/track2001_08_08.html',
#               'https://www.carolinagodiva.org/raceresults/track2002_05_22.html',
#               'https://www.carolinagodiva.org/raceresults/track2002_05_29.html',
#               'https://www.carolinagodiva.org/raceresults/track2002_06_05.html',
#               'https://www.carolinagodiva.org/raceresults/track2002_06_12.html',
#               'https://www.carolinagodiva.org/raceresults/track2002_06_19.html',
#               'https://www.carolinagodiva.org/raceresults/track2002_06_26.html',
#               'https://www.carolinagodiva.org/raceresults/track2002_07_03.html',
#               'https://www.carolinagodiva.org/raceresults/track2002_07_10.html',
#               'https://www.carolinagodiva.org/raceresults/track2002_07_17.html',
#               'https://www.carolinagodiva.org/raceresults/track2002_07_24.html',
#               'https://www.carolinagodiva.org/raceresults/track2002_07_31.html',
#               'https://www.carolinagodiva.org/raceresults/track2002_08_07.html',
#               'https://www.carolinagodiva.org/raceresults/track2003_05_21.html',
#               'https://www.carolinagodiva.org/raceresults/track2003_05_28.html',
#               'https://www.carolinagodiva.org/raceresults/track2003_06_04.html',
#               'https://www.carolinagodiva.org/raceresults/track2003_06_11.html',
#               'https://www.carolinagodiva.org/raceresults/track2003_06_18.html',
#               'https://www.carolinagodiva.org/raceresults/track2003_06_25.html',
#               'https://www.carolinagodiva.org/raceresults/track2003_07_02.html',
#               'https://www.carolinagodiva.org/raceresults/track2003_07_09.html',
#               'https://www.carolinagodiva.org/raceresults/track2003_07_16.html',
#               'https://www.carolinagodiva.org/raceresults/track2003_07_23.html',
#               'https://www.carolinagodiva.org/raceresults/track2003_07_30.html',
#               'https://www.carolinagodiva.org/raceresults/track2003_08_06.html',
#               'https://www.carolinagodiva.org/raceresults/track2004_05_19.html',
#               'https://www.carolinagodiva.org/raceresults/track2004_05_26.html',
#               'https://www.carolinagodiva.org/raceresults/track2004_06_02.html',
#               'https://www.carolinagodiva.org/raceresults/track2004_06_09.html',
#               'https://www.carolinagodiva.org/raceresults/track2004_06_16.html',
#               'https://www.carolinagodiva.org/raceresults/track2004_06_23.html',  
#               'https://www.carolinagodiva.org/raceresults/track2004_06_30.html',
#               'https://www.carolinagodiva.org/raceresults/track2004_07_07.html',
#               'https://www.carolinagodiva.org/raceresults/track2004_07_14.html',
#               'https://www.carolinagodiva.org/raceresults/track2004_07_21.html',
#               'https://www.carolinagodiva.org/raceresults/track2004_07_28.html',
#               'https://www.carolinagodiva.org/raceresults/track2004_08_04.html',
#               'https://www.carolinagodiva.org/raceresults/track2005_05_25.html',
#               'https://www.carolinagodiva.org/raceresults/track2005_06_01.html',
#               'https://www.carolinagodiva.org/raceresults/track2005_06_08.html',
#               'https://www.carolinagodiva.org/raceresults/track2005_06_15.html',
#               'https://www.carolinagodiva.org/raceresults/track2005_06_22.html',
#               'https://www.carolinagodiva.org/raceresults/track2005_06_29.html',
#               'https://www.carolinagodiva.org/raceresults/track2005_07_06.html',
#               'https://www.carolinagodiva.org/raceresults/track2005_07_13.html',
#               'https://www.carolinagodiva.org/raceresults/track2005_07_20.html',
#               'https://www.carolinagodiva.org/raceresults/track2005_07_27.html',
#               'https://www.carolinagodiva.org/raceresults/track2005_08_03.html',
#               'https://www.carolinagodiva.org/raceresults/track2005_08_10.html',
#               'https://www.carolinagodiva.org/raceresults/track2008_05_28.html',
#               'https://www.carolinagodiva.org/raceresults/track2008_06_04.html',
#               'https://www.carolinagodiva.org/raceresults/track2008_06_11.html',
#               'https://www.carolinagodiva.org/raceresults/track2008_06_18.html',
#               'https://www.carolinagodiva.org/raceresults/track2008_06_25.html',
#               'https://www.carolinagodiva.org/raceresults/track2008_07_02.html',
#               'https://www.carolinagodiva.org/raceresults/track2008_07_09.html',
#               'https://www.carolinagodiva.org/raceresults/track2008_07_16.html',
#               'https://www.carolinagodiva.org/raceresults/track2008_07_23.html',
#               'https://www.carolinagodiva.org/raceresults/track2008_07_30.html',
#               'https://www.carolinagodiva.org/raceresults/track2008_08_06.html',
#               'https://www.carolinagodiva.org/raceresults/track2007_05_23.html',
#               'https://www.carolinagodiva.org/raceresults/track2007_05_30.html',
#               'https://www.carolinagodiva.org/raceresults/track2007_06_06.html',
#               'https://www.carolinagodiva.org/raceresults/track2007_06_13.html',
#               'https://www.carolinagodiva.org/raceresults/track2007_06_20.html',
#               'https://www.carolinagodiva.org/raceresults/track2007_06_27.html',
#               'https://www.carolinagodiva.org/raceresults/track2007_07_05.html',
#               'https://www.carolinagodiva.org/raceresults/track2007_07_11.html',
#               'https://www.carolinagodiva.org/raceresults/track2007_07_18.html',
#               'https://www.carolinagodiva.org/raceresults/track2007_07_25.html',
#               'https://www.carolinagodiva.org/raceresults/track2007_08_01.html',
#               'https://www.carolinagodiva.org/raceresults/track2007_08_08.html',
#               'https://www.carolinagodiva.org/raceresults/track2006_05_24.html',
#               'https://www.carolinagodiva.org/raceresults/track2006_05_31.html',
#               'https://www.carolinagodiva.org/raceresults/track2006_06_07.html',
#               'https://www.carolinagodiva.org/raceresults/track2006_06_14.html',
#               'https://www.carolinagodiva.org/raceresults/track2006_06_21.html',
#               'https://www.carolinagodiva.org/raceresults/track2006_06_28.html',
#               'https://www.carolinagodiva.org/raceresults/track2006_07_05.html',
#               'https://www.carolinagodiva.org/raceresults/track2006_07_12.html',
#               'https://www.carolinagodiva.org/raceresults/track2006_07_19.html',
#               'https://www.carolinagodiva.org/raceresults/track2006_07_26.html',
#               'https://www.carolinagodiva.org/raceresults/track2006_08_02.html',
#               'https://www.carolinagodiva.org/raceresults/track2006_08_09.html',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---5-20-2009',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---5-27-2009',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---6-3-2009',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---6-10-2009',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---6-17-2009',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---6-24-2009',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---7-1-2009',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---7-8-2009',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---7-15-2009',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---7-22-2009',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---7-29-2009',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---8-5-2009',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---5-19-2010',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---5-26-2010',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---6-2-2010',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---6-9-2010',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---6-16-2010',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---6-23-2010',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---6-3-2010',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---7-7-2010',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---7-14-2010',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-21-2010',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---7-28-2010',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---8-4-2010',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---5-18-2011',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---5-25-2011',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---6-1-2011',
#               'https://www.carolinagodiva.org/index.php?page=Summer-Track---6-8-2011',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---6-15-2011',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---6-22-2011',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---6-29-2011',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---7-6-2011',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---7-13-2011',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---7-20-2011',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---7-27-2011',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---8-3-201',
#               'https://www.carolinagodiva.org/index.php?page=summer-track---8-10-2011',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-5-30-2012',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-06-2012',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-13-2012',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-20-2012',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-27-2012',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-05-2012',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-11-2012',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-18-2012',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-25-2012',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-8-1-2012',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-8-8-2012',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-5-15-2013',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-5-22-2013',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-5-29-2013',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-5-2013',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-12-2013',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-19-2013',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-26-2013',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-3-2013',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-10-2013',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-17-2013',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-24-2013',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-31-2013',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-8-07-2013',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-5-21-2014',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-5-28-2014',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-04-2014',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-11-2014',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-18-2014',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-25-2014',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-2-2014',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-9-2014',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-16-2014',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-23-2014',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-30-2014',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-8-6-2014',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-5-20-2015',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-5-27-2015',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-3-2015',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-10-2015',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-17-2015',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-24-2015',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-01-2015',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-08-2015',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-15-2015',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-22-2015',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-29-2015',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-8-05-2015',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-5-18-2016',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-5-25-2016',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-01-2016',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-08-2016',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-15-2016',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-22-2016',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-29-2016',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-06-2016',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-13-2016',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-20-2016',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-27-2016',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-8-03-2016',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-5-24-2017',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-5-31-2017',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-6-28-2017',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-5-2017',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-12-2017',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-19-2017',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-7-26-2017',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-8-2-2017',
#               'https://www.carolinagodiva.org/index.php?page=summer-track-8-9-2017',
evt_list <- c('https://www.carolinagodiva.org/index.php?page=summer-track-5-30-2018',
              'https://www.carolinagodiva.org/index.php?page=summer-track-6-06-2018',
              'https://www.carolinagodiva.org/index.php?page=summer-track-6-13-2018',
              'https://www.carolinagodiva.org/index.php?page=summer-track-6-20-2018',
              'https://www.carolinagodiva.org/index.php?page=summer-track-6-27-2018',
              'https://www.carolinagodiva.org/index.php?page=summer-track-7-04-2018',
              'https://www.carolinagodiva.org/index.php?page=summer-track-7-11-2018',
              'https://www.carolinagodiva.org/index.php?page=summer-track-7-18-2018',
              'https://www.carolinagodiva.org/index.php?page=summer-track-7-25-2018',
              'https://www.carolinagodiva.org/index.php?page=summer-track-8-1-2018')

weather_2018_ht <- read_html('https://www.carolinagodiva.org/index.php?page=track-season-weather-conditions')
temps_2018      <- weather_2018_ht %>% html_nodes(xpath="//tr[(((count(preceding-sibling::*) + 1) = 12) and parent::*)]//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//span | //tr[(((count(preceding-sibling::*) + 1) = 11) and parent::*)]//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//span | //tr[(((count(preceding-sibling::*) + 1) = 10) and parent::*)]//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//span | //tr[(((count(preceding-sibling::*) + 1) = 9) and parent::*)]//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//span | //tr[(((count(preceding-sibling::*) + 1) = 8) and parent::*)]//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//span | //tr[(((count(preceding-sibling::*) + 1) = 7) and parent::*)]//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//span | //tr[(((count(preceding-sibling::*) + 1) = 6) and parent::*)]//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//span | //tr[(((count(preceding-sibling::*) + 1) = 5) and parent::*)]//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//span | //tr[(((count(preceding-sibling::*) + 1) = 4) and parent::*)]//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//span | //tr[(((count(preceding-sibling::*) + 1) = 3) and parent::*)]//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//span") %>% 
    html_text() %>%
    as.numeric()
# write.csv(temps_2018, "C:/Users/Packr1/Documents/Personal/Track/temps_2018.csv", row.names = FALSE)

track_res <- data.frame()
p_list    <- list()

for(evt in 1:length(evt_list)){
dat1 <- read_html(evt_list[evt])
dig_date <- str_replace_all(evt_list[evt], "[^0-9]", "")
if(2000 <= as.numeric(str_sub(dig_date, 1, 4)) & 2050 >= as.numeric(str_sub(dig_date, 1, 4))){
    dtvar <- ymd(dig_date)
} else {
    dig_date <- paste0("0", dig_date)
    if(nchar(dig_date)==7){
        dig_date <- paste0(str_sub(dig_date, 1, 2), "0", str_sub(dig_date,3))
    }
    dtvar <- mdy(dig_date)
}
print(paste0(evt, " : ", dtvar))

dat1_events <- dat1 %>% html_nodes("strong") %>% html_text() %>% tbl_df(.) %>% distinct(value) %>% mutate(ct_evt = row_number() * 1000) %>%
    rename(Name = value)
# for local save when needed
# write.csv(dat1_events, paste0("C:/Users/Packr1/Documents/Personal/Track/Godiva_Summer_", evt, "_Events.csv"))
# For local read when needed (comment out lines above to start of for loop)
# dat1_events <- read.csv(paste0("C:/Users/Packr1/Documents/Personal/Track/Godiva_Summer_", evt, "_Events.csv"))
# 
dat2 <- dat1 %>% html_nodes("td") %>% html_text()


# structure(dat2, dim=1:4,length(dat2/4))
#     tbl_df(.)
#     left_join(., data1_events

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
# For local save when needed
  write.csv(dat3, paste0("C:/Users/Packr1/Documents/Personal/Track/Godiva_Summer_", evt, ".csv"), row.names = FALSE)
# For local read when needed (comment out lines above to start of for loop)
# dat3 <- read.csv(paste0("C:/Users/Packr1/Documents/Personal/Track/Godiva_Summer_", evt, ".csv"))


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

p1 <- ggplot(dat4, aes(Place, track_time)) + 
    geom_point() +
    facet_wrap(. ~ Event, scales = "free_y", ncol = 3) +
    xlab("Place") +
    ylab("Run time (seconds)") +
    ggtitle("Carolina Godiva Track Club Summer Track Series",
            subtitle = dtvar)
p_list[[evt]] <- p1
}
# write.csv(track_res, "C:/Users/Packr1/Documents/Personal/Track/track_res_2018.csv", row.names = FALSE)
track_res <- read_csv('C:/Users/Packr1/Documents/Personal/Track/track_res_2018.csv') %>%
            # Adding a bit of distance for Walks, to differentiate
            # in join with the age-graded tables
            mutate(dist_m = case_when(
                grepl("WALK", toupper(Event)) ~ as.integer(dist_m + 0.11),
                TRUE ~ dist_m))
track_res %>% group_by(Name, Sex, Event, Date_Meet) %>% 
         summarise(ct = n()) %>% dplyr::filter(ct > 1) %>% ungroup()
track_res %>% dplyr::filter(str_trim(Name) == 'Brendan Murray' & str_trim(toupper(Event)) == '5K RUN')
track_res <- track_res %>% dplyr::filter(!is.na(Age)) %>%
    distinct(Name, Sex, Event, Date_Meet, .keep_all = TRUE)
track_res <- track_res %>% dplyr::filter(!is.na(agegrp)) %>%
              # Age-graded track tables start at age 4
              dplyr::filter(Age > 4) 
              
m_ages_l <- length(unique(track_res %>% dplyr::filter(Sex=='M') %>% pull(Age)))
f_ages_l <- length(unique(track_res %>% dplyr::filter(Sex=='F') %>% pull(Age)))
m_ages <- unique(track_res %>% dplyr::filter(Sex=='M') %>% pull(Age))
f_ages <- unique(track_res %>% dplyr::filter(Sex=='F') %>% pull(Age))
dat_m1 <- data.frame()
dat_f1 <- data.frame()

for(m_age in 1:m_ages_l){
    m_age_in   <- m_ages[m_age]
    # Notice need sym instead of enquo and as.character because sym 
    # requires string
    m_age_nam  <- sym(as.character(m_age_in))
    WMA_Men_in <- WMA_Men %>% mutate(agefactor := !!m_age_nam) %>%
                    select(agefactor, dist_m, Sex)
      dat_m1_0 <- track_res %>% dplyr::filter(Age == m_age_in) %>%
                     inner_join(., WMA_Men_in, by = c("Sex", "dist_m")) %>%
                     mutate(age_grade_track_time = track_time * agefactor) %>%
                     select(Name, Sex, Date_Meet, dist_m, age_grade_track_time, agefactor)
      dat_m1   <- bind_rows(dat_m1, dat_m1_0)
}
for(f_age in 1:f_ages_l){
    f_age_in   <- f_ages[f_age]
    # Notice need sym instead of enquo and as.character because sym 
    # requires string
    f_age_nam  <- sym(as.character(f_age_in))
    WMA_Women_in <- WMA_Women %>% mutate(agefactor := !!f_age_nam) %>%
                     select(agefactor, dist_m, Sex)
      dat_f1_0 <- track_res %>% dplyr::filter(Age == f_age_in) %>%
                     inner_join(., WMA_Women_in, by = c("Sex", "dist_m")) %>%
                     mutate(age_grade_track_time = track_time * agefactor) %>%
                     select(Name, Sex, Date_Meet, dist_m, age_grade_track_time, agefactor)
      dat_f1   <- bind_rows(dat_f1, dat_f1_0)
}

dat_all <- bind_rows(dat_m1, dat_f1)
track_res_all <- left_join(track_res, dat_all, by = c("Name", "Sex", "Date_Meet", "dist_m"))

track_res_all <- track_res_all %>% dplyr::filter(!is.na(agegrp)) %>%
                    arrange(track_time) 

track_res_age <- track_res_all %>%
    group_by(dist_m, agegrp) %>%
    summarise(median_age = median(track_time)) %>%
    ungroup()

track_res_ind <- track_res_all %>%
    group_by(dist_m, Name) %>%
    summarise(median_ind = median(track_time)) %>%
    ungroup()

track_res_age_age <- track_res_all %>%
    group_by(dist_m, agegrp) %>%
    summarise(median_age = median(age_grade_track_time)) %>%
    ungroup()

track_res_ind_age <- track_res_all %>%
    group_by(dist_m, Name) %>%
    summarise(median_ind = median(age_grade_track_time)) %>%
    ungroup()

track_res_age_age %>% dplyr::filter(is.na(median_age)) %>% janitor::tabyl(dist_m) %>% arrange(desc(n))

# Diverging dot plot [top 3 per age group] by Selva Prabhakaran
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html   
plotfunc <- function(dist=100, subtitl_txt="", gender=c("M", "F"), sz = 28){
    
    track_res_all_top3age <-
        track_res_all %>%
        dplyr::filter(Sex %in% gender & dist_m == dist) %>%
        group_by(Name) %>%
        slice(which.min(track_time)) %>%
        group_by(agegrp) %>%
        top_n(-3, track_time) %>%
        mutate(Name = paste0(Name, " (", Age, ")")) %>%
        ungroup() %>%
        arrange(track_time)
    
    # track_res_all_top3age <- within(track_res_all_top3age, 
    #                                 Name2 <- factor(Name, 
    #                                                 levels=names(sort(track_time))))
    ggplot(track_res_all_top3age, aes(x = reorder(Name, desc(track_time)), y = track_time)) +
               geom_point(stat='identity', aes(col=agegrp), size = 6) +
               scale_color_viridis_d(name='Age Group') +
               xlab("Runner's Name (Age)") +
               ylab("Run time (seconds)") +
               ggtitle("Carolina Godiva Track Club Summer Track Series (2018)",
                       subtitle = subtitl_txt) +
        theme(plot.title = element_text(hjust = 0.5, color = '#EEEEEE',
                                        size = sz),
              plot.subtitle = element_text(hjust = 0.5, color = '#EEEEEE',
                                           size = sz * 20/28),
              text = element_text(size = 14, color = '#EEEEEE'),
              axis.text = element_text(size = 10, color = '#EEEEEE'),
              legend.text = element_text(size = 12, color = '#EEEEEE'),
              legend.background = element_rect(fill = '#333333'),
              panel.background = element_rect(fill = '#333333'),
              strip.background = element_rect(fill = '#333333'),
              plot.background = element_rect(fill = '#333333')) +
               coord_flip()
}
plotfunc(100, "100 Meter: dot plot [top 3 per age group]", c("M", "F"))

p100 <- plotfunc(100, "100 Meter: Adults dot plot [top 3 per age group]", c("M", "F"))
p100_M <- plotfunc(100, "100 Meter - Adult Males   : dot plot [top 3 per age group]", "M")
p100_F <- plotfunc(100, "100 Meter - Adult Females : dot plot [top 3 per age group]", "F")

p400   <- plotfunc(400, "400 Meter: Adults dot plot [top 3 per age group]", c("M", "F"))
p400_M <- plotfunc(400, "400 Meter - Adult Males   : dot plot [top 3 per age group]", "M")
p400_F <- plotfunc(400, "400 Meter - Adult Females : dot plot [top 3 per age group]", "F")

p1600   <- plotfunc(1609, "1 Mile (1609m): Adults dot plot [top 3 per age group]")
p1600_M <- plotfunc(1609, "1 Mile (1609m) - Adult Males   : dot plot [top 3 per age group]", "M")
p1600_F <- plotfunc(1609, "1 Mile (1609m) - Adult Females : dot plot [top 3 per age group]", "F")

plotfunc_grade <- function(dist=100, subtitl_txt="", gender=c("M", "F"), sz = 28){
    
    track_res_all_top3age_grade <-
        track_res_all %>%
        dplyr::filter(Sex %in% gender & dist_m == dist) %>%
        group_by(Name) %>%
        slice(which.min(track_time)) %>%
        group_by(agegrp) %>%
        top_n(-3, age_grade_track_time) %>%
        mutate(Name = paste0(Name, " (", Age, ")")) %>%
        ungroup() %>%
        arrange(age_grade_track_time)
    
    ggplot(track_res_all_top3age_grade, aes(x = reorder(Name, desc(age_grade_track_time)), y = age_grade_track_time)) +
        geom_point(stat='identity', aes(col=agegrp), size = 6) +
        scale_color_viridis_d(name='Age Group') +
        xlab("Runner's Name (Age)") +
        ylab("Age-Graded run time (seconds)") +
        ggtitle("Carolina Godiva Track Club Summer Track Series (2018)",
                subtitle = subtitl_txt) +
        theme(plot.title = element_text(hjust = 0.5, color = '#EEEEEE',
                                        size = sz),
              plot.subtitle = element_text(hjust = 0.5, color = '#EEEEEE',
                                           size = sz * 20/28),
              text = element_text(size = 14, color = '#EEEEEE'),
              axis.text = element_text(size = 10, color = '#EEEEEE'),
              legend.text = element_text(size = 12, color = '#EEEEEE'),
              legend.background = element_rect(fill = '#333333'),
              panel.background = element_rect(fill = '#333333'),
              strip.background = element_rect(fill = '#333333'),
              plot.background = element_rect(fill = '#333333')) +
        coord_flip()
}
p100G <- plotfunc_grade(100, "100 Meter: Adults dot plot [top 3 per age group]")
p100_MG <- plotfunc_grade(100, "100 Meter - Adult Males   : dot plot [top 3 per age group]", "M")
p100_FG <- plotfunc_grade(100, "100 Meter - Adult Females : dot plot [top 3 per age group]", "F")

p400G   <- plotfunc_grade(400, "400 Meter: Adults dot plot [top 3 per age group]")
p400_MG <- plotfunc_grade(400, "400 Meter - Adult Males   : dot plot [top 3 per age group]", "M")
p400_FG <- plotfunc_grade(400, "400 Meter - Adult Females : dot plot [top 3 per age group]", "F")

p1600G   <- plotfunc_grade(1609, "1 Mile (1609m): Adults dot plot [top 3 per age group]")
p1600_MG <- plotfunc_grade(1609, "1 Mile (1609m) - Adult Males   : dot plot [top 3 per age group]", "M")
p1600_FG <- plotfunc_grade(1609, "1 Mile (1609m) - Adult Females : dot plot [top 3 per age group]", "F")

    ggs <- function(grph, grphout){
       ggsave(grph, filename=paste0("C:/Users/Packr1/Documents/Personal/Track/Graphs/", grphout, ".png"), width=11, height = 8)
    }
    ggs(p100, "p100")
    ggs(p100_F, "p100_F")
    ggs(p100_M, "p100_M")
    ggs(p400, "p400")
    ggs(p400_F, "p400_F")
    ggs(p400_M, "p400_M")
    ggs(p1600, "p1600")
    ggs(p1600_F, "p1600_F")
    ggs(p1600_M, "p1600_M")
    ggs(p100G, "p100G")
    ggs(p100_FG, "p100_FG")
    ggs(p100_MG, "p100_MG")
    ggs(p400G, "p400G")
    ggs(p400_FG, "p400_FG")
    ggs(p400_MG, "p400_MG")
    ggs(p1600G, "p1600G")
    ggs(p1600_FG, "p1600_FG")
    ggs(p1600_MG, "p1600_MG")
    
    p1600_s  <- plotfunc(1609, "1 Mile (1609m): Adults dot plot [top 3 per age group]", sz = 16)
    p400_FGs <- plotfunc_grade(400, "400 Meter - Adult Females   : dot plot [top 3 per age group]", "F", sz = 16)
    p100_s  <- plotfunc(100, "100 Meter - Adults dot plot [top 3 per age group]", sz = 16)
    p100_Gs <- plotfunc_grade(100, "100 Meter - Adults dot plot [top 3 per age group]", sz = 16)
    p800_Gs <- plotfunc_grade(800, "800 Meter - Adults dot plot [top 3 per age group]", sz = 16)
    gridExtra::grid.arrange(p1600_s, p400_FGs, ncol = 2)
    gridExtra::grid.arrange(p100_s, p800_Gs, ncol = 2)
    gridExtra::grid.arrange(p100_s, p100_Gs, ncol = 2)
    
max_hamlyn <- track_res_all %>%
    group_by(Name) %>%
    mutate(Max_HamLyn_pts_total = sum(Max_Hamlyn_pts)) %>%
    slice(which.max(Age)) %>%
    ungroup() %>%
    mutate(Name = paste0(Name, " (", Age, ")"))

max_hamlyn_top10_F <- max_hamlyn %>% dplyr::filter(Sex=='F') %>% top_n(10, Max_HamLyn_pts_total)
ggplot(max_hamlyn_top10_F, 
       aes(x = reorder(Name, Max_HamLyn_pts_total), y = Max_HamLyn_pts_total)) +
    geom_point(stat='identity', aes(col=agegrp), size = 6) +
    scale_color_viridis_d(name='Age Group') +
    xlab("Runner's Name") +
    ylab("Max Hamlyn Points") +
    ggtitle("Carolina Godiva Track Club Summer Track Series (2018) [Top 10 Women]",
            subtitle = "Max Hamlyn Competition") +
    geom_text(label = max_hamlyn_top10_F$Max_HamLyn_pts_total, size = 08, color = '#EEEEEE', vjust = 1) + 
    theme(plot.title = element_text(hjust = 0.5, color = '#EEEEEE',
                                    size = 22),
          plot.subtitle = element_text(hjust = 0.5, color = '#EEEEEE',
                                       size = 22 * 20/28),
          text = element_text(size = 14, color = '#EEEEEE'),
          axis.text.x = element_text(size = 16, color = '#EEEEEE'),
          axis.text.y = element_text(size = 20, color = '#EEEEEE'),
          legend.text = element_text(size = 12, color = '#EEEEEE'),
          legend.background = element_rect(fill = '#333333'),
          panel.background = element_rect(fill = '#333333'),
          strip.background = element_rect(fill = '#333333'),
          plot.background = element_rect(fill = '#333333')) +
    coord_flip()

max_hamlyn_top10_M <- max_hamlyn %>% dplyr::filter(Sex=='M') %>% top_n(10, Max_HamLyn_pts_total)
ggplot(max_hamlyn_top10_M, 
       aes(x = reorder(Name, Max_HamLyn_pts_total), y = Max_HamLyn_pts_total)) +
    geom_point(stat='identity', aes(col=agegrp), size = 6) +
    scale_color_viridis_d(name='Age Group') +
    xlab("Runner's Name") +
    ylab("Max Hamlyn Points") +
    ggtitle("Carolina Godiva Track Club Summer Track Series (2018) [Top 10 Men]",
            subtitle = "Max Hamlyn Competition") +
    geom_text(label = max_hamlyn_top10_M$Max_HamLyn_pts_total, size = 08, color = '#EEEEEE', vjust = 1) + 
    theme(plot.title = element_text(hjust = 0.5, color = '#EEEEEE',
                                    size = 22),
          plot.subtitle = element_text(hjust = 0.5, color = '#EEEEEE',
                                       size = 22 * 20/28),
          text = element_text(size = 14, color = '#EEEEEE'),
          axis.text.x = element_text(size = 16, color = '#EEEEEE'),
          axis.text.y = element_text(size = 20, color = '#EEEEEE'),
          legend.text = element_text(size = 12, color = '#EEEEEE'),
          legend.background = element_rect(fill = '#333333'),
          panel.background = element_rect(fill = '#333333'),
          strip.background = element_rect(fill = '#333333'),
          plot.background = element_rect(fill = '#333333')) +
    coord_flip()

ken <- track_res_all %>% dplyr::filter(Name=='Ken Larsen') 

# ggrepel
ggplot(data=ken, aes( x = ymd(Date_Meet), y = track_time, color = dist_m, group = dist_m)) +
    geom_line() +
    scale_colour_viridis_c() +
    theme_classic() +
    xlab("Date of Meet") +
    ylab("Run time (seconds)")

ggplot(data=ken, aes( x = ymd(Date_Meet), y = track_time, color = dist_m, group = dist_m)) +
    geom_line() +
    scale_colour_viridis_c() +
    theme_classic() +
    xlab("Date of Meet") +
    ylab("Run time (seconds)")

# ken %>% dplyr::filter(Date_Meet=='2018-07-18') %>% tidyr::spread(., dist_m, track_time)


ken_col_events <- 
    ken %>% select(Date_Meet, Name, dist_m, track_time) %>% 
    tidyr::spread(., dist_m, track_time)
ken_col_events


ken_col_events$seq_event_grp <- rep(1:2, length.out = max(floor(nrow(ken_col_events)), nrow(ken_col_events)))
ken_col_events2 <- ken_col_events %>%
    group_by(seq_event_grp) %>%
    unite(short, "100", "200") %>%
    unite(med, "400", "800") %>%
    unite(long, "1500", "1609") %>%
    select(Name, Date_Meet, short, med, long)
