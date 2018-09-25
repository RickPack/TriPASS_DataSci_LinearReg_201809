Produce CSV with 2018 Summer Track Series results
================
Rick Pack
September 24, 2018

``` r
# World Masters Athletics (WMA) Age-graded track tables can be found on
# Howard Grubb's website:
# http://www.howardgrubb.co.uk/athletics/data/wavacalc06.xls

library(rvest)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(patchwork)
library(stringr)
library(ggplot2)

WMA_Women <- read_xls('C:/Users/Packr1/Documents/Personal/Track/wavacalc06.xls', sheet = 1) %>%
              select(2:ncol(.)) %>%
                mutate(`dist(km)_2` = case_when( 
                    grepl('1500MWALK', toupper(Event)) ~ 1.5,
                    grepl('1MILEWALK', toupper(Event)) ~ 1.609,
                    grepl('MWALK', toupper(Event)) ~ as.numeric(str_extract(Event, "[\\d]*")) + 0.11,
                    TRUE ~ (`dist(km)`)),
                    dist_m = case_when(
                        grepl("WALK",  toupper(Event)) ~ ((1000 * `dist(km)_2`) + 0.11),
                        TRUE ~ 1000 * `dist(km)_2`)) %>%
                mutate(Sex = 'F') %>%
                # dist(km)_2 is 0 for field events and "Mar.Walk" events
                # Unknown functional difference in events containing "Road"
                #   (e.g., 5kmRoad vs. 5km)
                dplyr::filter(`dist(km)_2` > 0 & 
                                !grepl('Road', Event))
print(nrow(WMA_Women) - nrow(WMA_Women %>% distinct(Event, dist_m)))
```

    ## [1] 0

``` r
message("Expecting 0 so no duplicate distance")

WMA_Men   <- read_xls('C:/Users/Packr1/Documents/Personal/Track/wavacalc06.xls', sheet = 2) %>%
              select(2:ncol(.)) %>%
                mutate(`dist(km)_2` = case_when( 
                            grepl('1500MWALK', toupper(Event)) ~ 1.5,
                            grepl('1MILEWALK', toupper(Event)) ~ 1.609,
                            grepl('MWALK', toupper(Event)) ~ as.numeric(str_extract(Event, "[\\d]*")) + 0.11,
                            TRUE ~ (`dist(km)`)),
                       dist_m = case_when(
                            grepl("WALK",  toupper(Event)) ~ ((1000 * `dist(km)_2`) + 0.11),
                            TRUE ~ 1000 * `dist(km)_2`)) %>%
                mutate(Sex = 'M') %>%
                # dist(km)_2 is 0 for field events and "Mar.Walk" events
                # Unknown functional difference in events containing "Road"
                #   (e.g., 5kmRoad vs. 5km)
                dplyr::filter(`dist(km)_2` > 0 & 
                                  !grepl('Road', Event))

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
# write.csv(temps_2018, "temps_2018.csv", row.names = FALSE)

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
# write.csv(dat1_events, paste0("Godiva_Summer_", evt, "_Events.csv"))
# For local read when needed (comment out lines above to start of for loop)
# dat1_events <- read.csv(paste0("Godiva_Summer_", evt, "_Events.csv"))
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
# write.csv(dat3, paste0("Godiva_Summer_", evt, ".csv"), row.names = FALSE)
# For local read when needed (comment out lines above to start of for loop)
# dat3 <- read.csv(paste0("Godiva_Summer_", evt, ".csv"))


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
           dist_m0    = case_when(
               str_detect(toupper(Event), "K RUN") ~ as.numeric(str_extract(Event, "(\\d)+")) * 1000,
               str_detect(temp_dist, "Mile") & !is.na(as.numeric(str_extract(Event, "(\\d)+"))) ~
                   as.numeric(str_extract(Event, "(\\d)+")) * 1609,
               # No number found for Mile then assume 1 mile
               str_detect(Event, "Mile") ~ 1609,
               # to make Max Hamlyn special 200.2 m into 200m
               str_detect(Event, "200.2") ~ 200,
               TRUE ~ as.numeric(temp_dist)),
           dist_m    = case_when(
               # Adding a bit of distance for Walks, to differentiate
               # in join with the age-graded tables
               grepl("WALK", toupper(Event)) ~ dist_m0 + 0.11,
               TRUE ~ dist_m0),
           Date_Meet = dtvar,
           Age = as.numeric(Age)) %>%
    dplyr::filter(!is.na(track_time)) %>%
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
```

    ## [1] "1 : 2018-05-30"

    ## Warning in eval_bare(f[[3]], env): NAs introduced by coercion

    ## Warning in evalq(as.numeric(Age), <environment>): NAs introduced by
    ## coercion

    ## [1] "2 : 2018-06-06"

    ## Warning in eval_bare(f[[3]], env): NAs introduced by coercion

    ## [1] "3 : 2018-06-13"

    ## Warning in eval_bare(f[[3]], env): NAs introduced by coercion

    ## [1] "4 : 2018-06-20"
    ## [1] "5 : 2018-06-27"

    ## Warning in eval_bare(f[[3]], env): NAs introduced by coercion

    ## [1] "6 : 2018-07-04"

    ## Warning in evalq(as.numeric(Age), <environment>): NAs introduced by
    ## coercion

    ## [1] "7 : 2018-07-11"

    ## Warning in eval_bare(f[[3]], env): NAs introduced by coercion

    ## [1] "8 : 2018-07-18"

    ## Warning in eval_bare(f[[3]], env): NAs introduced by coercion

    ## Warning in evalq(as.numeric(Age), <environment>): NAs introduced by
    ## coercion

    ## [1] "9 : 2018-07-25"

    ## Warning in eval_bare(f[[3]], env): NAs introduced by coercion

    ## [1] "10 : 2018-08-01"

    ## Warning in eval_bare(f[[3]], env): NAs introduced by coercion

``` r
print(track_res %>% distinct(Event, dist_m) %>% arrange(dist_m))
```

    ##               Event  dist_m
    ## 1             100 M  100.00
    ## 2         100 M Run  100.00
    ## 3     200 Meter Run  200.00
    ## 4         200 M Run  200.00
    ## 5       200.2 M Run  200.00
    ## 6         400 M Run  400.00
    ## 7             400 M  400.00
    ## 8         800 M Run  800.00
    ## 9            1K Run 1000.00
    ## 10       1500 M Run 1500.00
    ## 11           1500 M 1500.00
    ## 12 1500 M Race Walk 1500.11
    ## 13  1500 M Racewalk 1500.11
    ## 14         Mile Run 1609.00
    ## 15       1 Mile Run 1609.00
    ## 16    Mile Racewalk 1609.11
    ## 17  1 Mile Racewalk 1609.11
    ## 18  1998 M Racewalk 1998.11
    ## 19           3K Run 3000.00
    ## 20          3 K Run 3000.00
    ## 21           5K Run 5000.00

``` r
track_res %>% group_by(Name, Sex, dist_m, Event, Date_Meet) %>% 
    mutate(ct = n()) %>% dplyr::filter(ct > 1) %>% ungroup()
```

    ## # A tibble: 4 x 15
    ##   Name            Sex     Age Time  ct_evt Event  mins  secs  track_time
    ##   <chr>           <chr> <dbl> <chr>  <dbl> <chr>  <chr> <chr>      <dbl>
    ## 1 Brendan Murray  M        30 19:33   5000 5K Run 19    33          1173
    ## 2 Brendan Murray  M        30 19:33   5000 5K Run 19    33          1173
    ## 3 Jonathan Crouse M        13 22:13   5000 5K Run 22    13          1333
    ## 4 Jonathan Crouse M        13 22:13   5000 5K Run 22    13          1333
    ##   temp_dist dist_m0 dist_m Date_Meet  agegrp         ct
    ##   <chr>       <dbl>  <dbl> <date>     <chr>       <int>
    ## 1 5K           5000   5000 2018-05-30 Age 30 - 34     2
    ## 2 5K           5000   5000 2018-05-30 Age 30 - 34     2
    ## 3 5K           5000   5000 2018-07-11 Age < 30        2
    ## 4 5K           5000   5000 2018-07-11 Age < 30        2

``` r
track_res2 <- track_res %>% group_by(Name, Sex, dist_m, Event, Date_Meet) %>% 
             mutate(rownum = row_number()) %>% 
             ungroup() %>%
             dplyr::filter(rownum == 1) %>%
             group_by(Event, Sex) %>%
             arrange(track_time) %>%
             mutate(Place = dense_rank(track_time),
                    Max_Hamlyn_pts = case_when(
                       Place == 1 ~ 5,
                       Place == 2 ~ 4,
                       Place == 3 ~ 3,
                       Place == 4 ~ 2,
                       Place == 5 ~ 1,
                       TRUE ~ 0)) %>%
            ungroup()
message("expecting 0")
nrow(track_res2) - nrow(track_res %>% distinct(Name, Sex, Event, Date_Meet, .keep_all = TRUE))
```

    ## [1] 0

``` r
# write.csv(track_res2, "godiva_summer_track_res_2018.csv", row.names = FALSE)
# track_res2 <- read_csv("godiva_summer_track_res_2018.csv")
track_res2 %>%
    dplyr::filter(grepl("LEIF", toupper(Name))) %>%
    arrange(Date_Meet, ct_evt) %>%
    select(Name, Sex, Age, Date_Meet, Event, dist_m, ct_evt, Date_Meet, Max_Hamlyn_pts) %>%
    dplyr::filter(Max_Hamlyn_pts > 0) %>%
    print(tbl_df(.), n = nrow(track_res))
```

    ## # A tibble: 13 x 8
    ##    Name           Sex     Age Date_Meet  Event            dist_m ct_evt
    ##    <chr>          <chr> <dbl> <date>     <chr>             <dbl>  <dbl>
    ##  1 Leif Rasmussen M        15 2018-05-30 Mile Racewalk     1609.   3000
    ##  2 Leif Rasmussen M        15 2018-06-06 1500 M Race Walk  1500.   3000
    ##  3 Leif Rasmussen M        15 2018-06-13 Mile Racewalk     1609.   3000
    ##  4 Leif Rasmussen M        15 2018-06-20 100 M              100    2000
    ##  5 Leif Rasmussen M        15 2018-06-20 1500 M Racewalk   1500.   3000
    ##  6 Leif Rasmussen M        15 2018-06-20 400 M              400    4000
    ##  7 Leif Rasmussen M        15 2018-06-27 Mile Racewalk     1609.   3000
    ##  8 Leif Rasmussen M        15 2018-07-18 1500 M Run        1500    1000
    ##  9 Leif Rasmussen M        15 2018-07-18 100 M Run          100    2000
    ## 10 Leif Rasmussen M        15 2018-07-18 1500 M Racewalk   1500.   3000
    ## 11 Leif Rasmussen M        15 2018-07-25 1K Run            1000    1000
    ## 12 Leif Rasmussen M        15 2018-08-01 1 Mile Run        1609    1000
    ## 13 Leif Rasmussen M        15 2018-08-01 1 Mile Racewalk   1609.   3000
    ##    Max_Hamlyn_pts
    ##             <dbl>
    ##  1              3
    ##  2              5
    ##  3              2
    ##  4              3
    ##  5              5
    ##  6              2
    ##  7              5
    ##  8              3
    ##  9              2
    ## 10              5
    ## 11              4
    ## 12              4
    ## 13              5

``` r
milecheck <- track_res2 %>% 
    dplyr::filter(Date_Meet == ymd('2018-08-01') & grepl("1", Event)) %>% 
    arrange(dist_m, Event) %>% 
    dplyr::filter(Max_Hamlyn_pts > 0 )
print(milecheck, n = nrow(milecheck))
```

    ## # A tibble: 22 x 17
    ##    Name              Sex     Age Time  ct_evt Event           mins  secs 
    ##    <chr>             <chr> <dbl> <chr>  <dbl> <chr>           <chr> <chr>
    ##  1 Brendan Murray    M        30 5:22    1000 1 Mile Run      5     22   
    ##  2 Leif Rasmussen    M        15 5:31    1000 1 Mile Run      5     31   
    ##  3 Ted Richardson    M        48 5:45    1000 1 Mile Run      5     45   
    ##  4 Michael Fields    M        26 5:49    1000 1 Mile Run      5     49   
    ##  5 Nicholas Min      M        15 5:50    1000 1 Mile Run      5     50   
    ##  6 Kevin Nickodem    M        61 5:50    1000 1 Mile Run      5     50   
    ##  7 Brian Stull       M        47 5:50    1000 1 Mile Run      5     50   
    ##  8 Roxanne Springer  F        54 6:17    1000 1 Mile Run      6     17   
    ##  9 Kaina Morey       F        17 6:19    1000 1 Mile Run      6     19   
    ## 10 Robin Richardson  F        48 6:34    1000 1 Mile Run      6     34   
    ## 11 Amy Cummings      F        44 7:25    1000 1 Mile Run      7     25   
    ## 12 Amy Lowman        F        41 7:36    1000 1 Mile Run      7     36   
    ## 13 Leif Rasmussen    M        15 9:40    3000 1 Mile Racewalk 9     40   
    ## 14 Roxanne Springer  F        54 9:42    3000 1 Mile Racewalk 9     42   
    ## 15 Deb Springer      F        44 10:26   3000 1 Mile Racewalk 10    26   
    ## 16 John Min          M        48 11:01   3000 1 Mile Racewalk 11    01   
    ## 17 Tim O'Brien       M        66 11:06   3000 1 Mile Racewalk 11    06   
    ## 18 Isaac Mathias     M        14 11:17   3000 1 Mile Racewalk 11    17   
    ## 19 Barbara Hindenach F        67 11:22   3000 1 Mile Racewalk 11    22   
    ## 20 Kaina Morey       F        17 11:42   3000 1 Mile Racewalk 11    42   
    ## 21 Brendan Murray    M        30 11:45   3000 1 Mile Racewalk 11    45   
    ## 22 Robin Richardson  F        48 12:07   3000 1 Mile Racewalk 12    07   
    ##    track_time temp_dist dist_m0 dist_m Date_Meet  agegrp      rownum Place
    ##         <dbl> <chr>       <dbl>  <dbl> <date>     <chr>        <int> <int>
    ##  1        322 1            1609  1609  2018-08-01 Age 30 - 34      1     1
    ##  2        331 1            1609  1609  2018-08-01 Age < 30         1     2
    ##  3        345 1            1609  1609  2018-08-01 Age 45 - 50      1     3
    ##  4        349 1            1609  1609  2018-08-01 Age < 30         1     4
    ##  5        350 1            1609  1609  2018-08-01 Age < 30         1     5
    ##  6        350 1            1609  1609  2018-08-01 Age 61 - 64      1     5
    ##  7        350 1            1609  1609  2018-08-01 Age 45 - 50      1     5
    ##  8        377 1            1609  1609  2018-08-01 Age 51 - 55      1     1
    ##  9        379 1            1609  1609  2018-08-01 Age < 30         1     2
    ## 10        394 1            1609  1609  2018-08-01 Age 45 - 50      1     3
    ## 11        445 1            1609  1609  2018-08-01 Age 40 - 44      1     4
    ## 12        456 1            1609  1609  2018-08-01 Age 40 - 44      1     5
    ## 13        580 1            1609  1609. 2018-08-01 Age < 30         1     1
    ## 14        582 1            1609  1609. 2018-08-01 Age 51 - 55      1     1
    ## 15        626 1            1609  1609. 2018-08-01 Age 40 - 44      1     2
    ## 16        661 1            1609  1609. 2018-08-01 Age 45 - 50      1     2
    ## 17        666 1            1609  1609. 2018-08-01 Age 65 - 69      1     3
    ## 18        677 1            1609  1609. 2018-08-01 Age < 30         1     4
    ## 19        682 1            1609  1609. 2018-08-01 Age 65 - 69      1     3
    ## 20        702 1            1609  1609. 2018-08-01 Age < 30         1     4
    ## 21        705 1            1609  1609. 2018-08-01 Age 30 - 34      1     5
    ## 22        727 1            1609  1609. 2018-08-01 Age 45 - 50      1     5
    ##    Max_Hamlyn_pts
    ##             <dbl>
    ##  1              5
    ##  2              4
    ##  3              3
    ##  4              2
    ##  5              1
    ##  6              1
    ##  7              1
    ##  8              5
    ##  9              4
    ## 10              3
    ## 11              2
    ## 12              1
    ## 13              5
    ## 14              5
    ## 15              4
    ## 16              4
    ## 17              3
    ## 18              2
    ## 19              3
    ## 20              2
    ## 21              1
    ## 22              1

``` r
track_res_noage <- track_res2 %>% 
                     # Age-graded track tables start at age 4
                     dplyr::filter(is.na(Age) | Age <= 4)
track_res_noage %>% janitor::tabyl(Age)
```

    ##  Age  n    percent valid_percent
    ##    0  6 0.20689655    0.31578947
    ##    2  1 0.03448276    0.05263158
    ##    3  2 0.06896552    0.10526316
    ##    4 10 0.34482759    0.52631579
    ##   NA 10 0.34482759            NA

``` r
track_res_age   <- anti_join(track_res2, track_res_noage %>% select(Name, Sex, Event, Date_Meet), 
                             by = c('Name', 'Sex', 'Event', 'Date_Meet'))

m_ages_l <- length(unique(track_res_age %>% dplyr::filter(Sex=='M') %>% pull(Age)))
f_ages_l <- length(unique(track_res_age %>% dplyr::filter(Sex=='F') %>% pull(Age)))
m_ages <- unique(track_res_age %>% dplyr::filter(Sex=='M') %>% pull(Age))
f_ages <- unique(track_res_age %>% dplyr::filter(Sex=='F') %>% pull(Age))
dat_m1 <- data.frame()
dat_f1 <- data.frame()

for(m_age in 1:m_ages_l){
    m_age_in   <- m_ages[m_age]
    # Notice need sym instead of enquo and as.character because sym 
    # requires string
    m_age_nam  <- sym(as.character(m_age_in))
    WMA_Men_in <- WMA_Men %>% mutate(agefactor := !!m_age_nam) %>%
                    select(agefactor, dist_m, Sex)
      dat_m1_0 <- track_res_age %>% dplyr::filter(Age == m_age_in) %>%
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
      dat_f1_0 <- track_res_age %>% dplyr::filter(Age == f_age_in) %>%
                     inner_join(., WMA_Women_in, by = c("Sex", "dist_m")) %>%
                     mutate(age_grade_track_time = track_time * agefactor) %>%
                     select(Name, Sex, Date_Meet, dist_m, age_grade_track_time, agefactor)
      dat_f1   <- bind_rows(dat_f1, dat_f1_0)
}

dat_all <- bind_rows(dat_m1, dat_f1)
track_res_all <- left_join(track_res_age, dat_all, by = c("Name", "Sex", "Date_Meet", "dist_m")) %>%
                 bind_rows(., track_res_noage)

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
```

    ##     dist_m n percent
    ## 1  1998.11 7    0.35
    ## 2   100.00 2    0.10
    ## 3   800.00 2    0.10
    ## 4  1500.00 2    0.10
    ## 5   200.00 1    0.05
    ## 6   400.00 1    0.05
    ## 7  1500.11 1    0.05
    ## 8  1609.00 1    0.05
    ## 9  1609.11 1    0.05
    ## 10 3000.00 1    0.05
    ## 11 5000.00 1    0.05

``` r
# Diverging dot plot [top 3 per age group] by Selva Prabhakaran
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html   
plotfunc <- function(dist=100, subtitl_txt="", gender=c("M", "F"), sz = 28){
    
    track_res_all_top3age <-
        track_res_all %>%
        dplyr::filter(Sex %in% gender & dist_m == dist) %>%
        dplyr::filter(!is.na(agegrp)) %>%
        group_by(Name) %>%
        slice(which.min(track_time)) %>%
        group_by(agegrp) %>%
        top_n(-3, track_time) %>%
        mutate(Name = paste0(Name, " (", Age, ")")) %>%
        ungroup() %>%
        arrange(track_time)
    
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
```

<img src="SumTrack2018_files/figure-markdown_github/Segment 2-1.png" style="display: block; margin: auto;" />

``` r
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
        dplyr::filter(!is.na(agegrp)) %>%
        dplyr::filter(!is.na(age_grade_track_time)) %>%
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
       ggsave(grph, filename=paste0(grphout, ".png"), width=11, height = 8)
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
```

<img src="SumTrack2018_files/figure-markdown_github/Segment 2-2.png" style="display: block; margin: auto;" />

``` r
    gridExtra::grid.arrange(p100_s, p800_Gs, ncol = 2)
```

<img src="SumTrack2018_files/figure-markdown_github/Segment 2-3.png" style="display: block; margin: auto;" />

``` r
    gridExtra::grid.arrange(p100_s, p100_Gs, ncol = 2)
```

<img src="SumTrack2018_files/figure-markdown_github/Segment 2-4.png" style="display: block; margin: auto;" />

``` r
message("data check")
milecheck_all <- track_res_all %>%
    dplyr::filter(Date_Meet == ymd('2018-08-01') & grepl("1", Event)) %>%
    arrange(dist_m, Event, Sex, track_time) %>%
    dplyr::filter(Max_Hamlyn_pts > 0 ) %>%
    select(Name, Sex, Age, Event, dist_m, track_time, Place, Max_Hamlyn_pts)

print(milecheck_all, n = nrow(milecheck_all))
```

    ## # A tibble: 22 x 8
    ##    Name              Sex     Age Event           dist_m track_time Place
    ##    <chr>             <chr> <dbl> <chr>            <dbl>      <dbl> <int>
    ##  1 Roxanne Springer  F        54 1 Mile Run       1609         377     1
    ##  2 Kaina Morey       F        17 1 Mile Run       1609         379     2
    ##  3 Robin Richardson  F        48 1 Mile Run       1609         394     3
    ##  4 Amy Cummings      F        44 1 Mile Run       1609         445     4
    ##  5 Amy Lowman        F        41 1 Mile Run       1609         456     5
    ##  6 Brendan Murray    M        30 1 Mile Run       1609         322     1
    ##  7 Leif Rasmussen    M        15 1 Mile Run       1609         331     2
    ##  8 Ted Richardson    M        48 1 Mile Run       1609         345     3
    ##  9 Michael Fields    M        26 1 Mile Run       1609         349     4
    ## 10 Nicholas Min      M        15 1 Mile Run       1609         350     5
    ## 11 Kevin Nickodem    M        61 1 Mile Run       1609         350     5
    ## 12 Brian Stull       M        47 1 Mile Run       1609         350     5
    ## 13 Roxanne Springer  F        54 1 Mile Racewalk  1609.        582     1
    ## 14 Deb Springer      F        44 1 Mile Racewalk  1609.        626     2
    ## 15 Barbara Hindenach F        67 1 Mile Racewalk  1609.        682     3
    ## 16 Kaina Morey       F        17 1 Mile Racewalk  1609.        702     4
    ## 17 Robin Richardson  F        48 1 Mile Racewalk  1609.        727     5
    ## 18 Leif Rasmussen    M        15 1 Mile Racewalk  1609.        580     1
    ## 19 John Min          M        48 1 Mile Racewalk  1609.        661     2
    ## 20 Tim O'Brien       M        66 1 Mile Racewalk  1609.        666     3
    ## 21 Isaac Mathias     M        14 1 Mile Racewalk  1609.        677     4
    ## 22 Brendan Murray    M        30 1 Mile Racewalk  1609.        705     5
    ##    Max_Hamlyn_pts
    ##             <dbl>
    ##  1              5
    ##  2              4
    ##  3              3
    ##  4              2
    ##  5              1
    ##  6              5
    ##  7              4
    ##  8              3
    ##  9              2
    ## 10              1
    ## 11              1
    ## 12              1
    ## 13              5
    ## 14              4
    ## 15              3
    ## 16              2
    ## 17              1
    ## 18              5
    ## 19              4
    ## 20              3
    ## 21              2
    ## 22              1

``` r
max_hamlyn <- track_res_all %>%
    group_by(Name) %>%
    mutate(Max_HamLyn_pts_total = sum(Max_Hamlyn_pts)) %>%
    slice(which.max(Age)) %>%
    ungroup() %>%
    mutate(Name = paste0(Name, " (", Age, ")"))

max_hamlyn_top10_F <- max_hamlyn %>% dplyr::filter(Sex=='F') %>% top_n(10, Max_HamLyn_pts_total)
max_hamlyn_top10_F_plot <- 
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
print(max_hamlyn_top10_F_plot)
```

<img src="SumTrack2018_files/figure-markdown_github/Segment 2-5.png" style="display: block; margin: auto;" />

``` r
ggsave(max_hamlyn_top10_F_plot, filename=paste0("max_hamlyn_top10_F_2018", ".png"), width=11, height = 8)


max_hamlyn_top10_M <- max_hamlyn %>% dplyr::filter(Sex=='M') %>% top_n(10, Max_HamLyn_pts_total)
max_hamlyn_top10_M_plot <-
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
print(max_hamlyn_top10_M_plot)
```

<img src="SumTrack2018_files/figure-markdown_github/Segment 2-6.png" style="display: block; margin: auto;" />

``` r
ggsave(max_hamlyn_top10_M_plot, filename=paste0("max_hamlyn_top10_M_2018", ".png"), width=11, height = 8)


## AWAITING FUTURE WORK
# ken <- track_res_all %>% dplyr::filter(Name=='Ken Larsen') 
# 
# # ggrepel
# ggplot(data=ken, aes( x = ymd(Date_Meet), y = track_time, color = dist_m, group = dist_m)) +
#     geom_line() +
#     scale_colour_viridis_c() +
#     theme_classic() +
#     xlab("Date of Meet") +
#     ylab("Run time (seconds)")
# 
# ggplot(data=ken, aes( x = ymd(Date_Meet), y = track_time, color = dist_m, group = dist_m)) +
#     geom_line() +
#     scale_colour_viridis_c() +
#     theme_classic() +
#     xlab("Date of Meet") +
#     ylab("Run time (seconds)")
# 
# # ken %>% dplyr::filter(Date_Meet=='2018-07-18') %>% tidyr::spread(., dist_m, track_time)
# 
# 
# ken_col_events <- 
#     ken %>% select(Date_Meet, Name, dist_m, track_time) %>% 
#     tidyr::spread(., dist_m, track_time)
# ken_col_events
# 
# 
# ken_col_events$seq_event_grp <- rep(1:2, length.out = max(floor(nrow(ken_col_events)), nrow(ken_col_events)))
# ken_col_events2 <- ken_col_events %>%
#     group_by(seq_event_grp) %>%
#     unite(short, "100", "200") %>%
#     unite(med, "400", "800") %>%
#     unite(long, "1500", "1609") %>%
#     select(Name, Date_Meet, short, med, long)
```
