Linear Regression for TriPASS Data Sci 20180925 Meetup
================
Rick Pack
September 24, 2018

Topic
-----

Co-presentation with Kevin Feasel (linear regression). R code for regression of Carolina Godiva Summer Track Series (2018) data.

``` r
# Produced from 2018 results websites 
# see SumTrack2018.R in the R subfolder
track_res <- read_csv('godiva_summer_track_res_2018.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   Name = col_character(),
    ##   Sex = col_character(),
    ##   Age = col_integer(),
    ##   Time = col_character(),
    ##   ct_evt = col_integer(),
    ##   Event = col_character(),
    ##   mins = col_integer(),
    ##   secs = col_character(),
    ##   track_time = col_double(),
    ##   temp_dist = col_character(),
    ##   dist_m = col_integer(),
    ##   Place = col_integer(),
    ##   Max_Hamlyn_pts = col_integer(),
    ##   Date_Meet = col_date(format = ""),
    ##   agegrp = col_character()
    ## )

``` r
track_res %>% 
    # Differentiate walking events wit a bit of added distance
    mutate(dist_m = case_when(
                grepl("WALK", toupper(Event)) ~ as.integer(dist_m + 0.11),
                TRUE ~ dist_m)) %>%
    group_by(Name, Sex, Event, Date_Meet) %>% 
    summarise(ct = n()) %>% dplyr::filter(ct > 1)
```

    ## # A tibble: 2 x 5
    ## # Groups:   Name, Sex, Event [2]
    ##   Name            Sex   Event  Date_Meet     ct
    ##   <chr>           <chr> <chr>  <date>     <int>
    ## 1 Brendan Murray  M     5K Run 2018-05-30     2
    ## 2 Jonathan Crouse M     5K Run 2018-07-11     2

``` r
# Example of name appearing errantly more than once for same event / day
track_res %>% dplyr::filter(str_trim(Name) == 'Brendan Murray' & str_trim(toupper(Event)) == '5K RUN')
```

    ## # A tibble: 3 x 15
    ##   Name           Sex     Age Time  ct_evt Event   mins secs  track_time
    ##   <chr>          <chr> <int> <chr>  <int> <chr>  <int> <chr>      <dbl>
    ## 1 Brendan Murray M        30 19:33   5000 5K Run    19 33          1173
    ## 2 Brendan Murray M        30 19:33   5000 5K Run    19 33          1173
    ## 3 Brendan Murray M        30 18:52   5000 5K Run    18 52          1132
    ##   temp_dist dist_m Place Max_Hamlyn_pts Date_Meet  agegrp     
    ##   <chr>      <int> <int>          <int> <date>     <chr>      
    ## 1 5K            NA     1              5 2018-05-30 Age 30 - 34
    ## 2 5K            NA     1              5 2018-05-30 Age 30 - 34
    ## 3 5K            NA     2              4 2018-08-01 Age 30 - 34

``` r
track_res <- track_res %>% dplyr::filter(!is.na(Age)) %>%
    distinct(Name, Sex, Event, Date_Meet, .keep_all = TRUE)
track_res <- track_res %>% group_by(Name) %>% arrange(Name, Date_Meet, ct_evt) %>%
               mutate(EventNum = row_number()) %>% 
               group_by(Name, Date_Meet) %>%
               mutate(EventDay = row_number()) %>%
               group_by(Name, dist_m, Date_Meet) %>%
               mutate(DistNum = row_number()) %>%
               ungroup()

all_dates <- sort(unique(track_res$Date_Meet))
# downloaded from 'https://www.carolinagodiva.org/index.php?page=track-season-weather-conditions')
# see SumTrack2018.R in the R subfolder
temps_2018      <- read_csv("temps_2018.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Temp = col_integer(),
    ##   Date_Meet = col_date(format = "")
    ## )

``` r
track_res <- left_join(track_res, temps_2018, by = 'Date_Meet')

track_res %>% dplyr::filter(Name=='Rick Pack' & Date_Meet==ymd('2018-08-01'))
```

    ## # A tibble: 2 x 19
    ##   Name      Sex     Age Time  ct_evt Event      mins secs  track_time
    ##   <chr>     <chr> <int> <chr>  <int> <chr>     <int> <chr>      <dbl>
    ## 1 Rick Pack M        37 24.2    2000 200 M Run     0 24.2        24.2
    ## 2 Rick Pack M        37 2:30    4000 800 M Run     2 30         150  
    ##   temp_dist dist_m Place Max_Hamlyn_pts Date_Meet  agegrp      EventNum
    ##   <chr>      <int> <int>          <int> <date>     <chr>          <int>
    ## 1 200          200     2              4 2018-08-01 Age 35 - 39       21
    ## 2 800          800     1              5 2018-08-01 Age 35 - 39       22
    ##   EventDay DistNum  Temp
    ##      <int>   <int> <int>
    ## 1        1       1    80
    ## 2        2       1    80

``` r
track_res_base <- track_res %>% 
    mutate(Sex_num = case_when(
        Sex == "M" ~ 0,
        Sex == "F" ~ 1)) %>%
    select(Sex_num, Age, dist_m, track_time, DistNum, EventNum, EventDay, Temp)
track_lm <- lm(track_time ~ . , track_res_base)
summary(track_lm)
```

    ## 
    ## Call:
    ## lm(formula = track_time ~ ., data = track_res_base)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -267.93  -97.16  -49.72   29.73 2182.36 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept) -311.54525  107.54164  -2.897              0.00382 ** 
    ## Sex_num       28.12017   11.50550   2.444              0.01463 *  
    ## Age            2.11434    0.30468   6.940 0.000000000005731415 ***
    ## dist_m         0.31009    0.01012  30.654 < 0.0000000000000002 ***
    ## DistNum      174.21423   21.04185   8.279 0.000000000000000261 ***
    ## EventNum      -0.02554    0.59795  -0.043              0.96594    
    ## EventDay      61.12261    6.49232   9.415 < 0.0000000000000002 ***
    ## Temp          -0.62250    1.28210  -0.486              0.62737    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 220.6 on 1573 degrees of freedom
    ##   (246 observations deleted due to missingness)
    ## Multiple R-squared:  0.5382, Adjusted R-squared:  0.5361 
    ## F-statistic: 261.9 on 7 and 1573 DF,  p-value: < 0.00000000000000022

``` r
# What rows have missing data given lm reported 246 'observations deleted due to missingness'?
which(! complete.cases(track_res_base))
```

    ##  [1]   3  20  27  32  34  41  46  52  56  60  64  67  68  70  74  85  99
    ## [18] 104 109 119 129 134 135 138 143 153 155 159 189 193 196 208 213 218
    ## [35] 228 238 243 244 247 252 256 257 265 275 278 280 281 312 315 324
    ##  [ reached getOption("max.print") -- omitted 196 entries ]

``` r
track_res_base[c(3, 20, 27), ]
```

    ## # A tibble: 3 x 8
    ##   Sex_num   Age dist_m track_time DistNum EventNum EventDay  Temp
    ##     <dbl> <int>  <int>      <dbl>   <int>    <int>    <int> <int>
    ## 1       1    42     NA        790       1        3        3    76
    ## 2       1    44     NA        260       1        1        1    75
    ## 3       1    44     NA       1529       1        8        5    80

``` r
track_res_base_complete <- data.frame(track_res_base[complete.cases(track_res_base),])
track_res_base_complete$Predicted <- fitted(track_lm)
head(track_res_base_complete %>% select(Age, dist_m, track_time, Predicted))
```

    ##   Age dist_m track_time Predicted
    ## 1  42   1500        351  458.5096
    ## 2  42    400         87  178.5107
    ## 3   8    200         46  -20.8490
    ## 4  57   1609        517  520.9118
    ## 5  57    200         30  145.0959
    ## 6  57    800        217  392.2454

Flawed model, let's look at plots (the usual first step)
--------------------------------------------------------

P-value for intercept is significant, suggests model is flawed

``` r
plot(track_res_base)
```

![](README_files/figure-markdown_github/plot1-1.png)

### No variable looks linear to the response variable track\_time (column 4) but the combination of explanatory parameters (coefficients \* explanatory variables) could be.

``` r
lattice::xyplot(track_time ~ Predicted, track_res_base_complete, type = c("p","r"), col.line = "red")
```

![](README_files/figure-markdown_github/plot2-1.png)

#### We need to pursue more adjustments, such as developing separate models for shorter events (lower track\_time) vs. longer events, dropping insignificant predictors, and possibly a different kind of model than a linear one. Of course, this also illustrates the importance of checking the statistical assumptions first.

![Assumptions caution from statistician Frank Harrell](assumptions_frank_harrell.jpg)
