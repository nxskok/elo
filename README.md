
<!-- README.md is generated from README.Rmd. Please edit that file -->

# elo

<!-- badges: start -->

<!-- badges: end -->

The goal of elo is to estimate player/team strengths using Elo’s method.

## Initializing

### Introduction

A major problem with Elo ratings is that they are crucially dependent on
how they are initialized. [This blog
post](http://opisthokonta.net/?p=1412) suggests using games from an
earlier period to estimate initial ratings, under the assumption that
the rating for each team is constant over that period. It seems logical
to use maximum likelihood to estimate these initial ratings.

### Data format

Below are results from the 2018/19 season of the Scottish Premier soccer
league. These are the 33 games of the “regular season”, not the final 5
games that decide champions and relegation:

``` r
library(tidyverse)
#> ── Attaching packages ────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──
#> ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
#> ✓ tibble  2.1.3     ✓ dplyr   0.8.3
#> ✓ tidyr   1.0.0     ✓ stringr 1.4.0
#> ✓ readr   1.3.1     ✓ forcats 0.4.0
#> ── Conflicts ───────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
source("functions.R")
# source("cleaning.R")
scotland <- read_rds("scotland.rds")
scotland
#> # A tibble: 198 x 6
#>    time_stamp          t1                  t2                     s1    s2     r
#>    <dttm>              <chr>               <chr>               <int> <int> <dbl>
#>  1 2018-08-04 10:00:00 St. Mirren          Dundee                  2     1   1  
#>  2 2018-08-04 10:00:00 Kilmarnock          St. Johnstone           2     0   1  
#>  3 2018-08-04 10:00:00 Hamilton Academical Hearts                  1     4   0  
#>  4 2018-08-04 10:00:00 Celtic              Livingston              3     1   1  
#>  5 2018-08-05 08:00:00 Aberdeen            Rangers                 1     1   0.5
#>  6 2018-08-05 10:00:00 Hibernian           Motherwell              3     0   1  
#>  7 2018-08-11 07:30:00 Hearts              Celtic                  1     0   1  
#>  8 2018-08-11 10:00:00 Dundee              Aberdeen                0     1   0  
#>  9 2018-08-11 10:00:00 Motherwell          Hamilton Academical     0     1   0  
#> 10 2018-08-11 10:00:00 Livingston          Kilmarnock              0     0   0.5
#> # … with 188 more rows
```

The input data frame for initialization needs to have (at least) columns
called `t1`, `t2` and `r`. These are: the name of the home team, the
name of the away team, and the game result, with 1 meaning home win, 0.5
meaning draw (tie), and 0 meaning away win.

In my case, I have other columns (which will be ignored): the date and
time of game kickoff, and the number of goals scored by the home and
away teams.

### Estimating initial ratings

Uses the function `estimate_initial` whoes input is a data frame with
columns (at least) `t1`, `t2`, and `r`, as described above:

``` r
estimate_initial(scotland) %>% arrange(desc(rat))
#> n = 12
#> # A tibble: 12 x 4
#>       id name                  rat     h
#>    <int> <chr>               <dbl> <dbl>
#>  1     2 Celtic              1649.  36.3
#>  2    10 Rangers             1596.  36.3
#>  3     7 Kilmarnock          1547.  36.3
#>  4     1 Aberdeen            1537.  36.3
#>  5     5 Hearts              1520.  36.3
#>  6     8 Livingston          1519.  36.3
#>  7     6 Hibernian           1510.  36.3
#>  8    11 St. Johnstone       1502.  36.3
#>  9     9 Motherwell          1479.  36.3
#> 10     4 Hamilton Academical 1405.  36.3
#> 11     3 Dundee              1377.  36.3
#> 12    12 St. Mirren          1358.  36.3
```

I arranged the estimated ratings in descending order. The column `h` is
the estimated home field advantage (the same for every team).
