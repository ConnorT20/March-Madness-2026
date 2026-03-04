March Madness
================

``` r
library(hoopR)
library(elo)
library(gtools) 
library(ggplot2) # Load ggplot2
library(ggExtra) # Load ggExtra
library(dplyr) # Load dplyr
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggalluvial) # Load ggalluvial
library(tidyverse) # Load tidyverse
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.2
    ## ✔ lubridate 1.9.4     ✔ tibble    3.3.0
    ## ✔ purrr     1.1.0     ✔ tidyr     1.3.1
    ## ✔ readr     2.1.5

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggimage) # Load ggimage
library(GGally) # Load GGally
library(ggrepel) # Load ggrepel
source("~/Desktop/Mod 3/Mod3 Sport Analytics/Week 2/simple_dark_theme-2.R")
```

    ## 
    ## ========================================
    ## Simple Dark Themes Loaded Successfully!
    ## ========================================
    ## 
    ## Available functions:
    ##   - dark_theme_bw()      : Dark version of theme_bw
    ##   - dark_theme_gray()    : Dark version of theme_gray
    ##   - dark_theme_grey()    : Alias for dark_theme_gray
    ##   - dark_theme_minimal() : Dark version of theme_minimal
    ## 
    ## Example usage:
    ##   library(ggplot2)
    ##   p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
    ##   p + dark_theme_bw()

``` r
ls("package:hoopR") |> grep("espn_mbb_", x = _, value = TRUE)
```

    ##  [1] "espn_mbb_betting"           "espn_mbb_conferences"      
    ##  [3] "espn_mbb_game_all"          "espn_mbb_game_rosters"     
    ##  [5] "espn_mbb_pbp"               "espn_mbb_player_box"       
    ##  [7] "espn_mbb_player_stats"      "espn_mbb_rankings"         
    ##  [9] "espn_mbb_scoreboard"        "espn_mbb_standings"        
    ## [11] "espn_mbb_team_box"          "espn_mbb_team_stats"       
    ## [13] "espn_mbb_teams"             "espn_mbb_wp"               
    ## [15] "helper_espn_mbb_pbp"        "helper_espn_mbb_player_box"
    ## [17] "helper_espn_mbb_team_box"

``` r
tictoc::tic()
progressr::with_progress({
  mbb_pbp <-  hoopR::load_mbb_pbp()
})
tictoc::toc()
```

    ## 25.964 sec elapsed

``` r
summary(mbb_pbp)
```

    ##  game_play_number       id            sequence_number        type_id     
    ##  Min.   :  1.0    Min.   :4.018e+17   Min.   :113523368   Min.   : 91.0  
    ##  1st Qu.:117.0    1st Qu.:4.018e+17   1st Qu.:115075204   1st Qu.:558.0  
    ##  Median :233.0    Median :4.018e+17   Median :116689273   Median :584.0  
    ##  Mean   :236.4    Mean   :4.018e+17   Mean   :116687748   Mean   :568.4  
    ##  3rd Qu.:350.0    3rd Qu.:4.018e+17   3rd Qu.:118274088   3rd Qu.:586.0  
    ##  Max.   :833.0    Max.   :4.019e+17   Max.   :119847615   Max.   :618.0  
    ##                                                                          
    ##   type_text             text             away_score       home_score    
    ##  Length:2675701     Length:2675701     Min.   :  0.00   Min.   :  0.00  
    ##  Class :character   Class :character   1st Qu.: 17.00   1st Qu.: 19.00  
    ##  Mode  :character   Mode  :character   Median : 34.00   Median : 39.00  
    ##                                        Mean   : 35.25   Mean   : 40.21  
    ##                                        3rd Qu.: 52.00   3rd Qu.: 59.00  
    ##                                        Max.   :119.00   Max.   :149.00  
    ##                                                                         
    ##  period_number  period_display_value clock_display_value scoring_play   
    ##  Min.   :1.00   Length:2675701       Length:2675701      Mode :logical  
    ##  1st Qu.:1.00   Class :character     Class :character    FALSE:2205570  
    ##  Median :2.00   Mode  :character     Mode  :character    TRUE :470131   
    ##  Mean   :1.53                                                           
    ##  3rd Qu.:2.00                                                           
    ##  Max.   :5.00                                                           
    ##                                                                         
    ##   score_value      wallclock         shooting_play   coordinate_x_raw    
    ##  Min.   :0.0000   Length:2675701     Mode :logical   Min.   :-214748340  
    ##  1st Qu.:0.0000   Class :character   FALSE:1767460   1st Qu.:        25  
    ##  Median :0.0000   Mode  :character   TRUE :908241    Median :        25  
    ##  Mean   :0.6921                                      Mean   :  -1151367  
    ##  3rd Qu.:2.0000                                      3rd Qu.:        25  
    ##  Max.   :3.0000                                      Max.   :        50  
    ##                                                                          
    ##  coordinate_y_raw     points_attempted short_description     game_id         
    ##  Min.   :-214748365   Min.   :0.0000   Length:2675701     Min.   :401804830  
    ##  1st Qu.:         0   1st Qu.:0.0000   Class :character   1st Qu.:401812601  
    ##  Median :         0   Median :0.0000   Mode  :character   Median :401822758  
    ##  Mean   :  -1151384   Mean   :0.6865                      Mean   :401820174  
    ##  3rd Qu.:         4   3rd Qu.:2.0000                      3rd Qu.:401826991  
    ##  Max.   :        89   Max.   :3.0000                      Max.   :401861277  
    ##                                                                              
    ##      season      season_type  home_team_id    home_team_name    
    ##  Min.   :2026   Min.   :2    Min.   :     2   Length:2675701    
    ##  1st Qu.:2026   1st Qu.:2    1st Qu.:   172   Class :character  
    ##  Median :2026   Median :2    Median :  2026   Mode  :character  
    ##  Mean   :2026   Mean   :2    Mean   :  1552                     
    ##  3rd Qu.:2026   3rd Qu.:2    3rd Qu.:  2453                     
    ##  Max.   :2026   Max.   :2    Max.   :112358                     
    ##                                                                 
    ##  home_team_mascot   home_team_abbrev   home_team_name_alt  away_team_id   
    ##  Length:2675701     Length:2675701     Length:2675701     Min.   :     1  
    ##  Class :character   Class :character   Class :character   1st Qu.:   235  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :  2169  
    ##                                                           Mean   :  4719  
    ##                                                           3rd Qu.:  2529  
    ##                                                           Max.   :132013  
    ##                                                                           
    ##  away_team_name     away_team_mascot   away_team_abbrev   away_team_name_alt
    ##  Length:2675701     Length:2675701     Length:2675701     Length:2675701    
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##   game_spread  home_favorite  game_spread_available home_team_spread
    ##  Min.   :2.5   Mode:logical   Mode :logical         Min.   :2.5     
    ##  1st Qu.:2.5   TRUE:2675701   FALSE:2675701         1st Qu.:2.5     
    ##  Median :2.5                                        Median :2.5     
    ##  Mean   :2.5                                        Mean   :2.5     
    ##  3rd Qu.:2.5                                        3rd Qu.:2.5     
    ##  Max.   :2.5                                        Max.   :2.5     
    ##                                                                     
    ##       half          time           clock_minutes    clock_seconds 
    ##  Min.   :1.00   Length:2675701     Min.   : 0.000   Min.   : 0.0  
    ##  1st Qu.:1.00   Class :character   1st Qu.: 4.000   1st Qu.:13.0  
    ##  Median :2.00   Mode  :character   Median : 9.000   Median :29.0  
    ##  Mean   :1.53                      Mean   : 8.969   Mean   :28.6  
    ##  3rd Qu.:2.00                      3rd Qu.:14.000   3rd Qu.:44.0  
    ##  Max.   :5.00                      Max.   :20.000   Max.   :59.0  
    ##                                                                   
    ##  home_timeout_called away_timeout_called  lead_period      lead_half    
    ##  Mode :logical       Mode :logical       Min.   :1.000   Min.   :1.000  
    ##  FALSE:2664123       FALSE:2662853       1st Qu.:1.000   1st Qu.:1.000  
    ##  TRUE :11578         TRUE :12848         Median :2.000   Median :2.000  
    ##                                          Mean   :1.532   Mean   :1.532  
    ##                                          3rd Qu.:2.000   3rd Qu.:2.000  
    ##                                          Max.   :5.000   Max.   :5.000  
    ##                                          NA's   :5744    NA's   :5744   
    ##  start_period_seconds_remaining start_game_seconds_remaining
    ##  Min.   :   0.0                 Min.   :   0                
    ##  1st Qu.: 262.0                 1st Qu.: 534                
    ##  Median : 563.0                 Median :1170                
    ##  Mean   : 566.7                 Mean   :1141                
    ##  3rd Qu.: 858.0                 3rd Qu.:1741                
    ##  Max.   :1206.0                 Max.   :2406                
    ##                                                             
    ##  end_period_seconds_remaining end_game_seconds_remaining    team_id      
    ##  Min.   :   0.0               Min.   :   0               Min.   :     1  
    ##  1st Qu.: 262.0               1st Qu.: 533               1st Qu.:   201  
    ##  Median : 562.0               Median :1166               Median :  2097  
    ##  Mean   : 565.4               Mean   :1139               Mean   :  2998  
    ##  3rd Qu.: 856.0               3rd Qu.:1737               3rd Qu.:  2501  
    ##  Max.   :1200.0               Max.   :2400               Max.   :132013  
    ##  NA's   :5743                 NA's   :5743               NA's   :70832   
    ##   athlete_id_1       lag_period       lag_half      athlete_id_2    
    ##  Min.   :    179   Min.   :1.000   Min.   :1.000   Min.   :  18553  
    ##  1st Qu.:5105691   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:5105598  
    ##  Median :5176278   Median :2.000   Median :2.000   Median :5176030  
    ##  Mean   :5126026   Mean   :1.529   Mean   :1.529   Mean   :5119902  
    ##  3rd Qu.:5243011   3rd Qu.:2.000   3rd Qu.:2.000   3rd Qu.:5242555  
    ##  Max.   :5335077   Max.   :5.000   Max.   :5.000   Max.   :5332823  
    ##  NA's   :185072    NA's   :5744    NA's   :5744    NA's   :2519115  
    ##   coordinate_x         coordinate_y          game_date         
    ##  Min.   :-214748407   Min.   :-214748365   Min.   :2025-11-03  
    ##  1st Qu.:       -42   1st Qu.:         0   1st Qu.:2025-11-26  
    ##  Median :       -13   Median :         0   Median :2026-01-02  
    ##  Mean   :    -20225   Mean   :    -20225   Mean   :2025-12-30  
    ##  3rd Qu.:        42   3rd Qu.:         0   3rd Qu.:2026-01-31  
    ##  Max.   : 214748407   Max.   : 214748365   Max.   :2026-03-03  
    ##                                                                
    ##  game_date_time               
    ##  Min.   :2025-11-03 08:00:00  
    ##  1st Qu.:2025-11-26 21:30:00  
    ##  Median :2026-01-02 23:00:00  
    ##  Mean   :2025-12-31 15:22:30  
    ##  3rd Qu.:2026-01-31 18:35:00  
    ##  Max.   :2026-03-03 23:00:00  
    ## 

``` r
mbb_schedule_25 <- load_mbb_schedule(2025)
team_box_2k25 <- load_mbb_team_box(2025)
player_box_2k25 <- load_mbb_player_box(2025)
rankings <- espn_mbb_rankings()
standings_2k25 <- espn_mbb_standings(2025)
```

Game Results

- Home Win - 1
- Away Win - 0
- Overtime - 0.5 - Need to figure out how to add this in (Met with
  Martin on 2/10)

``` r
mbb_2025 <- load_mbb_pbp(2025)
clean_mbb_25 <- mbb_2025 %>%
  filter(!is.na(home_score)) %>%
  mutate(home_win = ifelse(home_score > away_score, 1, 0))
```

``` r
summary(mbb_2025$period_number)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   1.000   2.000   1.537   2.000   6.000

``` r
save(mbb_pbp, player_box_2k25, rankings, standings_2k25, team_box_2k25, mbb_schedule_25, mbb_2025, file = "mbb_data.rda")
```

Flip data frame on itself to get most recent game instead of the first
game to get more accurate ELO

``` r
temp_1 <- clean_mbb_25[nrow(clean_mbb_25):1,]

games_1 <- temp_1[!duplicated(temp_1$game_id), ]
```

``` r
ot_games <- unique(clean_mbb_25[which(clean_mbb_25$period_number > 2), c("game_id")])$game_id
```

``` r
games_1$home_win[which(games_1$game_id %in% ot_games)] <- 0.5
```

``` r
elo_2025 <- elo.run(
  home_win ~ home_team_name + away_team_name, 
  data = games_1, 
  k = 30
)

final_2025_elos <- final.elos(elo_2025)
```

``` r
priors_2026 <- (final_2025_elos * 0.75) + (1500 * 0.25) #New Season; different squads/lineups; I want to try to average out everybody just a little bit

view(priors_2026)
```

``` r
mbb_2026 <- load_mbb_pbp(2026)
clean_mbb_26 <- mbb_2026 %>% 
  filter(!is.na(home_score)) %>%
  mutate(home_win = ifelse(home_score > away_score, 1,0))

ot_games <- unique(clean_mbb_26[which(clean_mbb_26$period_number > 2), c("game_id")])$game_id

temp_2 <- clean_mbb_26[nrow(clean_mbb_26):1,]

games_2 <- temp_2[!duplicated(temp_2$game_id), ]

games_2$home_win[which(games_2$game_id %in% ot_games)] <- 0.5
```

``` r
elo_2026 <- elo.run(
  home_win ~ home_team_name + away_team_name, 
  data = games_2, 
  k = 30,
  priors = priors_2026 
)
```

``` r
summary(mbb_2026$period_number)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1.00    1.00    2.00    1.53    2.00    5.00

``` r
df_elo_26 <- as.data.frame(final.elos(elo_2026))
df_elo_25 <- as.data.frame(final.elos(elo_2025))
```

Keep for later to look at teams for graphing

``` r
teams_26 <- espn_mbb_teams(2026) 
```

    ## 2026-03-04 11:52:07.522188: Invalid arguments or no conferences info available!

    ## 2026-03-04 11:52:07.523837: Invalid arguments or no teams data available!

``` r
head(teams_26)
```

    ## # A tibble: 6 × 23
    ##   team_id abbreviation display_name       short_name mascot nickname team  color
    ##     <int> <chr>        <chr>              <chr>      <chr>  <chr>    <chr> <chr>
    ## 1    2000 ACU          Abilene Christian… Abilene C… Wildc… Abilene… Abil… 592d…
    ## 2    2005 AF           Air Force Falcons  Air Force  Falco… Air For… Air … 0035…
    ## 3    2006 AKR          Akron Zips         Akron      Zips   Akron    Akron 0028…
    ## 4    2010 AAMU         Alabama A&M Bulld… Alabama A… Bulld… Alabama… Alab… 7900…
    ## 5     333 ALA          Alabama Crimson T… Alabama    Crims… Alabama  Alab… 9e16…
    ## 6    2011 ALST         Alabama State Hor… Alabama St Horne… Alabama… Alab… e9a9…
    ## # ℹ 15 more variables: alternate_color <chr>, logo <chr>, logo_dark <chr>,
    ## #   logos_href_3 <chr>, logos_href_4 <chr>, logos_href_5 <chr>,
    ## #   logos_href_6 <chr>, logos_href_7 <chr>, logos_href_8 <chr>,
    ## #   logos_href_9 <chr>, logos_href_10 <chr>, logos_href_11 <chr>,
    ## #   logos_href_12 <chr>, logos_href_13 <chr>, logos_href_14 <chr>

get unique column value for display names, upload it to a CSV and then
use AI to populate with conferences, then load back in.

``` r
unique_teams <- unique(teams_26$display_name)
unique_teams
```

    ##   [1] "Abilene Christian Wildcats"           
    ##   [2] "Air Force Falcons"                    
    ##   [3] "Akron Zips"                           
    ##   [4] "Alabama A&M Bulldogs"                 
    ##   [5] "Alabama Crimson Tide"                 
    ##   [6] "Alabama State Hornets"                
    ##   [7] "Alcorn State Braves"                  
    ##   [8] "American University Eagles"           
    ##   [9] "App State Mountaineers"               
    ##  [10] "Arizona State Sun Devils"             
    ##  [11] "Arizona Wildcats"                     
    ##  [12] "Arkansas Razorbacks"                  
    ##  [13] "Arkansas State Red Wolves"            
    ##  [14] "Arkansas-Pine Bluff Golden Lions"     
    ##  [15] "Army Black Knights"                   
    ##  [16] "Auburn Tigers"                        
    ##  [17] "Austin Peay Governors"                
    ##  [18] "BYU Cougars"                          
    ##  [19] "Ball State Cardinals"                 
    ##  [20] "Baylor Bears"                         
    ##  [21] "Bellarmine Knights"                   
    ##  [22] "Belmont Bruins"                       
    ##  [23] "Bethune-Cookman Wildcats"             
    ##  [24] "Binghamton Bearcats"                  
    ##  [25] "Boise State Broncos"                  
    ##  [26] "Boston College Eagles"                
    ##  [27] "Boston University Terriers"           
    ##  [28] "Bowling Green Falcons"                
    ##  [29] "Bradley Braves"                       
    ##  [30] "Brown Bears"                          
    ##  [31] "Bryant Bulldogs"                      
    ##  [32] "Bucknell Bison"                       
    ##  [33] "Buffalo Bulls"                        
    ##  [34] "Butler Bulldogs"                      
    ##  [35] "Cal Poly Mustangs"                    
    ##  [36] "Cal State Bakersfield Roadrunners"    
    ##  [37] "Cal State Fullerton Titans"           
    ##  [38] "Cal State Northridge Matadors"        
    ##  [39] "California Baptist Lancers"           
    ##  [40] "California Golden Bears"              
    ##  [41] "Campbell Fighting Camels"             
    ##  [42] "Canisius Golden Griffins"             
    ##  [43] "Central Arkansas Bears"               
    ##  [44] "Central Connecticut Blue Devils"      
    ##  [45] "Central Michigan Chippewas"           
    ##  [46] "Charleston Cougars"                   
    ##  [47] "Charleston Southern Buccaneers"       
    ##  [48] "Charlotte 49ers"                      
    ##  [49] "Chattanooga Mocs"                     
    ##  [50] "Chicago State Cougars"                
    ##  [51] "Cincinnati Bearcats"                  
    ##  [52] "Clemson Tigers"                       
    ##  [53] "Cleveland State Vikings"              
    ##  [54] "Coastal Carolina Chanticleers"        
    ##  [55] "Colgate Raiders"                      
    ##  [56] "Colorado Buffaloes"                   
    ##  [57] "Colorado State Rams"                  
    ##  [58] "Columbia Lions"                       
    ##  [59] "Coppin State Eagles"                  
    ##  [60] "Cornell Big Red"                      
    ##  [61] "Creighton Bluejays"                   
    ##  [62] "Dartmouth Big Green"                  
    ##  [63] "Davidson Wildcats"                    
    ##  [64] "Dayton Flyers"                        
    ##  [65] "DePaul Blue Demons"                   
    ##  [66] "Delaware Blue Hens"                   
    ##  [67] "Delaware State Hornets"               
    ##  [68] "Denver Pioneers"                      
    ##  [69] "Detroit Mercy Titans"                 
    ##  [70] "Drake Bulldogs"                       
    ##  [71] "Drexel Dragons"                       
    ##  [72] "Duke Blue Devils"                     
    ##  [73] "Duquesne Dukes"                       
    ##  [74] "East Carolina Pirates"                
    ##  [75] "East Tennessee State Buccaneers"      
    ##  [76] "East Texas A&M Lions"                 
    ##  [77] "Eastern Illinois Panthers"            
    ##  [78] "Eastern Kentucky Colonels"            
    ##  [79] "Eastern Michigan Eagles"              
    ##  [80] "Eastern Washington Eagles"            
    ##  [81] "Elon Phoenix"                         
    ##  [82] "Evansville Purple Aces"               
    ##  [83] "Fairfield Stags"                      
    ##  [84] "Fairleigh Dickinson Knights"          
    ##  [85] "Florida A&M Rattlers"                 
    ##  [86] "Florida Atlantic Owls"                
    ##  [87] "Florida Gators"                       
    ##  [88] "Florida Gulf Coast Eagles"            
    ##  [89] "Florida International Panthers"       
    ##  [90] "Florida State Seminoles"              
    ##  [91] "Fordham Rams"                         
    ##  [92] "Fresno State Bulldogs"                
    ##  [93] "Furman Paladins"                      
    ##  [94] "Gardner-Webb Runnin' Bulldogs"        
    ##  [95] "George Mason Patriots"                
    ##  [96] "George Washington Revolutionaries"    
    ##  [97] "Georgetown Hoyas"                     
    ##  [98] "Georgia Bulldogs"                     
    ##  [99] "Georgia Southern Eagles"              
    ## [100] "Georgia State Panthers"               
    ## [101] "Georgia Tech Yellow Jackets"          
    ## [102] "Gonzaga Bulldogs"                     
    ## [103] "Grambling Tigers"                     
    ## [104] "Grand Canyon Lopes"                   
    ## [105] "Green Bay Phoenix"                    
    ## [106] "Hampton Pirates"                      
    ## [107] "Harvard Crimson"                      
    ## [108] "Hawai'i Rainbow Warriors"             
    ## [109] "High Point Panthers"                  
    ## [110] "Hofstra Pride"                        
    ## [111] "Holy Cross Crusaders"                 
    ## [112] "Houston Christian Huskies"            
    ## [113] "Houston Cougars"                      
    ## [114] "Howard Bison"                         
    ## [115] "IU Indianapolis Jaguars"              
    ## [116] "Idaho State Bengals"                  
    ## [117] "Idaho Vandals"                        
    ## [118] "Illinois Fighting Illini"             
    ## [119] "Illinois State Redbirds"              
    ## [120] "Incarnate Word Cardinals"             
    ## [121] "Indiana Hoosiers"                     
    ## [122] "Indiana State Sycamores"              
    ## [123] "Iona Gaels"                           
    ## [124] "Iowa Hawkeyes"                        
    ## [125] "Iowa State Cyclones"                  
    ## [126] "Jackson State Tigers"                 
    ## [127] "Jacksonville Dolphins"                
    ## [128] "Jacksonville State Gamecocks"         
    ## [129] "James Madison Dukes"                  
    ## [130] "Kansas City Roos"                     
    ## [131] "Kansas Jayhawks"                      
    ## [132] "Kansas State Wildcats"                
    ## [133] "Kennesaw State Owls"                  
    ## [134] "Kent State Golden Flashes"            
    ## [135] "Kentucky Wildcats"                    
    ## [136] "LSU Tigers"                           
    ## [137] "La Salle Explorers"                   
    ## [138] "Lafayette Leopards"                   
    ## [139] "Lamar Cardinals"                      
    ## [140] "Le Moyne Dolphins"                    
    ## [141] "Lehigh Mountain Hawks"                
    ## [142] "Liberty Flames"                       
    ## [143] "Lipscomb Bisons"                      
    ## [144] "Little Rock Trojans"                  
    ## [145] "Long Beach State Beach"               
    ## [146] "Long Island University Sharks"        
    ## [147] "Longwood Lancers"                     
    ## [148] "Louisiana Ragin' Cajuns"              
    ## [149] "Louisiana Tech Bulldogs"              
    ## [150] "Louisville Cardinals"                 
    ## [151] "Loyola Chicago Ramblers"              
    ## [152] "Loyola Maryland Greyhounds"           
    ## [153] "Loyola Marymount Lions"               
    ## [154] "Maine Black Bears"                    
    ## [155] "Manhattan Jaspers"                    
    ## [156] "Marist Red Foxes"                     
    ## [157] "Marquette Golden Eagles"              
    ## [158] "Marshall Thundering Herd"             
    ## [159] "Maryland Eastern Shore Hawks"         
    ## [160] "Maryland Terrapins"                   
    ## [161] "Massachusetts Minutemen"              
    ## [162] "McNeese Cowboys"                      
    ## [163] "Memphis Tigers"                       
    ## [164] "Mercer Bears"                         
    ## [165] "Mercyhurst Lakers"                    
    ## [166] "Merrimack Warriors"                   
    ## [167] "Miami (OH) RedHawks"                  
    ## [168] "Miami Hurricanes"                     
    ## [169] "Michigan State Spartans"              
    ## [170] "Michigan Wolverines"                  
    ## [171] "Middle Tennessee Blue Raiders"        
    ## [172] "Milwaukee Panthers"                   
    ## [173] "Minnesota Golden Gophers"             
    ## [174] "Mississippi State Bulldogs"           
    ## [175] "Mississippi Valley State Delta Devils"
    ## [176] "Missouri State Bears"                 
    ## [177] "Missouri Tigers"                      
    ## [178] "Monmouth Hawks"                       
    ## [179] "Montana Grizzlies"                    
    ## [180] "Montana State Bobcats"                
    ## [181] "Morehead State Eagles"                
    ## [182] "Morgan State Bears"                   
    ## [183] "Mount St. Mary's Mountaineers"        
    ## [184] "Murray State Racers"                  
    ## [185] "NC State Wolfpack"                    
    ## [186] "NJIT Highlanders"                     
    ## [187] "Navy Midshipmen"                      
    ## [188] "Nebraska Cornhuskers"                 
    ## [189] "Nevada Wolf Pack"                     
    ## [190] "New Hampshire Wildcats"               
    ## [191] "New Haven Chargers"                   
    ## [192] "New Mexico Lobos"                     
    ## [193] "New Mexico State Aggies"              
    ## [194] "New Orleans Privateers"               
    ## [195] "Niagara Purple Eagles"                
    ## [196] "Nicholls Colonels"                    
    ## [197] "Norfolk State Spartans"               
    ## [198] "North Alabama Lions"                  
    ## [199] "North Carolina A&T Aggies"            
    ## [200] "North Carolina Central Eagles"        
    ## [201] "North Carolina Tar Heels"             
    ## [202] "North Dakota Fighting Hawks"          
    ## [203] "North Dakota State Bison"             
    ## [204] "North Florida Ospreys"                
    ## [205] "North Texas Mean Green"               
    ## [206] "Northeastern Huskies"                 
    ## [207] "Northern Arizona Lumberjacks"         
    ## [208] "Northern Colorado Bears"              
    ## [209] "Northern Illinois Huskies"            
    ## [210] "Northern Iowa Panthers"               
    ## [211] "Northern Kentucky Norse"              
    ## [212] "Northwestern State Demons"            
    ## [213] "Northwestern Wildcats"                
    ## [214] "Notre Dame Fighting Irish"            
    ## [215] "Oakland Golden Grizzlies"             
    ## [216] "Ohio Bobcats"                         
    ## [217] "Ohio State Buckeyes"                  
    ## [218] "Oklahoma Sooners"                     
    ## [219] "Oklahoma State Cowboys"               
    ## [220] "Old Dominion Monarchs"                
    ## [221] "Ole Miss Rebels"                      
    ## [222] "Omaha Mavericks"                      
    ## [223] "Oral Roberts Golden Eagles"           
    ## [224] "Oregon Ducks"                         
    ## [225] "Oregon State Beavers"                 
    ## [226] "Pacific Tigers"                       
    ## [227] "Penn State Nittany Lions"             
    ## [228] "Pennsylvania Quakers"                 
    ## [229] "Pepperdine Waves"                     
    ## [230] "Pittsburgh Panthers"                  
    ## [231] "Portland Pilots"                      
    ## [232] "Portland State Vikings"               
    ## [233] "Prairie View A&M Panthers"            
    ## [234] "Presbyterian Blue Hose"               
    ## [235] "Princeton Tigers"                     
    ## [236] "Providence Friars"                    
    ## [237] "Purdue Boilermakers"                  
    ## [238] "Purdue Fort Wayne Mastodons"          
    ## [239] "Quinnipiac Bobcats"                   
    ## [240] "Radford Highlanders"                  
    ## [241] "Rhode Island Rams"                    
    ## [242] "Rice Owls"                            
    ## [243] "Richmond Spiders"                     
    ## [244] "Rider Broncs"                         
    ## [245] "Robert Morris Colonials"              
    ## [246] "Rutgers Scarlet Knights"              
    ## [247] "SE Louisiana Lions"                   
    ## [248] "SIU Edwardsville Cougars"             
    ## [249] "SMU Mustangs"                         
    ## [250] "Sacramento State Hornets"             
    ## [251] "Sacred Heart Pioneers"                
    ## [252] "Saint Francis Red Flash"              
    ## [253] "Saint Joseph's Hawks"                 
    ## [254] "Saint Louis Billikens"                
    ## [255] "Saint Mary's Gaels"                   
    ## [256] "Saint Peter's Peacocks"               
    ## [257] "Sam Houston Bearkats"                 
    ## [258] "Samford Bulldogs"                     
    ## [259] "San Diego State Aztecs"               
    ## [260] "San Diego Toreros"                    
    ## [261] "San Francisco Dons"                   
    ## [262] "San José State Spartans"              
    ## [263] "Santa Clara Broncos"                  
    ## [264] "Seattle U Redhawks"                   
    ## [265] "Seton Hall Pirates"                   
    ## [266] "Siena Saints"                         
    ## [267] "South Alabama Jaguars"                
    ## [268] "South Carolina Gamecocks"             
    ## [269] "South Carolina State Bulldogs"        
    ## [270] "South Carolina Upstate Spartans"      
    ## [271] "South Dakota Coyotes"                 
    ## [272] "South Dakota State Jackrabbits"       
    ## [273] "South Florida Bulls"                  
    ## [274] "Southeast Missouri State Redhawks"    
    ## [275] "Southern Illinois Salukis"            
    ## [276] "Southern Jaguars"                     
    ## [277] "Southern Miss Golden Eagles"          
    ## [278] "Southern Utah Thunderbirds"           
    ## [279] "St. Bonaventure Bonnies"              
    ## [280] "St. John's Red Storm"                 
    ## [281] "St. Thomas-Minnesota Tommies"         
    ## [282] "Stanford Cardinal"                    
    ## [283] "Stephen F. Austin Lumberjacks"        
    ## [284] "Stetson Hatters"                      
    ## [285] "Stonehill Skyhawks"                   
    ## [286] "Stony Brook Seawolves"                
    ## [287] "Syracuse Orange"                      
    ## [288] "TCU Horned Frogs"                     
    ## [289] "Tarleton State Texans"                
    ## [290] "Temple Owls"                          
    ## [291] "Tennessee State Tigers"               
    ## [292] "Tennessee Tech Golden Eagles"         
    ## [293] "Tennessee Volunteers"                 
    ## [294] "Texas A&M Aggies"                     
    ## [295] "Texas A&M-Corpus Christi Islanders"   
    ## [296] "Texas Longhorns"                      
    ## [297] "Texas Southern Tigers"                
    ## [298] "Texas State Bobcats"                  
    ## [299] "Texas Tech Red Raiders"               
    ## [300] "The Citadel Bulldogs"                 
    ## [301] "Toledo Rockets"                       
    ## [302] "Towson Tigers"                        
    ## [303] "Troy Trojans"                         
    ## [304] "Tulane Green Wave"                    
    ## [305] "Tulsa Golden Hurricane"               
    ## [306] "UAB Blazers"                          
    ## [307] "UAlbany Great Danes"                  
    ## [308] "UC Davis Aggies"                      
    ## [309] "UC Irvine Anteaters"                  
    ## [310] "UC Riverside Highlanders"             
    ## [311] "UC San Diego Tritons"                 
    ## [312] "UC Santa Barbara Gauchos"             
    ## [313] "UCF Knights"                          
    ## [314] "UCLA Bruins"                          
    ## [315] "UConn Huskies"                        
    ## [316] "UIC Flames"                           
    ## [317] "UL Monroe Warhawks"                   
    ## [318] "UMBC Retrievers"                      
    ## [319] "UMass Lowell River Hawks"             
    ## [320] "UNC Asheville Bulldogs"               
    ## [321] "UNC Greensboro Spartans"              
    ## [322] "UNC Wilmington Seahawks"              
    ## [323] "UNLV Rebels"                          
    ## [324] "USC Trojans"                          
    ## [325] "UT Arlington Mavericks"               
    ## [326] "UT Martin Skyhawks"                   
    ## [327] "UT Rio Grande Valley Vaqueros"        
    ## [328] "UTEP Miners"                          
    ## [329] "UTSA Roadrunners"                     
    ## [330] "Utah State Aggies"                    
    ## [331] "Utah Tech Trailblazers"               
    ## [332] "Utah Utes"                            
    ## [333] "Utah Valley Wolverines"               
    ## [334] "VCU Rams"                             
    ## [335] "VMI Keydets"                          
    ## [336] "Valparaiso Beacons"                   
    ## [337] "Vanderbilt Commodores"                
    ## [338] "Vermont Catamounts"                   
    ## [339] "Villanova Wildcats"                   
    ## [340] "Virginia Cavaliers"                   
    ## [341] "Virginia Tech Hokies"                 
    ## [342] "Wagner Seahawks"                      
    ## [343] "Wake Forest Demon Deacons"            
    ## [344] "Washington Huskies"                   
    ## [345] "Washington State Cougars"             
    ## [346] "Weber State Wildcats"                 
    ## [347] "West Georgia Wolves"                  
    ## [348] "West Virginia Mountaineers"           
    ## [349] "Western Carolina Catamounts"          
    ## [350] "Western Illinois Leathernecks"        
    ## [351] "Western Kentucky Hilltoppers"         
    ## [352] "Western Michigan Broncos"             
    ## [353] "Wichita State Shockers"               
    ## [354] "William & Mary Tribe"                 
    ## [355] "Winthrop Eagles"                      
    ## [356] "Wisconsin Badgers"                    
    ## [357] "Wofford Terriers"                     
    ## [358] "Wright State Raiders"                 
    ## [359] "Wyoming Cowboys"                      
    ## [360] "Xavier Musketeers"                    
    ## [361] "Yale Bulldogs"                        
    ## [362] "Youngstown State Penguins"

``` r
library(readr)
write_csv(teams_26, "teams_2026.csv")

conferences_26 <- read_csv('~/Desktop/Mod 3/Mod3 Sport Analytics/Sports Analytics Proj./teams_2026_final.csv')
```

    ## Rows: 362 Columns: 24
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (23): abbreviation, display_name, short_name, mascot, nickname, team, co...
    ## dbl  (1): team_id
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
conferences_26
```

    ## # A tibble: 362 × 24
    ##    team_id abbreviation display_name      short_name mascot nickname team  color
    ##      <dbl> <chr>        <chr>             <chr>      <chr>  <chr>    <chr> <chr>
    ##  1    2000 ACU          Abilene Christia… Abilene C… Wildc… Abilene… Abil… 592d…
    ##  2    2005 AF           Air Force Falcons Air Force  Falco… Air For… Air … 0035…
    ##  3    2006 AKR          Akron Zips        Akron      Zips   Akron    Akron 0028…
    ##  4    2010 AAMU         Alabama A&M Bull… Alabama A… Bulld… Alabama… Alab… 7900…
    ##  5     333 ALA          Alabama Crimson … Alabama    Crims… Alabama  Alab… 9e16…
    ##  6    2011 ALST         Alabama State Ho… Alabama St Horne… Alabama… Alab… e9a9…
    ##  7    2016 ALCN         Alcorn State Bra… Alcorn St  Braves Alcorn … Alco… 4b00…
    ##  8      44 AMER         American Univers… American   Eagles American Amer… c411…
    ##  9    2026 APP          App State Mounta… App State  Mount… App Sta… App … 0000…
    ## 10       9 ASU          Arizona State Su… Arizona St Sun D… Arizona… Ariz… 8e0c…
    ## # ℹ 352 more rows
    ## # ℹ 16 more variables: alternate_color <chr>, logo <chr>, logo_dark <chr>,
    ## #   logos_href_3 <chr>, logos_href_4 <chr>, logos_href_5 <chr>,
    ## #   logos_href_6 <chr>, logos_href_7 <chr>, logos_href_8 <chr>,
    ## #   logos_href_9 <chr>, logos_href_10 <chr>, logos_href_11 <chr>,
    ## #   logos_href_12 <chr>, logos_href_13 <chr>, logos_href_14 <chr>,
    ## #   conference_2025_26 <chr>

``` r
load_mbb_schedule(2026)
```

    ## ── ESPN MBB Schedule from hoopR data repository ───────────────── hoopR 2.1.0 ──

    ## ℹ Data updated: 2026-03-04 07:47:35 EST

    ## # A tibble: 6,332 × 86
    ##         id uid   date  attendance time_valid neutral_site conference_competition
    ##      <int> <chr> <chr>      <dbl> <lgl>      <lgl>        <lgl>                 
    ##  1  4.02e8 s:40… 2026…          0 FALSE      TRUE         FALSE                 
    ##  2  4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
    ##  3  4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
    ##  4  4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
    ##  5  4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
    ##  6  4.02e8 s:40… 2026…          0 FALSE      TRUE         FALSE                 
    ##  7  4.02e8 s:40… 2026…          0 FALSE      TRUE         FALSE                 
    ##  8  4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
    ##  9  4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
    ## 10  4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
    ## # ℹ 6,322 more rows
    ## # ℹ 79 more variables: play_by_play_available <lgl>, recent <lgl>,
    ## #   start_date <chr>, broadcast <chr>, highlights <chr>, notes_type <chr>,
    ## #   notes_headline <chr>, broadcast_market <chr>, broadcast_name <chr>,
    ## #   type_id <int>, type_abbreviation <chr>, venue_id <int>,
    ## #   venue_full_name <chr>, venue_address_city <chr>, venue_address_state <chr>,
    ## #   venue_indoor <lgl>, status_clock <dbl>, status_display_clock <chr>, …

``` r
schedule_26 <- load_mbb_schedule(2026)
head(schedule_26)
```

    ## ── ESPN MBB Schedule from hoopR data repository ───────────────── hoopR 2.1.0 ──
    ## ℹ Data updated: 2026-03-04 07:47:35 EST

    ## # A tibble: 6 × 86
    ##         id uid   date  attendance time_valid neutral_site conference_competition
    ##      <int> <chr> <chr>      <dbl> <lgl>      <lgl>        <lgl>                 
    ## 1   4.02e8 s:40… 2026…          0 FALSE      TRUE         FALSE                 
    ## 2   4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
    ## 3   4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
    ## 4   4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
    ## 5   4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
    ## 6   4.02e8 s:40… 2026…          0 FALSE      TRUE         FALSE                 
    ## # ℹ 79 more variables: play_by_play_available <lgl>, recent <lgl>,
    ## #   start_date <chr>, broadcast <chr>, highlights <chr>, notes_type <chr>,
    ## #   notes_headline <chr>, broadcast_market <chr>, broadcast_name <chr>,
    ## #   type_id <int>, type_abbreviation <chr>, venue_id <int>,
    ## #   venue_full_name <chr>, venue_address_city <chr>, venue_address_state <chr>,
    ## #   venue_indoor <lgl>, status_clock <dbl>, status_display_clock <chr>,
    ## #   status_period <dbl>, status_type_id <int>, status_type_name <chr>, …

``` r
conferences_26_clean <- conferences_26 %>% select(-c('display_name','short_name', 'nickname', 'logos_href_3','logos_href_4', 'logos_href_5', 'logos_href_6', 'logos_href_7', 'logos_href_8', 'logos_href_9', 'logos_href_10', 'logos_href_11', 'logos_href_12', 'logos_href_13', 'logos_href_14'))
```

``` r
save(espn_mbb_pbp, temp_1, temp_2, games_1, games_2, mbb_2026, mbb_2025, df_elo_25, df_elo_26, conferences_26_clean, schedule_26,   file = "mbb_data_2.rda")
```

``` r
df_elo_26$team_name <- rownames(df_elo_26)

colnames(df_elo_26)[1] <- "elo_rating"

merged_mbb_clean <- merge(
    x = df_elo_26,
    y = conferences_26_clean,
    by.x = 'team_name',      
    by.y = 'team',   
    all.x = FALSE,           
    all.y = TRUE             
)
```

Graph out all of the teams by their conference and their ELOs on the
season to this point.

``` r
g_1 <- ggplot(merged_mbb_clean, 
               aes(x = elo_rating, 
                   y = conference_2025_26  
                   )) + 
  geom_point(alpha = 0.3) + 
 geom_image(aes(image = logo_dark), size = 0.05, asp = 16 / 9) + 
  dark_theme_bw() + 
   theme(legend.position="none", 
     panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank(),
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold")) +  
    labs(x= "Active 2026 ELO", y ="DI Conferences", 
       title = "Current ELOs of NCAA MBB",
       subtitle = "NCAA DI Teams")

g_1
```

![](readme_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
MAAC_Conference <- merged_mbb_clean %>% 
  filter(conference_2025_26 == 'MAAC')

g_2_MAAC <- ggplot(MAAC_Conference, 
               aes(x = elo_rating, 
                   y = reorder(team_name, elo_rating) 
                   )) + 
  geom_point(alpha = 0.3) + 
 geom_image(aes(image = logo_dark), size = 0.05, asp = 16 / 9) + 
  dark_theme_bw() + 
   theme(legend.position="none", 
     panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank(),
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold")) +  
    labs(x= "Active 2026 MAAC ELOs", y ="MAAC Teams", 
       title = "Current ELOs of the Metro-Atlantic Athletic Conference",
       )

g_2_MAAC
```

![](readme_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
ACC_Conference <- merged_mbb_clean %>% 
  filter(conference_2025_26 == 'ACC')

g_3_ACC <- ggplot(ACC_Conference, 
               aes(x = elo_rating, 
                   y = reorder(team_name, elo_rating) 
                   )) + 
  geom_point(alpha = 0.3) + 
 geom_image(aes(image = logo_dark), size = 0.05, asp = 16 / 9) + 
  dark_theme_bw() + 
   theme(legend.position="none", 
     panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank(),
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold")) +  
    labs(x= "Active 2026 ACC ELOs", y ="ACC Teams", 
       title = "Current ELOs of the Atlantic Coast Conference",
       )

g_3_ACC
```

![](readme_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
B1G_Conference <- merged_mbb_clean %>% 
  filter(conference_2025_26 == 'Big Ten')

g_4_B1G <- ggplot(B1G_Conference, 
               aes(x = elo_rating, 
                   y = reorder(team_name, elo_rating) 
                   )) + 
  geom_point(alpha = 0.3) + 
 geom_image(aes(image = logo_dark), size = 0.05, asp = 16 / 9) + 
  dark_theme_bw() + 
   theme(legend.position="none", 
     panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank(),
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold")) +  
    labs(x= "Active 2026 B1G ELOs", y ="Big Ten Teams", 
       title = "Current ELOs of the Big Ten Conference",
       )

g_4_B1G
```

![](readme_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
A10_Conference <- merged_mbb_clean %>% 
  filter(conference_2025_26 == 'Atlantic 10')

g_5_A10 <- ggplot(A10_Conference, 
               aes(x = elo_rating, 
                   y = reorder(team_name, elo_rating) 
                   )) + 
  geom_point(alpha = 0.3) + 
 geom_image(aes(image = logo_dark), size = 0.05, asp = 16 / 9) + 
  dark_theme_bw() + 
   theme(legend.position="none", 
     panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank(),
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold")) +  
    labs(x= "Active 2026 A10 ELOs", y ="Atlantic 10 Teams", 
       title = "Current ELOs of the Atlantic 10 Conference",
       )

g_5_A10
```

![](readme_files/figure-gfm/unnamed-chunk-29-1.png)<!-- --> WORK WITH
MAAC CONFERENCE Testing out probability of win Creating a remaining
schedule for the MAAC teams based on the Schedule_26 DataFrame

``` r
# Win Probability of Siena against Saint Peter's example
MAAC_Conference[MAAC_Conference$team_name %in% c("Siena", "Saint Peter's"),]
```

    ##        team_name elo_rating team_id abbreviation   mascot  color
    ## 12 Saint Peter's   1580.856    2612          SPU Peacocks 004CC2
    ## 13         Siena   1581.605    2561          SIE   Saints 037961
    ##    alternate_color                                                logo
    ## 12            <NA> https://a.espncdn.com/i/teamlogos/ncaa/500/2612.png
    ## 13          eea60f https://a.espncdn.com/i/teamlogos/ncaa/500/2561.png
    ##                                                   logo_dark conference_2025_26
    ## 12 https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2612.png               MAAC
    ## 13 https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2561.png               MAAC

``` r
elo.prob(MAAC_Conference$elo_rating[MAAC_Conference$team_name == "Siena"],
         MAAC_Conference$elo_rating[MAAC_Conference$team_name == "Saint Peter's"])
```

    ## [1] 0.5010771

``` r
# Set seed
set.seed(3)

# Calculate win probability for United Siena v Saint Peter's
win_prob <- elo.prob(MAAC_Conference$elo_rating[MAAC_Conference$team_name == "Siena"],
         MAAC_Conference$elo_rating[MAAC_Conference$team_name == "Saint Peter's"])

# Generate number between 0 and 1
sim <- runif(1, min = 0, max = 1)
print(paste("Siena win probability is:", round(win_prob, 4), sep = " "))
```

    ## [1] "Siena win probability is: 0.5011"

``` r
print(paste("The simulated value is:", round(sim, 4), sep = " "))
```

    ## [1] "The simulated value is: 0.168"

``` r
if(sim < win_prob){
  print(paste("Siena Wins the Game"))
} else{
   print(paste("Saint Peter's Wins the Game"))
}
```

    ## [1] "Siena Wins the Game"

``` r
MAAC_remaining_schedule <- schedule_26 %>% 
  filter(status_type_name == "STATUS_SCHEDULED") %>% 
  dplyr::rename(
    home_team_name = home_location,
    away_team_name = away_location
  ) %>% 
  filter(home_team_name %in% MAAC_Conference$team_name | 
         away_team_name %in% MAAC_Conference$team_name) %>%
  select(id, game_date, home_team_name, away_team_name)

head(MAAC_remaining_schedule)
```

    ## ── ESPN MBB Schedule from hoopR data repository ───────────────── hoopR 2.1.0 ──

    ## ℹ Data updated: 2026-03-04 07:47:35 EST

    ## # A tibble: 6 × 4
    ##          id game_date  home_team_name away_team_name  
    ##       <int> <date>     <chr>          <chr>           
    ## 1 401851464 2026-03-07 Siena          Mount St. Mary's
    ## 2 401851462 2026-03-07 Quinnipiac     Marist          
    ## 3 401851460 2026-03-06 Saint Peter's  TBD             
    ## 4 401851457 2026-03-06 Merrimack      TBD             
    ## 5 401851456 2026-03-05 Fairfield      Manhattan       
    ## 6 401851454 2026-03-05 Iona           Sacred Heart

``` r
MAAC_remaining_schedule <- merge(
  MAAC_remaining_schedule, 
  MAAC_Conference[, c("team_name", "elo_rating")], 
  by.x = "home_team_name", 
  by.y = "team_name", 
  all.x = TRUE
)

colnames(MAAC_remaining_schedule)[colnames(MAAC_remaining_schedule) == "elo_rating"] <- "home_elo"


MAAC_remaining_schedule
```

    ## ───────────────────────────────────────────────────────────────── hoopR 2.1.0 ──

    ## # A tibble: 6 × 5
    ##   home_team_name        id game_date  away_team_name   home_elo
    ##   <chr>              <int> <date>     <chr>               <dbl>
    ## 1 Fairfield      401851456 2026-03-05 Manhattan           1566.
    ## 2 Iona           401851454 2026-03-05 Sacred Heart        1519.
    ## 3 Merrimack      401851457 2026-03-06 TBD                 1596.
    ## 4 Quinnipiac     401851462 2026-03-07 Marist              1545.
    ## 5 Saint Peter's  401851460 2026-03-06 TBD                 1581.
    ## 6 Siena          401851464 2026-03-07 Mount St. Mary's    1582.

Adding in the home team elos in the code above. This will help to build
out the remaining schedule and then to build the game
predictions/outcomes.

``` r
MAAC_remaining_schedule <- merge(
  MAAC_remaining_schedule, 
  MAAC_Conference[, c("team_name", "elo_rating")], 
  by.x = "away_team_name", 
  by.y = "team_name", 
  all.x = TRUE
)
colnames(MAAC_remaining_schedule)[colnames(MAAC_remaining_schedule) == "elo_rating"] <- "away_elo"

MAAC_remaining_schedule
```

    ## ───────────────────────────────────────────────────────────────── hoopR 2.1.0 ──

    ## # A tibble: 6 × 6
    ##   away_team_name   home_team_name        id game_date  home_elo away_elo
    ##   <chr>            <chr>              <int> <date>        <dbl>    <dbl>
    ## 1 Manhattan        Fairfield      401851456 2026-03-05    1566.    1446.
    ## 2 Marist           Quinnipiac     401851462 2026-03-07    1545.    1537.
    ## 3 Mount St. Mary's Siena          401851464 2026-03-07    1582.    1523.
    ## 4 Sacred Heart     Iona           401851454 2026-03-05    1519.    1473.
    ## 5 TBD              Merrimack      401851457 2026-03-06    1596.      NA 
    ## 6 TBD              Saint Peter's  401851460 2026-03-06    1581.      NA

Getting the win probability and then adding some points to the elo for
home court advantage

``` r
MAAC_remaining_schedule$home_prob <- elo.prob(
  elo.A = MAAC_remaining_schedule$home_elo + 100, 
  elo.B = MAAC_remaining_schedule$away_elo
)
```

Running code similar to what we did for our elo_world_cup.rmd

``` r
set.seed(33) 

# Generate a random number (0-1) for every game
MAAC_remaining_schedule$sim_value <- runif(nrow(MAAC_remaining_schedule))

# Determine the winner: If random number < home_prob, Home wins. Else, Away wins.
MAAC_remaining_schedule$predicted_winner <- ifelse(
  MAAC_remaining_schedule$sim_value < MAAC_remaining_schedule$home_prob, 
  MAAC_remaining_schedule$home_team_name, 
  MAAC_remaining_schedule$away_team_name
)

# View the results
head(MAAC_remaining_schedule)
```

    ## ───────────────────────────────────────────────────────────────── hoopR 2.1.0 ──

    ## # A tibble: 6 × 9
    ##   away_team_name   home_team_name      id game_date  home_elo away_elo home_prob
    ##   <chr>            <chr>            <int> <date>        <dbl>    <dbl>     <dbl>
    ## 1 Manhattan        Fairfield       4.02e8 2026-03-05    1566.    1446.     0.780
    ## 2 Marist           Quinnipiac      4.02e8 2026-03-07    1545.    1537.     0.651
    ## 3 Mount St. Mary's Siena           4.02e8 2026-03-07    1582.    1523.     0.713
    ## 4 Sacred Heart     Iona            4.02e8 2026-03-05    1519.    1473.     0.698
    ## 5 TBD              Merrimack       4.02e8 2026-03-06    1596.      NA     NA    
    ## 6 TBD              Saint Peter's   4.02e8 2026-03-06    1581.      NA     NA    
    ## # ℹ 2 more variables: sim_value <dbl>, predicted_winner <chr>

Here I am looking at the predicted winners of the remaining MAAC
Schedule and how many games these teams are going to win.

``` r
table(MAAC_remaining_schedule$predicted_winner)
```

    ## 
    ##    Fairfield   Quinnipiac Sacred Heart        Siena 
    ##            1            1            1            1

``` r
MAAC_current_elos <- setNames(MAAC_Conference$elo_rating, MAAC_Conference$team_name)

# 2. Sort schedule by date
# It is critical to simulate in order!
MAAC_remaining_schedule <- MAAC_remaining_schedule %>% arrange(game_date)

# 3. Create columns to store results
MAAC_remaining_schedule$home_prob <- NA
MAAC_remaining_schedule$sim_value <- NA
MAAC_remaining_schedule$predicted_winner <- NA
MAAC_remaining_schedule$home_elo_before <- NA
MAAC_remaining_schedule$away_elo_before <- NA

# 4. Loop through every game to simulate and update
# Set K-factor for updates (e.g., 30)
K <- 30
set.seed(33)
```

``` r
for(i in 1:nrow(MAAC_remaining_schedule)) {
  
  # A. Get Teams
  home_team <- MAAC_remaining_schedule$home_team_name[i]
  away_team <- MAAC_remaining_schedule$away_team_name[i]
  
  # B. Get Current ELOs (Handle missing teams if necessary)
  # If a team isn't in our list (e.g. non-D1), default to 1500 or skip
  h_elo <- if(home_team %in% names(MAAC_current_elos)) MAAC_current_elos[[home_team]] else 1500
  a_elo <- if(away_team %in% names(MAAC_current_elos)) MAAC_current_elos[[away_team]] else 1500
  
  # Store 'Before' ratings for reference
  MAAC_remaining_schedule$home_elo_before[i] <- h_elo
  MAAC_remaining_schedule$away_elo_before[i] <- a_elo
  
  # C. Calculate Win Prob (Current H_ELO + 100 vs Current A_ELO)
  h_prob <- elo.prob(h_elo + 100, a_elo)
  MAAC_remaining_schedule$home_prob[i] <- h_prob
  
  # D. Simulate Outcome
  sim_val <- runif(1)
  MAAC_remaining_schedule$sim_value[i] <- sim_val
  
  if(sim_val < h_prob) {
    # Home Wins
    MAAC_remaining_schedule$predicted_winner[i] <- home_team
    actual_score <- 1
  } else {
    # Away Wins
    MAAC_remaining_schedule$predicted_winner[i] <- away_team
    actual_score <- 0
  }

  # E. UPDATE ELOs!
  # Formula: New = Old + K * (Actual - Expected)
  # Note: The expected result for update excludes the 100 point HCA usually, 
  # or you can include it. Standard packages usually update based on the probability used.
  
  shift <- K * (actual_score - h_prob)
  
  # Apply updates to the lookup table
  if(home_team %in% names(MAAC_current_elos)) {
    MAAC_current_elos[[home_team]] <- h_elo + shift
  }
  if(away_team %in% names(MAAC_current_elos)) {
    MAAC_current_elos[[away_team]] <- a_elo - shift
  }
}

# 5. View the final "Simulated" Standings
final_MAAC_simulated_elos <- data.frame(
  team = names(MAAC_current_elos),
  final_elo = as.numeric(MAAC_current_elos)
) %>% arrange(desc(final_elo))

head(final_MAAC_simulated_elos)
```

    ##            team final_elo
    ## 1     Merrimack  1603.535
    ## 2         Siena  1590.205
    ## 3     Fairfield  1572.913
    ## 4 Saint Peter's  1558.684
    ## 5        Marist  1556.566
    ## 6          Iona  1528.001

``` r
head(MAAC_remaining_schedule)
```

    ## ───────────────────────────────────────────────────────────────── hoopR 2.1.0 ──

    ## # A tibble: 6 × 11
    ##   away_team_name   home_team_name      id game_date  home_elo away_elo home_prob
    ##   <chr>            <chr>            <int> <date>        <dbl>    <dbl>     <dbl>
    ## 1 Manhattan        Fairfield       4.02e8 2026-03-05    1566.    1446.     0.780
    ## 2 Sacred Heart     Iona            4.02e8 2026-03-05    1519.    1473.     0.698
    ## 3 TBD              Merrimack       4.02e8 2026-03-06    1596.      NA      0.756
    ## 4 TBD              Saint Peter's   4.02e8 2026-03-06    1581.      NA      0.739
    ## 5 Marist           Quinnipiac      4.02e8 2026-03-07    1545.    1537.     0.651
    ## 6 Mount St. Mary's Siena           4.02e8 2026-03-07    1582.    1523.     0.713
    ## # ℹ 4 more variables: sim_value <dbl>, predicted_winner <chr>,
    ## #   home_elo_before <dbl>, away_elo_before <dbl>

Atlantic 10 Conference

``` r
# 1. Create a fresh A10 schedule from the FULL schedule_26
A10_remaining_schedule <- schedule_26 %>% 
  filter(status_type_name == "STATUS_SCHEDULED") %>% 
  dplyr::rename(
    home_team_name = home_location,
    away_team_name = away_location
  ) %>% 
  # Filter specifically for A10 teams this time!
  filter(home_team_name %in% A10_Conference$team_name | 
         away_team_name %in% A10_Conference$team_name) %>%
  select(id, game_date, home_team_name, away_team_name)

# 2. Now merge your A10 ELOs onto this fresh schedule
A10_remaining_schedule <- merge(
  A10_remaining_schedule, 
  A10_Conference[, c("team_name", "elo_rating")], 
  by.x = "home_team_name", 
  by.y = "team_name", 
  all.x = TRUE
)

# Rename the column
colnames(A10_remaining_schedule)[colnames(A10_remaining_schedule) == "elo_rating"] <- "home_elo"

# View the results
head(A10_remaining_schedule)
```

    ## ───────────────────────────────────────────────────────────────── hoopR 2.1.0 ──

    ## # A tibble: 6 × 5
    ##   home_team_name           id game_date  away_team_name  home_elo
    ##   <chr>                 <int> <date>     <chr>              <dbl>
    ## 1 Davidson          401828457 2026-03-04 Saint Joseph's     1582.
    ## 2 Dayton            401828462 2026-03-06 VCU                1646.
    ## 3 Duquesne          401828464 2026-03-07 Richmond           1544.
    ## 4 Fordham           401828465 2026-03-07 Rhode Island       1538.
    ## 5 George Mason      401828466 2026-03-07 Saint Louis        1596.
    ## 6 George Washington 401828460 2026-03-04 St. Bonaventure    1534.

``` r
A10_remaining_schedule <- merge(
  A10_remaining_schedule, 
  A10_Conference[, c("team_name", "elo_rating")], 
  by.x = "away_team_name", 
  by.y = "team_name", 
  all.x = TRUE
)
colnames(A10_remaining_schedule)[colnames(A10_remaining_schedule) == "elo_rating"] <- "away_elo"

A10_remaining_schedule
```

    ## ───────────────────────────────────────────────────────────────── hoopR 2.1.0 ──

    ## # A tibble: 12 × 6
    ##    away_team_name    home_team_name           id game_date  home_elo away_elo
    ##    <chr>             <chr>                 <int> <date>        <dbl>    <dbl>
    ##  1 Davidson          St. Bonaventure   401828463 2026-03-07    1502.    1582.
    ##  2 Duquesne          Rhode Island      401828458 2026-03-04    1507.    1544.
    ##  3 Fordham           La Salle          401828459 2026-03-04    1410.    1538.
    ##  4 George Washington Loyola Chicago    401828467 2026-03-07    1393.    1534.
    ##  5 La Salle          Saint Joseph's    401828468 2026-03-07    1601.    1410.
    ##  6 Loyola Chicago    Saint Louis       401828461 2026-03-04    1715.    1393.
    ##  7 Rhode Island      Fordham           401828465 2026-03-07    1538.    1507.
    ##  8 Richmond          Duquesne          401828464 2026-03-07    1544.    1477.
    ##  9 Saint Joseph's    Davidson          401828457 2026-03-04    1582.    1601.
    ## 10 Saint Louis       George Mason      401828466 2026-03-07    1596.    1715.
    ## 11 St. Bonaventure   George Washington 401828460 2026-03-04    1534.    1502.
    ## 12 VCU               Dayton            401828462 2026-03-06    1646.    1685.

``` r
A10_remaining_schedule$home_prob <- elo.prob(
  elo.A = A10_remaining_schedule$home_elo + 100, 
  elo.B = A10_remaining_schedule$away_elo
)
```

``` r
set.seed(33) 

# Generate a random number (0-1) for every game
A10_remaining_schedule$sim_value <- runif(nrow(A10_remaining_schedule))

# Determine the winner: If random number < home_prob, Home wins. Else, Away wins.
A10_remaining_schedule$predicted_winner <- ifelse(
  A10_remaining_schedule$sim_value < A10_remaining_schedule$home_prob, 
  A10_remaining_schedule$home_team_name, 
  A10_remaining_schedule$away_team_name
)

# View the results
head(A10_remaining_schedule)
```

    ## ───────────────────────────────────────────────────────────────── hoopR 2.1.0 ──

    ## # A tibble: 6 × 9
    ##   away_team_name    home_team_name     id game_date  home_elo away_elo home_prob
    ##   <chr>             <chr>           <int> <date>        <dbl>    <dbl>     <dbl>
    ## 1 Davidson          St. Bonaventu… 4.02e8 2026-03-07    1502.    1582.     0.529
    ## 2 Duquesne          Rhode Island   4.02e8 2026-03-04    1507.    1544.     0.590
    ## 3 Fordham           La Salle       4.02e8 2026-03-04    1410.    1538.     0.460
    ## 4 George Washington Loyola Chicago 4.02e8 2026-03-07    1393.    1534.     0.441
    ## 5 La Salle          Saint Joseph's 4.02e8 2026-03-07    1601.    1410.     0.843
    ## 6 Loyola Chicago    Saint Louis    4.02e8 2026-03-04    1715.    1393.     0.919
    ## # ℹ 2 more variables: sim_value <dbl>, predicted_winner <chr>

``` r
A10_current_elos <- setNames(A10_Conference$elo_rating, A10_Conference$team_name)

# 2. Sort schedule by date
# It is critical to simulate in order!
A10_remaining_schedule <- A10_remaining_schedule %>% arrange(game_date)

# 3. Create columns to store results
A10_remaining_schedule$home_prob <- NA
A10_remaining_schedule$sim_value <- NA
A10_remaining_schedule$predicted_winner <- NA
A10_remaining_schedule$home_elo_before <- NA
A10_remaining_schedule$away_elo_before <- NA

# 4. Loop through every game to simulate and update
# Set K-factor for updates (e.g., 30)
K <- 30
set.seed(33)
```

``` r
for(i in 1:nrow(A10_remaining_schedule)) {
  
  # A. Get Teams
  home_team <- A10_remaining_schedule$home_team_name[i]
  away_team <- A10_remaining_schedule$away_team_name[i]
  
  # B. Get Current ELOs (Handle missing teams if necessary)
  # If a team isn't in our list (e.g. non-D1), default to 1500 or skip
  h_elo <- if(home_team %in% names(A10_current_elos)) A10_current_elos[[home_team]] else 1500
  a_elo <- if(away_team %in% names(A10_current_elos)) A10_current_elos[[away_team]] else 1500
  
  # Store 'Before' ratings for reference
  A10_remaining_schedule$home_elo_before[i] <- h_elo
  A10_remaining_schedule$away_elo_before[i] <- a_elo
  
  # C. Calculate Win Prob (Current H_ELO + 100 vs Current A_ELO)
  h_prob <- elo.prob(h_elo + 100, a_elo)
  A10_remaining_schedule$home_prob[i] <- h_prob
  
  # D. Simulate Outcome
  sim_val <- runif(1)
  A10_remaining_schedule$sim_value[i] <- sim_val
  
  if(sim_val < h_prob) {
    # Home Wins
    A10_remaining_schedule$predicted_winner[i] <- home_team
    actual_score <- 1
  } else {
    # Away Wins
    A10_remaining_schedule$predicted_winner[i] <- away_team
    actual_score <- 0
  }

  # E. UPDATE ELOs!
  # Formula: New = Old + K * (Actual - Expected)
  # Note: The expected result for update excludes the 100 point HCA usually, 
  # or you can include it. Standard packages usually update based on the probability used.
  
  shift <- K * (actual_score - h_prob)
  
  # Apply updates to the lookup table
  if(home_team %in% names(A10_current_elos)) {
    A10_current_elos[[home_team]] <- h_elo + shift
  }
  if(away_team %in% names(A10_current_elos)) {
    A10_current_elos[[away_team]] <- a_elo - shift
  }
}

# 5. View the final "Simulated" Standings
A10_final_simulated_elos <- data.frame(
  team = names(A10_current_elos),
  A10_final_elo = as.numeric(A10_current_elos)
) %>% arrange(desc(A10_final_elo))

head(A10_final_simulated_elos)
```

    ##             team A10_final_elo
    ## 1    Saint Louis      1701.590
    ## 2            VCU      1672.964
    ## 3         Dayton      1658.405
    ## 4 Saint Joseph's      1624.311
    ## 5   George Mason      1612.167
    ## 6       Davidson      1550.807

``` r
head(A10_remaining_schedule)
```

    ## ───────────────────────────────────────────────────────────────── hoopR 2.1.0 ──

    ## # A tibble: 6 × 11
    ##   away_team_name  home_team_name       id game_date  home_elo away_elo home_prob
    ##   <chr>           <chr>             <int> <date>        <dbl>    <dbl>     <dbl>
    ## 1 Duquesne        Rhode Island     4.02e8 2026-03-04    1507.    1544.     0.590
    ## 2 Fordham         La Salle         4.02e8 2026-03-04    1410.    1538.     0.460
    ## 3 Loyola Chicago  Saint Louis      4.02e8 2026-03-04    1715.    1393.     0.919
    ## 4 Saint Joseph's  Davidson         4.02e8 2026-03-04    1582.    1601.     0.614
    ## 5 St. Bonaventure George Washingt… 4.02e8 2026-03-04    1534.    1502.     0.682
    ## 6 VCU             Dayton           4.02e8 2026-03-06    1646.    1685.     0.586
    ## # ℹ 4 more variables: sim_value <dbl>, predicted_winner <chr>,
    ## #   home_elo_before <dbl>, away_elo_before <dbl>

Big Ten Conference Sims

``` r
B1G_remaining_schedule <- schedule_26 %>% 
  filter(status_type_name == "STATUS_SCHEDULED") %>% 
  dplyr::rename(
    home_team_name = home_location,
    away_team_name = away_location
  ) %>% 
  # Filter specifically for A10 teams this time!
  filter(home_team_name %in% B1G_Conference$team_name | 
         away_team_name %in% B1G_Conference$team_name) %>%
  select(id, game_date, home_team_name, away_team_name)

# 2. Now merge your A10 ELOs onto this fresh schedule
B1G_remaining_schedule <- merge(
  B1G_remaining_schedule, 
  B1G_Conference[, c("team_name", "elo_rating")], 
  by.x = "home_team_name", 
  by.y = "team_name", 
  all.x = TRUE
)

# Rename the column
colnames(B1G_remaining_schedule)[colnames(B1G_remaining_schedule) == "elo_rating"] <- "home_elo"

# View the results
head(B1G_remaining_schedule)
```

    ## ───────────────────────────────────────────────────────────────── hoopR 2.1.0 ──

    ## # A tibble: 6 × 5
    ##   home_team_name        id game_date  away_team_name home_elo
    ##   <chr>              <int> <date>     <chr>             <dbl>
    ## 1 Indiana        401825555 2026-03-04 Minnesota         1538.
    ## 2 Iowa           401825560 2026-03-05 Michigan          1607.
    ## 3 Maryland       401825567 2026-03-08 Illinois          1448.
    ## 4 Michigan       401825568 2026-03-08 Michigan State    1770.
    ## 5 Michigan State 401825561 2026-03-05 Rutgers           1684.
    ## 6 Minnesota      401825562 2026-03-07 Northwestern      1534.

``` r
B1G_remaining_schedule <- merge(
  B1G_remaining_schedule, 
  B1G_Conference[, c("team_name", "elo_rating")], 
  by.x = "away_team_name", 
  by.y = "team_name", 
  all.x = TRUE
)
colnames(B1G_remaining_schedule)[colnames(B1G_remaining_schedule) == "elo_rating"] <- "away_elo"

B1G_remaining_schedule
```

    ## ───────────────────────────────────────────────────────────────── hoopR 2.1.0 ──

    ## # A tibble: 16 × 6
    ##    away_team_name home_team_name        id game_date  home_elo away_elo
    ##    <chr>          <chr>              <int> <date>        <dbl>    <dbl>
    ##  1 Illinois       Maryland       401825567 2026-03-08    1448.    1713.
    ##  2 Indiana        Ohio State     401825563 2026-03-07    1583.    1538.
    ##  3 Iowa           Nebraska       401825569 2026-03-08    1690.    1607.
    ##  4 Maryland       Wisconsin      401825559 2026-03-04    1642.    1448.
    ##  5 Michigan       Iowa           401825560 2026-03-05    1607.    1770.
    ##  6 Michigan State Michigan       401825568 2026-03-08    1770.    1684.
    ##  7 Minnesota      Indiana        401825555 2026-03-04    1538.    1534.
    ##  8 Northwestern   Minnesota      401825562 2026-03-07    1534.    1522.
    ##  9 Ohio State     Penn State     401825557 2026-03-04    1461.    1583.
    ## 10 Penn State     Rutgers        401825570 2026-03-08    1467.    1461.
    ## 11 Purdue         Northwestern   401825556 2026-03-04    1522.    1637.
    ## 12 Rutgers        Michigan State 401825561 2026-03-05    1684.    1467.
    ## 13 UCLA           USC            401825566 2026-03-07    1540.    1625.
    ## 14 USC            Washington     401825558 2026-03-04    1503.    1540.
    ## 15 Washington     Oregon         401825564 2026-03-07    1472.    1503.
    ## 16 Wisconsin      Purdue         401825565 2026-03-07    1637.    1642.

``` r
B1G_remaining_schedule$home_prob <- elo.prob(
  elo.A = B1G_remaining_schedule$home_elo + 100, 
  elo.B = B1G_remaining_schedule$away_elo
)
```

``` r
set.seed(33) 

# Generate a random number (0-1) for every game
B1G_remaining_schedule$sim_value <- runif(nrow(B1G_remaining_schedule))

# Determine the winner: If random number < home_prob, Home wins. Else, Away wins.
B1G_remaining_schedule$predicted_winner <- ifelse(
  B1G_remaining_schedule$sim_value < B1G_remaining_schedule$home_prob, 
  B1G_remaining_schedule$home_team_name, 
  B1G_remaining_schedule$away_team_name
)

# View the results
head(B1G_remaining_schedule)
```

    ## ───────────────────────────────────────────────────────────────── hoopR 2.1.0 ──

    ## # A tibble: 6 × 9
    ##   away_team_name home_team_name        id game_date  home_elo away_elo home_prob
    ##   <chr>          <chr>              <int> <date>        <dbl>    <dbl>     <dbl>
    ## 1 Illinois       Maryland       401825567 2026-03-08    1448.    1713.     0.279
    ## 2 Indiana        Ohio State     401825563 2026-03-07    1583.    1538.     0.698
    ## 3 Iowa           Nebraska       401825569 2026-03-08    1690.    1607.     0.742
    ## 4 Maryland       Wisconsin      401825559 2026-03-04    1642.    1448.     0.844
    ## 5 Michigan       Iowa           401825560 2026-03-05    1607.    1770.     0.410
    ## 6 Michigan State Michigan       401825568 2026-03-08    1770.    1684.     0.744
    ## # ℹ 2 more variables: sim_value <dbl>, predicted_winner <chr>

``` r
B1G_current_elos <- setNames(B1G_Conference$elo_rating, B1G_Conference$team_name)

# 2. Sort schedule by date
# It is critical to simulate in order!
B1G_remaining_schedule <- B1G_remaining_schedule %>% arrange(game_date)

# 3. Create columns to store results
B1G_remaining_schedule$home_prob <- NA
B1G_remaining_schedule$sim_value <- NA
B1G_remaining_schedule$predicted_winner <- NA
B1G_remaining_schedule$home_elo_before <- NA
B1G_remaining_schedule$away_elo_before <- NA

# 4. Loop through every game to simulate and update
# Set K-factor for updates (e.g., 30)
K <- 30
set.seed(33)
```

``` r
for(i in 1:nrow(B1G_remaining_schedule)) {
  
  # A. Get Teams
  home_team <- B1G_remaining_schedule$home_team_name[i]
  away_team <- B1G_remaining_schedule$away_team_name[i]
  
  # B. Get Current ELOs (Handle missing teams if necessary)
  # If a team isn't in our list (e.g. non-D1), default to 1500 or skip
  h_elo <- if(home_team %in% names(B1G_current_elos)) B1G_current_elos[[home_team]] else 1500
  a_elo <- if(away_team %in% names(B1G_current_elos)) B1G_current_elos[[away_team]] else 1500

  # Store 'Before' ratings for reference
  B1G_remaining_schedule$home_elo_before[i] <- h_elo
  B1G_remaining_schedule$away_elo_before[i] <- a_elo
  
  # C. Calculate Win Prob (Current H_ELO + 100 vs Current A_ELO)
  h_prob <- elo.prob(h_elo + 100, a_elo)
  B1G_remaining_schedule$home_prob[i] <- h_prob
  
  # D. Simulate Outcome
  sim_val <- runif(1)
  B1G_remaining_schedule$sim_value[i] <- sim_val
  
  if(sim_val < h_prob) {
    # Home Wins
    B1G_remaining_schedule$predicted_winner[i] <- home_team
    actual_score <- 1
  } else {
    # Away Wins
    B1G_remaining_schedule$predicted_winner[i] <- away_team
    actual_score <- 0
  }

  # E. UPDATE ELOs!
  # Formula: New = Old + K * (Actual - Expected)
  # Note: The expected result for update excludes the 100 point HCA usually, 
  # or you can include it. Standard packages usually update based on the probability used.
  
  shift <- K * (actual_score - h_prob)
  
  # Apply updates to the lookup table
  if(home_team %in% names(A10_current_elos)) {
    B1G_current_elos[[home_team]] <- h_elo + shift
  }
  if(away_team %in% names(B1G_current_elos)) {
    B1G_current_elos[[away_team]] <- a_elo - shift
  }
}

# 5. View the final "Simulated" Standings
B1G_final_simulated_elos <- data.frame(
  team = names(B1G_current_elos),
  B1G_final_elo = as.numeric(B1G_current_elos)
) %>% arrange(desc(B1G_final_elo))

head(B1G_final_simulated_elos)
```

    ##             team B1G_final_elo
    ## 1       Michigan      1782.086
    ## 2 Michigan State      1707.042
    ## 3       Illinois      1691.008
    ## 4       Nebraska      1689.780
    ## 5         Purdue      1651.827
    ## 6      Wisconsin      1631.161

``` r
head(B1G_remaining_schedule)
```

    ## ───────────────────────────────────────────────────────────────── hoopR 2.1.0 ──

    ## # A tibble: 6 × 11
    ##   away_team_name home_team_name        id game_date  home_elo away_elo home_prob
    ##   <chr>          <chr>              <int> <date>        <dbl>    <dbl>     <dbl>
    ## 1 Maryland       Wisconsin      401825559 2026-03-04    1642.    1448.     0.844
    ## 2 Minnesota      Indiana        401825555 2026-03-04    1538.    1534.     0.645
    ## 3 Ohio State     Penn State     401825557 2026-03-04    1461.    1583.     0.468
    ## 4 Purdue         Northwestern   401825556 2026-03-04    1522.    1637.     0.478
    ## 5 USC            Washington     401825558 2026-03-04    1503.    1540.     0.589
    ## 6 Michigan       Iowa           401825560 2026-03-05    1607.    1770.     0.410
    ## # ℹ 4 more variables: sim_value <dbl>, predicted_winner <chr>,
    ## #   home_elo_before <dbl>, away_elo_before <dbl>

Atlantic Coast Conference

``` r
ACC_remaining_schedule <- schedule_26 %>% 
  filter(status_type_name == "STATUS_SCHEDULED") %>% 
  dplyr::rename(
    home_team_name = home_location,
    away_team_name = away_location
  ) %>% 
  # Filter specifically for A10 teams this time!
  filter(home_team_name %in% ACC_Conference$team_name | 
         away_team_name %in% ACC_Conference$team_name) %>%
  select(id, game_date, home_team_name, away_team_name)

# 2. Now merge your A10 ELOs onto this fresh schedule
ACC_remaining_schedule <- merge(
  ACC_remaining_schedule, 
  ACC_Conference[, c("team_name", "elo_rating")], 
  by.x = "home_team_name", 
  by.y = "team_name", 
  all.x = TRUE
)

# Rename the column
colnames(ACC_remaining_schedule)[colnames(ACC_remaining_schedule) == "elo_rating"] <- "home_elo"

# View the results
head(ACC_remaining_schedule)
```

    ## ───────────────────────────────────────────────────────────────── hoopR 2.1.0 ──

    ## # A tibble: 6 × 5
    ##   home_team_name        id game_date  away_team_name home_elo
    ##   <chr>              <int> <date>     <chr>             <dbl>
    ## 1 Boston College 401820793 2026-03-07 Notre Dame        1442.
    ## 2 Clemson        401820787 2026-03-07 Georgia Tech      1629.
    ## 3 Duke           401820788 2026-03-07 North Carolina    1785.
    ## 4 Florida State  401820789 2026-03-07 SMU               1566.
    ## 5 Georgia Tech   401820780 2026-03-04 California        1427.
    ## 6 Miami          401820790 2026-03-07 Louisville        1684.

``` r
ACC_remaining_schedule <- merge(
  ACC_remaining_schedule, 
  ACC_Conference[, c("team_name", "elo_rating")], 
  by.x = "away_team_name", 
  by.y = "team_name", 
  all.x = TRUE
)
colnames(ACC_remaining_schedule)[colnames(ACC_remaining_schedule) == "elo_rating"] <- "away_elo"

ACC_remaining_schedule
```

    ## ───────────────────────────────────────────────────────────────── hoopR 2.1.0 ──

    ## # A tibble: 13 × 6
    ##    away_team_name home_team_name        id game_date  home_elo away_elo
    ##    <chr>          <chr>              <int> <date>        <dbl>    <dbl>
    ##  1 California     Georgia Tech   401820780 2026-03-04    1427.    1625.
    ##  2 California     Wake Forest    401820795 2026-03-07    1537.    1625.
    ##  3 Florida State  Pittsburgh     401820783 2026-03-04    1463.    1566.
    ##  4 Georgia Tech   Clemson        401820787 2026-03-07    1629.    1427.
    ##  5 Louisville     Miami          401820790 2026-03-07    1684.    1632.
    ##  6 Miami          SMU            401820784 2026-03-04    1575.    1684.
    ##  7 North Carolina Duke           401820788 2026-03-07    1785.    1702.
    ##  8 Notre Dame     Boston College 401820793 2026-03-07    1442.    1478.
    ##  9 Pittsburgh     Syracuse       401820792 2026-03-07    1518.    1463.
    ## 10 SMU            Florida State  401820789 2026-03-07    1566.    1575.
    ## 11 Stanford       NC State       401820791 2026-03-07    1607.    1579.
    ## 12 Stanford       Notre Dame     401820782 2026-03-04    1478.    1579.
    ## 13 Virginia Tech  Virginia       401820794 2026-03-07    1734.    1569.

``` r
ACC_remaining_schedule$home_prob <- elo.prob(
  elo.A = ACC_remaining_schedule$home_elo + 100, 
  elo.B = ACC_remaining_schedule$away_elo
)
```

``` r
set.seed(33) 

# Generate a random number (0-1) for every game
ACC_remaining_schedule$sim_value <- runif(nrow(ACC_remaining_schedule))

# Determine the winner: If random number < home_prob, Home wins. Else, Away wins.
ACC_remaining_schedule$predicted_winner <- ifelse(
  ACC_remaining_schedule$sim_value < ACC_remaining_schedule$home_prob, 
  ACC_remaining_schedule$home_team_name, 
  ACC_remaining_schedule$away_team_name
)

# View the results
head(ACC_remaining_schedule)
```

    ## ───────────────────────────────────────────────────────────────── hoopR 2.1.0 ──

    ## # A tibble: 6 × 9
    ##   away_team_name home_team_name        id game_date  home_elo away_elo home_prob
    ##   <chr>          <chr>              <int> <date>        <dbl>    <dbl>     <dbl>
    ## 1 California     Georgia Tech   401820780 2026-03-04    1427.    1625.     0.363
    ## 2 California     Wake Forest    401820795 2026-03-07    1537.    1625.     0.518
    ## 3 Florida State  Pittsburgh     401820783 2026-03-04    1463.    1566.     0.495
    ## 4 Georgia Tech   Clemson        401820787 2026-03-07    1629.    1427.     0.850
    ## 5 Louisville     Miami          401820790 2026-03-07    1684.    1632.     0.706
    ## 6 Miami          SMU            401820784 2026-03-04    1575.    1684.     0.486
    ## # ℹ 2 more variables: sim_value <dbl>, predicted_winner <chr>

``` r
ACC_current_elos <- setNames(ACC_Conference$elo_rating, ACC_Conference$team_name)

# 2. Sort schedule by date
# It is critical to simulate in order!
ACC_remaining_schedule <- ACC_remaining_schedule %>% arrange(game_date)

# 3. Create columns to store results
ACC_remaining_schedule$home_prob <- NA
ACC_remaining_schedule$sim_value <- NA
ACC_remaining_schedule$predicted_winner <- NA
ACC_remaining_schedule$home_elo_before <- NA
ACC_remaining_schedule$away_elo_before <- NA

# 4. Loop through every game to simulate and update
# Set K-factor for updates (e.g., 30)
K <- 30
set.seed(33)
```

``` r
for(i in 1:nrow(ACC_remaining_schedule)) {
  
  # A. Get Teams
  home_team <- ACC_remaining_schedule$home_team_name[i]
  away_team <- ACC_remaining_schedule$away_team_name[i]
  
  # B. Get Current ELOs (Handle missing teams if necessary)
  # If a team isn't in our list (e.g. non-D1), default to 1500 or skip
  h_elo <- if(home_team %in% names(ACC_current_elos)) ACC_current_elos[[home_team]] else 1500
  a_elo <- if(away_team %in% names(ACC_current_elos)) ACC_current_elos[[away_team]] else 1500

  # Store 'Before' ratings for reference
  ACC_remaining_schedule$home_elo_before[i] <- h_elo
  ACC_remaining_schedule$away_elo_before[i] <- a_elo
  
  # C. Calculate Win Prob (Current H_ELO + 100 vs Current A_ELO)
  h_prob <- elo.prob(h_elo + 100, a_elo)
  ACC_remaining_schedule$home_prob[i] <- h_prob
  
  # D. Simulate Outcome
  sim_val <- runif(1)
  ACC_remaining_schedule$sim_value[i] <- sim_val
  
  if(sim_val < h_prob) {
    # Home Wins
    ACC_remaining_schedule$predicted_winner[i] <- home_team
    actual_score <- 1
  } else {
    # Away Wins
    ACC_remaining_schedule$predicted_winner[i] <- away_team
    actual_score <- 0
  }

  # E. UPDATE ELOs!
  # Formula: New = Old + K * (Actual - Expected)
  # Note: The expected result for update excludes the 100 point HCA usually, 
  # or you can include it. Standard packages usually update based on the probability used.
  
  shift <- K * (actual_score - h_prob)
  
  # Apply updates to the lookup table
  if(home_team %in% names(A10_current_elos)) {
    ACC_current_elos[[home_team]] <- h_elo + shift
  }
  if(away_team %in% names(B1G_current_elos)) {
    ACC_current_elos[[away_team]] <- a_elo - shift
  }
}

# 5. View the final "Simulated" Standings
ACC_final_simulated_elos <- data.frame(
  team = names(ACC_current_elos),
  ACC_final_elo = as.numeric(ACC_current_elos)
) %>% arrange(desc(ACC_final_elo))

head(ACC_final_simulated_elos)
```

    ##             team ACC_final_elo
    ## 1           Duke      1785.187
    ## 2       Virginia      1733.744
    ## 3 North Carolina      1701.888
    ## 4          Miami      1684.345
    ## 5     Louisville      1632.234
    ## 6        Clemson      1628.619

``` r
head(ACC_remaining_schedule)
```

    ## ───────────────────────────────────────────────────────────────── hoopR 2.1.0 ──

    ## # A tibble: 6 × 11
    ##   away_team_name home_team_name        id game_date  home_elo away_elo home_prob
    ##   <chr>          <chr>              <int> <date>        <dbl>    <dbl>     <dbl>
    ## 1 California     Georgia Tech   401820780 2026-03-04    1427.    1625.     0.363
    ## 2 Florida State  Pittsburgh     401820783 2026-03-04    1463.    1566.     0.495
    ## 3 Miami          SMU            401820784 2026-03-04    1575.    1684.     0.486
    ## 4 Stanford       Notre Dame     401820782 2026-03-04    1478.    1579.     0.499
    ## 5 California     Wake Forest    401820795 2026-03-07    1537.    1625.     0.518
    ## 6 Georgia Tech   Clemson        401820787 2026-03-07    1629.    1427.     0.850
    ## # ℹ 4 more variables: sim_value <dbl>, predicted_winner <chr>,
    ## #   home_elo_before <dbl>, away_elo_before <dbl>

``` r
save(espn_mbb_pbp, temp_1, temp_2, games_1, games_2, mbb_2026, mbb_2025, df_elo_25, df_elo_26, conferences_26_clean, schedule_26, ACC_Conference, ACC_final_simulated_elos, ACC_remaining_schedule, A10_Conference, A10_final_simulated_elos, A10_remaining_schedule, B1G_Conference, B1G_final_simulated_elos, B1G_remaining_schedule, MAAC_Conference, MAAC_remaining_schedule, merged_mbb_clean,  file = "mbb_data_3.rda")
```

BUILD OUT THE TOURNAMENT FIELD WORK WITH MARTIN FOR MORE ACCURATE ELOs

``` r
 ranking_data <- data.frame(
  Rank = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25),
  Team = c(
    "Purdue", "Houston", "Florida", "UConn", "St. John's",
    "Duke", "Michigan", "BYU", "Kentucky", "Texas Tech",
    "Louisville", "UCLA", "Arizona", "Arkansas", "Alabama",
    "Iowa State", "Illinois", "Tennessee", "Kansas", "Auburn",
    "Gonzaga", "Michigan State", "Creighton", "Wisconsin", "North Carolina"
  ),
  Points = c(
    1485, 1459, 1382, 1299, 1203, 1123, 1084, 1078, 1056, 1015,
    966, 741, 715, 695, 620, 616, 567, 462, 453, 424,
    387, 188, 158, 136, 104
  )
)

# Print the dataframe to verify
print(ranking_data)
```

    ##    Rank           Team Points
    ## 1     1         Purdue   1485
    ## 2     2        Houston   1459
    ## 3     3        Florida   1382
    ## 4     4          UConn   1299
    ## 5     5     St. John's   1203
    ## 6     6           Duke   1123
    ## 7     7       Michigan   1084
    ## 8     8            BYU   1078
    ## 9     9       Kentucky   1056
    ## 10   10     Texas Tech   1015
    ## 11   11     Louisville    966
    ## 12   12           UCLA    741
    ## 13   13        Arizona    715
    ## 14   14       Arkansas    695
    ## 15   15        Alabama    620
    ## 16   16     Iowa State    616
    ## 17   17       Illinois    567
    ## 18   18      Tennessee    462
    ## 19   19         Kansas    453
    ## 20   20         Auburn    424
    ## 21   21        Gonzaga    387
    ## 22   22 Michigan State    188
    ## 23   23      Creighton    158
    ## 24   24      Wisconsin    136
    ## 25   25 North Carolina    104

``` r
season_games <- load_mbb_schedule(2026) %>%
  filter(status_type_completed == TRUE) 


elo_results <- elo.run(score(home_score, away_score) ~ home_location + away_location, 
                       data = season_games, 
                       k = 30)

final_elos <- as.data.frame(final.elos(elo_results))

team_vec <- final_elos
team_vec[,1] <- 1500

for(i  in 1:nrow(ranking_data)){
  team_vec[which(rownames(team_vec) == ranking_data$Team[i]),1] <- 1500 + ranking_data$Points[i]
}

team_vec[which(rownames(team_vec) %in% ranking_data$Team),]
```

    ##  [1] 2120 2215 2195 1924 2578 1658 2623 2882 1887 2959 2067 2116 1953 2556 2466
    ## [16] 2584 1688 1604 2985 2703 1962 2515 2241 2799 1636

``` r
team_vals <- team_vec[,1]
names(team_vals) <- rownames(team_vec)

elo_results <- elo.run(score(home_score, away_score) ~ home_location + away_location, 
                       data = season_games[2,], 
                       k = 30, initial.elos = team_vals)



final_elos <- as.data.frame(final.elos(elo_results))
colnames(final_elos) <- "projected_elo"

team_elo_res <- team_vals

k_vec <- rep(20, nrow(season_games))

k_vec[which(season_games$home_conference_id == season_games$away_conference_id)] <- 25


k_vec[which(season_games$home_conference_id == "3")] <- 20
k_vec[which(season_games$home_conference_id == "2")] <- 30
k_vec[which(season_games$home_conference_id == "1")] <- 15
k_vec[which(season_games$home_conference_id == "62")] <- 18
k_vec[which(season_games$home_conference_id == "46")] <- 15
k_vec[which(season_games$home_conference_id == "8")] <- 30
k_vec[which(season_games$home_conference_id == "4")] <- 25
k_vec[which(season_games$home_conference_id == "5")] <- 15
k_vec[which(season_games$home_conference_id == "6")] <- 15
k_vec[which(season_games$home_conference_id == "7")] <- 30
k_vec[which(season_games$home_conference_id == "9")] <- 18
k_vec[which(season_games$home_conference_id == "10")] <- 15
k_vec[which(season_games$home_conference_id == "11")] <- 15
k_vec[which(season_games$home_conference_id == "45")] <- 15
k_vec[which(season_games$home_conference_id == "12")] <- 15
k_vec[which(season_games$home_conference_id == "13")] <- 20
k_vec[which(season_games$home_conference_id == "14")] <- 18
k_vec[which(season_games$home_conference_id == "16")] <- 15
k_vec[which(season_games$home_conference_id == "44")] <- 20
k_vec[which(season_games$home_conference_id == "18")] <- 15
k_vec[which(season_games$home_conference_id == "19")] <- 15
k_vec[which(season_games$home_conference_id == "20")] <- 15
k_vec[which(season_games$home_conference_id == "23")] <- 30
k_vec[which(season_games$home_conference_id == "24")] <- 15
k_vec[which(season_games$home_conference_id == "25")] <- 18
k_vec[which(season_games$home_conference_id == "49")] <- 15
k_vec[which(season_games$home_conference_id == "27")] <- 18
k_vec[which(season_games$home_conference_id == "26")] <- 15
k_vec[which(season_games$home_conference_id == "30")] <- 15
k_vec[which(season_games$home_conference_id == "29")] <- 20

season_games <- as.data.frame(season_games)
for(i in 1:nrow(season_games)) {
  temp <- elo.run(score(home_score, away_score) ~ home_location + away_location, 
                       data = season_games[i,], 
                       k = k_vec[i], 
                       initial.elos = team_elo_res)
  t1 <- as.data.frame(temp)
  team_elo_res[which(names(team_elo_res) == t1$team.A)] <- t1$elo.A
  team_elo_res[which(names(team_elo_res) == t1$team.B)] <- t1$elo.B
  #final_elos <- as.data.frame(final.elos(temp))
  #team_elo_res <- final_elos[,1]
#names(team_elo_res) <- rownames(final_elos)
  
}

final_elos <- as.data.frame(team_elo_res)
projected_standings <- final_elos
projected_standings$team <- rownames(final_elos)  
rownames(projected_standings) <- NULL             


projected_standings <- projected_standings[order(projected_standings$team_elo_res, decreasing = TRUE), ]


head(projected_standings)
```

    ##     team_elo_res       team
    ## 244     2818.395    Houston
    ## 494     2784.483     Purdue
    ## 645     2761.967      UConn
    ## 192     2730.499    Florida
    ## 161     2623.716       Duke
    ## 582     2582.445 St. John's

``` r
top_64 <- projected_standings %>%
  arrange(desc(team_elo_res)) %>%
  head(64) %>%
  dplyr::rename(team_name = team, elo_rating = team_elo_res) %>%
  mutate(overall_rank = 1:64)


regions <- rep(c("East", "West", "South", "Midwest"), 16)

# Snake the draft for competitive balance
for(i in 1:16) {
  if(i %% 2 == 0) {
    indices <- ((i-1)*4 + 1):(i*4)
    regions[indices] <- rev(regions[indices])
  }
}

top_64$region <- regions
top_64$regional_seed <- rep(1:16, each = 4)


bracket_order <- c(1, 16, 8, 9, 5, 12, 4, 13, 6, 11, 3, 14, 7, 10, 2, 15)

tourney_field <- data.frame() 

# Loop through regions to sort teams into correct Bracket Order
for(r in unique(regions)) {
  # Get teams for this region
  region_teams <- top_64[top_64$region == r, ]
  
  # Sort them: 1 vs 16, 8 vs 9, etc.
  region_teams <- region_teams[match(bracket_order, region_teams$regional_seed), ]
  
  # Add to the final dataframe
  tourney_field <- rbind(tourney_field, region_teams)
}

print(head(tourney_field))
```

    ##    elo_rating   team_name overall_rank region regional_seed
    ## 1    2818.395     Houston            1   East             1
    ## 64   1613.629  New Mexico           64   East            16
    ## 32   1677.342       Miami           32   East             8
    ## 33   1675.557        Iowa           33   East             9
    ## 17   2061.171    Illinois           17   East             5
    ## 48   1637.251 Santa Clara           48   East            12

``` r
# 1. Grab all "Top" teams in the bracket (Rows 1, 3, 5, etc.)
team_A <- tourney_field[seq(1, 64, by = 2), ]

# 2. Grab all "Bottom" teams in the bracket (Rows 2, 4, 6, etc.)
team_B <- tourney_field[seq(2, 64, by = 2), ]

# 3. Combine them into a single Matchup Dataframe
r64_matchups <- data.frame(
  Region = team_A$region,
  Seed_A = team_A$regional_seed,
  Team_A = team_A$team_name,
  Elo_A  = team_A$elo_rating,
  
  # "VS" column just for readability
  VS = "vs",
  
  Seed_B = team_B$regional_seed,
  Team_B = team_B$team_name,
  Elo_B  = team_B$elo_rating
)

# 4. View the matchups to confirm logic (Check that 1 is playing 16!)
head(r64_matchups, 10)
```

    ##    Region Seed_A     Team_A    Elo_A VS Seed_B         Team_B    Elo_B
    ## 1    East      1    Houston 2818.395 vs     16     New Mexico 1613.629
    ## 2    East      8      Miami 1677.342 vs      9           Iowa 1675.557
    ## 3    East      5   Illinois 2061.171 vs     12    Santa Clara 1637.251
    ## 4    East      4       UCLA 2066.257 vs     13     Seton Hall 1637.013
    ## 5    East      6   Nebraska 1769.381 vs     11 Oklahoma State 1651.800
    ## 6    East      3        BYU 2374.853 vs     14       Colorado 1623.312
    ## 7    East      7 Vanderbilt 1741.338 vs     10     California 1653.858
    ## 8    East      2 Texas Tech 2412.862 vs     15        Belmont 1621.394
    ## 9    West      1     Purdue 2784.483 vs     16     Cincinnati 1614.230
    ## 10   West      8 Miami (OH) 1686.017 vs      9   Saint Mary's 1673.766

``` r
# --- 1. SETUP & INITIALIZATION ---

set.seed(123452033)
k <- 30

# Initialize columns to track progress through the tournament
# We assume 'tourney_field' is already sorted by matchup order (1v16, 8v9, etc.)
tourney_field$r64_game <- rep(1:32, each = 2) # Assign Game IDs 1-32
tourney_field$r32_game <- NA
tourney_field$r16_game <- NA
tourney_field$r8_game  <- NA
tourney_field$r4_game  <- NA
tourney_field$r2_game  <- NA
tourney_field$champion <- NA

# --- 2. ROUND OF 64 (32 Games) ---
cat("\n\n=== ROUND OF 64 ===\n")
```

    ## 
    ## 
    ## === ROUND OF 64 ===

``` r
for(i in 1:32){
  # Find the 2 teams playing in Game 'i'
  indices <- which(tourney_field$r64_game == i)
  match_teams <- tourney_field[indices, ]
  
  # Calculate Win Probability (No Home Court)
  prob <- elo.prob(match_teams$elo_rating[1], match_teams$elo_rating[2])
  sim <- runif(1)
  
  if(sim < prob) {
    # Team 1 Wins
    winner_idx <- indices[1]
    res <- 1
    cat(sprintf("Game %02d: %-20s def. %-20s\n", i, match_teams$team_name[1], match_teams$team_name[2]))
  } else {
    # Team 2 Wins (Upset?)
    winner_idx <- indices[2]
    res <- 0
    cat(sprintf("Game %02d: %-20s def. %-20s\n", i, match_teams$team_name[2], match_teams$team_name[1]))
  }
  
  # Update Elos
  new_elo <- elo.calc(wins.A = res, elo.A = match_teams$elo_rating[1], elo.B = match_teams$elo_rating[2], k = k)
  tourney_field$elo_rating[indices] <- as.numeric(new_elo[1, ])
  
  # Advance Winner to Next Round (Game ID maps to ceiling(i/2))
  tourney_field$r32_game[winner_idx] <- ceiling(i / 2)
}
```

    ## Game 01: Houston              def. New Mexico          
    ## Game 02: Miami                def. Iowa                
    ## Game 03: Illinois             def. Santa Clara         
    ## Game 04: UCLA                 def. Seton Hall          
    ## Game 05: Nebraska             def. Oklahoma State      
    ## Game 06: BYU                  def. Colorado            
    ## Game 07: Vanderbilt           def. California          
    ## Game 08: Texas Tech           def. Belmont             
    ## Game 09: Purdue               def. Cincinnati          
    ## Game 10: Saint Mary's         def. Miami (OH)          
    ## Game 11: Tennessee            def. Stephen F. Austin   
    ## Game 12: Alabama              def. Texas               
    ## Game 13: Indiana              def. North Carolina      
    ## Game 14: Arizona              def. Virginia Tech       
    ## Game 15: Wisconsin            def. SMU                 
    ## Game 16: Michigan             def. Yale                
    ## Game 17: UConn                def. George Mason        
    ## Game 18: Georgia              def. Missouri            
    ## Game 19: Kansas               def. Ohio State          
    ## Game 20: Iowa State           def. NC State            
    ## Game 21: Michigan State       def. Texas A&M           
    ## Game 22: Kentucky             def. Akron               
    ## Game 23: TCU                  def. Virginia            
    ## Game 24: St. John's           def. Creighton           
    ## Game 25: Florida              def. UNC Wilmington      
    ## Game 26: UCF                  def. Clemson             
    ## Game 27: Gonzaga              def. Utah State          
    ## Game 28: Arkansas             def. McNeese             
    ## Game 29: Auburn               def. USC                 
    ## Game 30: Louisville           def. Baylor              
    ## Game 31: Villanova            def. Saint Louis         
    ## Game 32: Duke                 def. High Point

``` r
# --- 3. ROUND OF 32 (16 Games) ---
cat("\n\n=== ROUND OF 32 ===\n")
```

    ## 
    ## 
    ## === ROUND OF 32 ===

``` r
for(i in 1:16){
  indices <- which(tourney_field$r32_game == i)
  match_teams <- tourney_field[indices, ]
  
  # Safety check: ensure 2 teams made it
  if(length(indices) == 2) {
    prob <- elo.prob(match_teams$elo_rating[1], match_teams$elo_rating[2])
    sim <- runif(1)
    
    if(sim < prob) {
      winner_idx <- indices[1]
      res <- 1
      cat(sprintf("Game %02d: %-20s def. %-20s\n", i, match_teams$team_name[1], match_teams$team_name[2]))
    } else {
      winner_idx <- indices[2]
      res <- 0
      cat(sprintf("Game %02d: %-20s def. %-20s\n", i, match_teams$team_name[2], match_teams$team_name[1]))
    }
    
    new_elo <- elo.calc(wins.A = res, elo.A = match_teams$elo_rating[1], elo.B = match_teams$elo_rating[2], k = k)
    tourney_field$elo_rating[indices] <- as.numeric(new_elo[1, ])
    
    tourney_field$r16_game[winner_idx] <- ceiling(i / 2)
  }
}
```

    ## Game 01: Houston              def. Miami               
    ## Game 02: UCLA                 def. Illinois            
    ## Game 03: BYU                  def. Nebraska            
    ## Game 04: Texas Tech           def. Vanderbilt          
    ## Game 05: Purdue               def. Saint Mary's        
    ## Game 06: Alabama              def. Tennessee           
    ## Game 07: Arizona              def. Indiana             
    ## Game 08: Michigan             def. Wisconsin           
    ## Game 09: UConn                def. Georgia             
    ## Game 10: Iowa State           def. Kansas              
    ## Game 11: Kentucky             def. Michigan State      
    ## Game 12: St. John's           def. TCU                 
    ## Game 13: Florida              def. UCF                 
    ## Game 14: Arkansas             def. Gonzaga             
    ## Game 15: Louisville           def. Auburn              
    ## Game 16: Duke                 def. Villanova

``` r
# --- 4. SWEET 16 (8 Games) ---
cat("\n\n=== SWEET 16 ===\n")
```

    ## 
    ## 
    ## === SWEET 16 ===

``` r
for(i in 1:8){
  indices <- which(tourney_field$r16_game == i)
  match_teams <- tourney_field[indices, ]
  
  if(length(indices) == 2) {
    prob <- elo.prob(match_teams$elo_rating[1], match_teams$elo_rating[2])
    sim <- runif(1)
    
    if(sim < prob) {
      winner_idx <- indices[1]
      res <- 1
      cat(sprintf("Game %d: %-20s def. %-20s\n", i, match_teams$team_name[1], match_teams$team_name[2]))
    } else {
      winner_idx <- indices[2]
      res <- 0
      cat(sprintf("Game %d: %-20s def. %-20s\n", i, match_teams$team_name[2], match_teams$team_name[1]))
    }
    
    new_elo <- elo.calc(wins.A = res, elo.A = match_teams$elo_rating[1], elo.B = match_teams$elo_rating[2], k = k)
    tourney_field$elo_rating[indices] <- as.numeric(new_elo[1, ])
    
    tourney_field$r8_game[winner_idx] <- ceiling(i / 2)
  }
}
```

    ## Game 1: Houston              def. UCLA                
    ## Game 2: Texas Tech           def. BYU                 
    ## Game 3: Purdue               def. Alabama             
    ## Game 4: Michigan             def. Arizona             
    ## Game 5: UConn                def. Iowa State          
    ## Game 6: St. John's           def. Kentucky            
    ## Game 7: Florida              def. Arkansas            
    ## Game 8: Duke                 def. Louisville

``` r
# --- 5. ELITE 8 (4 Games) ---
cat("\n\n=== ELITE 8 ===\n")
```

    ## 
    ## 
    ## === ELITE 8 ===

``` r
for(i in 1:4){
  indices <- which(tourney_field$r8_game == i)
  match_teams <- tourney_field[indices, ]
  
  if(length(indices) == 2) {
    prob <- elo.prob(match_teams$elo_rating[1], match_teams$elo_rating[2])
    sim <- runif(1)
    
    if(sim < prob) {
      winner_idx <- indices[1]
      res <- 1
      cat(sprintf("Region %d: %-20s def. %-20s\n", i, match_teams$team_name[1], match_teams$team_name[2]))
    } else {
      winner_idx <- indices[2]
      res <- 0
      cat(sprintf("Region %d: %-20s def. %-20s\n", i, match_teams$team_name[2], match_teams$team_name[1]))
    }
    
    new_elo <- elo.calc(wins.A = res, elo.A = match_teams$elo_rating[1], elo.B = match_teams$elo_rating[2], k = k)
    tourney_field$elo_rating[indices] <- as.numeric(new_elo[1, ])
    
    tourney_field$r4_game[winner_idx] <- ceiling(i / 2)
  }
}
```

    ## Region 1: Houston              def. Texas Tech          
    ## Region 2: Michigan             def. Purdue              
    ## Region 3: UConn                def. St. John's          
    ## Region 4: Florida              def. Duke

``` r
# --- 6. FINAL FOUR (2 Games) ---
cat("\n\n=== FINAL FOUR ===\n")
```

    ## 
    ## 
    ## === FINAL FOUR ===

``` r
for(i in 1:2){
  indices <- which(tourney_field$r4_game == i)
  match_teams <- tourney_field[indices, ]
  
  if(length(indices) == 2) {
    prob <- elo.prob(match_teams$elo_rating[1], match_teams$elo_rating[2])
    sim <- runif(1)
    
    if(sim < prob) {
      winner_idx <- indices[1]
      res <- 1
      cat(sprintf("Semi %d: %-20s def. %-20s\n", i, match_teams$team_name[1], match_teams$team_name[2]))
    } else {
      winner_idx <- indices[2]
      res <- 0
      cat(sprintf("Semi %d: %-20s def. %-20s\n", i, match_teams$team_name[2], match_teams$team_name[1]))
    }
    
    new_elo <- elo.calc(wins.A = res, elo.A = match_teams$elo_rating[1], elo.B = match_teams$elo_rating[2], k = k)
    tourney_field$elo_rating[indices] <- as.numeric(new_elo[1, ])
    
    tourney_field$r2_game[winner_idx] <- 1 # Both winners go to Game 1 (The Championship)
  }
}
```

    ## Semi 1: Houston              def. Michigan            
    ## Semi 2: UConn                def. Florida

``` r
# --- 7. NATIONAL CHAMPIONSHIP (1 Game) ---
cat("\n\n=== NATIONAL CHAMPIONSHIP ===\n")
```

    ## 
    ## 
    ## === NATIONAL CHAMPIONSHIP ===

``` r
indices <- which(tourney_field$r2_game == 1)
match_teams <- tourney_field[indices, ]

if(length(indices) == 2) {
  prob <- elo.prob(match_teams$elo_rating[1], match_teams$elo_rating[2])
  sim <- runif(1)
  
  if(sim < prob) {
    cat(sprintf("CHAMPION: %s\n", match_teams$team_name[1]))
    tourney_field$champion[indices[1]] <- 1
  } else {
    cat(sprintf("CHAMPION: %s\n", match_teams$team_name[2]))
    tourney_field$champion[indices[2]] <- 1
  }
}
```

    ## CHAMPION: Houston

``` r
# Set number of simulations
n_sims <- 10000

# Set seed for reproducibility
set.seed(1234520733)

# Create empty list to store results
results_list <- list()

# Define K-factor for dynamic Elo updates inside the tournament
k <- 30

# --- START SIMULATION LOOP ---
for(s in 1:n_sims){
  
  # 1. Reset team database to initial state using your qualified teams
  # We assume 'tourney_field' is already sorted by bracket matchup (1vs16, 8vs9, etc.)
  team_db_sim <- tourney_field
  
  # 2. Initialize columns for Game IDs
  # Round of 64: Assign games 1-32 based on row order
  team_db_sim$games_64 <- rep(1:32, each = 2) 
  
  # Future rounds: Initialize as NA
  team_db_sim$games_32 <- NA
  team_db_sim$games_16 <- NA
  team_db_sim$games_8  <- NA
  team_db_sim$games_4  <- NA
  team_db_sim$games_2  <- NA
  
  # --- ROUND 1: Round of 64 (32 Games) ---
  for(i in 1:32){
    match_teams <- team_db_sim[which(team_db_sim$games_64 == i),]
    
    if(nrow(match_teams) == 2) {
      prob <- elo.prob(match_teams$elo_rating[1], match_teams$elo_rating[2])
      sim_val <- runif(1)
      
      # Determine Winner (1 = Team 1 wins, 0 = Team 2 wins)
      res <- ifelse(sim_val < prob, 1, 0)
      
      # Update Elo Ratings
      new_elo <- elo.calc(res, match_teams$elo_rating[1], match_teams$elo_rating[2], k = k)
      team_db_sim$elo_rating[which(team_db_sim$games_64 == i)] <- as.numeric(new_elo[1, ])
      
      # Assign Winner to Next Round (Round of 32)
      # Logic: Game i feeds into Game ceiling(i/2)
      winner_idx <- which(team_db_sim$games_64 == i)[ifelse(res == 1, 1, 2)]
      team_db_sim$games_32[winner_idx] <- ceiling(i / 2)
    }
  }
  
  # --- ROUND 2: Round of 32 (16 Games) ---
  for(i in 1:16){
    match_teams <- team_db_sim[which(team_db_sim$games_32 == i),]
    
    if(nrow(match_teams) == 2) {
      prob <- elo.prob(match_teams$elo_rating[1], match_teams$elo_rating[2])
      sim_val <- runif(1)
      res <- ifelse(sim_val < prob, 1, 0)
      
      new_elo <- elo.calc(res, match_teams$elo_rating[1], match_teams$elo_rating[2], k = k)
      team_db_sim$elo_rating[which(team_db_sim$games_32 == i)] <- as.numeric(new_elo[1, ])
      
      winner_idx <- which(team_db_sim$games_32 == i)[ifelse(res == 1, 1, 2)]
      team_db_sim$games_16[winner_idx] <- ceiling(i / 2)
    }
  }
  
  # --- ROUND 3: Sweet 16 (8 Games) ---
  for(i in 1:8){
    match_teams <- team_db_sim[which(team_db_sim$games_16 == i),]
    
    if(nrow(match_teams) == 2) {
      prob <- elo.prob(match_teams$elo_rating[1], match_teams$elo_rating[2])
      sim_val <- runif(1)
      res <- ifelse(sim_val < prob, 1, 0)
      
      new_elo <- elo.calc(res, match_teams$elo_rating[1], match_teams$elo_rating[2], k = k)
      team_db_sim$elo_rating[which(team_db_sim$games_16 == i)] <- as.numeric(new_elo[1, ])
      
      winner_idx <- which(team_db_sim$games_16 == i)[ifelse(res == 1, 1, 2)]
      team_db_sim$games_8[winner_idx] <- ceiling(i / 2)
    }
  }

  # --- ROUND 4: Elite 8 (4 Games) ---
  for(i in 1:4){
    match_teams <- team_db_sim[which(team_db_sim$games_8 == i),]
    
    if(nrow(match_teams) == 2) {
      prob <- elo.prob(match_teams$elo_rating[1], match_teams$elo_rating[2])
      sim_val <- runif(1)
      res <- ifelse(sim_val < prob, 1, 0)
      
      new_elo <- elo.calc(res, match_teams$elo_rating[1], match_teams$elo_rating[2], k = k)
      team_db_sim$elo_rating[which(team_db_sim$games_8 == i)] <- as.numeric(new_elo[1, ])
      
      winner_idx <- which(team_db_sim$games_8 == i)[ifelse(res == 1, 1, 2)]
      team_db_sim$games_4[winner_idx] <- ceiling(i / 2)
    }
  }
  
  # --- ROUND 5: Final Four (2 Games) ---
  for(i in 1:2){
    match_teams <- team_db_sim[which(team_db_sim$games_4 == i),]
    
    if(nrow(match_teams) == 2) {
      prob <- elo.prob(match_teams$elo_rating[1], match_teams$elo_rating[2])
      sim_val <- runif(1)
      res <- ifelse(sim_val < prob, 1, 0)
      
      new_elo <- elo.calc(res, match_teams$elo_rating[1], match_teams$elo_rating[2], k = k)
      team_db_sim$elo_rating[which(team_db_sim$games_4 == i)] <- as.numeric(new_elo[1, ])
      
      winner_idx <- which(team_db_sim$games_4 == i)[ifelse(res == 1, 1, 2)]
      team_db_sim$games_2[winner_idx] <- 1
    }
  }
  
  # --- ROUND 6: Championship (1 Game) ---
  match_teams <- team_db_sim[which(team_db_sim$games_2 == 1),]
  if(nrow(match_teams) == 2) {
      prob <- elo.prob(match_teams$elo_rating[1], match_teams$elo_rating[2])
      sim_val <- runif(1)
      res <- ifelse(sim_val < prob, 1, 0)
      
      # Determine Champion
      winner_row <- match_teams[ifelse(res == 1, 1, 2), ]
      
      # Store result in list
      results_list[[s]] <- winner_row$team_name
  }
}


results_list_df <- data.frame(champion = unlist(results_list))


print(head(sort(table(results_list_df$champion), decreasing=TRUE), 10))
```

    ## 
    ##    Houston      UConn     Purdue    Florida       Duke   Michigan St. John's 
    ##       3659       2334       1789       1367        362        258        201 
    ## Texas Tech    Arizona        BYU 
    ##         20          4          4

``` r
#use the original 'tourney_field' to get the full list of team names

champ_counts <- rep(0, nrow(tourney_field))
names(champ_counts) <- tourney_field$team_name


for(i in 1:n_sims){
  
  champion <- results_list[[i]]
  

  if(length(champion) > 0){
    champ_counts[champion] <- champ_counts[champion] + 1
  }
}


champ_prob <- data.frame(
  team = names(champ_counts),
  wins = as.numeric(champ_counts),
  probability = as.numeric(champ_counts) / n_sims
)


champ_prob <- champ_prob[order(champ_prob$wins, decreasing = TRUE),]


head(champ_prob, 10)
```

    ##          team wins probability
    ## 1     Houston 3659      0.3659
    ## 33      UConn 2334      0.2334
    ## 17     Purdue 1789      0.1789
    ## 49    Florida 1367      0.1367
    ## 63       Duke  362      0.0362
    ## 31   Michigan  258      0.0258
    ## 47 St. John's  201      0.0201
    ## 15 Texas Tech   20      0.0020
    ## 11        BYU    4      0.0004
    ## 27    Arizona    4      0.0004

``` r
ggplot(head(champ_prob, 10), aes(x = reorder(team, probability), y = probability)) +
  geom_bar(stat = "identity", fill = "#CC5500") +
  coord_flip() +
  labs(
    title = "March Madness Championship Probabilities",
    subtitle = paste("Based on", n_sims, "simulations"),
    x = "Team",
    y = "Probability of Winning"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10)
  )
```

![](readme_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

``` r
save(espn_mbb_pbp, temp_1, temp_2, games_1, games_2, mbb_2026, mbb_2025, df_elo_25, df_elo_26, conferences_26_clean, schedule_26, ACC_Conference, ACC_final_simulated_elos, ACC_remaining_schedule, A10_Conference, A10_final_simulated_elos, A10_remaining_schedule, B1G_Conference, B1G_final_simulated_elos, B1G_remaining_schedule, MAAC_Conference, MAAC_remaining_schedule, merged_mbb_clean, r64_matchups, season_games, projected_standings, final_elos, ranking_data, tourney_field, team_db_sim, results_list_df, champ_counts, champ_prob, file = "mbb_data_4.rda")
```
