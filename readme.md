March Madness
================

``` r
library(hoopR)
library(elo)
library(gtools) 
library(ggplot2) # Load ggplot2
library(ggExtra) # Load ggExtra
library(dplyr) # Load dplyr
library(ggalluvial) # Load ggalluvial
library(tidyverse) # Load tidyverse
library(ggimage) # Load ggimage
library(GGally) # Load GGally
library(ggrepel) # Load ggrepel
source("~/Desktop/Mod 3/Mod3 Sport Analytics/Week 2/simple_dark_theme-2.R")
```


    ========================================
    Simple Dark Themes Loaded Successfully!
    ========================================

    Available functions:
      - dark_theme_bw()      : Dark version of theme_bw
      - dark_theme_gray()    : Dark version of theme_gray
      - dark_theme_grey()    : Alias for dark_theme_gray
      - dark_theme_minimal() : Dark version of theme_minimal

    Example usage:
      library(ggplot2)
      p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
      p + dark_theme_bw()

``` r
ls("package:hoopR") |> grep("espn_mbb_", x = _, value = TRUE)
```

     [1] "espn_mbb_betting"           "espn_mbb_conferences"      
     [3] "espn_mbb_game_all"          "espn_mbb_game_rosters"     
     [5] "espn_mbb_pbp"               "espn_mbb_player_box"       
     [7] "espn_mbb_player_stats"      "espn_mbb_rankings"         
     [9] "espn_mbb_scoreboard"        "espn_mbb_standings"        
    [11] "espn_mbb_team_box"          "espn_mbb_team_stats"       
    [13] "espn_mbb_teams"             "espn_mbb_wp"               
    [15] "helper_espn_mbb_pbp"        "helper_espn_mbb_player_box"
    [17] "helper_espn_mbb_team_box"  

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

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      1.000   1.000   2.000   1.537   2.000   6.000 

``` r
save(player_box_2k25, rankings, standings_2k25, team_box_2k25, mbb_schedule_25, mbb_2025, file = "mbb_data.rda")
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

Get the ELOs from 2025 to use as a baseline to make ELO ratings for the
preseason of 2026 and build out the 2026 regular season

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

Complete the same process for 2026

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

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
       1.00    1.00    2.00    1.53    2.00    5.00 

``` r
df_elo_26 <- as.data.frame(final.elos(elo_2026))
df_elo_25 <- as.data.frame(final.elos(elo_2025))
```

Keep for later to look at teams for graphing

``` r
teams_26 <- espn_mbb_teams(2026) 
head(teams_26)
```

    # A tibble: 6 × 23
      team_id abbreviation display_name       short_name mascot nickname team  color
        <int> <chr>        <chr>              <chr>      <chr>  <chr>    <chr> <chr>
    1    2000 ACU          Abilene Christian… Abilene C… Wildc… Abilene… Abil… 592d…
    2    2005 AF           Air Force Falcons  Air Force  Falco… Air For… Air … 0035…
    3    2006 AKR          Akron Zips         Akron      Zips   Akron    Akron 0028…
    4    2010 AAMU         Alabama A&M Bulld… Alabama A… Bulld… Alabama… Alab… 7900…
    5     333 ALA          Alabama Crimson T… Alabama    Crims… Alabama  Alab… 9e16…
    6    2011 ALST         Alabama State Hor… Alabama St Horne… Alabama… Alab… e9a9…
    # ℹ 15 more variables: alternate_color <chr>, logo <chr>, logo_dark <chr>,
    #   logos_href_3 <chr>, logos_href_4 <chr>, logos_href_5 <chr>,
    #   logos_href_6 <chr>, logos_href_7 <chr>, logos_href_8 <chr>,
    #   logos_href_9 <chr>, logos_href_10 <chr>, logos_href_11 <chr>,
    #   logos_href_12 <chr>, logos_href_13 <chr>, logos_href_14 <chr>

Get unique column value for display names, upload it to a CSV and then
use AI to populate with conferences, then load back in.

``` r
unique_teams <- unique(teams_26$display_name)
unique_teams
```

      [1] "Abilene Christian Wildcats"           
      [2] "Air Force Falcons"                    
      [3] "Akron Zips"                           
      [4] "Alabama A&M Bulldogs"                 
      [5] "Alabama Crimson Tide"                 
      [6] "Alabama State Hornets"                
      [7] "Alcorn State Braves"                  
      [8] "American University Eagles"           
      [9] "App State Mountaineers"               
     [10] "Arizona State Sun Devils"             
     [11] "Arizona Wildcats"                     
     [12] "Arkansas Razorbacks"                  
     [13] "Arkansas State Red Wolves"            
     [14] "Arkansas-Pine Bluff Golden Lions"     
     [15] "Army Black Knights"                   
     [16] "Auburn Tigers"                        
     [17] "Austin Peay Governors"                
     [18] "BYU Cougars"                          
     [19] "Ball State Cardinals"                 
     [20] "Baylor Bears"                         
     [21] "Bellarmine Knights"                   
     [22] "Belmont Bruins"                       
     [23] "Bethune-Cookman Wildcats"             
     [24] "Binghamton Bearcats"                  
     [25] "Boise State Broncos"                  
     [26] "Boston College Eagles"                
     [27] "Boston University Terriers"           
     [28] "Bowling Green Falcons"                
     [29] "Bradley Braves"                       
     [30] "Brown Bears"                          
     [31] "Bryant Bulldogs"                      
     [32] "Bucknell Bison"                       
     [33] "Buffalo Bulls"                        
     [34] "Butler Bulldogs"                      
     [35] "Cal Poly Mustangs"                    
     [36] "Cal State Bakersfield Roadrunners"    
     [37] "Cal State Fullerton Titans"           
     [38] "Cal State Northridge Matadors"        
     [39] "California Baptist Lancers"           
     [40] "California Golden Bears"              
     [41] "Campbell Fighting Camels"             
     [42] "Canisius Golden Griffins"             
     [43] "Central Arkansas Bears"               
     [44] "Central Connecticut Blue Devils"      
     [45] "Central Michigan Chippewas"           
     [46] "Charleston Cougars"                   
     [47] "Charleston Southern Buccaneers"       
     [48] "Charlotte 49ers"                      
     [49] "Chattanooga Mocs"                     
     [50] "Chicago State Cougars"                
     [51] "Cincinnati Bearcats"                  
     [52] "Clemson Tigers"                       
     [53] "Cleveland State Vikings"              
     [54] "Coastal Carolina Chanticleers"        
     [55] "Colgate Raiders"                      
     [56] "Colorado Buffaloes"                   
     [57] "Colorado State Rams"                  
     [58] "Columbia Lions"                       
     [59] "Coppin State Eagles"                  
     [60] "Cornell Big Red"                      
     [61] "Creighton Bluejays"                   
     [62] "Dartmouth Big Green"                  
     [63] "Davidson Wildcats"                    
     [64] "Dayton Flyers"                        
     [65] "DePaul Blue Demons"                   
     [66] "Delaware Blue Hens"                   
     [67] "Delaware State Hornets"               
     [68] "Denver Pioneers"                      
     [69] "Detroit Mercy Titans"                 
     [70] "Drake Bulldogs"                       
     [71] "Drexel Dragons"                       
     [72] "Duke Blue Devils"                     
     [73] "Duquesne Dukes"                       
     [74] "East Carolina Pirates"                
     [75] "East Tennessee State Buccaneers"      
     [76] "East Texas A&M Lions"                 
     [77] "Eastern Illinois Panthers"            
     [78] "Eastern Kentucky Colonels"            
     [79] "Eastern Michigan Eagles"              
     [80] "Eastern Washington Eagles"            
     [81] "Elon Phoenix"                         
     [82] "Evansville Purple Aces"               
     [83] "Fairfield Stags"                      
     [84] "Fairleigh Dickinson Knights"          
     [85] "Florida A&M Rattlers"                 
     [86] "Florida Atlantic Owls"                
     [87] "Florida Gators"                       
     [88] "Florida Gulf Coast Eagles"            
     [89] "Florida International Panthers"       
     [90] "Florida State Seminoles"              
     [91] "Fordham Rams"                         
     [92] "Fresno State Bulldogs"                
     [93] "Furman Paladins"                      
     [94] "Gardner-Webb Runnin' Bulldogs"        
     [95] "George Mason Patriots"                
     [96] "George Washington Revolutionaries"    
     [97] "Georgetown Hoyas"                     
     [98] "Georgia Bulldogs"                     
     [99] "Georgia Southern Eagles"              
    [100] "Georgia State Panthers"               
    [101] "Georgia Tech Yellow Jackets"          
    [102] "Gonzaga Bulldogs"                     
    [103] "Grambling Tigers"                     
    [104] "Grand Canyon Lopes"                   
    [105] "Green Bay Phoenix"                    
    [106] "Hampton Pirates"                      
    [107] "Harvard Crimson"                      
    [108] "Hawai'i Rainbow Warriors"             
    [109] "High Point Panthers"                  
    [110] "Hofstra Pride"                        
    [111] "Holy Cross Crusaders"                 
    [112] "Houston Christian Huskies"            
    [113] "Houston Cougars"                      
    [114] "Howard Bison"                         
    [115] "IU Indianapolis Jaguars"              
    [116] "Idaho State Bengals"                  
    [117] "Idaho Vandals"                        
    [118] "Illinois Fighting Illini"             
    [119] "Illinois State Redbirds"              
    [120] "Incarnate Word Cardinals"             
    [121] "Indiana Hoosiers"                     
    [122] "Indiana State Sycamores"              
    [123] "Iona Gaels"                           
    [124] "Iowa Hawkeyes"                        
    [125] "Iowa State Cyclones"                  
    [126] "Jackson State Tigers"                 
    [127] "Jacksonville Dolphins"                
    [128] "Jacksonville State Gamecocks"         
    [129] "James Madison Dukes"                  
    [130] "Kansas City Roos"                     
    [131] "Kansas Jayhawks"                      
    [132] "Kansas State Wildcats"                
    [133] "Kennesaw State Owls"                  
    [134] "Kent State Golden Flashes"            
    [135] "Kentucky Wildcats"                    
    [136] "LSU Tigers"                           
    [137] "La Salle Explorers"                   
    [138] "Lafayette Leopards"                   
    [139] "Lamar Cardinals"                      
    [140] "Le Moyne Dolphins"                    
    [141] "Lehigh Mountain Hawks"                
    [142] "Liberty Flames"                       
    [143] "Lipscomb Bisons"                      
    [144] "Little Rock Trojans"                  
    [145] "Long Beach State Beach"               
    [146] "Long Island University Sharks"        
    [147] "Longwood Lancers"                     
    [148] "Louisiana Ragin' Cajuns"              
    [149] "Louisiana Tech Bulldogs"              
    [150] "Louisville Cardinals"                 
    [151] "Loyola Chicago Ramblers"              
    [152] "Loyola Maryland Greyhounds"           
    [153] "Loyola Marymount Lions"               
    [154] "Maine Black Bears"                    
    [155] "Manhattan Jaspers"                    
    [156] "Marist Red Foxes"                     
    [157] "Marquette Golden Eagles"              
    [158] "Marshall Thundering Herd"             
    [159] "Maryland Eastern Shore Hawks"         
    [160] "Maryland Terrapins"                   
    [161] "Massachusetts Minutemen"              
    [162] "McNeese Cowboys"                      
    [163] "Memphis Tigers"                       
    [164] "Mercer Bears"                         
    [165] "Mercyhurst Lakers"                    
    [166] "Merrimack Warriors"                   
    [167] "Miami (OH) RedHawks"                  
    [168] "Miami Hurricanes"                     
    [169] "Michigan State Spartans"              
    [170] "Michigan Wolverines"                  
    [171] "Middle Tennessee Blue Raiders"        
    [172] "Milwaukee Panthers"                   
    [173] "Minnesota Golden Gophers"             
    [174] "Mississippi State Bulldogs"           
    [175] "Mississippi Valley State Delta Devils"
    [176] "Missouri State Bears"                 
    [177] "Missouri Tigers"                      
    [178] "Monmouth Hawks"                       
    [179] "Montana Grizzlies"                    
    [180] "Montana State Bobcats"                
    [181] "Morehead State Eagles"                
    [182] "Morgan State Bears"                   
    [183] "Mount St. Mary's Mountaineers"        
    [184] "Murray State Racers"                  
    [185] "NC State Wolfpack"                    
    [186] "NJIT Highlanders"                     
    [187] "Navy Midshipmen"                      
    [188] "Nebraska Cornhuskers"                 
    [189] "Nevada Wolf Pack"                     
    [190] "New Hampshire Wildcats"               
    [191] "New Haven Chargers"                   
    [192] "New Mexico Lobos"                     
    [193] "New Mexico State Aggies"              
    [194] "New Orleans Privateers"               
    [195] "Niagara Purple Eagles"                
    [196] "Nicholls Colonels"                    
    [197] "Norfolk State Spartans"               
    [198] "North Alabama Lions"                  
    [199] "North Carolina A&T Aggies"            
    [200] "North Carolina Central Eagles"        
    [201] "North Carolina Tar Heels"             
    [202] "North Dakota Fighting Hawks"          
    [203] "North Dakota State Bison"             
    [204] "North Florida Ospreys"                
    [205] "North Texas Mean Green"               
    [206] "Northeastern Huskies"                 
    [207] "Northern Arizona Lumberjacks"         
    [208] "Northern Colorado Bears"              
    [209] "Northern Illinois Huskies"            
    [210] "Northern Iowa Panthers"               
    [211] "Northern Kentucky Norse"              
    [212] "Northwestern State Demons"            
    [213] "Northwestern Wildcats"                
    [214] "Notre Dame Fighting Irish"            
    [215] "Oakland Golden Grizzlies"             
    [216] "Ohio Bobcats"                         
    [217] "Ohio State Buckeyes"                  
    [218] "Oklahoma Sooners"                     
    [219] "Oklahoma State Cowboys"               
    [220] "Old Dominion Monarchs"                
    [221] "Ole Miss Rebels"                      
    [222] "Omaha Mavericks"                      
    [223] "Oral Roberts Golden Eagles"           
    [224] "Oregon Ducks"                         
    [225] "Oregon State Beavers"                 
    [226] "Pacific Tigers"                       
    [227] "Penn State Nittany Lions"             
    [228] "Pennsylvania Quakers"                 
    [229] "Pepperdine Waves"                     
    [230] "Pittsburgh Panthers"                  
    [231] "Portland Pilots"                      
    [232] "Portland State Vikings"               
    [233] "Prairie View A&M Panthers"            
    [234] "Presbyterian Blue Hose"               
    [235] "Princeton Tigers"                     
    [236] "Providence Friars"                    
    [237] "Purdue Boilermakers"                  
    [238] "Purdue Fort Wayne Mastodons"          
    [239] "Quinnipiac Bobcats"                   
    [240] "Radford Highlanders"                  
    [241] "Rhode Island Rams"                    
    [242] "Rice Owls"                            
    [243] "Richmond Spiders"                     
    [244] "Rider Broncs"                         
    [245] "Robert Morris Colonials"              
    [246] "Rutgers Scarlet Knights"              
    [247] "SE Louisiana Lions"                   
    [248] "SIU Edwardsville Cougars"             
    [249] "SMU Mustangs"                         
    [250] "Sacramento State Hornets"             
    [251] "Sacred Heart Pioneers"                
    [252] "Saint Francis Red Flash"              
    [253] "Saint Joseph's Hawks"                 
    [254] "Saint Louis Billikens"                
    [255] "Saint Mary's Gaels"                   
    [256] "Saint Peter's Peacocks"               
    [257] "Sam Houston Bearkats"                 
    [258] "Samford Bulldogs"                     
    [259] "San Diego State Aztecs"               
    [260] "San Diego Toreros"                    
    [261] "San Francisco Dons"                   
    [262] "San José State Spartans"              
    [263] "Santa Clara Broncos"                  
    [264] "Seattle U Redhawks"                   
    [265] "Seton Hall Pirates"                   
    [266] "Siena Saints"                         
    [267] "South Alabama Jaguars"                
    [268] "South Carolina Gamecocks"             
    [269] "South Carolina State Bulldogs"        
    [270] "South Carolina Upstate Spartans"      
    [271] "South Dakota Coyotes"                 
    [272] "South Dakota State Jackrabbits"       
    [273] "South Florida Bulls"                  
    [274] "Southeast Missouri State Redhawks"    
    [275] "Southern Illinois Salukis"            
    [276] "Southern Jaguars"                     
    [277] "Southern Miss Golden Eagles"          
    [278] "Southern Utah Thunderbirds"           
    [279] "St. Bonaventure Bonnies"              
    [280] "St. John's Red Storm"                 
    [281] "St. Thomas-Minnesota Tommies"         
    [282] "Stanford Cardinal"                    
    [283] "Stephen F. Austin Lumberjacks"        
    [284] "Stetson Hatters"                      
    [285] "Stonehill Skyhawks"                   
    [286] "Stony Brook Seawolves"                
    [287] "Syracuse Orange"                      
    [288] "TCU Horned Frogs"                     
    [289] "Tarleton State Texans"                
    [290] "Temple Owls"                          
    [291] "Tennessee State Tigers"               
    [292] "Tennessee Tech Golden Eagles"         
    [293] "Tennessee Volunteers"                 
    [294] "Texas A&M Aggies"                     
    [295] "Texas A&M-Corpus Christi Islanders"   
    [296] "Texas Longhorns"                      
    [297] "Texas Southern Tigers"                
    [298] "Texas State Bobcats"                  
    [299] "Texas Tech Red Raiders"               
    [300] "The Citadel Bulldogs"                 
    [301] "Toledo Rockets"                       
    [302] "Towson Tigers"                        
    [303] "Troy Trojans"                         
    [304] "Tulane Green Wave"                    
    [305] "Tulsa Golden Hurricane"               
    [306] "UAB Blazers"                          
    [307] "UAlbany Great Danes"                  
    [308] "UC Davis Aggies"                      
    [309] "UC Irvine Anteaters"                  
    [310] "UC Riverside Highlanders"             
    [311] "UC San Diego Tritons"                 
    [312] "UC Santa Barbara Gauchos"             
    [313] "UCF Knights"                          
    [314] "UCLA Bruins"                          
    [315] "UConn Huskies"                        
    [316] "UIC Flames"                           
    [317] "UL Monroe Warhawks"                   
    [318] "UMBC Retrievers"                      
    [319] "UMass Lowell River Hawks"             
    [320] "UNC Asheville Bulldogs"               
    [321] "UNC Greensboro Spartans"              
    [322] "UNC Wilmington Seahawks"              
    [323] "UNLV Rebels"                          
    [324] "USC Trojans"                          
    [325] "UT Arlington Mavericks"               
    [326] "UT Martin Skyhawks"                   
    [327] "UT Rio Grande Valley Vaqueros"        
    [328] "UTEP Miners"                          
    [329] "UTSA Roadrunners"                     
    [330] "Utah State Aggies"                    
    [331] "Utah Tech Trailblazers"               
    [332] "Utah Utes"                            
    [333] "Utah Valley Wolverines"               
    [334] "VCU Rams"                             
    [335] "VMI Keydets"                          
    [336] "Valparaiso Beacons"                   
    [337] "Vanderbilt Commodores"                
    [338] "Vermont Catamounts"                   
    [339] "Villanova Wildcats"                   
    [340] "Virginia Cavaliers"                   
    [341] "Virginia Tech Hokies"                 
    [342] "Wagner Seahawks"                      
    [343] "Wake Forest Demon Deacons"            
    [344] "Washington Huskies"                   
    [345] "Washington State Cougars"             
    [346] "Weber State Wildcats"                 
    [347] "West Georgia Wolves"                  
    [348] "West Virginia Mountaineers"           
    [349] "Western Carolina Catamounts"          
    [350] "Western Illinois Leathernecks"        
    [351] "Western Kentucky Hilltoppers"         
    [352] "Western Michigan Broncos"             
    [353] "Wichita State Shockers"               
    [354] "William & Mary Tribe"                 
    [355] "Winthrop Eagles"                      
    [356] "Wisconsin Badgers"                    
    [357] "Wofford Terriers"                     
    [358] "Wright State Raiders"                 
    [359] "Wyoming Cowboys"                      
    [360] "Xavier Musketeers"                    
    [361] "Yale Bulldogs"                        
    [362] "Youngstown State Penguins"            

``` r
library(readr)
write_csv(teams_26, "teams_2026.csv")

conferences_26 <- read_csv('~/Desktop/Mod 3/Mod3 Sport Analytics/Sports Analytics Proj./teams_2026_final.csv')
conferences_26
```

    # A tibble: 362 × 24
       team_id abbreviation display_name      short_name mascot nickname team  color
         <dbl> <chr>        <chr>             <chr>      <chr>  <chr>    <chr> <chr>
     1    2000 ACU          Abilene Christia… Abilene C… Wildc… Abilene… Abil… 592d…
     2    2005 AF           Air Force Falcons Air Force  Falco… Air For… Air … 0035…
     3    2006 AKR          Akron Zips        Akron      Zips   Akron    Akron 0028…
     4    2010 AAMU         Alabama A&M Bull… Alabama A… Bulld… Alabama… Alab… 7900…
     5     333 ALA          Alabama Crimson … Alabama    Crims… Alabama  Alab… 9e16…
     6    2011 ALST         Alabama State Ho… Alabama St Horne… Alabama… Alab… e9a9…
     7    2016 ALCN         Alcorn State Bra… Alcorn St  Braves Alcorn … Alco… 4b00…
     8      44 AMER         American Univers… American   Eagles American Amer… c411…
     9    2026 APP          App State Mounta… App State  Mount… App Sta… App … 0000…
    10       9 ASU          Arizona State Su… Arizona St Sun D… Arizona… Ariz… 8e0c…
    # ℹ 352 more rows
    # ℹ 16 more variables: alternate_color <chr>, logo <chr>, logo_dark <chr>,
    #   logos_href_3 <chr>, logos_href_4 <chr>, logos_href_5 <chr>,
    #   logos_href_6 <chr>, logos_href_7 <chr>, logos_href_8 <chr>,
    #   logos_href_9 <chr>, logos_href_10 <chr>, logos_href_11 <chr>,
    #   logos_href_12 <chr>, logos_href_13 <chr>, logos_href_14 <chr>,
    #   conference_2025_26 <chr>

``` r
load_mbb_schedule(2026)
```

    # A tibble: 6,333 × 86
            id uid   date  attendance time_valid neutral_site conference_competition
         <int> <chr> <chr>      <dbl> <lgl>      <lgl>        <lgl>                 
     1  4.02e8 s:40… 2026…          0 FALSE      TRUE         FALSE                 
     2  4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
     3  4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
     4  4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
     5  4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
     6  4.02e8 s:40… 2026…          0 FALSE      TRUE         FALSE                 
     7  4.02e8 s:40… 2026…          0 FALSE      TRUE         FALSE                 
     8  4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
     9  4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
    10  4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
    # ℹ 6,323 more rows
    # ℹ 79 more variables: play_by_play_available <lgl>, recent <lgl>,
    #   start_date <chr>, broadcast <chr>, highlights <chr>, notes_type <chr>,
    #   notes_headline <chr>, broadcast_market <chr>, broadcast_name <chr>,
    #   type_id <int>, type_abbreviation <chr>, venue_id <int>,
    #   venue_full_name <chr>, venue_address_city <chr>, venue_address_state <chr>,
    #   venue_indoor <lgl>, status_clock <dbl>, status_display_clock <chr>, …

``` r
schedule_26 <- load_mbb_schedule(2026)
head(schedule_26)
```

    # A tibble: 6 × 86
            id uid   date  attendance time_valid neutral_site conference_competition
         <int> <chr> <chr>      <dbl> <lgl>      <lgl>        <lgl>                 
    1   4.02e8 s:40… 2026…          0 FALSE      TRUE         FALSE                 
    2   4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
    3   4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
    4   4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
    5   4.02e8 s:40… 2026…          0 TRUE       TRUE         FALSE                 
    6   4.02e8 s:40… 2026…          0 FALSE      TRUE         FALSE                 
    # ℹ 79 more variables: play_by_play_available <lgl>, recent <lgl>,
    #   start_date <chr>, broadcast <chr>, highlights <chr>, notes_type <chr>,
    #   notes_headline <chr>, broadcast_market <chr>, broadcast_name <chr>,
    #   type_id <int>, type_abbreviation <chr>, venue_id <int>,
    #   venue_full_name <chr>, venue_address_city <chr>, venue_address_state <chr>,
    #   venue_indoor <lgl>, status_clock <dbl>, status_display_clock <chr>,
    #   status_period <dbl>, status_type_id <int>, status_type_name <chr>, …

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

![](readme_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

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

![](readme_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

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

![](readme_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

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

![](readme_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

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

![](readme_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

WORK WITH MAAC CONFERENCE Testing out probability of win Creating a
remaining schedule for the MAAC teams based on the Schedule_26 DataFrame
(If this goes well I will build things out for the other conferences I
am interested in)

``` r
# Win Probability of Siena against Saint Peter's example
MAAC_Conference[MAAC_Conference$team_name %in% c("Siena", "Saint Peter's"),]
```

           team_name elo_rating team_id abbreviation   mascot  color
    12 Saint Peter's   1580.856    2612          SPU Peacocks 004CC2
    13         Siena   1581.605    2561          SIE   Saints 037961
       alternate_color                                                logo
    12            <NA> https://a.espncdn.com/i/teamlogos/ncaa/500/2612.png
    13          eea60f https://a.espncdn.com/i/teamlogos/ncaa/500/2561.png
                                                      logo_dark conference_2025_26
    12 https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2612.png               MAAC
    13 https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2561.png               MAAC

``` r
elo.prob(MAAC_Conference$elo_rating[MAAC_Conference$team_name == "Siena"],
         MAAC_Conference$elo_rating[MAAC_Conference$team_name == "Saint Peter's"])
```

    [1] 0.5010771

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

    [1] "Siena win probability is: 0.5011"

``` r
print(paste("The simulated value is:", round(sim, 4), sep = " "))
```

    [1] "The simulated value is: 0.168"

``` r
if(sim < win_prob){
  print(paste("Siena Wins the Game"))
} else{
   print(paste("Saint Peter's Wins the Game"))
}
```

    [1] "Siena Wins the Game"

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

    # A tibble: 6 × 4
             id game_date  home_team_name away_team_name  
          <int> <date>     <chr>          <chr>           
    1 401851464 2026-03-07 Siena          Mount St. Mary's
    2 401851462 2026-03-07 Quinnipiac     Marist          
    3 401851460 2026-03-06 Saint Peter's  TBD             
    4 401851457 2026-03-06 Merrimack      TBD             
    5 401851456 2026-03-05 Fairfield      Manhattan       
    6 401851454 2026-03-05 Iona           Sacred Heart    

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

    # A tibble: 6 × 5
      home_team_name        id game_date  away_team_name   home_elo
      <chr>              <int> <date>     <chr>               <dbl>
    1 Fairfield      401851456 2026-03-05 Manhattan           1566.
    2 Iona           401851454 2026-03-05 Sacred Heart        1519.
    3 Merrimack      401851457 2026-03-06 TBD                 1596.
    4 Quinnipiac     401851462 2026-03-07 Marist              1545.
    5 Saint Peter's  401851460 2026-03-06 TBD                 1581.
    6 Siena          401851464 2026-03-07 Mount St. Mary's    1582.

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

    # A tibble: 6 × 6
      away_team_name   home_team_name        id game_date  home_elo away_elo
      <chr>            <chr>              <int> <date>        <dbl>    <dbl>
    1 Manhattan        Fairfield      401851456 2026-03-05    1566.    1446.
    2 Marist           Quinnipiac     401851462 2026-03-07    1545.    1537.
    3 Mount St. Mary's Siena          401851464 2026-03-07    1582.    1523.
    4 Sacred Heart     Iona           401851454 2026-03-05    1519.    1473.
    5 TBD              Merrimack      401851457 2026-03-06    1596.      NA 
    6 TBD              Saint Peter's  401851460 2026-03-06    1581.      NA 

Getting the win probability and then adding some points to the elo for
home court advantage

``` r
MAAC_remaining_schedule$home_prob <- elo.prob(
  elo.A = MAAC_remaining_schedule$home_elo + 100, 
  elo.B = MAAC_remaining_schedule$away_elo
)
```

Running code similar to what we did for the elo_world_cup.rmd in Sports
Analytics with Martin

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

    # A tibble: 6 × 9
      away_team_name   home_team_name      id game_date  home_elo away_elo home_prob
      <chr>            <chr>            <int> <date>        <dbl>    <dbl>     <dbl>
    1 Manhattan        Fairfield       4.02e8 2026-03-05    1566.    1446.     0.780
    2 Marist           Quinnipiac      4.02e8 2026-03-07    1545.    1537.     0.651
    3 Mount St. Mary's Siena           4.02e8 2026-03-07    1582.    1523.     0.713
    4 Sacred Heart     Iona            4.02e8 2026-03-05    1519.    1473.     0.698
    5 TBD              Merrimack       4.02e8 2026-03-06    1596.      NA     NA    
    6 TBD              Saint Peter's   4.02e8 2026-03-06    1581.      NA     NA    
    # ℹ 2 more variables: sim_value <dbl>, predicted_winner <chr>

Here I am looking at the predicted winners of the remaining MAAC
Schedule and how many games these teams are going to win.

``` r
table(MAAC_remaining_schedule$predicted_winner)
```


       Fairfield   Quinnipiac Sacred Heart        Siena 
               1            1            1            1 

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

Predict the winning games of the remaining schedule in the regular
season (Games remaining will go down the more that the season gets
towards the end and get ready for the conference tourneys)

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

               team final_elo
    1     Merrimack  1603.535
    2         Siena  1590.205
    3     Fairfield  1572.913
    4 Saint Peter's  1558.684
    5        Marist  1556.566
    6          Iona  1528.001

``` r
head(MAAC_remaining_schedule)
```

    # A tibble: 6 × 11
      away_team_name   home_team_name      id game_date  home_elo away_elo home_prob
      <chr>            <chr>            <int> <date>        <dbl>    <dbl>     <dbl>
    1 Manhattan        Fairfield       4.02e8 2026-03-05    1566.    1446.     0.780
    2 Sacred Heart     Iona            4.02e8 2026-03-05    1519.    1473.     0.698
    3 TBD              Merrimack       4.02e8 2026-03-06    1596.      NA      0.756
    4 TBD              Saint Peter's   4.02e8 2026-03-06    1581.      NA      0.739
    5 Marist           Quinnipiac      4.02e8 2026-03-07    1545.    1537.     0.651
    6 Mount St. Mary's Siena           4.02e8 2026-03-07    1582.    1523.     0.713
    # ℹ 4 more variables: sim_value <dbl>, predicted_winner <chr>,
    #   home_elo_before <dbl>, away_elo_before <dbl>

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

    # A tibble: 6 × 5
      home_team_name        id game_date  away_team_name    home_elo
      <chr>              <int> <date>     <chr>                <dbl>
    1 Dayton         401828462 2026-03-06 VCU                  1646.
    2 Duquesne       401828464 2026-03-07 Richmond             1527.
    3 Fordham        401828465 2026-03-07 Rhode Island         1517.
    4 George Mason   401828466 2026-03-07 Saint Louis          1596.
    5 Loyola Chicago 401828467 2026-03-07 George Washington    1389.
    6 Saint Joseph's 401828468 2026-03-07 La Salle             1615.

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

    # A tibble: 7 × 6
      away_team_name    home_team_name         id game_date  home_elo away_elo
      <chr>             <chr>               <int> <date>        <dbl>    <dbl>
    1 Davidson          St. Bonaventure 401828463 2026-03-07    1503.    1568.
    2 George Washington Loyola Chicago  401828467 2026-03-07    1389.    1533.
    3 La Salle          Saint Joseph's  401828468 2026-03-07    1615.    1430.
    4 Rhode Island      Fordham         401828465 2026-03-07    1517.    1523.
    5 Richmond          Duquesne        401828464 2026-03-07    1527.    1477.
    6 Saint Louis       George Mason    401828466 2026-03-07    1596.    1719.
    7 VCU               Dayton          401828462 2026-03-06    1646.    1685.

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

    # A tibble: 6 × 9
      away_team_name    home_team_name     id game_date  home_elo away_elo home_prob
      <chr>             <chr>           <int> <date>        <dbl>    <dbl>     <dbl>
    1 Davidson          St. Bonaventu… 4.02e8 2026-03-07    1503.    1568.     0.551
    2 George Washington Loyola Chicago 4.02e8 2026-03-07    1389.    1533.     0.437
    3 La Salle          Saint Joseph's 4.02e8 2026-03-07    1615.    1430.     0.838
    4 Rhode Island      Fordham        4.02e8 2026-03-07    1517.    1523.     0.632
    5 Richmond          Duquesne       4.02e8 2026-03-07    1527.    1477.     0.704
    6 Saint Louis       George Mason   4.02e8 2026-03-07    1596.    1719.     0.467
    # ℹ 2 more variables: sim_value <dbl>, predicted_winner <chr>

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

                team A10_final_elo
    1    Saint Louis      1703.152
    2            VCU      1672.964
    3         Dayton      1658.405
    4   George Mason      1612.237
    5 Saint Joseph's      1590.247
    6       Davidson      1554.075

``` r
head(A10_remaining_schedule)
```

    # A tibble: 6 × 11
      away_team_name    home_team_name     id game_date  home_elo away_elo home_prob
      <chr>             <chr>           <int> <date>        <dbl>    <dbl>     <dbl>
    1 VCU               Dayton         4.02e8 2026-03-06    1646.    1685.     0.586
    2 Davidson          St. Bonaventu… 4.02e8 2026-03-07    1503.    1568.     0.551
    3 George Washington Loyola Chicago 4.02e8 2026-03-07    1389.    1533.     0.437
    4 La Salle          Saint Joseph's 4.02e8 2026-03-07    1615.    1430.     0.838
    5 Rhode Island      Fordham        4.02e8 2026-03-07    1517.    1523.     0.632
    6 Richmond          Duquesne       4.02e8 2026-03-07    1527.    1477.     0.704
    # ℹ 4 more variables: sim_value <dbl>, predicted_winner <chr>,
    #   home_elo_before <dbl>, away_elo_before <dbl>

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

    # A tibble: 6 × 5
      home_team_name        id game_date  away_team_name home_elo
      <chr>              <int> <date>     <chr>             <dbl>
    1 Iowa           401825560 2026-03-05 Michigan          1607.
    2 Maryland       401825567 2026-03-08 Illinois          1441.
    3 Michigan       401825568 2026-03-08 Michigan State    1770.
    4 Michigan State 401825561 2026-03-05 Rutgers           1684.
    5 Minnesota      401825562 2026-03-07 Northwestern      1519.
    6 Nebraska       401825569 2026-03-08 Iowa              1690.

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

    # A tibble: 11 × 6
       away_team_name home_team_name        id game_date  home_elo away_elo
       <chr>          <chr>              <int> <date>        <dbl>    <dbl>
     1 Illinois       Maryland       401825567 2026-03-08    1441.    1713.
     2 Indiana        Ohio State     401825563 2026-03-07    1593.    1553.
     3 Iowa           Nebraska       401825569 2026-03-08    1690.    1607.
     4 Michigan       Iowa           401825560 2026-03-05    1607.    1770.
     5 Michigan State Michigan       401825568 2026-03-08    1770.    1684.
     6 Northwestern   Minnesota      401825562 2026-03-07    1519.    1512.
     7 Penn State     Rutgers        401825570 2026-03-08    1467.    1451.
     8 Rutgers        Michigan State 401825561 2026-03-05    1684.    1467.
     9 UCLA           USC            401825566 2026-03-07    1524.    1625.
    10 Washington     Oregon         401825564 2026-03-07    1472.    1520.
    11 Wisconsin      Purdue         401825565 2026-03-07    1648.    1649.

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

    # A tibble: 6 × 9
      away_team_name home_team_name        id game_date  home_elo away_elo home_prob
      <chr>          <chr>              <int> <date>        <dbl>    <dbl>     <dbl>
    1 Illinois       Maryland       401825567 2026-03-08    1441.    1713.     0.271
    2 Indiana        Ohio State     401825563 2026-03-07    1593.    1553.     0.692
    3 Iowa           Nebraska       401825569 2026-03-08    1690.    1607.     0.742
    4 Michigan       Iowa           401825560 2026-03-05    1607.    1770.     0.410
    5 Michigan State Michigan       401825568 2026-03-08    1770.    1684.     0.744
    6 Northwestern   Minnesota      401825562 2026-03-07    1519.    1512.     0.649
    # ℹ 2 more variables: sim_value <dbl>, predicted_winner <chr>

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

                team B1G_final_elo
    1       Michigan      1782.086
    2       Illinois      1720.914
    3       Nebraska      1689.780
    4 Michigan State      1677.042
    5         Purdue      1647.683
    6           UCLA      1639.730

``` r
head(B1G_remaining_schedule)
```

    # A tibble: 6 × 11
      away_team_name home_team_name        id game_date  home_elo away_elo home_prob
      <chr>          <chr>              <int> <date>        <dbl>    <dbl>     <dbl>
    1 Michigan       Iowa           401825560 2026-03-05    1607.    1770.     0.410
    2 Rutgers        Michigan State 401825561 2026-03-05    1684.    1467.     0.862
    3 Indiana        Ohio State     401825563 2026-03-07    1593.    1553.     0.692
    4 Northwestern   Minnesota      401825562 2026-03-07    1519.    1512.     0.649
    5 UCLA           USC            401825566 2026-03-07    1524.    1625.     0.498
    6 Washington     Oregon         401825564 2026-03-07    1472.    1520.     0.574
    # ℹ 4 more variables: sim_value <dbl>, predicted_winner <chr>,
    #   home_elo_before <dbl>, away_elo_before <dbl>

Atlantic Coast Conference

``` r
ACC_remaining_schedule <- schedule_26 %>% 
  filter(status_type_name == "STATUS_SCHEDULED") %>% 
  dplyr::rename(
    home_team_name = home_location,
    away_team_name = away_location
  ) %>% 
  # Filter specifically for ACC teams this time
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

    # A tibble: 6 × 5
      home_team_name        id game_date  away_team_name home_elo
      <chr>              <int> <date>     <chr>             <dbl>
    1 Boston College 401820793 2026-03-07 Notre Dame        1442.
    2 Clemson        401820787 2026-03-07 Georgia Tech      1629.
    3 Duke           401820788 2026-03-07 North Carolina    1785.
    4 Florida State  401820789 2026-03-07 SMU               1577.
    5 Miami          401820790 2026-03-07 Louisville        1695.
    6 NC State       401820791 2026-03-07 Stanford          1607.

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

    # A tibble: 9 × 6
      away_team_name home_team_name        id game_date  home_elo away_elo
      <chr>          <chr>              <int> <date>        <dbl>    <dbl>
    1 California     Wake Forest    401820795 2026-03-07    1537.    1632.
    2 Georgia Tech   Clemson        401820787 2026-03-07    1629.    1420.
    3 Louisville     Miami          401820790 2026-03-07    1695.    1632.
    4 North Carolina Duke           401820788 2026-03-07    1785.    1702.
    5 Notre Dame     Boston College 401820793 2026-03-07    1442.    1467.
    6 Pittsburgh     Syracuse       401820792 2026-03-07    1518.    1452.
    7 SMU            Florida State  401820789 2026-03-07    1577.    1564.
    8 Stanford       NC State       401820791 2026-03-07    1607.    1590.
    9 Virginia Tech  Virginia       401820794 2026-03-07    1734.    1569.

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

# Determine the winner: If random number < home_prob, Home wins. Else, Away wins
ACC_remaining_schedule$predicted_winner <- ifelse(
  ACC_remaining_schedule$sim_value < ACC_remaining_schedule$home_prob, 
  ACC_remaining_schedule$home_team_name, 
  ACC_remaining_schedule$away_team_name
)

# View the results
head(ACC_remaining_schedule)
```

    # A tibble: 6 × 9
      away_team_name home_team_name        id game_date  home_elo away_elo home_prob
      <chr>          <chr>              <int> <date>        <dbl>    <dbl>     <dbl>
    1 California     Wake Forest    401820795 2026-03-07    1537.    1632.     0.507
    2 Georgia Tech   Clemson        401820787 2026-03-07    1629.    1420.     0.856
    3 Louisville     Miami          401820790 2026-03-07    1695.    1632.     0.718
    4 North Carolina Duke           401820788 2026-03-07    1785.    1702.     0.742
    5 Notre Dame     Boston College 401820793 2026-03-07    1442.    1467.     0.606
    6 Pittsburgh     Syracuse       401820792 2026-03-07    1518.    1452.     0.722
    # ℹ 2 more variables: sim_value <dbl>, predicted_winner <chr>

``` r
ACC_current_elos <- setNames(ACC_Conference$elo_rating, ACC_Conference$team_name)

# 2. Sort schedule by date
# It is critical to simulate in order
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

                team ACC_final_elo
    1           Duke      1785.187
    2       Virginia      1733.744
    3 North Carolina      1701.888
    4          Miami      1694.768
    5     Louisville      1632.234
    6     California      1632.160

``` r
head(ACC_remaining_schedule)
```

    # A tibble: 6 × 11
      away_team_name home_team_name        id game_date  home_elo away_elo home_prob
      <chr>          <chr>              <int> <date>        <dbl>    <dbl>     <dbl>
    1 California     Wake Forest    401820795 2026-03-07    1537.    1632.     0.507
    2 Georgia Tech   Clemson        401820787 2026-03-07    1629.    1420.     0.856
    3 Louisville     Miami          401820790 2026-03-07    1695.    1632.     0.718
    4 North Carolina Duke           401820788 2026-03-07    1785.    1702.     0.742
    5 Notre Dame     Boston College 401820793 2026-03-07    1442.    1467.     0.606
    6 Pittsburgh     Syracuse       401820792 2026-03-07    1518.    1452.     0.722
    # ℹ 4 more variables: sim_value <dbl>, predicted_winner <chr>,
    #   home_elo_before <dbl>, away_elo_before <dbl>

``` r
save(espn_mbb_pbp, temp_1, temp_2, games_1, games_2, mbb_2026, mbb_2025, df_elo_25, df_elo_26, conferences_26_clean, schedule_26, ACC_Conference, ACC_final_simulated_elos, ACC_remaining_schedule, A10_Conference, A10_final_simulated_elos, A10_remaining_schedule, B1G_Conference, B1G_final_simulated_elos, B1G_remaining_schedule, MAAC_Conference, MAAC_remaining_schedule, merged_mbb_clean,  file = "mbb_data_3.rda")
```

BUILD OUT THE TOURNAMENT FIELD WORK WITH MARTIN FOR MORE ACCURATE ELOs

Here I used NCAA rankings from the preseason to add extra points to the
top 25 preseason teams

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
  ) / 2
)

# Print the dataframe to verify
print(ranking_data)
```

       Rank           Team Points
    1     1         Purdue  742.5
    2     2        Houston  729.5
    3     3        Florida  691.0
    4     4          UConn  649.5
    5     5     St. John's  601.5
    6     6           Duke  561.5
    7     7       Michigan  542.0
    8     8            BYU  539.0
    9     9       Kentucky  528.0
    10   10     Texas Tech  507.5
    11   11     Louisville  483.0
    12   12           UCLA  370.5
    13   13        Arizona  357.5
    14   14       Arkansas  347.5
    15   15        Alabama  310.0
    16   16     Iowa State  308.0
    17   17       Illinois  283.5
    18   18      Tennessee  231.0
    19   19         Kansas  226.5
    20   20         Auburn  212.0
    21   21        Gonzaga  193.5
    22   22 Michigan State   94.0
    23   23      Creighton   79.0
    24   24      Wisconsin   68.0
    25   25 North Carolina   52.0

Below I am building out the weights for ELOs based on each conference.
The more competitive conferences are going to be rewarded more as they
are playing a more difficult schdule with more high end talent. Each
conference has its own unique id so each on needs its own line of code
to receive its proper k value weighting

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

     [1] 1810.0 1857.5 1847.5 1712.0 2039.0 1579.0 2061.5 2191.0 1693.5 2229.5
    [11] 1783.5 1808.0 1726.5 2028.0 1983.0 2042.0 1594.0 1552.0 2242.5 2101.5
    [21] 1731.0 2007.5 1870.5 2149.5 1568.0

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

        team_elo_res     team
    645     2142.860    UConn
    244     2133.981  Houston
    161     2100.246     Duke
    494     2094.236   Purdue
    192     2083.559  Florida
    356     2072.666 Michigan

Build out the different conference tournaments. Most tournaments follow
one of four types of structures which is reflected in the code chunk
below with examples for each.

``` r
# --- PREP CONFERENCE DATA ---
final_standings_conf <- merge(
  x = projected_standings,
  y = conferences_26_clean,
  by.x = "team",
  by.y = "team",
  all.x = TRUE
) %>% 
  dplyr::rename(elo_rating = team_elo_res) %>%
  drop_na(conference_2025_26)

# Helper function to simulate a single game
sim_game <- function(t_a, t_b, conf_data) {
  prob <- elo.prob(conf_data$elo_rating[conf_data$team == t_a], 
                   conf_data$elo_rating[conf_data$team == t_b])
  ifelse(runif(1) < prob, t_a, t_b)
}

# --- THE 4 Tournament Structures for 2026 ---

# Traditional Single-Elimination (No Byes)
# Handles standard 8-team brackets, and the 4-team Ivy League bracket
sim_traditional_tourney <- function(conf_data, is_ivy = FALSE) {
  
  if(is_ivy) {
    teams <- conf_data %>% arrange(desc(elo_rating)) %>% head(4)
    if(nrow(teams) < 4) return(list(Champion = teams$team[1]))
    
    # Ivy League is just Semis and Finals
    sf1 <- sim_game(teams$team[1], teams$team[4], conf_data)
    sf2 <- sim_game(teams$team[2], teams$team[3], conf_data)
    champ <- sim_game(sf1, sf2, conf_data)
    
    return(list(Semifinals = c(sf1, sf2), Champion = champ))
  } 
  
  else {
    teams <- conf_data %>% arrange(desc(elo_rating)) %>% head(8)
    if(nrow(teams) < 8) return(list(Champion = teams$team[1]))
    
    # Quarterfinals
    qf1 <- sim_game(teams$team[1], teams$team[8], conf_data)
    qf2 <- sim_game(teams$team[4], teams$team[5], conf_data)
    qf3 <- sim_game(teams$team[3], teams$team[6], conf_data)
    qf4 <- sim_game(teams$team[2], teams$team[7], conf_data)
    
    # Semifinals
    sf1 <- sim_game(qf1, qf2, conf_data)
    sf2 <- sim_game(qf3, qf4, conf_data)
    
    # Championship
    champ <- sim_game(sf1, sf2, conf_data)
    
    return(list(Quarterfinals = c(qf1, qf2, qf3, qf4), Semifinals = c(sf1, sf2), Champion = champ))
  }
}

# 2. Single-Bye Format (Standard Staggered)
# Top seeds get a bye to Quarter Finals, lower seeds play in R1
sim_single_bye_tourney <- function(conf_data) {
  teams <- conf_data %>% arrange(desc(elo_rating)) %>% head(10)
  if(nrow(teams) < 10) return(list(Champion = teams$team[1])) 
  
  # Round 1 (Bottom seeds play-in)
  r1_g1 <- sim_game(teams$team[8], teams$team[9], conf_data)
  r1_g2 <- sim_game(teams$team[7], teams$team[10], conf_data)
  
  # Quarterfinals (Top seeds + R1 Winners)
  qf_1 <- sim_game(teams$team[1], r1_g1, conf_data)         
  qf_2 <- sim_game(teams$team[4], teams$team[5], conf_data) 
  qf_3 <- sim_game(teams$team[2], r1_g2, conf_data)         
  qf_4 <- sim_game(teams$team[3], teams$team[6], conf_data) 
  
  sf_1 <- sim_game(qf_1, qf_2, conf_data)
  sf_2 <- sim_game(qf_3, qf_4, conf_data)
  
  champ <- sim_game(sf_1, sf_2, conf_data)
  
  return(list(Round_1 = c(r1_g1, r1_g2), Quarterfinals = c(qf_1, qf_2, qf_3, qf_4), Semifinals = c(sf_1, sf_2), Champion = champ))
}

# 3. Double-Bye & Triple-Bye Format (The Power Model)
# Top 4 seeds jump straight to QF (3 wins to a title). 
sim_power_tourney <- function(conf_data) {
  teams <- conf_data %>% arrange(desc(elo_rating)) %>% head(15)
  if(nrow(teams) < 15) return(list(Champion = teams$team[1]))
  
  # Round 1
  r1_g1 <- sim_game(teams$team[12], teams$team[13], conf_data)
  r1_g2 <- sim_game(teams$team[11], teams$team[14], conf_data)
  r1_g3 <- sim_game(teams$team[10], teams$team[15], conf_data)
  
  # Round 2
  r2_g1 <- sim_game(teams$team[8], teams$team[9], conf_data)
  r2_g2 <- sim_game(teams$team[5], r1_g1, conf_data)
  r2_g3 <- sim_game(teams$team[6], r1_g2, conf_data)
  r2_g4 <- sim_game(teams$team[7], r1_g3, conf_data)
  
  # Quarterfinals (Top 4 enter here)
  qf_1 <- sim_game(teams$team[1], r2_g1, conf_data)
  qf_2 <- sim_game(teams$team[4], r2_g2, conf_data)
  qf_3 <- sim_game(teams$team[2], r2_g4, conf_data)
  qf_4 <- sim_game(teams$team[3], r2_g3, conf_data)
  
  sf_1 <- sim_game(qf_1, qf_2, conf_data)
  sf_2 <- sim_game(qf_3, qf_4, conf_data)
  
  champ <- sim_game(sf_1, sf_2, conf_data)
  
  return(list(Round_1 = c(r1_g1, r1_g2, r1_g3), Round_2 = c(r2_g1, r2_g2, r2_g3, r2_g4), Quarterfinals = c(qf_1, qf_2, qf_3, qf_4), Semifinals = c(sf_1, sf_2), Champion = champ))
}

# 4. The Stepladder Format (Direct to Semifinals)
# Top 2 seeds jump straight to SF (2 wins to a title).
sim_stepladder_tourney <- function(conf_data) {
  teams <- conf_data %>% arrange(desc(elo_rating)) %>% head(8)
  if(nrow(teams) < 8) return(list(Champion = teams$team[1])) 
  
  # Round 1
  r1_1 <- sim_game(teams$team[5], teams$team[8], conf_data)
  r1_2 <- sim_game(teams$team[6], teams$team[7], conf_data)
  
  # Quarterfinals
  qf_1 <- sim_game(teams$team[4], r1_1, conf_data)
  qf_2 <- sim_game(teams$team[3], r1_2, conf_data)
  
  # Semifinals (Top 2 enter here)
  sf_1 <- sim_game(teams$team[1], qf_1, conf_data)
  sf_2 <- sim_game(teams$team[2], qf_2, conf_data)
  
  champ <- sim_game(sf_1, sf_2, conf_data)
  
  return(list(Round_1 = c(r1_1, r1_2), Quarterfinals = c(qf_1, qf_2), Semifinals = c(sf_1, sf_2), Champion = champ))
}

# --- RUN TOURNAMENT SIMULATIONS ---

conf_split <- split(final_standings_conf, final_standings_conf$conference_2025_26)
auto_bids <- character(0)
all_tourney_results <- list() 

# Define the groupings 
stepladder_confs <- c("WCC", "OVC", "Southland")
power_confs <- c("AAC", "Atlantic 10", "ACC", "Big 12", "CAA", "SEC", "Sun Belt", "Big Ten")
single_bye_confs <- c("ASUN", "Big East", "Big Sky", "Big South", "Horizon", "MAAC", "MVC", "Mountain West", "Patriot", "SoCon")
# The remaining conferences default to Traditional Single-Elimination

for(conf_name in names(conf_split)) {
  conf_data <- conf_split[[conf_name]]
  
  # Route to the correct mathematical bracket
  if(conf_name %in% stepladder_confs) {
    results <- sim_stepladder_tourney(conf_data)
  } 
  else if(conf_name %in% power_confs) {
    results <- sim_power_tourney(conf_data)
  } 
  else if(conf_name %in% single_bye_confs) {
    results <- sim_single_bye_tourney(conf_data)
  } 
  else if(conf_name == "Ivy League") {
    # Ivy League uses Traditional but only invites 4 teams
    results <- sim_traditional_tourney(conf_data, is_ivy = TRUE)
  } 
  else {
    # Default: Traditional 8-team bracket
    results <- sim_traditional_tourney(conf_data, is_ivy = FALSE)
  }
  
  all_tourney_results[[conf_name]] <- results
  auto_bids <- c(auto_bids, results$Champion)
}

# --- 4. ASSEMBLE THE BIDS ---

auto_bid_df <- final_standings_conf %>% filter(team %in% auto_bids)
at_large_df <- final_standings_conf %>% filter(!team %in% auto_bids) %>% arrange(desc(elo_rating)) %>% head(36)

# --- 5. THE FIRST FOUR (PLAY-INS) ---

auto_locks <- head(auto_bid_df %>% arrange(desc(elo_rating)), nrow(auto_bid_df) - 4)
auto_playin <- tail(auto_bid_df %>% arrange(desc(elo_rating)), 4)

at_large_locks <- head(at_large_df, nrow(at_large_df) - 4)
at_large_playin <- tail(at_large_df, 4)

play_playin <- function(t1, t2) {
  prob <- elo.prob(t1$elo_rating, t2$elo_rating)
  if(runif(1) < prob) return(t1) else return(t2)
}

ff_w1 <- play_playin(auto_playin[1,], auto_playin[2,])
ff_w2 <- play_playin(auto_playin[3,], auto_playin[4,])
ff_w3 <- play_playin(at_large_playin[1,], at_large_playin[2,])
ff_w4 <- play_playin(at_large_playin[3,], at_large_playin[4,])



final_64_unranked <- bind_rows(auto_locks, at_large_locks, ff_w1, ff_w2, ff_w3, ff_w4)
```

Below is the code for the initial simulation of the NCAA Tournament. It
includes the top 64 teams, assigns them to their region (East, West,
South, Midwest), sets the matchups for the tournament and then will
simulate each round and predicts each winner of the games until the end
of the tournament and presents the winner.

``` r
top_64 <- final_64_unranked %>%
  arrange(desc(elo_rating)) %>%
  head(64) %>%
  dplyr::rename(team_name = team) %>%
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

           team_name elo_rating team_id abbreviation     mascot  color
    1          UConn   2142.860      41         CONN    Huskies 0c2340
    64         Omaha   1474.947    2437          OMA  Mavericks e3193e
    32   Saint Louis   1674.657     139          SLU  Billikens 00539C
    33         Miami   1673.924    2390          MIA Hurricanes 005030
    17       Gonzaga   1815.239    2250         GONZ   Bulldogs 041e42
    48 South Alabama   1568.497       6          USA    Jaguars 003E7E
       alternate_color                                                logo
    1           f1f2f3   https://a.espncdn.com/i/teamlogos/ncaa/500/41.png
    64          474648 https://a.espncdn.com/i/teamlogos/ncaa/500/2437.png
    32          ebebeb  https://a.espncdn.com/i/teamlogos/ncaa/500/139.png
    33          f47321 https://a.espncdn.com/i/teamlogos/ncaa/500/2390.png
    17          c8102e https://a.espncdn.com/i/teamlogos/ncaa/500/2250.png
    48            <NA>    https://a.espncdn.com/i/teamlogos/ncaa/500/6.png
                                                      logo_dark conference_2025_26
    1    https://a.espncdn.com/i/teamlogos/ncaa/500-dark/41.png           Big East
    64 https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2437.png             Summit
    32  https://a.espncdn.com/i/teamlogos/ncaa/500-dark/139.png        Atlantic 10
    33 https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2390.png                ACC
    17 https://a.espncdn.com/i/teamlogos/ncaa/500-dark/2250.png                WCC
    48    https://a.espncdn.com/i/teamlogos/ncaa/500-dark/6.png           Sun Belt
       overall_rank region regional_seed
    1             1   East             1
    64           64   East            16
    32           32   East             8
    33           33   East             9
    17           17   East             5
    48           48   East            12

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

# 4. View the matchups to confirm logic (Check that 1 is playing 16)
head(r64_matchups, 10)
```

       Region Seed_A      Team_A    Elo_A VS Seed_B             Team_B    Elo_B
    1    East      1       UConn 2142.860 vs     16              Omaha 1474.947
    2    East      8 Saint Louis 1674.657 vs      9              Miami 1673.924
    3    East      5     Gonzaga 1815.239 vs     12      South Alabama 1568.497
    4    East      4     Alabama 1843.649 vs     13 California Baptist 1562.950
    5    East      6      Auburn 1727.204 vs     11  Stephen F. Austin 1637.492
    6    East      3  Texas Tech 1948.627 vs     14           Columbia 1544.970
    7    East      7  Vanderbilt 1724.982 vs     10         Utah State 1640.885
    8    East      2     Arizona 2006.967 vs     15   UC Santa Barbara 1544.404
    9    West      1     Houston 2133.981 vs     16        New Orleans 1483.526
    10   West      8     Georgia 1676.855 vs      9       Saint Mary's 1668.721

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



    === ROUND OF 64 ===

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

    Game 01: UConn                def. Omaha               
    Game 02: Saint Louis          def. Miami               
    Game 03: Gonzaga              def. South Alabama       
    Game 04: Alabama              def. California Baptist  
    Game 05: Auburn               def. Stephen F. Austin   
    Game 06: Texas Tech           def. Columbia            
    Game 07: Vanderbilt           def. Utah State          
    Game 08: Arizona              def. UC Santa Barbara    
    Game 09: Houston              def. New Orleans         
    Game 10: Saint Mary's         def. Georgia             
    Game 11: Kansas               def. Colorado State      
    Game 12: Illinois             def. UT Martin           
    Game 13: Akron                def. North Carolina      
    Game 14: Iona                 def. BYU                 
    Game 15: Virginia             def. California          
    Game 16: St. John's           def. Long Island University
    Game 17: Duke                 def. Norfolk State       
    Game 18: UCF                  def. Clemson             
    Game 19: Tennessee            def. St. Thomas-Minnesota
    Game 20: Wichita State        def. Arkansas            
    Game 21: High Point           def. Nebraska            
    Game 22: Robert Morris        def. Iowa State          
    Game 23: SMU                  def. Miami (OH)          
    Game 24: Michigan             def. Mercer              
    Game 25: Purdue               def. Missouri State      
    Game 26: Wisconsin            def. Iowa                
    Game 27: Michigan State       def. Liberty             
    Game 28: Kentucky             def. Northern Iowa       
    Game 29: UCLA                 def. UNC Wilmington      
    Game 30: UMBC                 def. Louisville          
    Game 31: Villanova            def. Missouri            
    Game 32: Florida              def. Montana State       

``` r
# --- 3. ROUND OF 32 (16 Games) ---
cat("\n\n=== ROUND OF 32 ===\n")
```



    === ROUND OF 32 ===

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

    Game 01: UConn                def. Saint Louis         
    Game 02: Alabama              def. Gonzaga             
    Game 03: Texas Tech           def. Auburn              
    Game 04: Arizona              def. Vanderbilt          
    Game 05: Houston              def. Saint Mary's        
    Game 06: Illinois             def. Kansas              
    Game 07: Akron                def. Iona                
    Game 08: St. John's           def. Virginia            
    Game 09: Duke                 def. UCF                 
    Game 10: Wichita State        def. Tennessee           
    Game 11: High Point           def. Robert Morris       
    Game 12: Michigan             def. SMU                 
    Game 13: Purdue               def. Wisconsin           
    Game 14: Kentucky             def. Michigan State      
    Game 15: UCLA                 def. UMBC                
    Game 16: Florida              def. Villanova           

``` r
# --- 4. SWEET 16 (8 Games) ---
cat("\n\n=== SWEET 16 ===\n")
```



    === SWEET 16 ===

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

    Game 1: UConn                def. Alabama             
    Game 2: Arizona              def. Texas Tech          
    Game 3: Houston              def. Illinois            
    Game 4: St. John's           def. Akron               
    Game 5: Duke                 def. Wichita State       
    Game 6: Michigan             def. High Point          
    Game 7: Purdue               def. Kentucky            
    Game 8: Florida              def. UCLA                

``` r
# --- 5. ELITE 8 (4 Games) ---
cat("\n\n=== ELITE 8 ===\n")
```



    === ELITE 8 ===

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

    Region 1: UConn                def. Arizona             
    Region 2: St. John's           def. Houston             
    Region 3: Duke                 def. Michigan            
    Region 4: Purdue               def. Florida             

``` r
# --- 6. FINAL FOUR (2 Games) ---
cat("\n\n=== FINAL FOUR ===\n")
```



    === FINAL FOUR ===

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

    Semi 1: UConn                def. St. John's          
    Semi 2: Duke                 def. Purdue              

``` r
# --- 7. NATIONAL CHAMPIONSHIP (1 Game) ---
cat("\n\n=== NATIONAL CHAMPIONSHIP ===\n")
```



    === NATIONAL CHAMPIONSHIP ===

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

    CHAMPION: UConn

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


         UConn       Duke    Houston     Purdue    Florida   Michigan St. John's 
          2390       1664       1509       1250        945        774        568 
       Arizona Texas Tech        BYU 
           430        136         68 

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

             team wins probability
    1       UConn 2390      0.2390
    33       Duke 1664      0.1664
    17    Houston 1509      0.1509
    49     Purdue 1250      0.1250
    63    Florida  945      0.0945
    47   Michigan  774      0.0774
    31 St. John's  568      0.0568
    15    Arizona  430      0.0430
    11 Texas Tech  136      0.0136
    27        BYU   68      0.0068

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

![](readme_files/figure-gfm/unnamed-chunk-65-1.png)<!-- -->

``` r
save(espn_mbb_pbp, temp_1, temp_2, games_1, games_2, mbb_2026, mbb_2025, df_elo_25, df_elo_26, conferences_26_clean, schedule_26, ACC_Conference, ACC_final_simulated_elos, ACC_remaining_schedule, A10_Conference, A10_final_simulated_elos, A10_remaining_schedule, B1G_Conference, B1G_final_simulated_elos, B1G_remaining_schedule, MAAC_Conference, MAAC_remaining_schedule, merged_mbb_clean, r64_matchups, season_games, projected_standings, final_elos, ranking_data, tourney_field, team_db_sim, results_list_df, champ_counts, champ_prob, file = "mbb_data_4.rda")
```

``` r
save(espn_mbb_pbp, temp_1, temp_2, games_1, games_2, mbb_2026, mbb_2025, df_elo_25, df_elo_26, conferences_26_clean, schedule_26, ACC_Conference, ACC_final_simulated_elos, ACC_remaining_schedule, A10_Conference, A10_final_simulated_elos, A10_remaining_schedule, B1G_Conference, B1G_final_simulated_elos, B1G_remaining_schedule, MAAC_Conference, MAAC_remaining_schedule, merged_mbb_clean, r64_matchups, season_games, projected_standings, final_elos, ranking_data, tourney_field, team_db_sim, results_list_df, champ_counts, champ_prob, final_64_unranked, conf_split, auto_playin, auto_locks, auto_bid_df, auto_bids, ff_w4, ff_w3, ff_w2, ff_w1, at_large_playin, at_large_locks, at_large_df, all_tourney_results, sim_stepladder_tourney, sim_power_tourney, sim_single_bye_tourney, sim_traditional_tourney, file = "mbb_data_5.rda")
```
