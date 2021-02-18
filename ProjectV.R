library(readr)
library(dplyr)

games_2021 <- read.csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Games.csv")

View(
  games_2021 %>% filter(Result == "TBD",
                        !is.na(Money_Line)) %>%
    select(Date, Team, Opponent, Line_Favored, 
           Line_Amount, Over_Under, Money_Line,
           Money_Line_Opp,Implied_Odds, Implied_Odds_Opp)
)

box_2021 <- read.csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Box_Score.csv")
