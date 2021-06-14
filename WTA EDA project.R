library(tidyverse)

# Read in data ------------------------------------------------------------

wta_2018_2021_matches <-
  map_dfr(c(2018:2021),
          function(year) {
            read_csv(paste0("https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_matches_",
                            year, ".csv")) %>%
              mutate(winner_seed = as.character(winner_seed),
                     loser_seed = as.character(loser_seed))
          })

# EDA ---------------------------------------------------------------------

# minutes by round
wta_2018_2021_matches %>%
  group_by(round) %>%
  ggplot(aes(x=minutes))+
  geom_boxplot()+
  theme_bw()

wta_2018_2021_matches %>%
  ggplot(aes(x=round, y=minutes))+
  geom_point(alpha=.05)+
  theme_bw()

# first serve in by hand
wta_2018_2021_matches %>%
  ggplot(aes(x = w_1stIn))+
  geom_histogram()+
  theme_bw()+
  facet_wrap(~ winner_hand, ncol=2)

#aces by court surface
surface_ace <- wta_2018_2021_matches %>%
  mutate(aces=w_ace + l_ace) %>%
  ggplot(aes(x=surface,
             y=aces))+
  geom_boxplot()+
  theme_bw()
surface_ace &
  ylim(min(0), max(30))

#comparing double faults of winners and losers ---
windf <- wta_2018_2021_matches %>%
  ggplot(aes(x=w_df))+
  geom_boxplot()+
  theme_bw()

losedf <- wta_2018_2021_matches %>%
  ggplot(aes(x=l_df))+
  geom_boxplot()+
  theme_bw()
#adjust so boxplots have the same scale
losedf_adj <- losedf &
  xlim(min(0), max(75))

install.packages("patchwork")
library(patchwork)
windf / losedf_adj

#comparing aces of winners and losers --- 
winace <- wta_2018_2021_matches %>%
  ggplot(aes(x=w_ace))+
  geom_boxplot()+
  theme_bw()

loseace <- wta_2018_2021_matches %>%
  ggplot(aes(x=l_ace))+
  geom_boxplot()+
  theme_bw()
#adjust so boxplots have the same scale
loseace_adj <- loseace &
  xlim(min(0), max(75))

winace / loseace_adj

#scatter plot df
wta_2018_2021_matches %>%
  ggplot(aes(x=w_df, y=l_df))+
  geom_point(alpha=.05)+
  theme_bw()

#scatter plot aces
wta_2018_2021_matches %>%
  ggplot(aes(x=w_ace, y=l_ace))+
  geom_point(alpha=.05)+
  theme_bw()

#serve win rate by surface
wta_2018_2021_matches %>%
  mutate(w_swr = w_)
