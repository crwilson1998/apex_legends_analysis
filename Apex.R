# This project aims to discern which team composition work best for ranked and 
# which legend was the player's best for the season.

# Import the libraries

library(ggplot2)
library(dplyr)

# Import the data
season_fifteen <- read.csv("Apex_Game_History_Season15S1.csv")

# Understand the data to see if I want to make any changes
colnames(season_fifteen)
glimpse(season_fifteen)
summary(season_fifteen)
str(season_fifteen)

# Start cleaning by removing null values and dirty data
season_fifteen <- na.omit(season_fifteen) %>%
                  select(-ï..date, -teamate_count, -teamate_quit_count)
season_fifteen <- season_fifteen[season_fifteen$my_legend != 'Cantage',]
season_fifteen['game'] <- as.logical(season_fifteen$game)


colnames(season_fifteen)
glimpse(season_fifteen)
summary(season_fifteen)
str(season_fifteen)

# Which team comps had the highest rp earned on broken moon?
team_comps <- season_fifteen %>% 
              select(squad_placed, my_legend, 
                     teamate_1_legend, teamate_2_legend, rp_earned)
head(team_comps)

best_comp <- team_comps %>% 
             group_by(my_legend, teamate_1_legend, teamate_2_legend) %>% 
             summarise(total_rp_earned = sum(rp_earned)) %>%
             arrange(desc(total_rp_earned))
head(best_comp,10)

# Which legend did the player do his best on based on Kills, Knocks, Assists for each rank

unique(season_fifteen$my_legend)
legends <- season_fifteen %>% 
           select(my_rank, my_legend, my_damage, my_kills, my_knocks, my_assists)
head(legends)
total_stats <- legends %>% 
               select(-my_rank) %>%
               group_by(my_legend) %>%
               summarise(total_damage = sum(my_damage), 
                         total_kills = sum(my_kills),
                         total_knocks = sum(my_knocks), 
                         total_assists = sum(my_assists)) %>%
               arrange(desc(total_damage))
total_stats

# Who was the top legend for each rank

total_stats_per_rank <- legends %>% 
                        group_by(my_legend, my_rank) %>%
                        summarise(total_damage = sum(my_damage), 
                                  total_kills = sum(my_kills),
                                  total_knocks = sum(my_knocks), 
                                  total_assists = sum(my_assists)) %>%
                        arrange(my_rank, desc(total_damage))
print(total_stats_per_rank,n=20)

# I believe that games where voice chat was enabled, the team is more
# likely to place in the top 10

voice_chat <- season_fifteen %>% 
                      select(squad_placed, my_legend, 
                      teamate_1_legend, teamate_2_legend, voice_chat)

nrow(voice_chat)
enabled <- nrow(voice_chat[voice_chat$squad_placed >= 10 & 
                voice_chat$voice_chat == 'yes',])
disabled <- nrow(voice_chat[voice_chat$squad_placed >= 10 & 
                voice_chat$voice_chat == 'no',])
enabled
disabled

# What was the average rp gained per rank per legend
rp_gain_spread <- season_fifteen %>% 
  select(my_legend,my_rank,rp_earned,match_type) %>%
  filter(match_type == 'ranked')

avg_rp_gain_per_legend <- rp_gain_spread %>% 
  group_by(my_legend) %>% 
  summarise(avg_sr_gained = mean(rp_earned)) %>%
  arrange(desc(avg_sr_gained))

avg_rp_gain_per_legend 
avg_rp_gain_per_legend_per_rank <- rp_gain_spread %>% 
                                   group_by(my_legend) %>% 
                                   summarise(avg_sr_gained = mean(rp_earned)) %>%
                                   arrange(desc(avg_sr_gained))


# What was the total rp gain for each legend played   
rp_gain_spread <- season_fifteen %>% 
                      select(my_legend,my_rank,rp_earned,match_type) %>%
                      filter(match_type == 'ranked')
rp_gain_spread

rp_gain_in_total <- rp_gain_spread %>% 
                  group_by(my_legend) %>% 
                  summarise(total_sr_gained = sum(rp_earned)) %>%
                  arrange(desc(total_sr_gained))
rp_gain_in_total 

# Let's visualize the result set
ggplot(rp_gain_in_total,aes(x = my_legend, y = total_sr_gained, fill = my_legend)) + 
  geom_bar(stat="identity",position = "dodge") +
  labs(title = "RP Gained by Legend", x = "Season Rank", y = "Total RP Gained") +
  theme(axis.text.x = element_text(angle = 90))

# Now lets see what it was per rank
sr_gain_per_rank <- rp_gain_spread %>% 
                    group_by(my_legend,my_rank) %>% 
                    summarise(total_sr_gained = sum(rp_earned)) %>%
                    arrange(desc(total_sr_gained),my_rank)  
sr_gain_per_rank

# If you want to see a specific legends journey for the season uncomment the next line 
# and set my_legend equal to the legend you want

chosen_legend <- subset(sr_gain_per_rank,my_legend == 'Vantage')
ggplot(chosen_legend, aes(x = my_rank, y = total_sr_gained, group = my_legend)) +
  geom_line() +
  geom_point() +
  labs(title = "RP Gained by Rank for a Legend", x = "Rank", y = "RP Gained") +
  theme_minimal()

