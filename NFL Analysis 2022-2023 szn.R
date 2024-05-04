install.packages("nflfastR")
install.packages("tidyverse")
install.packages("ggimage")
install.packages("gt")

library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)



## Viewing Play By Play Data for 2022 - 2023

pbp <- load_pbp(2022:2023)

pbp %>% head()
pbp %>% select(posteam, defteam, season, down, ydstogo, rush, pass, yards_gained) %>% head()


## Filtering for Rushes and Passes (offensive plays)

pbp_runpass <- pbp %>%
  filter(rush == 1 | pass == 1) %>%
  filter(!is.na(epa))



## EPA's for 2022 season

offenses_22 <- pbp_runpass %>%
  filter(season == 2022) %>%
  group_by(posteam) %>%
  summarize(epa_22 = mean(epa))



## EPA's for 2023 season

offenses_23 <- pbp_runpass %>%
  filter(season == 2023) %>%
  group_by(posteam) %>%
  summarize(epa_23 = mean(epa))



## Combine 2022 and 2023 datasets

offenses_both <- offenses_22 %>%
  left_join(offenses_23, by ="posteam")



## Combining both years' EPA's and team logos in same dataset

View(teams_colors_logos)

offenses_both <- offenses_both %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))



## Viewing both years' EPA's on graph with team logos

offenses_both %>%
  #Plot with 2022 epa as x axis, 2023 epa as y axis
  ggplot(aes(x = epa_22, y = epa_23)) +
  
  
  
  
  # y intercept line is the mean of the 2023 epa's, x intercept is 2022 mean epa's
  
  geom_hline(yintercept = mean(offenses_both$epa_23), linetype = "dashed") +
  geom_vline(xintercept = mean(offenses_both$epa_22), linetype = "dashed") +
  
  
  
  
  #line of best fit lm = linear model, se = standard error shade
  
  geom_smooth(method = "lm", color = "black", se = F) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  theme_bw() +
  
  
  
  #labels/titles/caption
  
  labs(x = "Offenses EPA/Play 2022",
       y = "Offenses EPA/Play 2023",
       title = "Offensive EPA/Play in 2022 Compared to 2023",
       caption = "By Aaron Kashian | kash2121@icloud.com") +
  
  
  
  #centering title
  
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

ggsave("off-epa-22-23.png", width = 14, height = 10, dpi = "retina")




