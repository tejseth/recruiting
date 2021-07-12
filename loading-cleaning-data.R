library(tidyverse)
library(ggthemes)

theme_reach <- function() {
  theme_fivethirtyeight() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 13, hjust = 0.5),
      axis.title.x = element_text(size=16),
      axis.title.y = element_text(size=16),
      axis.text = element_text(size = 12)
    )
}

recruiting_247_2010 <- read.csv("~/recruiting/247_recruiting/recruiting_247_2010.csv")
recruiting_247_2011 <- read.csv("~/recruiting/247_recruiting/recruiting_247_2011.csv")
recruiting_247_2012 <- read.csv("~/recruiting/247_recruiting/recruiting_247_2012.csv")
recruiting_247_2013 <- read.csv("~/recruiting/247_recruiting/recruiting_247_2013.csv")
recruiting_247_2014 <- read.csv("~/recruiting/247_recruiting/recruiting_247_2014.csv")
recruiting_247_2015 <- read.csv("~/recruiting/247_recruiting/recruiting_247_2015.csv")
recruiting_247_2016 <- read.csv("~/recruiting/247_recruiting/recruiting_247_2016.csv")
recruiting_247_2017 <- read.csv("~/recruiting/247_recruiting/recruiting_247_2017.csv")
recruiting_247_2018 <- read.csv("~/recruiting/247_recruiting/recruiting_247_2018.csv")
recruiting_247_2019 <- read.csv("~/recruiting/247_recruiting/recruiting_247_2019.csv")
recruiting_247_2020 <- read.csv("~/recruiting/247_recruiting/recruiting_247_2020.csv")

recruiting_data <- do.call("rbind", list(recruiting_247_2010, recruiting_247_2011, recruiting_247_2012,
                                         recruiting_247_2013, recruiting_247_2014, recruiting_247_2015,
                                         recruiting_247_2016, recruiting_247_2017, recruiting_247_2018,
                                         recruiting_247_2019, recruiting_247_2020))

NCAA_WAA_6_12 <- read_csv("247_recruiting/NCAA_WAA_6_12.csv")

recruiting_data_filtered <- recruiting_data %>%
  filter(!is.na(data.rating))

total_ncaa_waa <- NCAA_WAA_6_12 %>%
  group_by(player, player_id, position) %>%
  summarize(seasons = n(),
            total_waa = sum(WAA),
            waa_per_season = total_waa / seasons,
            total_snaps = sum(total_snaps),
            snaps_per_season = total_snaps / seasons)

recruiting_data_filtered <- recruiting_data_filtered %>%
  left_join(total_ncaa_waa, by = c("data.player_id" = "player_id"))

recruiting_with_waa <- recruiting_data_filtered %>%
  filter(!is.na(total_waa))

recruiting_with_waa <- recruiting_with_waa %>%
  mutate(same_pos = ifelse(data.position == position, 1, 0))

recruiting_with_waa <- recruiting_with_waa %>%
  mutate(star = case_when(
    data.rating > 0.9830 ~ 5,
    data.rating <= 0.9830 & data.rating > 0.8900 ~ 4,
    data.rating <= 0.8900 & data.rating >= 0.7970 ~ 3,
    data.rating < 0.7970 ~ 2
  ))

recruiting_with_waa_season <- recruiting_with_waa %>%
  filter(seasons > 1)






