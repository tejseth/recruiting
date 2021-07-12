rec_waa_season_pos <- recruiting_with_waa_season %>%
  filter(same_pos == 1)

rec_waa_season_pos <- rec_waa_season_pos %>%
  mutate(`Star` = case_when(
    star == 5 ~ "5 Star",
    star == 4 ~ "4 Star",
    star == 3 ~ "3 Star",
    star < 3 ~ "2 Star or Below"
  ))

rec_waa_season_pos %>%
  filter(data.position %in% c("WR", "C", "T", "G", "HB", "QB", "TE")) %>%
  ggplot(aes(x = data.rating, y = total_waa)) +
  geom_jitter(aes(color = `Star`), alpha = 0.35, size = 3) +
  geom_smooth(color = "black", size = 1.5) +
  theme_reach() +
  labs(x = "247 Rating",
       y = "Total WAA",
       title = "How 247 Rating Influences Total Wins Above Average on Offense",
       subtitle = "2012-2020, color is for star rating") +
  facet_wrap(~data.position, scales = "free_y") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  theme(strip.text.x = element_text(size = 14, face = "bold"),
        legend.position = c(0.84, 0.09),
        legend.text=element_text(size=14),
        legend.justification = c(1, 0))
ggsave('5-rec.png', width = 15, height = 10, dpi = "retina")

rec_waa_season_pos %>%
  filter(data.position %in% c("WR", "C", "T", "G", "HB", "QB", "TE")) %>%
  ggplot(aes(x = data.rating, y = total_snaps)) +
  geom_jitter(aes(color = `Star`), alpha = 0.35, size = 3) +
  geom_smooth(color = "black", size = 1.5) +
  theme_reach() +
  labs(x = "247 Rating",
       y = "Total WAA",
       title = "How 247 Rating Influences Total Snaps on Offense",
       subtitle = "2012-2020, Color is for star rating") +
  facet_wrap(~data.position, scales = "free_y") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  theme(strip.text.x = element_text(size = 14, face = "bold"),
        legend.position = c(0.84, 0.09),
        legend.text=element_text(size=14),
        legend.justification = c(1, 0))

rec_waa_season_pos %>%
  filter(data.position %in% c("T", "G", "C")) %>%
  mutate(pos = case_when(
    data.position == "T" ~ "Tackle",
    data.position == "G" ~ "Guard",
    data.position == "C" ~ "Center"
  )) %>%
  filter(data.weight < 399) %>%
  filter(!is.na(data.weight)) %>%
  ggplot(aes(x = data.weight, y = total_waa)) +
  geom_jitter(aes(color = pos), alpha = 0.8, size = 4) +
  geom_smooth(color = "black", size = 2) +
  facet_wrap(~pos) +
  theme_reach() +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Weight Recruited At",
       y = "Total WAA in College",
       title = "How Weight Recruited At Affects Total WAA for Offensive Lineman",
       subtitle = "2012-2020, WAA = Wins Above Average") +
  theme(strip.text = element_text(size = 14, face = "bold"))
ggsave('6-rec.png', width = 15, height = 10, dpi = "retina")

rec_waa_season_pos %>%
  filter(data.position %in% c("DI", "ED", "LB", "CB", "S")) %>%
  ggplot(aes(x = data.rating, y = total_waa)) +
  geom_jitter(aes(color = `Star`), alpha = 0.35, size = 3) +
  geom_smooth(color = "black", size = 1.5) +
  theme_reach() +
  labs(x = "247 Rating",
       y = "Total WAA",
       title = "How 247 Rating Influences Total Wins Above Average on Defense",
       subtitle = "2012-2020, color is for star rating") +
  facet_wrap(~data.position, scales = "free_y") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  theme(strip.text.x = element_text(size = 14, face = "bold"),
        legend.position = c(0.84, 0.09),
        legend.text=element_text(size=14),
        legend.justification = c(1, 0))
