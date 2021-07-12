recruiting_with_waa_season %>%
  filter(data.position %in% c("WR", "C", "T", "G", "HB", "QB", "TE")) %>%
  filter(same_pos == 1) %>%
  ggplot(aes(x = data.rating, y = total_waa)) +
  geom_point(aes(color = as.factor(star)), alpha = 0.25, size = 3) +
  geom_point(data = change_pos_offense, aes(x = data.rating, y = total_waa), color = "black", alpha = 0.25, size = 3) +
  geom_smooth(color = "black", size = 1.5) +
  theme_reach() +
  labs(x = "247 Rating",
       y = "Total WAA",
       title = "How 247 Rating Influences Total Wins Above Average on Offense",
       subtitle = "Color is for star rating, black dots are for when a player changes positions") +
  facet_wrap(~data.position, scales = "free_y") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  theme(strip.text.x = element_text(size = 14, face = "bold"))
