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
        legend.position = c(0.91, 0.09),
        legend.text=element_text(size=14),
        legend.justification = c(1, 0),
        legend.direction = "vertical")
ggsave('8-rec.png', width = 15, height = 10, dpi = "retina")

rec_waa_season_pos %>%
  filter(data.position %in% c("ED", "DI")) %>%
  mutate(pos = case_when(
    data.position == "ED" ~ "Edge Rusher",
    data.position == "DI" ~ "Interior Defensive Lineman",
  )) %>%
  filter(!is.na(data.weight)) %>%
  ggplot(aes(x = data.weight, y = total_waa)) +
  geom_jitter(aes(color = pos), alpha = 0.8, size = 3.75) +
  geom_smooth(color = "black", size = 2) +
  facet_wrap(~pos, scales = "free") +
  theme_reach() +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Weight Recruited At",
       y = "Total WAA in College",
       title = "How Weight Recruited At Affects Total WAA for Defensive Lineman",
       subtitle = "2012-2020, WAA = Wins Above Average") +
  theme(strip.text = element_text(size = 14, face = "bold"))
ggsave('9-rec.png', width = 15, height = 10, dpi = "retina")


