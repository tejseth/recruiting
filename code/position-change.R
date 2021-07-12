same_pos_stats <- recruiting_with_waa_season %>%
  filter(!data.position %in% c("K", "LS", "P", "FB")) %>%
  group_by(data.position) %>%
  summarize(total = n(),
            `Played Some Position` = sum(same_pos),
            `Changed Position` = total - `Played Some Position`,
            same_perc = `Played Some Position` / total)

same_pos_stats <- same_pos_stats %>%
  pivot_longer(cols = c(`Played Some Position`, `Changed Position`))

same_pos_stats <- same_pos_stats %>%
  mutate(perc = paste0(round(same_perc*100, 1), "%"))

same_pos_stats %>%
  mutate(pos = fct_reorder(data.position, -same_perc),
         Decision = name) %>%
  ggplot(aes(fill=Decision, y=value, x=pos)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(x = pos, y = 50, label = perc), size = 5.5) +
  theme_reach() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "bottom") +
  labs(x = "Position in High School",
       y = "# of Recruits",
       title = "Which Positions Experience the Most Change From High School to College",
       subtitle = "Ordered from least change to most change with percent that stayed the same listed")
ggsave('1-rec.png', width = 15, height = 10, dpi = "retina")