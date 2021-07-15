same_pos_stats <- recruiting_with_waa_season %>%
  filter(!data.position %in% c("K", "LS", "P", "FB")) %>%
  group_by(data.position) %>%
  summarize(total = n(),
            `Played Same Position` = sum(same_pos),
            `Changed Position` = total - `Played Same Position`,
            same_perc = `Played Same Position` / total)

same_pos_stats <- same_pos_stats %>%
  pivot_longer(cols = c(`Played Same Position`, `Changed Position`))

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
       subtitle = "Ordered from least change to most change with percent that stayed the same listed") +
  theme(legend.text=element_text(size=14))
ggsave('1-rec.png', width = 15, height = 10, dpi = "retina")

changed_pos_data <- recruiting_with_waa_season %>%
  filter(same_pos == 0)

change_pos_defense <- changed_pos_data %>%
  filter(data.position %in% c("DI", "ED", "LB", "CB", "S"))

change_pos_defense_stats <- change_pos_defense  %>%
  group_by(data.position, position) %>%
  summarize(count = n()) %>%
  filter(position %in% c("S", "WR", "ED", "G", "DI", "LB", "T", "TE", "FB", "CB"))

change_pos_defense_stats %>%
  mutate(`College \n Position` = position) %>%
  ggplot(aes(x = `College \n Position`, y = count)) +
  geom_bar(aes(fill = `College \n Position`), stat = "identity", color = "black") +
  theme_reach() +
  scale_fill_brewer(palette = "Set3") +
  facet_wrap(~data.position, scales = "free_y") +
  labs(x = "Position Played in College",
       y = "# of Players",
       title = "Where Defensive Players Usually Went During a Position Change",
       subtitle = "High School positions listed on the top with college positions in each bar") +
  theme(legend.position = c(1, 0.12),
        legend.justification = c(1, 0),
        strip.text.x = element_text(size = 14, face = "bold"))
ggsave('2-rec.png', width = 15, height = 10, dpi = "retina")

change_pos_offense <- changed_pos_data %>%
  filter(data.position %in% c("WR", "C", "T", "G", "HB", "QB", "TE"))

change_pos_offense_stats <- change_pos_offense  %>%
  group_by(data.position, position) %>%
  summarize(count = n()) %>%
  filter(position %in% c("G", "CB", "LB", "S", "WR", "TE", "C", "DI", "ED", "T", "HB", "QB"))

change_pos_offense_stats %>%
  mutate(`College \n Position` = position) %>%
  ggplot(aes(x = `College \n Position`, y = count)) +
  geom_bar(aes(fill = `College \n Position`), stat = "identity", color = "black") +
  theme_reach() +
  scale_fill_brewer(palette = "Set3") +
  facet_wrap(~data.position, scales = "free_y") +
  labs(x = "Position Played in College",
       y = "# of Players",
       title = "Where Offensive Players Usually Went During a Position Change",
       subtitle = "High School positions listed on the top with college positions in each bar") +
  theme(legend.position = c(0.8, 0.09),
        legend.justification = c(1, 0),
        strip.text.x = element_text(size = 14, face = "bold"))
ggsave('3-rec.png', width = 15, height = 10, dpi = "retina")

waa_pos_avgs <- recruiting_with_waa_season %>%
  group_by(position) %>%
  summarize(avg_pos_waa = mean(total_waa))

changed_pos_data_waa <- changed_pos_data %>%
  left_join(waa_pos_avgs, by = c("position"))

changed_pos_data_waa <- changed_pos_data_waa %>%
  filter(!position %in% c("FB", "P", "K", "ST", "LS")) %>%
  mutate(waa_oe_pos_avg = total_waa - avg_pos_waa)

changed_pos_data_waa %>%
  filter(waa_oe_pos_avg < 1.1 & waa_oe_pos_avg > -1.1) %>%
  ggplot(aes(x = position, y = waa_oe_pos_avg)) +
  geom_boxplot(aes(fill = position)) +
  geom_hline(yintercept = 0) +
  theme_reach() +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "Position Played in College",
       y = "WAA Over Average of That Position",
       title = "Changing Positions From High School to College is Usually a Losing Bet",
       subtitle = "No position is harder to transition to than quarterback, WAA = Wins Above Average") 
ggsave('4-rec.png', width = 15, height = 10, dpi = "retina")






