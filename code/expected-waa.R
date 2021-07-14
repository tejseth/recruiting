library(glmnet)

colSums(is.na(recruiting_with_waa))

rec_join <- recruiting_with_waa %>%
  filter(!data.position %in% c("K", "P", "LS", "FB"))

rec_model_data <- recruiting_with_waa %>%
  filter(!data.position %in% c("K", "P", "LS", "FB")) %>%
  select(total_waa, data.position, data.rating)

rec_model_data$data.position <- as.factor(rec_model_data$data.position)

matrix_x <- rec_model_data %>%
  dplyr::select(-total_waa) %>%
  as.matrix()

model_matrix_x <- model.matrix(total_waa ~ ., rec_model_data)[,-1]

model_y <- rec_model_data$total_waa

init_reg_fit <- lm(total_waa ~ ., rec_model_data)
summary(init_reg_fit)

library(broom)

tidy(init_reg_fit) %>%
  mutate(coef_sign = as.factor(sign(estimate)),
         term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = term, y = estimate, fill = coef_sign)) +
  geom_bar(stat = "identity", color = "white") +
  theme_reach() +
  scale_fill_manual(values = c("darkred", "darkblue"), guide = FALSE) +
  coord_flip()

init_ridge_fit <- glmnet(model_matrix_x, model_y, alpha = 0)

plot(init_ridge_fit, xvar = "lambda")

fit_ridge_cv <- cv.glmnet(model_matrix_x, model_y, alpha = 0)

tidy_ridge_coef <- tidy(fit_ridge_cv$glmnet.fit)

tidy_ridge_coef %>%
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fit_ridge_cv$lambda.min) +
  geom_vline(xintercept = fit_ridge_cv$lambda.1se,
             linetype = "dashed", color = "red") +
  theme_reach()

tidy_ridge_cv <- tidy(fit_ridge_cv)

tidy_ridge_cv %>%
  ggplot(aes(x = lambda, y = estimate)) +
  geom_line() + scale_x_log10() +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.25) +
  geom_vline(xintercept = fit_ridge_cv$lambda.min) +
  geom_vline(xintercept = fit_ridge_cv$lambda.1se,
             linetype = "dashed", color = "red") +
  theme_reach()

tidy_ridge_coef %>%
  mutate(coef_sign = as.factor(sign(estimate)),
         term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = term, y = estimate, fill = coef_sign)) +
  geom_bar(stat = "identity", color = "white") +
  theme_reach() +
  scale_fill_manual(values = c("darkred", "darkblue"), guide = FALSE) +
  coord_flip()

fit_lasso_cv <- cv.glmnet(model_matrix_x, model_y, 
                          alpha = 1)

matrix_x <- rec_model_data %>%
  dplyr::select(-total_waa) %>%
  as.matrix()

response_y <- rec_model_data$total_waa

set.seed(2020)
fold_id <- sample(rep(1:10, length.out = nrow(matrix_x)))

cv_en_25 <- cv.glmnet(model_matrix_x, model_y, foldid = fold_id, alpha = .25)
cv_en_50 <- cv.glmnet(model_matrix_x, model_y, foldid = fold_id, alpha = .5)
cv_ridge <- cv.glmnet(model_matrix_x, model_y, foldid = fold_id, alpha = 0)
cv_lasso <- cv.glmnet(model_matrix_x, model_y, foldid = fold_id, alpha = 1)

cv_en_25$cvm
which.min(c(min(cv_en_25$cvm), min(cv_en_50$cvm), min(cv_ridge$cvm), min(cv_lasso$cvm)))

library(broom)

tidy(cv_ridge) %>%
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = cv_ridge$lambda.min) +
  geom_vline(xintercept = cv_ridge$lambda.1se, 
             linetype = "dashed", 
             color = "red") +
  scale_x_log10() + 
  theme_bw()

rec_model_data <- rec_model_data %>% 
  mutate(test_fold = sample(rep(1:5, length.out = n())))

ridge_model <- cv.glmnet(model_matrix_x, rec_model_data$total_waa, alpha = 0)
ridge_preds = as.numeric(predict(ridge_model, newx = model_matrix_x))

rec_proj_waa <- cbind(rec_join, ridge_preds)

teams_colors_logos <- read_csv("https://raw.githubusercontent.com/saiemgilani/cfbfastR-data/master/teams/teams_colors_logos.csv")

teams_colors_logos <- teams_colors_logos %>%
  mutate(data.team = case_when(
    school == "Hawai'i" ~ "Hawaii",
    school == "Louisiana Monroe" ~ "Louisiana-Monroe",
    school == "UMass" ~ "Massachusetts",
    school == "Miami" ~ "Miami (FL)",
    school == "Ole Miss" ~ "Mississippi",
    school == "NC State" ~ "North Carolina State",
    school == "Southern Mississippi" ~ "Southern Miss",
    school == "South Florida" ~ "USF",
    school == "UT San Antonio" ~ "UTSA"
    TRUE ~ school
  ))

school_waa_stats <- rec_proj_waa %>%
  filter(season > 2013) %>%
  group_by(data.team) %>%
  summarize(croots = n(),
            exp_waa = round(sum(ridge_preds), 3),
            actual_waa = round(sum(total_waa), 3),
            waa_oe = actual_waa - exp_waa,
            waa_oe_per = waa_oe / croots) %>%
  filter(croots > 5) %>%
  left_join(teams_colors_logos, by = c("data.team"))

school_waa_stats <- school_waa_stats %>%
  mutate(conference = ifelse(data.team == "Notre Dame", "ACC", conference),
         conference = ifelse(data.team == "Army", "Mid-American", conference),
         conference = ifelse(data.team == "Navy", "Mid-American", conference))

p_5 <- c("SEC", "Pac-12", "Big 12", "ACC", "Big Ten")
g_5 <- c("American Athletic", "Conference USA", "Mid-American", "Mountain West", "Sun Belt")

p5_waa <- school_waa_stats %>%
  filter(conference %in% p_5)

p5_waa %>%
  ggplot(aes(x = exp_waa, y = actual_waa)) +
  geom_image(aes(image = logo), asp = 16/9, size = 0.04) +
  theme_reach() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_hline(yintercept = mean(p5_waa$actual_waa), linetype = "dashed") +
  geom_vline(xintercept = mean(p5_waa$exp_waa), linetype = "dashed") +
  labs(x = "Expected WAA From <span style= 'color:red'>Recruiting</span>",
       y = "Actual WAA From <span style= 'color:blue'>Playing</span>",
       title = "How Power 5 Schools <span style= 'color:red'>Recruit</span> and <span style= 'color:blue'>Develop</span> Talent, 2014-2020",
       subtitle = "WAA = Wins Above Average, expected WAA determined by a ridge regression formula") +
  theme(plot.title = element_markdown(size = 20, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 13, hjust = 0.5),
        axis.title.x = element_markdown(size=16),
        axis.title.y = element_markdown(size=16),
        axis.text = element_markdown(size = 8))
ggsave('10-rec.png', width = 15, height = 10, dpi = "retina")

g5_waa <- school_waa_stats %>%
  filter(conference %in% g_5)

g5_waa %>%
  ggplot(aes(x = exp_waa, y = actual_waa)) +
  geom_image(aes(image = logo), asp = 16/9, size = 0.04) +
  theme_reach() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_hline(yintercept = mean(g5_waa$actual_waa), linetype = "dashed") +
  geom_vline(xintercept = mean(g5_waa$exp_waa), linetype = "dashed") +
  labs(x = "Expected WAA From <span style= 'color:red'>Recruiting</span>",
       y = "Actual WAA From <span style= 'color:blue'>Playing</span>",
       title = "How Group of 5 Schools <span style= 'color:red'>Recruit</span> and <span style= 'color:blue'>Develop</span> Talent, 2014-2020",
       subtitle = "WAA = Wins Above Average, expected WAA determined by a ridge regression formula") +
  theme(plot.title = element_markdown(size = 20, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 13, hjust = 0.5),
        axis.title.x = element_markdown(size=16),
        axis.title.y = element_markdown(size=16),
        axis.text = element_markdown(size = 8))
ggsave('11-rec.png', width = 15, height = 10, dpi = "retina")

season_waa_stats <- rec_proj_waa %>%
  filter(season > 2013) %>%
  group_by(data.team, season) %>%
  summarize(croots = n(),
            exp_waa = round(sum(ridge_preds), 3),
            actual_waa = round(sum(total_waa), 3),
            waa_oe = actual_waa - exp_waa,
            waa_oe_per = waa_oe / croots) %>%
  filter(croots > 5) %>%
  left_join(teams_colors_logos, by = c("data.team")) %>%
  filter(conference %in% p_5)

season_waa_stats <- season_waa_stats %>%
  mutate(abbr = paste0(abbreviation, substring(season, 3,4)))

season_waa_stats %>% 
  ggplot(aes(x = exp_waa, y = waa_oe)) +
  geom_jitter(aes(fill = color, color = alt_color, size = croots), shape = 21, alpha = 0.7) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  geom_smooth(method = "lm", color = "black", size = 1.75) +
  ggrepel::geom_text_repel(aes(label = abbr), max.overlaps = 2, size = 4) +
  theme_reach() +
  labs(x = "Recruiting Score",
       y = "Development Score",
       title = "Power 5 Colleges Who Recruit Well Usually Develop Their Classes Well Also",
       subtitle = "Every Power 5 recruiting class since 2014, class is labeled by team and year")
ggsave('12-rec.png', width = 15, height = 10, dpi = "retina")

season_waa_stats %>% 
  ggplot(aes(x = croots, y = waa_oe_per)) +
  geom_jitter(aes(fill = color, color = alt_color, size = croots), shape = 21, alpha = 0.7) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  geom_smooth(method = "lm", color = "black", size = 1.75) +
  ggrepel::geom_text_repel(aes(label = abbr), max.overlaps = 2, size = 4) +
  theme_reach() +
  labs(x = "Number of Recruits",
       y = "WAA Over Expected Per Recruit",
       title = "A Recruiting Class' Size Has Some Affect on Development",
       subtitle = "Every Power 5 recruiting class since 2014, class is labeled by team and year")
ggsave('13-rec.png', width = 15, height = 10, dpi = "retina")

