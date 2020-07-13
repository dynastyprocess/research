# remotes::install_github('dynastyprocess/ffscrapr')

library(ffscrapr)
library(tidyverse)
library(tidymodels)
library(tidytext)

base_conn <- mfl_connect(2020,rate_limit_number = 2,rate_limit_seconds = 3,user_agent = "dynastyprocess/script")

sfb_leagues <- mfl_getendpoint(base_conn,"leagueSearch", SEARCH = "#SFBX Conference") %>%
  pluck("content","leagues","league") %>%
  tibble() %>%
  unnest_wider(1) %>%
  mutate(conn = map(id,~mfl_connect(2020,
                                    .x,
                                    rate_limit_number = 2,
                                    rate_limit_seconds = 3,
                                    user_agent = "dynastyprocess/script")))

df_sfbpicks <- map_dfr(sfb_leagues$conn,ff_draft) %>%
  filter(!is.na(player_name)) %>%
  group_by(division_name,pos) %>%
  mutate(pos_adp = row_number()) %>%
  ungroup() %>%
  group_by(player_id,player_name,pos,team,age) %>%
  mutate(pos_adp = mean(pos_adp,na.rm = TRUE),
         count = n()) %>%
  ungroup() %>%
  group_by(franchise_name,pos) %>%
  mutate(pos_slot = rank(pos_adp)) %>%
  ungroup()


user <- "TanHo"

user_picks <- df_sfbpicks %>%
  filter(str_detect(franchise_name,user) & !is.na(player_name))

sim_scores <- df_sfbpicks %>%
  mutate(sim_score = 1000*exp(-0.01 * pos_adp)) %>%
  select(timestamp,division_name,round,pick,franchise_name,player_id,player_name,pos,age,team,pos_adp,count,sim_score) %>%
  semi_join(user_picks,by = c('player_id')) %>%
  group_by(franchise_name) %>%
  summarise(total_sim_score = sum(sim_score,na.rm=TRUE),
            matching_count = n(),
            matching_players = paste(player_name,collapse = " | ")) %>%
  arrange(desc(total_sim_score))

posadp_wide <- df_sfbpicks %>%
  select(division_name,franchise_name,pos,pos_slot,pos_adp) %>%
  mutate(pos = fct_relevel(pos,c("QB","RB","WR","TE"))) %>%
  arrange(franchise_name,pos,pos_slot) %>%
  filter((pos %in% c("QB","TE") & pos_slot <=2) | (pos %in% c("RB","WR") & pos_slot <=4)) %>%
  pivot_wider(names_from = c(pos,pos_slot),
              names_sep = "",
              values_fill = 100,
              values_from = pos_adp) %>%
  select(division_name,franchise_name,starts_with("QB"),starts_with("RB"),starts_with("WR"),starts_with("TE"))

# PCA

pca_rec <- recipe(~.,data = posadp_wide) %>%
  update_role(division_name,franchise_name, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(),threshold = 0.8)

pca_prep <- prep(pca_rec)

tidied_pca <- tidy(pca_prep,2)

pca_varexplained <- pca_prep %>%
  pluck("steps",2,"res","sdev") %>%
  as_tibble_col("sdev") %>%
  mutate(component = unique(tidied_pca$component),
         percent_var = sdev^2/sum(sdev^2),
         cumulative_var = cumsum(percent_var))


tidied_pca %>%
  filter(component %in% paste0("PC",1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value,terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component,nrow = 1) +
  labs(y = NULL)

tidied_pca %>%
  filter(component %in% paste0("PC", 1:6)) %>%
  group_by(component) %>%
  top_n(8, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = ifelse(value > 0,"Late","Early"))) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_y_reordered() +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Early or Late"
  )
  # hrbrthemes::theme_modern_rc()

pca_juice <- juice(pca_prep)

pca_dist <- pca_juice %>%
  select(-division_name,-franchise_name) %>%
  dist() %>%
  as.matrix(nrow = nrow(pca_juice)) %>%
  as_tibble() %>%
  set_names(pca_juice$franchise_name) %>%
  bind_cols(franchise_name = pca_juice$franchise_name,.)

pca_sims <- pca_dist %>%
  select(franchise_name,contains(user)) %>%
  arrange(across(contains(user)))

juice(pca_prep) %>%
  # filter(abs(PC1)>4& abs(PC2)>2) %>%
  ggplot(aes(PC3, PC4, label = franchise_name)) +
  geom_point(aes(color = division_name), alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward") +
  labs(color = NULL) +
  hrbrthemes::theme_ft_rc(base_family = 'Arial') +
  theme(legend.position = 'none')

