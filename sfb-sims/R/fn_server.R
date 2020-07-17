# Server functions

calculate_playersims <- function(sfb_picks,user){

  user_picks <- sfb_picks %>%
    filter(franchise_name == user & !is.na(player_name))

  sfb_picks %>%
    mutate(sim_score = 1000*exp(-0.03 * pos_adp)) %>%
    select(division_name,franchise_name,
           player_id,player_name,pos,age,team,
           pos_adp,sim_score) %>%
    semi_join(user_picks,by = c('player_id')) %>%
    group_by(franchise_name) %>%
    summarise(sim_score = sum(sim_score,na.rm=TRUE),
              sim_score = round(sim_score),
              matching_count = n(),
              matching_players = paste(player_name,collapse = "; ")) %>%
    select(franchise_name,sim_score,matching_count,matching_players) %>%
    arrange(desc(sim_score)) %>%
    slice(-1)

}

datatable_playersims <- function(df){
  df %>%
    mutate_if(is.numeric,round,2) %>%
    datatable(rownames = FALSE,
              class = "compact stripe nowrap",
              extensions = "Responsive",
              options = list(
                pageLength = 5,
                scrollX = TRUE
              ))
}

calculate_strategysims <- function(pca_dist,user){

  df <- pca_dist %>%
    select(franchise_name,
           sim_score = {{user}}) %>%
    arrange(sim_score) %>%
    slice(-1)

  df

}

calculate_strategy <- function(pca_juice,user,pca_desc){

  df <- pca_juice %>%
    filter(franchise_name == user) %>%
    pivot_longer(c(-division_name,-franchise_name),names_to = "component") %>%
    mutate(effect_strength = case_when(abs(value) > 2 ~ "Very Strong",
                                       abs(value) > 1 ~ "Strong",
                                       abs(value) > 0.5 ~ "Moderate",
                                       TRUE ~ "Weak"),
           effect_strength = suppressWarnings(fct_relevel(effect_strength,
                                                          c("Very Strong","Strong","Moderate","Weak"))),
           effect_direction = ifelse(value > 0, "Positive","Negative")) %>%
    arrange(desc(abs(value))) %>%
    left_join(pca_desc, by = c("component","effect_direction")) %>%
    filter(!is.na(effect_description))

  browser()

  user_summary <- df %>%
    group_by(effect_strength) %>%
    summarise(desc = paste(effect_description,collapse = "; "))

  paste("The Strategic Similarity model thinks this team has:",
        unlist(paste(user_summary$effect_strength,"tendency for",user_summary$desc)),
        collapse = ", "
        )

  df
}
