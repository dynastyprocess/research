# Server functions

get_team <- function(sfb_picks,user){

  sfb_picks %>%
    filter(franchise_name == user) %>%
    mutate(pos = fct_relevel(pos,c("QB","RB","WR","TE")),
           `Pos ADP` = round(pos_adp,2),
           `Player` = glue("{player_name} {pos} {team}"),
           `Slot` = paste0(pos,pos_slot)) %>%
    arrange(pos,pos_slot) %>%
    select(Slot,`Player`, `Pos ADP`)

}

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
    mutate(franchise_name = str_trunc(franchise_name,60,"left")) %>%
    datatable(rownames = FALSE,
              selection = "none",
              class = "compact stripe nowrap",
              # extensions = "Responsive",
              options = list(
                pageLength = 5,
                scrollX = TRUE,
                info = FALSE,
                ordering = FALSE
              ))
}

datatable_myteam <- function(df){
  df %>%
    mutate_if(is.numeric,round,2) %>%
    datatable(rownames = FALSE,
              selection = "none",
              class = "compact stripe nowrap",
              # extensions = "Responsive",
              options = list(
                pageLength = 5,
                scrollX = TRUE,
                scrollY = 300,
                paging = FALSE,
                ordering = FALSE,
                searching = FALSE,
                info = FALSE
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
    mutate(effect_strength = case_when(abs(value) > 2 ~ "Very strong",
                                       abs(value) > 1 ~ "Strong",
                                       abs(value) > 0.5 ~ "Moderate",
                                       TRUE ~ "Weak"),
           effect_strength = suppressWarnings(fct_relevel(effect_strength,
                                                          c("Very strong","Strong","Moderate","Weak"))),
           effect_direction = ifelse(value > 0, "Positive","Negative")) %>%
    arrange(desc(abs(value))) %>%
    left_join(pca_desc, by = c("component","effect_direction")) %>%
    filter(!is.na(effect_description))

  user_summary <- df %>%
    slice(1:3) %>%
    group_by(effect_strength) %>%
    summarise(desc = paste(effect_description,collapse = "; "))

  # browser()

  c("The Strategic Similarity model thinks this team has:",
        paste("<li>",user_summary$effect_strength,"tendencies for <strong>",user_summary$desc,"</strong>")) %>%
    paste(collapse = " <br> ")

}
