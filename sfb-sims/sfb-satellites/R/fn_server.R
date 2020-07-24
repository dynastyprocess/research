# Server functions

get_teamrosters <- function(sfb_picks,user,comparisons){

  users <- c(user,comparisons)

  sfb_picks %>%
    filter(franchise_name %in% users) %>%
    mutate(pos = fct_relevel(pos,c("QB","RB","WR","TE")),
           `Pos ADP` = round(pos_adp,2),
           `Player` = glue("{player_name} {pos} {team}"),
           `Slot` = paste0(pos,pos_slot)) %>%
    arrange(pos,pos_slot) %>%
    select(franchise_name, Slot,`Player`, `Pos ADP`) %>%
    pivot_wider(names_from = franchise_name,
                values_from = c(Player,`Pos ADP`),
                names_glue = "{franchise_name} | {.value}") %>%
    select(Slot,contains(user),contains(comparisons))

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

datatable_myteam <- function(df,user,comparisons){

  users <- c(user,comparisons)

  custom_container <- withTags(table(
    class = "stripe",
    thead(
      tr(
        th(rowspan = 2, "Slot"),
        lapply(users,th,colspan = 2,style = "text-align:center; max-width = 60px;")
      ),
      tr(
        lapply(rep(c("Player","PosADP"),length(users)),th,style = "text-align:center;max-width = 60px;")
      )
    )

  ))

  df %>%
    mutate_if(is.numeric,round,2) %>%
    datatable(rownames = FALSE,
              selection = "none",
              class = "compact stripe nowrap",
              container = custom_container,
              # extensions = "Responsive",
              options = list(
                pageLength = 5,
                columnDefs = list(
                  list(className = 'dt-center',targets = "_all")
                ),
                scrollX = TRUE,
                # scrollY = 300,
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

generate_comparisoninputs <- function(simscores_strategy,simscores_player){

  list_strategy <- simscores_strategy$franchise_name

  list_player <- simscores_player$franchise_name

  tagList(
    pickerInput("comparison_1",
                "Comparison 1",
                choices = list_strategy,
                selected = list_strategy[[1]],
                width = '100%',
                options = list(
                  `live-search` = TRUE,
                  `size` = 10
                )),
    pickerInput("comparison_2",
                "Comparison 2",
                width = '100%',
                choices = list_player,
                selected = list_player[[1]],
                options = list(
                  `live-search` = TRUE,
                  `size` = 10
                ))
  )
}

calculate_pcatable <- function(pca_juice,pca_desc,user,comparisons){

  users <- c(user,comparisons)

  df <- pca_juice %>%
    filter(franchise_name %in% users) %>%
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
    filter(!is.na(effect_description)) %>%
    mutate(effect_label = paste(effect_strength,"tendency for",effect_description))

}

generate_pcachart <- function(pca_data){

  plot <- pca_data %>%
    mutate(component = fct_relevel(component,paste0("PC",8:1))) %>%
    ggplot(aes(x = component, y = value, color = franchise_name, text = effect_label)) +
    geom_point() +
    ylim(-5,5) +
    # hrbrthemes::theme_modern_rc() +
    theme_minimal() +
    scale_color_brewer(palette = "Dark2")+
    coord_flip()

  ggplotly(plot) %>%
    layout(legend = list(orientation = "h", y = -1))

}
