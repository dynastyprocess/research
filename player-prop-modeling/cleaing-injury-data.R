# Injury Report Data

clean_injury_field <- function(injury_field) {
  
  case_when(
    str_detect(injury_field, "(Not injury related)|rest|Rest|(Load Management)|(Not Injury Related)|Travel") |
      is.na(injury_field) ~
      "No Injuries",
    str_detect(injury_field, "hand|Hand|finger|Finger|thumb|Thumb|Wrist") ~ "Hand/Wrist",
    str_detect(injury_field, "Illness|COVID|Covid") ~ "Illness",
    str_detect(
      injury_field,
      "Knee|knee|Hamstring|Calf|Thigh|Quad|Groin|groin|Hip|Achilles|Shin|Fibula|Glute|Butt|Adductor|calve|calf|Leg|leg|Pelvis") ~
      "Leg/Hip",
    
    str_detect(injury_field, "Ankle|ankle|Foot|Toe|Heel|Feet") ~ "Foot/Ankle",
    str_detect(injury_field, "Back|Rib|Abdomen|Oblique|Core|Stomach") ~ "Stomach/Back/Rib",
    str_detect(injury_field, "Neck|Concussion|Head") ~ "Head/Neck",
    str_detect(injury_field,
               "Arm|arm|Elbow|Chest|Shoulder|shoulder|Pectoral|Bicep|Tricep|Collarbone|Clavicle|tricep") ~
      "Arm/Chest",
    TRUE ~ "Other")
  
}

injury_data <- nflreadr::load_injuries(seasons = 2012:2021) %>%
  filter(position %in% c("QB","WR","RB","TE"),
         !(full_name == "Martellus Bennett" & week == 10 & season == 2017 & team == "GB")) %>%
  mutate(merge_name = clean_player_names(full_name),
         merge_name = case_when(merge_name == "Charles D Johnson" ~ "Charles Johnson",
                                merge_name == "Bisi Johnson" ~ "Olabisi Johnson",
                                merge_name == "William Fuller" ~ "Will Fuller",
                                TRUE ~ merge_name),
         position = case_when(merge_name == "JD McKissic" ~ "RB",
                              merge_name == "Ty Montgomery" ~ "RB",
                              merge_name == "Dexter McCluster" ~ "RB",
                              merge_name == "Lynn Bowden" ~ "RB",
                              TRUE ~ position),
         injury_team = clean_team_abbrs(team),
         # Zach Ertz gets traded
         report_status = case_when(merge_name == "Zach Ertz" & week == 6 & season == 2021 ~ NA_character_,
                                   TRUE ~ report_status),
         practice_status = if_else(practice_status == "Out (Definitely Will Not Play)",
                                   "Did Not Participate In Practice",
                                   practice_status),
         report_primary_injury_cleaned = clean_injury_field(report_primary_injury),
         report_secondary_injury_cleaned = clean_injury_field(report_secondary_injury),
         practice_primary_injury_cleaned = clean_injury_field(practice_primary_injury),
         practice_secondary_injury_cleaned = clean_injury_field(practice_secondary_injury)
  )