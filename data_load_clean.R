# Libraries
library(tidyverse)
library(readr)

# ------------------------- #
# Mario Kart 8 Charakter and Combinations Analysis
# ------------------------- #


# 1. Functions ####
# -- Rewriting all Columns to Lowercase

all_col_toLower <- function(df) {
  for(n in 1:ncol(df)) {
    colnames(df)[n] <- tolower(colnames(df)[n])
  }
  
  return(df)
}

# 2. Loading the Data ####
# _ 2.1 Characters ####

# Load Data
csv_characters <- read_csv("Data/characters.csv")
tibble(csv_characters)

# Clean the Dataframe (different Speed and Handling Values are weighted approxamitely to their share in each track)
clean_characters <- csv_characters %>% 
  mutate(Speed = `Speed (Ground)` * 0.8 + `Speed (Air)` * 0.1 + `Speed (Water)` *0.1,
         Handling = `Handling(Ground)` * 0.8 + `Handling (Air)` * 0.1 + `Handling (Water)` *0.1) %>% 
  select(-c(starts_with("Speed ("), starts_with("Handling ("), "Handling")) %>% 
  rename("mini_turbo" = "Mini Turbo",
         "handling" = "Handling(Ground)")

clean_characters <- all_col_toLower(clean_characters)

# DF giving identical Character "Types"
unique_characters <- clean_characters %>% 
  select(-character) %>% 
  unique() %>% # Testing for duplicates (based on values)
  mutate(paste_id = paste0(speed, acceleration, weight, handling, traction, mini_turbo, sep = "_")) %>% # ID for the matching (next Step)
  select(paste_id)

# Giving each individual group a name
unique_characters$group <- c("Light_1", "Light_2", "Medium_1", "Medium_2", "Heavy_1", "Heavy_2", "Heavy_3")

# Join the unique Groups to the DF
characters <- clean_characters %>% 
  mutate(paste_id = paste0(speed, acceleration, weight, handling, traction, mini_turbo, sep = "_"),
         class = factor(class, levels = c("Light", "Medium", "Heavy"))) %>% 
  left_join(unique_characters, by = "paste_id") %>% 
  select(-paste_id) %>% 
  mutate(group = factor(group, levels = c("Light_1", "Light_2", "Medium_1", "Medium_2", "Heavy_1", "Heavy_2", "Heavy_3")))

# Clean the Workspace
rm(clean_characters, csv_characters)
# _ 2.2 Karts ####

# Identical method as before. Tables have small differences which are individually corrected.
csv_bodies <- read_csv("Data/bodies.csv")

clean_bodies <- csv_bodies %>% 
  rename("mini_turbo" = "Mini Turbo",
         "handling" = "Handling")

clean_bodies <- all_col_toLower(clean_bodies)

unique_bodies_comb <- clean_bodies %>% 
  select(-vehicle) %>% 
  unique() %>% 
  mutate(paste_id = paste(speed, acceleration, weight, handling, traction, mini_turbo, sep = "_")) %>% 
  select(paste_id)

# Giving Groups ID, not Names since their classification is not as easy as for the characters
unique_bodies_comb$comb_id <- 1:18

bodies <- clean_bodies %>% 
  mutate(paste_id = paste(speed, acceleration, weight, handling, traction, mini_turbo, sep = "_")) %>% 
  left_join(unique_bodies_comb, by = "paste_id") %>% 
  select(-paste_id)

rm(csv_bodies, clean_bodies)

# _ 2.3 Tires ####

csv_tires <- read_csv("Data/tires.csv")

clean_tires <- csv_tires %>% 
  mutate(Speed = `Speed (Ground)` * 0.8 + `Speed (Air)` * 0.1 + `Speed (Water)` *0.1,
         Handling = `Handling(Ground)` * 0.8 + `Handling (Air)` * 0.1 + `Handling (Water)` *0.1) %>% 
  select(-contains("(")) %>% 
  rename("mini_turbo" = "Mini Turbo",
         "handling" = "Handling")

clean_tires <- all_col_toLower(clean_tires)

unique_tires_comb <- clean_tires %>% 
  select(-body) %>% 
  unique() %>% 
  mutate(paste_id = paste(speed, acceleration, weight, handling, traction, mini_turbo, sep = "_")) %>% 
  select(paste_id)

unique_tires_comb$comb_id <- 1:7

tires <- clean_tires %>% 
  mutate(paste_id = paste(speed, acceleration, weight, handling, traction, mini_turbo, sep = "_")) %>% 
  left_join(unique_tires_comb, by = "paste_id") %>% 
  select(-paste_id)

rm(csv_tires, clean_tires)

# _ 2.4 Glider ####

csv_gliders <- read_csv("Data/gliders.csv")

clean_gliders <- csv_gliders %>% 
  mutate(Speed = `Speed (Ground)` * 0.8 + `Speed (Air)` * 0.1 + `Speed (Water)` *0.1,
         Handling = `Handling(Ground)` * 0.8 + `Handling (Air)` * 0.1 + `Handling (Water)` *0.1) %>% 
  select(-contains("(")) %>% 
  rename("mini_turbo" = "Mini Turbo",
         "handling" = "Handling")

clean_gliders <- all_col_toLower(clean_gliders)

unique_gliders_comb <- clean_gliders %>% 
  select(-c(body, type)) %>% 
  unique() %>% 
  mutate(paste_id = paste(speed, acceleration, weight, handling, traction, mini_turbo, sep = "_")) %>% 
  select(paste_id)

unique_gliders_comb$comb_id <- 1:2


gliders <- clean_gliders %>% 
  mutate(paste_id = paste(speed, acceleration, weight, handling, traction, mini_turbo, sep = "_")) %>% 
  left_join(unique_gliders_comb, by = "paste_id") %>% 
  select(-paste_id)

rm(csv_gliders, clean_gliders)

# 3. Creating all Combinations through crossing() ####

# Creating all Combinations based on their Groud IDs --> 1764 Combinations
cross_df <- crossing(char = unique_characters$group, 
                                 bodies = unique_bodies_comb$comb_id,
                                 tires = unique_tires_comb$comb_id,
                                 glider = unique_gliders_comb$comb_id) %>% 
  
  # Joining the remaining information to the combinations above
  # Cars
  left_join(bodies, by = c("bodies" = "comb_id")) %>% 
  select(-vehicle) %>% 
  unique() %>% 
  
  # Characters
  left_join(characters, by = c("char" = "group"), suffix = c("_body", "_char")) %>% 
  select(-c(character, class)) %>% 
  unique() %>% 
  
  # Tires
  left_join(tires, by = c("tires" = "comb_id")) %>% 
  select(-body) %>% 
  unique() %>% 
  
  # Gliders
  left_join(gliders, by = c("glider" = "comb_id"), suffix = c("_tires", "_glider")) %>% 
  select(-c(type, body)) %>% 
  unique() %>% 
  
  # Creating aggregated values for the main stats
  mutate(speed_sum = speed_body + speed_char + speed_tires + speed_glider,
         acceleration_sum = acceleration_body + acceleration_char +acceleration_tires + acceleration_glider,
         weight_sum = weight_body + weight_char + weight_tires + weight_glider,
         handling_sum = handling_body + handling_char + handling_tires + handling_glider,
         traction_sum = traction_body + traction_char + traction_tires + traction_glider,
         mini_turbo_sum = mini_turbo_body + mini_turbo_char + mini_turbo_tires + mini_turbo_glider) %>% 
  
  # Removing the single columns of each part
  select(-c(ends_with("_body"), ends_with("_char"), ends_with("_tires"), ends_with("_glider")))

tibble(cross_df)

rm(unique_bodies_comb, unique_characters, unique_tires_comb, unique_gliders_comb)
# 4. Creating a formula for weighted evaluation of the combinations. #####

# The custom scores are based on personal preference. 
# A player might prefer a fast combination over a combination with good handling and control. 
# Even though there is a clear pattern of which archetypes are present within the game, A player can prefer speed and handling. 
# The data will try and give him a a score based on his values
cross_df_archetype <- cross_df %>% 
  mutate(
        # Archetype Levels are a numeric value, describing how much a certain combination represents one of the archetypes. 
         # -1 represents the small, control type, whereas 1 represents a fast and heavy combination
         # Version 1 is based on true values
         archetype_lvl = (0 - (acceleration_sum + handling_sum + traction_sum + mini_turbo_sum) / 4
                           + (speed_sum + weight_sum) / 2) / 4,
         # Version 2 is based on the difference to the mean (average)
         archetype_lvl_mean = (0 - ((acceleration_sum - mean(acceleration_sum)) + 
                                    (handling_sum  - mean(handling_sum)) +
                                    (traction_sum - mean(handling_sum)) +
                                    (mini_turbo_sum - mean(mini_turbo_sum))) / 4 
                                 + ((speed_sum - mean(speed_sum)) +
                                    (weight_sum - mean(weight_sum))) / 2) /4)
  
rm(cross_df)




# . #####