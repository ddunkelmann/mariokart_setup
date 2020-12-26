# Libraries
library(plotly)
library(ggplot2)
library(viridisLite)
library(GGally)

# ------------------ #
# Mario Kart Analysis Visuals
# ------------------ #

# 1. Correlation within the different building parts ####

# _ 1.1 Characters ####

# Creating a ggplot-friendly DF to build a heatmap for the specific attributes
plot_character_class <- characters %>% 
  group_by(group) %>% 
  summarize(speed = mean(speed),
            acceleration = mean(acceleration),
            weight = mean(weight),
            handling = mean(handling),
            traction = mean(traction),
            mini_turbo = mean(mini_turbo)
  ) %>% 
  gather("attribut", "mean", -group) %>% 
  mutate(attribut = factor(attribut, levels = c("weight", "speed", "acceleration", "mini_turbo", "traction", "handling")))

# Used when creating the Heatmap for each character instead of the groups
# character = factor(character, levels = c("Baby Mario", "Baby Luigi","Baby Peach","Baby Daisy","Baby Rosalina","Lemmy Koopa","Mii Light",    
#                                          "Toad"          ,  "Shy Guy"       ,  "Koopa Troopa" ,   "Lakitu"     ,     "Wendy Koopa"    , "Larry Koopa"  ,   "Toadette" ,      
#                                          "Peach"      ,     "Daisy"     ,      "Yoshi"       ,    "Mario"       ,    "Luigi"     ,      "Iggy Koopa"   ,   "Ludwig Koopa" ,  
#                                          "Mii Medium"  ,    "Donkey Kong"  ,   "Waluigi"     ,    "Rosalina"    ,    "Roy Koopa"    ,   "Metal Mario"   ,  "Pink Gold Peach",
#                                          "Wario"     ,      "Bowser"     ,     "Morton Koopa"  ,  "Mii Heavy" )))

# Heatmap of the different Charactergroups and their values
ggplot(plot_character_class, aes(x = attribut, y = group)) +
  geom_tile(aes(fill = mean)) +
  geom_text(aes(label = mean)) +
  
  scale_fill_gradientn(colours = viridis(256, option = "D")) +
  theme_classic() +
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "Attributes Heatmap", subtitle = "Characters", y = NULL, x = NULL)

# The Heatmap shows, that there is a clear correlation between the 6 different Attributes. 
# These being: 
# - Weight and Speed
# - Acceleration, Mini Turbo, Traction, Handling

# Attributes within a group increase together, whereas attributes from different groups do not (if one increases, the other group decreases)
# This means, that you can pick two different types of characters: Heavy and Fast or Small with good Handling

# _ 1.2 Cars ####
# Same Method as above
# The difference of the cars (and other parts next to characters) is that they modify (increase or decrease) the base values given by the Character. 
# The changes are lot smaller and include negative Values

plot_bodies_class <- bodies %>% 
  group_by(comb_id) %>% 
  summarize(speed = mean(speed),
            acceleration = mean(acceleration),
            weight = mean(weight),
            handling = mean(handling),
            traction = mean(traction),
            mini_turbo = mean(mini_turbo)
  ) %>% 
  mutate(sum_diff = speed + acceleration + weight + handling + traction + mini_turbo) %>% 
  gather("attribut", "mean", -c(comb_id, sum_diff)) %>% 
  mutate(attribut = factor(attribut, levels = c("weight", "speed", "acceleration", "mini_turbo", "traction", "handling")))

# Heatmap
ggplot(plot_bodies_class, aes(x = attribut, y = reorder(comb_id, desc(sum_diff)))) +
  geom_tile(aes(fill = mean)) +
  geom_text(aes(label = mean)) +
  
  # Different Colorscale to highlight positive/negative values
  scale_fill_gradient2(high = "#fde725", low = "#440154", mid = "white") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  ) +
  labs(title = "Attributes Heatmap", subtitle = "Cars", y = NULL, x = NULL)



# _ 1.3 Wheels ####
plot_tires_class <- tires %>% 
  select(-body) %>% 
  unique() %>% 
  mutate(sum_diff = speed + acceleration + weight + handling + traction + mini_turbo) %>% 
  gather("attribut", "mean", -c(comb_id, sum_diff)) %>% 
  mutate(attribut = factor(attribut, levels = c("weight", "speed", "acceleration", "mini_turbo", "traction", "handling")))

ggplot(plot_tires_class, aes(x = attribut, y = reorder(comb_id, sum_diff))) +
  geom_tile(aes(fill = mean)) +
  geom_text(aes(label = mean)) +
  
  scale_fill_gradient2(high = "#fde725", low = "#440154", mid = "white") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  ) +
  labs(title = "Attributes Heatmap", subtitle = "Wheels", y = NULL, x = NULL)


# _ 1.4 Gliders ####
plot_gliders_class <- gliders %>% 
  select(-c(body, type)) %>% 
  unique() %>% 
  mutate(sum_diff = speed + acceleration + weight + handling + traction + mini_turbo) %>% 
  gather("attribut", "mean", -c(comb_id, sum_diff)) %>% 
  mutate(attribut = factor(attribut, levels = c("weight", "speed", "acceleration", "mini_turbo", "traction", "handling")))

ggplot(plot_gliders_class, aes(x = attribut, y = reorder(comb_id, sum_diff))) +
  geom_tile(aes(fill = mean)) +
  geom_text(aes(label = mean)) +
  
  scale_fill_gradient2(high = "#fde725", low = "#440154", mid = "white") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  ) +
  labs(title = "Attributes Heatmap", subtitle = "Glider", y = NULL, x = NULL)

# _ 1.5 Takeaways ####
# There is a correlation throughout all of the different parts in a setup. 
# As described above there are two groups of attributes, correlation with each other:
# - Weight and Speed
# - Acceleration, Mini Turbo, Traction, Handling

# Attributes within a group increase together, whereas attributes from different groups do not (if one increases, the other group decreases)
# This means, that you can pick two different types of characters: Heavy and Fast or Small with good Handling

# Scatter für unterschiedliche Kombinationen
# Testen eines multiplen Korrelations diagramm


# 2. Correlation Matrix ####
# Using the function ggpairs() to create a correlatio Matrix of the 6 Attributes available
GGally::ggpairs(cross_df_archetype, columns = 5:10)

# As the visual shows are all of the correlation significant.
# The strongest positive cor. are:
# - Weight x Speed (+0.89)
# - Acceleration x Mini Turbo (+0.822)

# The strongest negative cor. are:
# - Speed x Acceleration (-0.802)
# - Handling x Weight (-0.772)

# This information gives proof to the ideas of archetypes above

# Saving
ggsave("Plots/cor_matrix.pdf", width = 15, height = 15)


# 3. Visualizing Scores ####
# This visualization wants to show how the 1700 combinations are spread across the two archetypes.
# It shows, that more combinations are leaning towards the heavy and fast type, 
#  this could be due to the 3 heavy charactersgroups existing (in contrary to only two small charactergroups)

# Histogram for all scores
ggplot(cross_df_archetype, aes(x = archetype_lvl)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(limits = c(-1.25, 1.25)) +
  theme_minimal() +
  labs(title = "Histogram of Archetype Scores", subtitle = "-1 = Handling and Accel., 1 = Heavy and Speed", x = "Score", y = NULL)


# 4. Further Analysis ####

# Inside of the data_load_clean.R a user could change the scoring to his personal preferences. 
# The DF could get them give him the best setup for his preferences, which can then be found on our -1 to 1 scale. 
# Different testings resulted in various results. This method would lean towards one of the archetypes but would not give the most extreme values. 

# Function that gives a DF with the 3 best Scores
give_personal_best <- function(speed_score, acceleration_score, 
                               weight_score, handling_score, 
                               traction_score, mini_turbo_score) {
  personal_score <- cross_df_archetype %>% 
    mutate(
      custom_score = speed_sum * speed_score + 
        acceleration_sum * acceleration_score +
        weight_sum * weight_score + 
        handling_sum * handling_score + 
        traction_sum * traction_score +
        mini_turbo_sum * mini_turbo_score
    ) %>% 
    slice_max(custom_score, n = 3)
  
  personal_score$position = 1:nrow(personal_score)
  
  return(personal_score)
}

# Example
personal_score <- give_personal_best(5, 1, 1, 0.5, 5, 1)

tibble(personal_score)

# Drawing the best combination into the histogram above
give_personal_plot <- function(speed_score, acceleration_score, 
                               weight_score, handling_score, 
                               traction_score, mini_turbo_score) {
  
  p <- ggplot(cross_df_archetype, aes(x = archetype_lvl)) +
    geom_histogram(bins = 50) +
    scale_x_continuous(limits = c(-1.25, 1.25)) +
    theme_minimal() +
    labs(title = "Histogram of Archetype Scores", subtitle = "-1 = Handling and Accel., 1 = Heavy and Speed | Alpha = Position", 
         x = "Score", y = NULL,
         caption = paste("Speed: ", speed_score, ", Acc.: ", acceleration_score,
                         ", Weight: ", weight_score, ", Handling: ", handling_score,
                         ", Traction: ", traction_score, ", Turbo: ", mini_turbo_score, sep = "")) +
    
    geom_vline(data = personal_score, aes(xintercept = archetype_lvl, alpha = rev(position)), color = "green", size = 1.3) +
    theme(legend.position = "none")
  
  return(p)
}

give_personal_plot(5, 1, 1, 0.5, 5, 1)

ggsave("Plots/personal_score_archetype.pdf", width = 10, height = 6)

# 5. What's next? ####

# Another idea would be to test all of these setups in real races. 
# Then the player himself/herself could change the scoring based on his experiences.
# With that, a player could slowly iterate towards his "perfect" Setup. 
# However, no "one best setup" could be identified, rather a knowledge of the archetypes can be given.


# . #####

