library(tidyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(scales)
library(dplyr)
library(DescTools)
library(patchwork)

folder <- #add path to the folder where the data is stored

  # Read each file individually
df1 <- read.csv(file.path(folder, "combined_workers_q_0.7_s1.csv"))
df2 <- read.csv(file.path(folder, "combined_workers_q_0.7_s2.csv"))
df3 <- read.csv(file.path(folder, "combined_workers_q_0.7_s3.csv"))
df4 <- read.csv(file.path(folder, "combined_workers_q_0.7_s4.csv"))
df5 <- read.csv(file.path(folder, "combined_workers_q_0.7_s5.csv"))

# Add a 'scenario' column to each dataframe, marking the B_S initialisation employed
df1$scenario <- "Scenario 1"
df2$scenario <- "Scenario 2"
df3$scenario <- "Scenario 3"
df4$scenario <- "Scenario 4"
df5$scenario <- "Scenario 5"

# Merge all 5 dataframes into a single one
df <- bind_rows(df1, df2, df3, df4, df5)

#Correct for floating point errors when H=0.5 and tt=ft, as explained in the Appendix
df <- df %>%
  mutate(
    across(
      c(final_global, final_local_static, final_local_dynamic),
      ~ if_else(near(tt, ft) & near(H_prior, 0.5) & scenario %in% c("Scenario 1", "Scenario 2", "Scenario 3"), 1/2, .x),
      .names = "corrected_{.col}"
    )
  )

#Group the posteriors computed globally and sequentially under the single variable "type_dynamics"
df <- df %>%
  pivot_longer(
    cols = c(corrected_final_global, corrected_final_local_static, corrected_final_local_dynamic),
    names_to = "type_dynamics",
    values_to = "corrected_value"
  ) 

#Create a label for every N_S combination
df$N_S <- paste(
  "tt=", df$tt,
  "ft=", df$ft,
  "rand=", df$rand
)

#relabel and reorder scenarios and kinds of sequential processing
df <- df %>%
  mutate(
    scenario = factor(
      scenario,
      levels = c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5"),
      labels = c("No Expectations", "Consistent Reporting",
                 "Erratic Reporting", "As Good As They Come",
                 "Beware Consistency")
    ),
    type_dynamics = factor(
      type_dynamics,
      levels = c("corrected_final_local_dynamic",
                 "corrected_final_local_static",
                 "corrected_final_global"),
      labels = c("Dynamic Sequential", "Static Sequential", "Global")
    )
  )

#generate a new data frame which extends df with attitudes based on k=0.25
df1 <- df %>%
  mutate(
    believe = ifelse(corrected_value > 3/4, "H", ifelse(corrected_value < 1/4, "not H", "neither"))
  )

#generate a new data frame which extends df with attitudes based on k=0.5
df2 <- df %>%
  mutate(
    believe = ifelse(corrected_value > 0.5, "H", ifelse(corrected_value < 0.5, "not H", "neither"))
  )

#summary statistics on df1
summary_text1 <- df1 %>% 
  filter(abs(tt - ft) >= 0.4 & abs(tt - ft) <= 0.5) %>%
  group_by(
    H_prior, N_S, type_dynamics, scenario, believe
  ) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>% 
  complete(
    H_prior, N_S, type_dynamics, scenario, believe = c("H", "not H", "neither"), ,
    fill = list(n = 0)
  ) %>%
  filter(type_dynamics != "final_global") %>%
  group_by(
    H_prior, N_S, type_dynamics, scenario
  ) %>%
  group_modify(~ {
    
    ci <- DescTools::MultinomCI(.x$n, method = "goodman")
    
    bind_cols(
      .x,
      prob_believe = ci[, "est"],
      low = ci[, "lwr.ci"],
      high = ci[, "upr.ci"]
    )
    
  }) %>%
  ungroup()

#summary statistics on df2
summary_text2 <- df2 %>% 
  filter(abs(tt - ft) >= 0.4 & abs(tt - ft) <= 0.5) %>%
  group_by(
    H_prior, N_S, type_dynamics, scenario, believe
  ) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>% 
  complete(
    H_prior, N_S, type_dynamics, scenario, believe = c("H", "not H", "neither"), ,
    fill = list(n = 0)
  ) %>%
  filter(type_dynamics != "final_global") %>%
  group_by(
    H_prior, N_S, type_dynamics, scenario
  ) %>%
  group_modify(~ {
    
    ci <- DescTools::MultinomCI(.x$n, method = "goodman")
    
    bind_cols(
      .x,
      prob_believe = ci[, "est"],
      low = ci[, "lwr.ci"],
      high = ci[, "upr.ci"]
    )
    
  }) %>%
  ungroup()

#function to generate a plot
base_plot <- function(data, min_ci_width = 0) {
  
  ggplot(data,
         aes(x = H_prior, y = prob_believe, color = believe, group = believe)) +
    
    geom_point(size = 2) +
    
    geom_line(linewidth = 1, linetype = "dotted") +
    
    geom_errorbar(
      data = function(d) subset(d, (high - low) > min_ci_width),
      aes(ymin = low, ymax = high),
      width = 0.05,
      linewidth = 0.4,
      colour = "black",
      alpha=0.7
    ) +
    
    scale_color_manual(
      values = c("H" = "#B784C4", "not H" = "#F0A868", "neither" = "grey"),
      breaks = c("H", "not H", "neither"), 
      labels = c("H" = "H", "neither" = "Neither", "not H" = "Not H"),
      name   = "Attitude"
    ) +
    
    scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
    
    labs(
      x = "Prior Probability of H",
      y = "Probability of Attitude"
    ) +
    
    theme_bw() +
    
    theme(
      title = element_text(face="bold", size=15),
      axis.text.x = element_text(angle = 0, hjust = 1, size = 11),
      axis.text.y = element_text(size = 11),
      axis.title.x = element_text( size = 13),
      axis.title.y = element_text( size = 11),
      strip.text = element_text(face ="bold", size = 8.5),
      legend.title = element_text(face = "bold", size = 13),
      legend.text = element_text(size = 12),
      legend.position = "bottom"
    )
}

#generate the plot for k=0.5
p_k <- base_plot(summary_text2 %>% filter( type_dynamics == "Static Sequential" )) +
  facet_grid(N_S ~ scenario) +
  labs(title = "k=0.5") 

#generate the plot for k=0.25
p_kk <- base_plot(summary_text1 %>% filter( type_dynamics == "Static Sequential" )) +
  facet_grid(N_S ~ scenario) +     
  theme(legend.position = "bottom") +
  labs(title = "k=0.25") 

#stack the two plots into a single one
k_overview <- p_k / p_kk +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

