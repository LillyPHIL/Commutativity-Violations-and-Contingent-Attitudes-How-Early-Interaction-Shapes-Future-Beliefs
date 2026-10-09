library(tidyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(dplyr)
library(patchwork)

#import the data
folder <- #add path to the folder where the data is stored


# Read each file individually
df1 <- read.csv(file.path(folder, "combined_workers_q_0.7_s1.csv"))
df2 <- read.csv(file.path(folder, "combined_workers_q_0.7_s2.csv"))
df3 <- read.csv(file.path(folder, "combined_workers_q_0.7_s3.csv"))
df4 <- read.csv(file.path(folder, "combined_workers_q_0.7_s4.csv"))
df5 <- read.csv(file.path(folder, "combined_workers_q_0.7_s5.csv"))

# Add a 'scenario' column to each marking the B_S initialisation employed
df1$scenario <- "Scenario 1"
df2$scenario <- "Scenario 2"
df3$scenario <- "Scenario 3"
df4$scenario <- "Scenario 4"
df5$scenario <- "Scenario 5"


# Merge all 5 dataframes in a single one
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

#Group all 
df <- df %>%
  pivot_longer(
    cols = c(corrected_final_global, corrected_final_local_static, corrected_final_local_dynamic),
    names_to = "type_dynamics",
    values_to = "value"
  )

#Create a label for every N_S combination
df$N_S <- paste(
  "tt=", df$tt,
  "ft=", df$ft,
  "rand=", df$rand
)


#Based on the (corrected) value, generate a column checking which attitude the agent has formed
df <- df %>%
  mutate(
    believe = ifelse(value > 1/2, "H", ifelse(value < 1/2, "not H", "neither"))
  )

#Summary statistics 
summary_all <- df %>% 
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
  filter(type_dynamics != "corrected_final_global") %>%
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

#function to generate the plot checking whether the attitude is contingent
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
      title = element_text(face="bold", size=20),
      axis.text.x = element_text(angle = 0, hjust = 1, size = 11),
      axis.text.y = element_text(size = 11),
      axis.title.x = element_text( size = 13),
      axis.title.y = element_text( size = 13),
      strip.text = element_text(face ="bold", size = 10),
      legend.title = element_text(face = "bold", size = 13),
      legend.text = element_text(size = 12),
      legend.position = "bottom"
    )
}

#generate plot by filtering the type_dynamics variable. Either "corrected_final_local_dynamic" or "corrected_final_local_static"
p_all <- base_plot(summary_all %>% filter(type_dynamics == "corrected_final_local_dynamic" )) +
  facet_grid(factor(scenario, labels = c("No Expectations", "Consistent Reporting", 
                                         "Erratic Reporting", 
                                         "As Good As They Come", "Beware Consistency")) ~ N_S) + 
  labs(title = "Sequential [add type here] Update") 

p_all
