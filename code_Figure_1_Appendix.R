library(tidyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(scales)


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

#Group the posteriors computed globally and sequentially under the single variable "type_dynamics
df_long <- df %>%
  pivot_longer(
    cols = c(corrected_final_global, corrected_final_local_static, corrected_final_local_dynamic),
    names_to = "type_dynamics",
    values_to = "value"
  )

#Create a label for every N_S combination
df_long$N_S <- paste(
  "tt=", df_long$tt,
  "ft=", df_long$ft,
  "rand=", df_long$rand
)

#Based on the (corrected) value, calculate the absolute distance from 0.5
df_long$ext <-  with(df_long, abs(value - 0.5))

#Based on the (corrected) value, check whether the attitude formed favours H or not
df_long <- df_long %>%
  mutate(
    believe = ifelse(value > 0.5, "H", ifelse(value < 0.5, "not H", "neither"))
  )

#Restrict to data for sequential processing
df_long <- df_long %>% filter(type_dynamics != "corrected_final_global")

#relabel and reorder scenarios and kind of sequential processing
df_long <- df_long %>%
  mutate(
    scenario = factor(
      scenario,
      levels = sort(unique(scenario)),   
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

#generate plot
ggplot(df_long, aes(x=as.factor(H_prior), y=ext)) +
  geom_jitter(
    position = position_jitterdodge(
      dodge.width = 0.9,
      jitter.width = 0.6,
      jitter.height = 0,
    ),
    size = 1
  ) +

  xlab("Prior Probability of H") +
  facet_grid(
    scenario ~ 
      type_dynamics
  ) +
  theme_bw() +
  
  labs(
    x = "Prior probability of H",
    y = "Absolute Distance from 0.5",
  ) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text( size = 13),
    axis.title.y = element_text( size = 13),
    strip.text = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.position = "bottom"
  )

