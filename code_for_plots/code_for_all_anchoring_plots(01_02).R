library(tidyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(ggridges)
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

#Based on the (corrected) value, check whether the attitude formed favours H or not
df <- df %>%
  mutate(
    believe = ifelse(value > 1/2, 1, 0)
  )

#Create a column checking the first value of each report sequence
df <- df %>%
  mutate(
    first_val = {
      cleaned <- gsub("\\[|\\]|\\s", "", base_sequence)
      mat <- do.call(rbind, strsplit(cleaned, ","))
      mat <- matrix(as.integer(mat), nrow = nrow(mat))
      mat[, 1]
    }
  )

#Summary statistics 
summary_df <- df %>%
  group_by(H_prior, N_S, type_dynamics, first_val, scenario) %>%
  summarise(
    proportion = mean(believe),
    n = n(),
    z = 1.96,
    lower = (proportion + z^2 / (2 * n) -
               z * sqrt(proportion * (1 - proportion) / n + z^2 / (4 * n^2))) /
      (1 + z^2 / n),
    upper = (proportion + z^2 / (2 * n) +
               z * sqrt(proportion * (1 - proportion) / n + z^2 / (4 * n^2))) /
      (1 + z^2 / n),
    .groups = "drop"
  )

#function to generate the plot checking whether the likelihood that one favours H depends on the first item observed
make_plot <- function(data) {
  

  ggplot(data, aes(x = H_prior, y = proportion,
                   colour = factor(first_val),
                   group = first_val)) +
    
    geom_point(size = 1.1) +
    geom_errorbar(aes(ymin = lower, ymax = upper, width = 0.1)) +
 
    scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
    scale_y_continuous(trans = scales::pseudo_log_trans(base = 10, sigma = 1)) +
    scale_colour_manual(
      values = c(
        "0" = "#0072B2",  # blue
        "1" = "#E69F00"   # orange
      ), labels = c("0" = "¬Rep", "1" = "Rep")
    ) +
    labs(y = "Probability of Favouring H", x = "Prior Probability of H", colour = "First Element in the Sequence") +
    
    theme_bw() +
    
    theme(
      title=element_text(size=20,face = "bold"),
      axis.text.x = element_text(angle = 0, hjust = 1, size = 13),
      axis.text.y = element_text(size = 13),
      axis.title.x = element_text( size = 13),
      axis.title.y = element_text( size = 13),
      strip.text = element_text(face = "bold",size = 11),
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 11),
      legend.position = "bottom"
    )}


#generate plot by filtering the type_dynamics variable. Either "corrected_final_local_dynamic" or "corrected_final_local_static"
plot <- make_plot(summary_df %>% filter(type_dynamics == "corrected_final_local_static")) +    
  facet_grid(factor(scenario, labels = c("No Expectations", "Consistent Reporting", 
                        "Erratic Reporting", 
                        "As Good As They Come", "Beware Consistency")) ~ N_S) +
  labs(title = "Sequential [add type here] Update") 

