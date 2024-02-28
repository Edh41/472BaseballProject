## Might need to change to have you have the csv file called on your computer
## Load data
BBData <- read.csv("Baseball Data - Sheet1-3.csv")

## Packages That might be helpful feel free to add more
library(ggplot2)
library(dplyr)
library(broom)
library(GGally)
library(tidyverse)


## Graft I like -Kooper
# Calculate average salary for each team
avg_salary <- BBData %>%
  group_by(Tm) %>%
  summarise(avg_salary = mean(Salary, na.rm = TRUE)) %>%
  arrange(desc(avg_salary))
ggplot(avg_salary, aes(x = reorder(Tm, avg_salary), y = avg_salary)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = 70000000, linetype = "dashed", color = "red") +  # Add a dashed red line at $70 million
  labs(x = "Team", y = "Average Salary", title = "Average Salaries for Each Team (Highest to Lowest)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
