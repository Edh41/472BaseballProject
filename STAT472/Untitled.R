## Might need to change to have you have the csv file called on your computer
## Load data
BBData <- read.csv("Baseball Data - Sheet1-9.csv")

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
  summarise(avg_salary = mean(as.numeric(Salary), na.rm = TRUE)) %>%
  arrange(desc(avg_salary))

avg_SalWin <- BBData %>%
  group_by(Tm) %>%
  summarise(avg_SalWin = mean(as.numeric(SalWin), na.rm = TRUE)) %>%
  arrange(desc(avg_SalWin))

avg_sal_cl <- avg_salary$avg_salary/1000000

ggplot(avg_salary, aes(x = reorder(Tm, avg_sal_cl), y = avg_sal_cl)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = 70, linetype = "dashed", color = "red") +  # Add a dashed red line at $70 million
  labs(x = "Teams", y = "Average Salary in Millions ($)", title = "Average Salaries for Each Team (Lowest to Highest)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(avg_SalWin, aes(x = reorder(Tm, avg_SalWin), y = avg_SalWin)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Teams", y = "Average Money Spent per Win", title = "Average Money Spent per Win (Lowest to Highest)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



summary(avg_salary)

BBData %>%
  transmute(Tm = Tm, WinRate = W/G) %>%
  mutate(LossRate = 1 - WinRate) %>%
  pivot_longer(!Tm) %>% 
  ggplot(aes(fill=name, y=value, x=Tm)) +
  geom_bar(position="fill",stat="identity") +
  labs(x = "Teams", y = "Win/Loss Rate", title = "Win Loss Ratio") +
  guides(fill=guide_legend(title="Key")) +
  theme_bw() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
