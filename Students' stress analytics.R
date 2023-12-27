# Record Values
install.packages("tidyverse")
library(tidyverse)
df <- read_csv("D:/ME/Data Analytics Projects/Stress/Student Stress Factors.csv")
View(df)

#Variable types
glimpse(df)
df <- df %>%
  mutate(timestamp = as.POSIXct(timestamp, format="%d/%m/%Y %H:%M:%S"))
summary(df)
View(df)

# Missing value
df <- df %>%
  drop_na()
df <- df %>%
  distinct()
df %>%
  select(sleep_quality, suffer_headaches, academic_performance, extracurricular_activities) %>%
  summarise_all(funs(length(unique(.))))

summary(df)

# Check outliers
boxplot(df$sleep_quality, df$suffer_headaches, df$academic_performance, df$study_load, df$extracurricular_activities, df$stress_levels)

# Visualization
## Correlation Heatmap
install.packages("reshape2")
library(reshape2)

cor_matrix <- cor(df[, c("sleep_quality", "suffer_headaches", "academic_performance", "study_load", "extracurricular_activities", "stress_levels")])

ggplot(data = melt(cor_matrix), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#FCB03C", high = "#157D6E", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1))

## Pair Plots
install.packages("GGally")
library(GGally)

ggpairs(df[, c("sleep_quality", "suffer_headaches", "academic_performance", "study_load", "extracurricular_activities", "stress_levels")])

## Bar Charts: distribution of sleep quality
ggplot(df, aes(x = sleep_quality)) +
  geom_bar(fill = "#157D6E") +
  ggtitle("Distribution of Sleep Quality")+
  xlab("Sleep Quality") +
  ylab("Count") +
  theme_minimal()

## Box Plots: stress level by sleep quality
ggplot(df, aes(x = sleep_quality, y = stress_levels, fill = sleep_quality)) +
  geom_boxplot(fill = "#FCB03C") +
  ggtitle("Stress Levels by Sleep Quality") +
  xlab("Sleep Quality") +
  ylab("Stress Levels") +
  theme_minimal()

## Box Plots: study load by academic performance
ggplot(df, aes(x = academic_performance, y = study_load)) +
  geom_boxplot(fill = "#157D6E") +
  ggtitle("Study Load by Academic Performance")+
  xlab("Study Load") +
  ylab("Academic Performance") +
  theme_minimal()
