library(haven)
library(tidyverse)
data <- read_sav("./data/data_regression1055.sav")

# 固定宽度
data %>%
  mutate(gender2 = factor(gender2)) %>%
  ggplot(mapping = aes(x = gender2, y = MGIDYQ)) +
  geom_violin(aes(fill = gender2)) +
  geom_boxplot(width = 0.2) +
  scale_x_discrete(labels = c(
    "male", "female",
    "transgender", "nonbinary/queer"
  )) +
  scale_fill_discrete(labels = c(
    "male", "female",
    "transgender", "nonbinary/queer"
  )) +
  scale_y_continuous(expand = c(0, 1.2)) +
  labs(
    y = "total score of GIDYQ-AA",
    x = "gender identity",
    fill = "gender identity"
  ) +
  theme_classic() +
  theme(legend.position = "top")

# 不固定宽度
data %>%
  mutate(psytype = factor(psytype)) %>%
  ggplot(mapping = aes(x = psytype, y = hyper_2)) +
  geom_violin(aes(fill = psytype)) +
  geom_boxplot(varwidth = T) +
  scale_x_discrete(labels = c(
    "Anxiety disorder", "Depressive disorder",
    "Bipolar disorder", "Schizophrenia"
  )) +
  scale_fill_discrete(labels = c(
    "Anxiety disorder", "Depressive disorder",
    "Bipolar disorder", "Schizophrenia"
  )) +
  scale_y_continuous(expand = c(0, 1.2)) +
  labs(
    y = "Total score of hyperarousal PTSD",
    x = "Psychosis type",
    fill = "Psychosis type"
  ) +
  theme_classic() +
  theme(legend.position = "null")
