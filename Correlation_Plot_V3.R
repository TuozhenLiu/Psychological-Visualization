library(tidyverse)
library(haven)
library(Hmisc)
# windowsFonts(myFont1 = windowsFont('Times'))


data <- read_sav("./data/data_clean_merge_9799.sav")

vari_list <- c(
  "Tcomply3", "Tanxiety", "Tdepress", "Tscl90", "Tsoma", "Tobes",
  "Trelation", "Thosti", "Thorri", "Tparan", "Tpsycho", "Tother", "Taltruism"
)
vari_list_full <- c(
  "顺从性", "焦虑", "抑郁", "精神症状", "躯体化", "强迫", "人际敏感",
  "敌对", "恐怖", "偏执", "精神病性", "其他", "利他"
)
data <- data %>% select(all_of(vari_list))
data %>%
  `colnames<-`(1:ncol(data)) %>%
  as.matrix() %>%
  rcorr() -> cor_results

cor_results[["r"]] %>%
  as.data.frame() %>%
  mutate(X = 1:ncol(data)) %>%
  pivot_longer(1:ncol(data), names_to = "Y", values_to = "cor") %>%
  mutate(Y = as.numeric(Y)) -> r_temp
cor_results[["P"]] %>%
  as.data.frame() %>%
  mutate(X = 1:ncol(data)) %>%
  pivot_longer(1:ncol(data), names_to = "Y", values_to = "p") %>%
  mutate(Y = as.numeric(Y)) -> p_temp

r_temp %>%
  left_join(p_temp) %>%
  mutate(
    cor = ifelse(Y >= X, NA, cor),
    cor = ifelse(Y == 1 & X == 1, 1, cor)
  ) %>%
  mutate(
    p = ifelse(Y > X, NA, p),
    p = case_when(
      p <= 0.001 ~ "***",
      p <= 0.01 & p > 0.001 ~ "**",
      p <= 0.05 & p > 0.01 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  mutate(X_lab = vari_list_full[X], Y_lab = vari_list_full[as.numeric(Y)]) %>% # view
  mutate(
    X_lab = factor(X_lab, levels = vari_list_full),
    Y_lab = factor(Y_lab, levels = vari_list_full)
  ) %>%
  mutate(
    round = round(cor, 2),
    round = case_when(
      round < 0 ~ str_pad(round, 5, side = "right", pad = "0"),
      round > 0 & round < 1 ~ str_pad(round, 4, side = "right", pad = "0"),
      round == 0 ~ paste0(round, ".00"),
      TRUE ~ as.character(round)
    ),
    label = ifelse(is.na(cor) | X == Y, cor, paste0(round, p))
  ) %>%
  ggplot(aes(x = factor(X_lab, levels = vari_list_full), y = factor(Y_lab, levels = vari_list_full))) +
  geom_tile(aes(fill = cor)) +
  geom_text(aes(label = label), family = "STXihei", size = 4) +
  scale_fill_gradient2(
    name = "", low = "#3B9AB2", mid = "#EEEEEE",
    high = "#F21A00", midpoint = 0, na.value = "white", limits = c(-1, 1)
  ) +
  labs(x = "", y = "") +
  theme(
    axis.text = element_text(family = "STXihei", size = 10),
    legend.text = element_text(family = "STXihei", size = 10),
    legend.key.size = unit(16, "pt"),
    axis.ticks = element_line(colour = "transparent"),
    axis.line = element_line(color = "black"),
    panel.background = element_rect(fill = "white")
  )
ggsave("fig.png", dpi = 600, width = 13, height = 7.12)
