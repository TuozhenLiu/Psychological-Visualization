library(tidyverse)
library(haven)
# library(extrafont)
# font_import()
# loadfonts()
# windowsFonts(myFont1 = windowsFont('Times New Roman'))
data <- read_sav("./data/data_NXU_clean_GIDYQ-AA_2535-R.sav")

vari_list <- c("MGIDYQ", "Tanxiety", "Tdepress", "Suicide")
vari_list_full <- c("gender dysphoria", "anxiety", "depression", "suicide ideation")
data %<>% select(all_of(vari_list))
data %>%
  `colnames<-`(1:ncol(data)) %>%
  cor(method = "spearman") %>%
  as.data.frame() %>%
  mutate(X = 1:ncol(data)) %>%
  pivot_longer(1:ncol(data), names_to = "Y", values_to = "cor") %>%
  mutate(
    cor = ifelse(Y >= X, NA, cor),
    cor = ifelse(X == 1 & Y == 1, 1, cor)
  ) %>%
  mutate(X_lab = vari_list_full[X], Y_lab = vari_list_full[as.numeric(Y)]) %>% # view
  mutate(
    X_lab = factor(X_lab, levels = vari_list_full),
    Y_lab = factor(Y_lab, levels = vari_list_full)
  ) %>%
  mutate(
    round = round(cor, 2),
    round = case_when(
      cor == 1 ~ as.character(cor),
      cor < 0 ~ str_pad(round, 5, side = "right", pad = "0"),
      cor > 0 & cor < 1 ~ str_pad(round, 4, side = "right", pad = "0"),
      cor == 0 ~ paste0(round, ".00")
    ),
    label = ifelse(is.na(cor) | X == Y, cor, paste0(round, "**"))
  ) %>%
  ggplot(aes(x = X_lab, y = Y_lab)) +
  geom_tile(aes(fill = cor)) +
  geom_text(aes(label = label)) +
  scale_fill_gradient2(
    name = "", low = "#3B9AB2", mid = "#EEEEEE",
    high = "#F21A00", midpoint = 0, na.value = "white", limits = c(-1, 1)
  ) +
  labs(x = "", y = "") +
  theme(
    axis.text = element_text(size = 10),
    legend.text = element_text(),
    axis.ticks = element_line(colour = "transparent"),
    axis.line = element_line(color = "black"),
    panel.background = element_rect(fill = "white")
  )
