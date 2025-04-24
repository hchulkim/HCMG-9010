library(tidyverse)
library(data.table)
library(fixest)
library(texreg)
library(modelsummary)
library(patchwork)

data <- read_rds("proposal/baseline_data.rds")

data %>% 
  mutate(label = if_else(rank(-immstock) <= 30, as.character(iso), NA_character_)) %>%
  ggplot(aes(x = iso, y = immstock, color = iso)) +
  geom_point() +
  scale_y_continuous(
    name = "# of immigration stock",
    labels = scales::label_number(scale_cut = scales::cut_short_scale()),  # formats like 1k, 10k
    limits = c(0, 20000000)
  ) +
  geom_text(aes(label = label), size = 3, vjust = -0.5) +
  scale_x_discrete(name = "Country") +
  facet_wrap(~year) +
  theme_bw() +
  labs(title = "Total number of immigration stock (1960 - 2000)") +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_blank(),       # removes axis labels
    axis.ticks.x = element_blank()       # removes tick marks
  )
ggsave("imm.png", width = 10, height = 7)
  
datasummary(
  Factor(year) * (intra + inter) ~ (mean), 
  data = conflict %>% filter(year >= 1995, year <= 2000) %>% 
    rename(intra = conflict_intra, inter = conflict_inter)
)

datasummary(
  stock + share ~ Factor(year), 
  data = data %>% summarise(stock = sum(immstock, na.rm=T), share = mean(imm_share, na.rm=T), .by = year) 
)


conflict <- read_rds("proposal/conflict.rds")

conflict %>%
  summarise(intra_sum = sum(conflict_intra, na.rm = TRUE), .by = iso_c) %>%
  mutate(label = if_else(rank(-intra_sum) <= 5, as.character(iso_c), NA_character_)) %>%
  ggplot(aes(x = iso_c, y = intra_sum, color = iso_c)) +
  scale_y_continuous(name = "# of intra-state war") +
  scale_x_discrete(name = "Country") +
  geom_point() +
  geom_text(aes(label = label), size = 3, vjust = -0.5) +
  theme_bw() +
  labs(title = "Total number of intrastate war (1960 - 2022)") +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_blank(),       # removes axis labels
    axis.ticks.x = element_blank()       # removes tick marks
  )
ggsave("intra.png", width = 10, height = 7)


conflict %>%
  summarise(inter_sum = sum(conflict_inter, na.rm = TRUE), .by = iso_c) %>%
  mutate(label = if_else(rank(-inter_sum) <= 10, as.character(iso_c), NA_character_)) %>%
  ggplot(aes(x = iso_c, y = inter_sum, color = iso_c)) +
  scale_y_continuous(name = "# of inter-state war") +
  scale_x_discrete(name = "Country") +
  geom_point() +
  geom_text(aes(label = label), size = 3, vjust = -0.5) +
  theme_bw() +
  labs(title = "Total number of interstate war (1960 - 2022)") +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_blank(),       # removes axis labels
    axis.ticks.x = element_blank()       # removes tick marks
  )
ggsave("inter.png", width = 10, height = 7)

