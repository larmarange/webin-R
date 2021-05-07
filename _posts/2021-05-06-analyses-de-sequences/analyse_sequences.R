# WEBIN-R #16 : Analyses de séquences

library(tidyverse)
library(labelled)
library(TraMineR)
library(tidyr)

donnees <- read_csv("trajpro.csv")

donnees$generation <- factor(
  donnees$generation,
  levels = 1:3,
  labels = c("1930-1938", "1939-1945", "1946-1950")
)

labels <- c(
  agric = "agriculteurs exploitants", # 1
  acce = "artisans, commercants et chefs d'entreprise", # 2
  cadr = "cadres et professions intellectuelles supérieures", # 3
  pint = "professions intermédiaires", # 4
  empl = "employés", # 5
  ouvr = "ouvriers", # 6
  etud = "études", # 7
  inact = "inactivité", # 8
  smil = "service militaire" # 9
)

seq <- seqdef(
  donnees %>% select(csp1:csp37),
  alphabet = 1:9,
  states = names(labels),
  labels = names(labels)
)

seq.om <- seqdist(seq, method = "LCS") %>%
  as.dist()

seq.arbre <- hclust(seq.om, method = "ward.D2")

plot(seq.arbre)

seq.arbre$height %>% 
  sort(decreasing = TRUE) %>% 
  head(20) %>%
  plot(type = "s")

seq.part <- cutree(seq.arbre, k = 5) %>%
  factor(levels = 1:5, labels = paste("Classe", 1:5))

questionr::freq(seq.part)

seqdplot(seq, group = seq.part, xtlab = 14:50)

ordre <- seq.om %>% cmdscale(k = 1)
seqIplot(seq, group = seq.part, xtlab = 14:50, sortv = ordre)

library(seqhandbook)
seq_heatmap(seq, seq.arbre, labCol = 14:50)

donnees$id <- 1:nrow(donnees)
donnees$classe <- seq.part
donnees$ordre <- ordre %>% rank(ties.method = "random")

long <- donnees %>%
  pivot_longer(
    cols = csp1:csp37,
    names_to = "annee",
    values_to = "csp"
  )

long$csp <- factor(
  long$csp,
  levels = 1:9,
  labels = labels
)

long$age <- long$annee %>%
  str_sub(start = 4) %>%
  as.integer() + 13

ggplot(long) +
  aes(x = age, y = factor(ordre), fill = csp) +
  geom_raster() +
  theme_bw() +
  scale_fill_brewer(palette = "Set3") +
  facet_grid(rows = vars(classe), scales = "free_y", space = "free_y") +
  scale_y_discrete(label = NULL) +
  scale_x_continuous(
    limits = c(14, 50), 
    breaks = c(14, 20, 25, 30, 35, 40, 45, 50)
  )

seqfplot(seq, group = seq.part, xtlab = 14:50)

seqmsplot(seq, group = seq.part, xtlab = 14:50)

seqmtplot(seq, group = seq.part, xtlab = 14:50)

seqrplot(seq, group = seq.part, xtlab = 14:50, dist.matrix = seq.om, method = "dist")

seqHtplot(seq, group = seq.part, xtlab = 14:50)

library(gtsummary)
donnees %>%
  select(generation, classe) %>%
  tbl_summary(by = classe, percent = "row") %>%
  add_p()
