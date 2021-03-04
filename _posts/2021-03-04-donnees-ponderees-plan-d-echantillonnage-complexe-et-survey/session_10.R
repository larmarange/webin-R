# Session 10 : données pondérées et plan d'échantillonage

library(tidyverse)
library(labelled)
library(questionr)
data("hdv2003")

library(survey)

# simuler des strates et des grappes
hdv2003$grappe <- rep.int(1:25, times = 80)

hdv2003 <- hdv2003 %>%
  arrange(grappe)

hdv2003$strate <- "a"
hdv2003$strate[801:1200] <- "b"
hdv2003$strate[1201:2000] <- "c"

# conseil essentiel
# Faire toutes les recodifications avant de définir le plan d'échantillonnage

# définir un plan d'échantillonnage -----------

dp <- svydesign(ids = ~ 1, weights = ~ poids, data = hdv2003)

dp_strates <- svydesign(ids = ~ 1, weights = ~ poids, data = hdv2003, strata = ~ strate)

dp_grappes <- svydesign(ids = ~ grappe, weights = ~ poids, data = hdv2003)
# Pour un échantillonage à plusieurs degrés
# ids = ~ var1 + var2

dp_strates_grappes <- svydesign(ids = ~ grappe, weights = ~ poids, data = hdv2003, strata = ~ strate)

# si on a besoin d'un sous-échantillon ---------
# toujours définir l'objet survey sur le tableau entier
# puis appliquer la fonction subset

dp_femmes <- subset(dp, sexe == "Femme")

# si besoin de récupérer les poids ------
weights(dp_strates)


# Tableau croisé et test du Chi² --------

tab <- xtabs(poids ~ sexe + sport, data = hdv2003)
tab
rprop(tab)
chisq.test(tab) # bad !!!!

tab <- svytable(~ sexe + sport, design = dp)
tab
rprop(tab)
svychisq(~ sexe + sport, design = dp)

svytable(~ sexe + sport, design = dp_grappes)

svychisq(~ sexe + sport, design = dp_grappes)
svychisq(~ sexe + sport, design = dp_strates)
svychisq(~ sexe + sport, design = dp_strates_grappes)

# gtsummary::tbl_svysummary ------

library(gtsummary)

dp %>%
  tbl_svysummary(include = c("sexe", "sport", "cuisine"))

dp %>%
  tbl_svysummary(
    include = c("sexe", "sport", "cuisine"), 
    by = "sexe",
    statistic = all_categorical() ~ "{p}% (obs. : {n_unweighted})"
  ) %>%
  modify_header(update = all_stat_cols() ~ "**{level}**, N = {n_unweighted}") %>%
  add_overall(last = TRUE, col_label = "**Overall**, N = {N_unweighted}") %>%
  add_p()

# Graphiques ggplot2 ------

library(GGally)

ggplot(hdv2003) +
  aes(x = sexe, fill = sport, weight = poids, by = sexe) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5))

questionr::ggsurvey(dp) +
  aes(x = sexe, fill = sport, by = sexe) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5))
