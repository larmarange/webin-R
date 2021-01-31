# Analyse de la pratique du sport  ----

## Chargement des données et des packages ----

library(tidyverse)
library(labelled)
library(questionr)
library(gtsummary)
library(GGally)
library(effects)
library(ggeffects)

theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")
theme_gtsummary_mean_sd()

data(hdv2003)

## Étiquettes de variables ------

hdv2003 <- hdv2003 %>%
  set_variable_labels(
    sport = "Pratique du sport ?",
    age = "Âge",
    sexe = "Sexe",
    nivetud = "Niveau d'études",
    relig = "Rapport à la religion",
    heures.tv = "Heures quotidiennes de TV"
  )

## Recodages -------

hdv2003$groupe_age <- hdv2003$age %>% 
  cut(
    include.lowest = TRUE,
    right = FALSE,
    dig.lab = 4,
    breaks = c(18, 25, 45, 60, 97)
  ) %>% 
  fct_recode(
    "18-24" = "[18,25)",
    "25-44" = "[25,45)",
    "45-59" = "[45,60)",
    "60 et +" = "[60,97]"
  )
var_label(hdv2003$groupe_age) <- "Groupe d'âges"

hdv2003$sexe <- hdv2003$sexe %>%
  fct_relevel("Femme")

hdv2003$etudes <- hdv2003$nivetud %>% 
  fct_recode(
    "Primaire" = "N'a jamais fait d'etudes",
    "Primaire" = "A arrete ses etudes, avant la derniere annee d'etudes primaires",
    "Primaire" = "Derniere annee d'etudes primaires",
    "Secondaire" = "1er cycle",
    "Secondaire" = "2eme cycle",
    "Technique/Professionnel" = "Enseignement technique ou professionnel court",
    "Technique/Professionnel" = "Enseignement technique ou professionnel long",
    "Supérieur" = "Enseignement superieur y compris technique superieur"
  ) %>%
  fct_explicit_na("Manquant")
var_label(hdv2003$etudes) <- "Niveau d'études"

## Analyse univariée --------

hdv2003 %>%
  select(sport, groupe_age, sexe, etudes, relig, heures.tv) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{p}% [{n}]",
    digits = all_categorical() ~ c(1, 0)
  )

## Analyse bivariée --------

hdv2003 %>%
  select(sport, groupe_age, sexe, etudes, relig, heures.tv) %>%
  tbl_summary(
    by = "sport",
    percent = "row",
    statistic = all_categorical() ~ "{p}% [{n}]",
    digits = all_categorical() ~ c(1, 0)
  ) %>%
  add_overall(last = TRUE) %>%
  add_p()

hdv2003 %>%
  ggbivariate(
    outcome = "sport",
    explanatory = c("groupe_age", "sexe", "etudes", "relig", "heures.tv")
  )


## Calcul de la régression logistique -----

mod <- glm(
  sport ~ groupe_age + sexe + etudes + relig + heures.tv,
  family = binomial(),
  data = hdv2003
)

mod %>% 
  tbl_regression(
    intercept = TRUE,
    exponentiate = TRUE,
    add_estimate_to_reference_row = TRUE
  )

ggcoef_model(mod, exponentiate = TRUE)


## Effets marginaux ------

mod %>% allEffects() %>% plot()
# plot(allEffects(mod))

ggeffect(mod) %>% 
  plot() %>%
  cowplot::plot_grid(plotlist = .)
