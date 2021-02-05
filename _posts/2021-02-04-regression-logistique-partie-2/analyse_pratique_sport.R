#' ---
#' title: "Analyse de la pratique du sport (hdv2003)"
#' author: "Joseph Larmarange"
#' ---

#' # Mise en place

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

#' # Analyse uni et bivariée

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

#' # Régression logistique

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

## Effet global de chaque variable ------

mod %>% 
  tbl_regression(
    intercept = TRUE,
    exponentiate = TRUE,
    add_estimate_to_reference_row = TRUE
  ) %>%
  add_global_p(keep = TRUE)

## Sélection pas à pas descendante ------

mod2 <- step(mod)

mod2 %>% ggcoef_model()

ggcoef_compare(
  list("modèle complet" = mod, "modèle simplifié" = mod2),
  exponentiate = TRUE,
  type = "f"
)

t1 <- mod %>% 
  tbl_regression(exponentiate = TRUE, add_estimate_to_reference_row = TRUE) %>% 
  add_global_p()
t2 <- mod2 %>% 
  tbl_regression(exponentiate = TRUE, add_estimate_to_reference_row = TRUE) %>% 
  add_global_p()

tbl_merge(
  tbls = list(t1, t2),
  tab_spanner = c("**Modèle complet**", "**Modèle simplifié**")
)

## Y a-t-il un effet d'interaction entre l'âge et le sexe ?

mod3 <- glm(
  sport ~ sexe * groupe_age + etudes + heures.tv, 
  # sport ~ sexe + groupe_age + sexe:groupe_age + etudes + heures.tv
  family = binomial, data = hdv2003
)

mod3 %>% tbl_regression() %>% add_global_p()

ggcoef_model(mod3)

plot(allEffects(mod3))

mod3 %>%
  ggeffect(c("groupe_age", "sexe")) %>%
  plot()

mod4 <- glm(
  sport ~ sexe:groupe_age + etudes + heures.tv, 
  family = binomial, data = hdv2003
)
ggcoef_model(mod4)
mod4 %>%
  ggeffect(c("groupe_age", "sexe")) %>%
  plot()

mod5 <- glm(
  sport ~ groupe_age + sexe:groupe_age + etudes + heures.tv, 
  family = binomial, data = hdv2003
)
ggcoef_model(mod5)
mod5 %>%
  ggeffect(c("groupe_age", "sexe")) %>%
  plot()

## Interaction sexe et niveau d'étude -----


mod6 <- glm(
  sport ~ groupe_age + sexe * etudes + heures.tv, 
  family = binomial, data = hdv2003
)
mod6 %>%
  ggeffect(c("etudes", "sexe")) %>%
  plot()
car::Anova(mod6)

## Y a-t-il un risque de multicolinéarité ?

car::vif(mod)

# add_vif() sera disponible dans lr pcohaine version de gtsummary