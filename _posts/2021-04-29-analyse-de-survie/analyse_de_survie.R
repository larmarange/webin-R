# ANALYSE DE SURVIE

# Chargement des données -----

library(tidyverse)
library(labelled)
library(questionr)
library(lubridate)
library(survival)
library(survminer)

data(fecondite)

# Recodages -------

## durée d'observation

enfants <- enfants %>%
  left_join(
    femmes %>% select(id_femme, date_entretien),
    by = "id_femme"
  )

enfants <- enfants %>%
  mutate(
    duree_observation = interval(date_naissance, date_entretien) %>% 
      time_length(unit = "month")
  )

enfants <- enfants %>%
  mutate(
    date_entretien = date_entretien %>%
      recode_if(
        date_entretien < date_naissance,
        date_entretien %m+% months(1)
      ),
    duree_observation = interval(date_naissance, date_entretien) %>% 
      time_length(unit = "month"),
    duree_observation_mois_revolus = trunc(duree_observation)
  )

## age au décès imputation

enfants <- enfants %>%
  mutate(
    age_deces_impute = age_deces + runif(n())  
  )

## variables de survie

enfants <- enfants %>%
  mutate(
    deces = if_else(survie == 0, 1, 0),
    time = if_else(
      survie == 0,
      age_deces_impute,
      duree_observation
    ),
    time_revolus = if_else(
      survie == 0,
      age_deces,
      duree_observation_mois_revolus
    )
  ) %>%
  set_variable_labels(deces = "Est décédé ?") %>%
  set_value_labels(deces = c("en vie" = 0, "décédé" = 1))


## variables explicatives

enfants <- enfants %>%
  left_join(
    femmes %>% 
      select(id_femme, id_menage, milieu, educ, date_naissance_mere = date_naissance, nb_enf_ideal),
    by = "id_femme"
  ) %>%
  left_join(
    menages %>%
      select(id_menage, structure, richesse),
    by = "id_menage"
  )

enfants <- enfants %>%
  mutate(
    sexe = to_factor(sexe),
    richesse = to_factor(richesse),
    milieu = to_factor(milieu),
    structure = to_factor(structure) %>%
      fct_drop() %>%
      fct_relevel("deux adultes de sexe opposé"),
    educ2 = to_factor(educ) %>%
      fct_recode(
        "secondaire et sup" = "secondaire", 
        "secondaire et sup" = "supérieur"
      ),
    age_mere_naissance = interval(date_naissance_mere, date_naissance) %>%
      time_length(unit = "years")
  )

enfants$gp_age_mere_naissance <- cut(enfants$age_mere_naissance,
  include.lowest = TRUE,
  right = FALSE,
  breaks = c(-Inf, 20, 30, Inf)
)

enfants$gp_age_mere_naissance <- enfants$gp_age_mere_naissance %>%
  fct_recode(
    "19 ans ou moins" = "[-Inf,20)",
    "20-29 ans" = "[20,30)",
    "30 ans ou plus" = "[30, Inf]"
  )

enfants <- enfants %>%
  arrange(id_femme, date_naissance) %>%
  group_by(id_femme) %>%
  mutate(
    rang = rank(date_naissance, ties.method = "max"),
    nb_enf_ideal = nb_enf_ideal %>% user_na_to_na() %>% unclass(), # transformer les valeurs indiquées manquantes en NA
    rang_apres_ideal = if_else(rang > nb_enf_ideal, "oui", "non") %>%
      factor(levels = c("non", "oui"))
  ) %>%
  set_variable_labels(rang_apres_ideal = "Rang de naissance plus élevé qu'idéal")

# Courbe de Kaplan-Meier -----

km_globale <- survfit(Surv(time, deces) ~ 1, data = enfants)
ggsurvplot(km_globale)
ggsurvplot(km_globale, fun = "event", risk.table = TRUE, surv.scale = "percent", break.time.by = 12)

km_globale_revolus <- survfit(Surv(time_revolus, deces) ~ 1, data = enfants)
ggsurvplot(km_globale_revolus, fun = "event", risk.table = TRUE, surv.scale = "percent", break.time.by = 12)

km_sexe <- survfit(Surv(time, deces) ~ sexe, data = enfants)
survdiff(Surv(time, deces) ~ sexe, data = enfants)
ggsurvplot(km_sexe, fun = "event", risk.table = TRUE, surv.scale = "percent", break.time.by = 12, pval = TRUE)

km_sexe_age <- survfit(Surv(time, deces) ~ sexe + gp_age_mere_naissance, data = enfants)
ggsurvplot(km_sexe_age, fun = "event", risk.table = TRUE, surv.scale = "percent", break.time.by = 12, pval = TRUE)


## Modèle de Cox

mod1 <- coxph(
  Surv(time, deces) ~ sexe + milieu + richesse + 
    structure + educ2 + gp_age_mere_naissance + rang_apres_ideal,
  data = enfants
)

library(gtsummary)
library(GGally)
tbl <- tbl_regression(mod1, exponentiate = TRUE)
tbl %>% 
  add_global_p(keep = TRUE) %>%
  add_significance_stars(hide_p = FALSE)

plot(tbl)
ggcoef_model(mod1, exponentiate = TRUE)

mod2 <- step(mod1)
ggcoef_model(mod2, exponentiate = TRUE)

ggforest(mod2)

## Validité du modèle

test <- cox.zph(mod2)
test
ggcoxzph(test)
