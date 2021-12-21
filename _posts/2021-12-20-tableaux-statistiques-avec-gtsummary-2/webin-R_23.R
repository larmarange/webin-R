# webin-R #23

library(gtsummary)
library(tidyverse)

theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")

mod <- glm(
  response ~ grade * age + trt,
  data = trial,
  family = binomial
)

# Syntaxe correcte par rapport à la vidéo pour le label de l'interaction.
# Il faut mettre des guillements, car sinon grade:age est compris selon 
# la syntaxe tidyselect comme de la variable grade à la variable age.

tbl <- mod %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      age ~ "Âge en années",
      "grade:age" ~ "Interaction" 
    ),
    intercept = TRUE
  )

show_header_names(tbl)

tbl %>%
  modify_header(estimate ~ "**Odds Ratio**") %>%
  modify_footnote(estimate ~ NA, abbreviation = TRUE)

mod %>%
  tbl_regression(
    exponentiate = TRUE,
    conf.level = 0.9
  )

mod %>%
  tbl_regression(exponentiate = TRUE) %>%
  add_significance_stars(
    hide_ci = FALSE,
    hide_p = FALSE,
    hide_se = TRUE
  )

mod %>%
  tbl_regression(
    show_single_row = all_dichotomous(),
    label = trt ~ "Traitement B vs Traitement A"
  ) %>%
  italicize_labels()

mod %>%
  tbl_regression(
    estimate_fun = scales::label_number(accuracy = .001, decimal.mark = ","),
    pvalue_fun = scales::label_pvalue(accuracy = .001, add_p = TRUE, decimal.mark = ",")
  )

mod %>%
  tbl_regression(
    estimate_fun = function(x) {style_sigfig(x, digits = 3)},
    pvalue_fun = function(x) {style_pvalue(x, digits = 3)}
  )

mod %>%
  tbl_regression(
    estimate_fun = ~ style_sigfig(.x, digits = 3),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  )

mod %>%
  tbl_regression(
    estimate_fun = purrr::partial(style_sigfig, digits = 3),
    pvalue_fun = purrr::partial(style_pvalue, digits = 3)
  )

mod %>%
  tbl_regression(
    exponentiate = TRUE,
    add_estimate_to_reference_rows = TRUE
  )

mod %>%
  tbl_regression() %>%
  add_global_p(keep = TRUE)

mod %>%
  tbl_regression() %>%
  add_vif()

mod %>%
  tbl_regression(exponentiate = TRUE) %>%
  plot()

mod %>%
  GGally::ggcoef_model(exponentiate = TRUE)

mod %>% broom::tidy()
mod %>% broom.helpers::tidy_plus_plus()

mod %>% broom::glance()

mod %>%
  tbl_regression() %>%
  add_glance_table(include = c("AIC", "nobs"))

mod %>%
  tbl_regression() %>%
  add_glance_source_note(include = c("AIC", "nobs"))

# Combiner des tableaux

t1 <- 
  glm(response ~ trt, trial, family = binomial) %>%
  tbl_regression(exponentiate = TRUE)

t2 <- 
  glm(response ~ grade + marker + trt + age, trial, family = binomial) %>%
  tbl_regression(exponentiate = TRUE)

t1

tbl_stack(
  list(t1, t2),
  group_header = c("MODELE BIVARIE", "MODELE MULTIVARIE")
)

tbl_merge(
  list(t1, t2),
  tab_spanner = c("Modèle bivarié", "Modèle multivarié")
)

tmp <- trial %>%
  select(age, grade, stage, trt)
levels(tmp$grade) <- c("Grade I", "Grade II", "Grade III")

tmp %>%
  tbl_strata(
    strata = grade,
    .tbl_fun = 
      ~ .x %>%
        tbl_summary(by = trt, missing = "no") %>%
        add_n()
  )


# Analyses univariées

tbl_uni <- trial %>%
  select(response, age, grade, stage) %>%
  tbl_uvregression(
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    hide_n = TRUE
  )

tbl_desc <- trial %>%
  tbl_summary(
    by = response,
    include = c(age, grade, stage)
  ) %>%
  add_p()

tbl_multi <-
  glm(response ~ age + grade + stage, data = trial, family = binomial) %>%
  tbl_regression(exponentiate = TRUE)

tbl_merge(
  list(tbl_desc, tbl_uni, tbl_multi),
  tab_spanner = c("**Analyse Descriptive**", "**Modèles bivariés**", "**Modèle multivarié**")
)

## tbl_continuous()

trial %>%
  tbl_continuous(
    by = trt,
    include = c(stage, grade),
    variable = age,
    statistic = ~ "{mean}",
    digits = ~ 1
  )

## tbl_custom_summary()

trial %>%
  tbl_custom_summary(
    by = trt,
    include = c(stage, grade),
    stat_fns = ~ continuous_summary("age"),
    statistic = ~ "{mean} ({sd})",
    digits = ~ 1,
    overall_row = TRUE
  ) %>%
  add_overall()


trial %>%
  tbl_custom_summary(
    by = trt,
    include = grade,
    stat_fns = ~ proportion_summary("stage", value = c("T3", "T4")),
    statistic = ~ "{prop}% [{conf.low}-{conf.high}]",
    digits = ~ scales::label_percent(accuracy = .1, suffix = "")
  ) %>%
  modify_footnote(all_stat_cols() ~ "Prop de T3/T4 [IC 95%]")

trial %>%
  tbl_custom_summary(
    by = trt,
    include = stage,
    stat_fns = ~ ratio_summary("response", "ttdeath"),
    statistic = ~ "{ratio} ({num}/{denom})",
    digits = ~ c(3, 0, 0)
  )


trial %>%
  tbl_summary(
    by = trt,
    include = c(age, marker),
    statistic = ~ "{mean}"
  ) %>%
  add_difference()


# tbl_survfit()

library(survival)
km <- survfit(Surv(ttdeath, death) ~ trt, trial)

survminer::ggsurvplot(km)

km %>%
  tbl_survfit(
    times = c(0, 6, 12),
    label_header = "**Mois {time}**"
  )

km %>%
  tbl_survfit(probs = c(.25, .5, .75))
