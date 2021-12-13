library(gtsummary)
library(labelled)
library(tidyverse)

trial %>% look_for()

theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")

trial %>% 
  tbl_summary(
    include = c(age, marker, stage, response),
    by = trt,
    statistic = list(
      all_continuous() ~ "m: {mean} - et: {sd}",
      all_categorical() ~ "{p}% ({n}/{N})"
    ),
    sort = all_categorical() ~ "frequency",
    percent = "cell"
  )

# exemple d'interaction

library(questionr)
data("hdv2003")
hdv2003$groupe <- interaction(hdv2003$sexe, hdv2003$sport)

hdv2003 %>%
  tbl_summary(
    by = groupe,
    include = heures.tv
  )

trial %>%
  tbl_summary(
    include = c(age, marker, grade),
    by = stage,
    statistic = list(
      age ~ "{median} [{p25} - {p75}]",
      marker ~ "{mean} ({sd})"
    )
  ) %>%
  add_stat_label(location = "column")


trial %>%
  tbl_summary(
    include = c(age, marker, ttdeath),
    type = c(age, marker) ~ "continuous2",
    statistic = all_continuous2() ~ c("{median} ({p25} - {p75})", "{mean} ({sd})", "{min} - {max}", "{sum}")
  )


trial %>%
  tbl_summary(
    include = c(age, stage),
    by = trt,
    digits = list(
      all_continuous() ~ c(2, 1, 1),
      all_categorical() ~ c(0, 1)
    )
  )


library(scales)
f2 <- label_number(accuracy = 0.01, decimal.mark = "-")
f2(12.2456)

pour_mille <- label_number(accuracy = .1, scale = 1000, suffix = "\u2030")
pour_mille(0.2548)

trial %>%
  tbl_summary(
    include = c(age, stage),
    statistic = all_categorical() ~ "{n} ({p})",
    by = trt,
    digits = list(
      all_continuous() ~ f2,
      all_categorical() ~ c(0, pour_mille)
    )
  )

# données manquantes

trial %>%
  tbl_summary(
    include = c(age, response, grade),
    missing = "no"
  )

tmp <- iris
tmp$Species[1:5] <- NA
tmp$Species2 <- tmp$Species %>% 
  fct_explicit_na(na_level = "manquant")

tmp %>%
  tbl_summary(include = starts_with("Sp"))

# étiquettes de variabes

iris %>%
  set_variable_labels(Petal.Length = "Longueur du pétale", Petal.Width = "Largeur du pétale") %>%
  tbl_summary(
    label = list(
      Species ~ "Espèce",
      Petal.Length ~ "MON ETIQUETTE"
    )
  )

trial %>%
  tbl_summary(
    include = c(age, marker),
    by = trt,
    missing = "no"
  ) %>%
  add_n(
    statistic = "{n}/{N}",
    col_label = "**Effectifs** (obs./total)",
    last = TRUE,
    footnote = TRUE
  )

trial %>%
  tbl_summary(
    include = c(age, grade, stage),
    by = trt
  ) %>%
  bold_labels() %>%
  italicize_levels()

tbl <- trial %>%
  tbl_summary(
    include = c(age, grade),
    by = trt
  ) %>% 
  add_overall()
show_header_names(tbl)

tbl %>%
  modify_header(update = list(
    label ~ "**Variables**",  
    all_stat_cols(stat_0 = FALSE) ~ "*{level}* (n={n}, {style_percent(p)}%)"
  )) %>%
  modify_footnote(everything() ~ NA) %>%
  modify_spanning_header(all_stat_cols(stat_0 = FALSE) ~ "**Traitement**")

# tests de comparaison

trial %>%
  tbl_summary(
    include = c(marker, age, response, stage),
    statistic = all_continuous() ~ "{mean} ({sd})",
    by = trt
  ) %>%
  add_p(
    test = all_continuous() ~ "t.test",
    pvalue_fun = scales::label_pvalue(accuracy = .001, decimal.mark = ",")
  ) %>%
  separate_p_footnotes()

# intervalles de confiance

trial %>%
  tbl_summary(
    include = c(marker, age, response),
    by = trt,
    statistic = list(
      age ~ "{median}",
      marker ~ "{mean}",
      all_categorical() ~ "{p}%"
    ),
    missing = "no"
  ) %>%
  add_stat_label(location = "column") %>%
  add_ci(
    method = list(
      age ~ "wilcox.test",
      marker ~ "t.test"
    )
  )


trial %>%
  tbl_summary(
    include = c(marker, age, response),
    by = trt,
    statistic = list(
      age ~ "{median}",
      marker ~ "{mean}",
      all_categorical() ~ "{p}%"
    ),
    missing = "no"
  ) %>%
  add_p(
    test = list(
      age ~ "kruskal.test",
      marker ~ "t.test"
    )
  ) 

# add_difference

trial %>%
  tbl_summary(
    include = c(age, marker, response),
    by = trt,
    statistic = list(
      all_continuous() ~ "{mean}",
      all_categorical() ~ "{p}%"
    ),
    digits = list(all_continuous() ~ 2, all_categorical() ~ 1),
    missing = "no"
  ) %>%
  add_difference()

# Tableau croisé

trial %>%
  tbl_cross(
    row = grade,
    col = trt,
    percent = "row"
  ) %>%
  add_p(source_note = TRUE)

trial %>%
  tbl_summary(
    include = grade,
    by = trt,
    percent = "row"
  ) %>%
  add_overall(last = TRUE)

