# Session 11 - Analyse factorielles

## Exemple introductif ----

library(tidyverse)
library(ade4)
library(factoextra)

ggplot(iris) + 
  aes(x = Petal.Length, y = Petal.Width) +
  geom_point()

res <- iris %>%
  select(starts_with("Petal")) %>%
  dudi.pca(nf = 2, scannf = FALSE)

res2 <- explor::prepare_results(res)
explor::PCA_ind_plot(res2, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = NULL, labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = FALSE, transitions = TRUE,
                     labels_positions = NULL)


## Exemple plus complet -----

library(questionr)
data(hdv2003)

hdv2003$grpage <- cut(hdv2003$age, c(16, 25, 45, 65, 93), right = FALSE, include.lowest = TRUE)

hdv2003$etud <- hdv2003$nivetud
levels(hdv2003$etud) <- c(
  "Primaire", "Primaire", "Primaire", "Secondaire", "Secondaire",
  "Technique/Professionnel", "Technique/Professionnel", "Supérieur"
)

d2 <- hdv2003 %>%
  select(grpage, sexe, etud, peche.chasse, cinema, cuisine, bricol, sport, lecture.bd)

acm <- dudi.acm(d2, scannf = FALSE, nf = Inf)

# explor::explor(acm)

screeplot(acm)
fviz_screeplot(acm, choice = "eigenvalue")
fviz_screeplot(acm)

s.corcircle(acm$co, clabel = .7)
fviz_mca_var(acm, repel = TRUE)

res <- explor::prepare_results(acm)
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = "Variable",
                     size_var = NULL, size_range = c(10, 300), labels_size = 10, point_size = 56,
                     transitions = TRUE, labels_positions = NULL, labels_prepend_var = FALSE,
                     xlim = c(-1.45, 1.81), ylim = c(-2.03, 1.23))

boxplot(acm)
boxplot(acm, xax = 2)

fviz_contrib(acm, choice = "var", axes = 1)

par(mfrow = c(2, 2))
for (i in 1:4) 
  barplot(acm$cr[, i], names.arg = row.names(acm$cr), las = 2, main = paste("Axe", i))
par(mfrow = c(1, 1))

fviz_mca_ind(acm, geom ="point", alpha.ind = .25)

# devtools::install_github("larmarange/JLutils")
library(JLutils)
s.freq(acm$li)

s.hist(acm$li, clabel = 0, pch = 15)

s.class(acm$li, d2$sexe, col = c("red", "darkgreen"))
fviz_mca_ind(acm, geom = "point", habillage = d2$sexe, addEllipses = TRUE)

s.class(acm$li, hdv2003$relig)
fviz_mca_ind(acm, geom = "point", habillage = hdv2003$relig)

scatter(acm, col = RColorBrewer::brewer.pal(5, "Set1"))

# ACM alternative

d3 <- hdv2003 %>%
  select(peche.chasse, cinema, cuisine, bricol, sport, lecture.bd)
acm2 <- dudi.acm(d3, scannf = FALSE, nf = Inf)

fviz_mca_ind(acm2, geom = "point", habillage = hdv2003$sexe, addEllipses = TRUE)

fviz_mca_ind(acm2, geom = "point", habillage = hdv2003$grpage, addEllipses = TRUE)

scatter(acm2, col = RColorBrewer::brewer.pal(5, "Set1"))

# Variables supplémentaires avec FactorMineR

library(FactoMineR)
acm3 <- MCA(d2, quali.sup = 1:3)
