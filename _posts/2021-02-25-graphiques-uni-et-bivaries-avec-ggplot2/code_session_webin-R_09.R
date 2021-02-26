# Graphiques uni- et bi-variés

library(tidyverse)
library(questionr)
data(hdv2003)
library(gtsummary)
library(labelled)
library(scales)
library(GGally)

# 1 variable continue -----

ggplot(hdv2003) +
  aes(x = age) +
  geom_histogram(fill ="orange", color = "black")

ggplot(hdv2003) +
  aes(x = age) +
  geom_histogram(bins = 15, fill ="orange", color = "black")

ggplot(hdv2003) +
  aes(x = age) +
  geom_histogram(binwidth = 5, fill ="orange", color = "black")

ggplot(hdv2003) +
  aes(x = age) +
  geom_histogram(binwidth = 10, fill ="orange", color = "black")


# incorrect
ggplot(hdv2003) +
  aes(x = age) +
  geom_histogram(breaks = c(18, 20, 30, 50, 80, 97), fill ="orange", color = "black")

# correct
ggplot(hdv2003) +
  aes(x = age, y = after_stat(count) / after_stat(width)) +
  geom_histogram(breaks = c(18, 20, 30, 50, 80, 97), fill ="orange", color = "black")

# 1 variable continue et 1 variable catégorielle

ggplot(hdv2003) +
  aes(x = age, fill = sexe) +
  geom_histogram() +
  facet_grid(cols = vars(sexe))

ggplot(hdv2003) +
  aes(x = age, fill = sexe, y = after_stat(density)) +
  geom_histogram() +
  facet_grid(cols = vars(sexe))

ggplot(hdv2003) +
  aes(x = age, color = sexe) +
  geom_density()

ggplot(hdv2003) +
  aes(x = age, y = after_stat(density)) +
  geom_histogram(fill = "orange", alpha = .5, color = "grey50") +
  geom_density() +
  facet_grid(cols = vars(sexe))

ggplot(hdv2003) +
  aes(x = age, y = after_stat(density)) +
  geom_histogram(fill = "orange", alpha = .5, color = "grey50") +
  geom_density(adjust = 2) +
  facet_grid(cols = vars(sexe))

ggplot(hdv2003) +
  aes(x = age, y = after_stat(density)) +
  geom_histogram(fill = "orange", alpha = .5, color = "grey50") +
  geom_density(adjust = .5) +
  facet_grid(cols = vars(sexe))

ggplot(hdv2003) +
  aes(x = sexe, y = age) +
  geom_boxplot()

ggplot(hdv2003) +
  aes(x = sexe, y = age, fill = sport) +
  geom_boxplot()

ggplot(hdv2003) +
  aes(x = sexe, y = age, fill = sport) +
  geom_violin()

ggplot(hdv2003) +
  aes(x = relig, y = age) +
  geom_violin() +
  geom_point(alpha = .1, position = position_jitter(height = 0, width = .15))


ggplot(hdv2003) +
  aes(x = age) +
  geom_histogram(color = "grey50", fill = "blue", alpha = .25) +
  geom_freqpoly(size = 1)


# Bonus -----

# annotate

p <- ggplot(hdv2003) +
  aes(x = sexe, y = age, fill = sport) +
  geom_boxplot()
  
p + annotate("text", x = 1.5, y = 85, label = "MON ETIQUETTE") 

## différence facet_grid et facet_wrap

ggplot(hdv2003) +
  aes(x = age) +
  geom_histogram(fill = "orange", alpha = .5, color = "grey50") +
  facet_grid(cols = vars(relig))

ggplot(hdv2003) +
  aes(x = age) +
  geom_histogram(fill = "orange", alpha = .5, color = "grey50") +
  facet_grid(rows = vars(relig))

ggplot(hdv2003) +
  aes(x = age) +
  geom_histogram(fill = "orange", alpha = .5, color = "grey50") +
  facet_wrap(vars(relig))


ggplot(hdv2003) +
  aes(x = age) +
  geom_histogram(fill = "orange", alpha = .5, color = "grey50") +
  facet_wrap(vars(relig), nrow = 3)

ggplot(hdv2003) +
  aes(x = age) +
  geom_histogram(fill = "orange", alpha = .5, color = "grey50") +
  facet_grid(rows = vars(relig), cols = vars(sexe))

ggplot(hdv2003) +
  aes(x = age) +
  geom_histogram(fill = "orange", alpha = .5, color = "grey50") +
  facet_wrap(vars(relig, sexe))

## mettre en forme des nombres avec number de scales

x <- c(1.2, 0.123, 10.456, 0.02, 14568.789)
number(x)
number(x, decimal.mark = ",", big.mark = "'")
number(x, prefix = "$", suffix = " CAD")

number(x, accuracy = 1)
number(x, accuracy = .001)
number(x, accuracy = .25)
number(x, accuracy = 10)

number(x, scale = 100, suffix = "%")
number(x, accuracy = .1, scale = 100, suffix = "%")

y <- c(17548978879, 1456798679, 456787)
number(y, accuracy = .1, scale = 1/1000000, suffix = " millions")

en_millions <- label_number(accuracy = .1, scale = 1/1000000, suffix = " millions")
class(en_millions)
en_millions(y)

# Diagrammes en barre ------

ggplot(trial) +
  aes(x = stage) +
  geom_bar()
  
ggplot(trial) +
  aes(x = stage, fill = grade) +
  geom_bar()

ggplot(trial) +
  aes(x = stage, fill = grade) +
  geom_bar(position = "dodge")

ggplot(trial) +
  aes(x = stage, fill = grade) +
  geom_bar(position = "fill")

ggplot(trial) +
  aes(x = stage, fill = grade) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = label_percent(suffix = " %"))

ggplot(trial) +
  aes(y = stage, fill = grade) +
  geom_bar(position = "fill") +
  scale_x_continuous(labels = label_percent(suffix = " %"))

# avec étiquettes

ggplot(trial) +
  aes(x = stage, fill = grade, label = after_stat(count)) +
  geom_bar() +
  geom_text(stat = "count", position = position_stack(.5))

ggplot(trial) +
  aes(x = stage, fill = grade) +
  geom_bar() +
  geom_text(
    mapping = aes(label = after_stat(count)),
    stat = "count", 
    position = position_stack(.5)
  )

ggplot(trial) +
  aes(x = stage, fill = grade, label = after_stat(count)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", position = position_dodge(.9))

ggplot(trial) +
  aes(x = stage, fill = grade, label = after_stat(count)) +
  geom_bar(position = "dodge", width = 0.5) +
  geom_text(stat = "count", position = position_dodge(.5))

ggplot(trial) +
  aes(x = stage, fill = grade, label = after_stat(count)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", position = position_dodge(.9), vjust = 1)

ggplot(trial) +
  aes(x = stage, fill = grade, label = after_stat(count)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", position = position_dodge(.9), vjust = 0)

ggplot(trial) +
  aes(x = stage, fill = grade, label = after_stat(count)) +
  geom_bar(position = "dodge") +
  geom_text(
    mapping = aes(y = after_stat(count) + 1),
    stat = "count", position = position_dodge(.9), vjust = 0
  )

ggplot(trial) +
  aes(x = stage, fill = grade, label = after_stat(count)) +
  geom_bar(position = "dodge") +
  geom_text(
    mapping = aes(y = after_stat(count) - after_stat(count) + 1),
    stat = "count", position = position_dodge(.9), vjust = 0
  )

ggplot(trial) +
  aes(x = stage, fill = grade, label = after_stat(count)) +
  geom_bar(position = "fill") +
  geom_text(stat = "count", position = position_fill(0.5))

ggplot(trial) +
  aes(
    x = stage, fill = grade, 
    label = percent(after_stat(prop), accuracy = 1), 
    by = stage
  ) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(0.5))


ggplot(trial) +
  aes(
    x = stage, fill = grade, 
    label = percent(after_stat(prop), accuracy = 1), 
    by = stage
  ) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(0.5)) +
  facet_wrap(vars(trt))
