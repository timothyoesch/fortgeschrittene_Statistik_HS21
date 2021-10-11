###########################################
### Fortgeschrittene Statistik: Woche 2 ###
###########################################

### Einführungsbeispiel

library(tidyverse)
library(haven)

# Daten einlesen und filtern
df_votes = read_dta("aggregat2.dta")
df_votes = df_votes %>% filter(id > 333) # ab Ende 1985

## Welchen Einfluss hat der Ja-Anteil im Nationalrat auf
## den Ja-Anteil an der Urne?

# Streudiagram
ggplot(df_votes, aes(x = nationalrat, y = acceptpo)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  xlab("Schlussabstimmung Nationalrat (Verhältnis Ja/Nein-Stimmen ohne Enthaltungen)") + 
  ylab("Anteil Ja-Stimmen an der Urne")

# lineare Regressionslinie hinzufügen
# ggplot(df_votes, aes(x = nationalrat, y = acceptpo)) +
ggplot(df_votes, aes(x = nationalrat, y = acceptpo)) + 
  geom_point() +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  xlab("Schlussabstimmung Nationalrat (Verhältnis Ja/Nein-Stimmen ohne Enthaltungen)") + 
  ylab("Anteil Ja-Stimmen an der Urne") +
  geom_smooth(method = "lm", se = FALSE)


### OLS Schätzer

# remove missings
df_votes = df_votes %>% filter(!is.na(nationalrat) & !is.na(acceptpo))

# Kovarianz
cov_xy = sum((df_votes$acceptpo - mean(df_votes$acceptpo)) *
               (df_votes$nationalrat - mean(df_votes$nationalrat))) / (nrow(df_votes)-1)
cov(df_votes$acceptpo, df_votes$nationalrat)

# Varianz
var_x = sum((df_votes$nationalrat - mean(df_votes$nationalrat))^2) / (nrow(df_votes)-1) 
var(df_votes$nationalrat)

# beta1
beta1 = cov_xy / var_x
beta1

# beta0
beta0 = mean(df_votes$acceptpo) - beta1 * mean(df_votes$nationalrat)
beta0

## lineare Regression Schätzen
model = lm(acceptpo ~ nationalrat, data = df_votes)
model

# calculate residuals (e = y - yhat) (y: acceptpo, yhat: beta0 + beta1 nationalrat)
e = df_votes$acceptpo - (beta0 + beta1 * df_votes$nationalrat)

# s^2
s2 = sum(e^2) / (nrow(df_votes) - 2)

# s.e. von beta0
se_beta0 = sqrt(s2 * (1/nrow(df_votes) + 
    mean(df_votes$nationalrat)^2 / 
    sum((df_votes$nationalrat - mean(df_votes$nationalrat))^2)))

# s.e. von beta1
se_beta1 = sqrt(s2 / sum((df_votes$nationalrat - mean(df_votes$nationalrat))^2))

# mit der Funktion lm
summary(model)


### lineare Regression schätzen und interpretieren

model1 = lm(acceptpo ~ nationalrat, data = df_votes)
summary(model1)

# für den Datensatz:
# install.packages("devtools")
# devtools::install_github("benjaminschlegel/schlegel")

# mit Gewicht
model2 = lm(lr_self ~ age, data = schlegel::selects2015,
            weights = weight_total)
summary(model2)

### Hypothesen testen

## alpha-Wert von 0.05 (= 0.025 auf beiden Seiten)

# get betas
beta = coef(model1)

# get s.e.
se = sqrt(diag(vcov(model1)))

# 95% confidence interval
lower = beta + se * qt(0.025, summary(model1)$df[2])
upper = beta + se * qt(0.025, summary(model1)$df[2], lower.tail = FALSE)
cbind(lower, beta, upper)
summary(model)

# Konfidenzinterval direkt berechnen
confint(model1)
