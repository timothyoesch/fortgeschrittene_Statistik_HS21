###########################################
### Fortgeschrittene Statistik: Woche 3 ###
###########################################

### Multivariate Regression

library(haven)
library(tidyverse)

# Daten einlesen und filtern
df_votes = read_dta("aggregat2.dta")

# y_i = beta_0 + beta_1*x_1i + beta_2*x_2i + epsilon_i
# acceptpo_i = beta_0 + beta_1 * suppshare2_i + beta_2 * swjalager_i + epsilon_i

model = lm(acceptpo ~ suppshare2 + swjalager, data = df_votes)
summary(model)

## schÃ¶ne Tabelle
library(texreg)
screenreg(model, custom.coef.names = c("Konstante", "Ja-Anteil Ausgaben",
                                       "Ja-Anteil Parteiparolen"))
# word
htmlreg(model, custom.coef.names = c("Konstante", "Ja-Anteil Ausgaben",
                                     "Ja-Anteil Parteiparolen"),
        file = "Regressionstabelle.html")

# latex
texreg(model, custom.coef.names = c("Konstante", "Ja-Anteil Ausgaben",
                                    "Ja-Anteil Parteiparolen"),
       file = "Regressionstabelle.tex")

### Multivariate Regression "von Hand"


# NAs entfernen, um von Hand rechnen zu können
df_votes = df_votes %>% filter(!is.na(suppshare2) & !is.na(swjalager))

# beta_1 schÃ¤tzen
model_y_x2 = lm(acceptpo ~ swjalager, data = df_votes)
u1 = residuals(model_y_x2)

model_x1_x2 = lm(suppshare2 ~ swjalager, data = df_votes)
v1 = residuals(model_x1_x2)

summary(lm(u1 ~ v1))

# beta_2 schÃ¤tzen
model_y_x2 = lm(acceptpo ~ suppshare2, data = df_votes)
u2 = residuals(model_y_x2)

model_x1_x2 = lm(swjalager ~ suppshare2, data = df_votes)
v2 = residuals(model_x1_x2)

summary(lm(u2 ~ v2))

# multivariate Regression
model = lm(acceptpo ~ suppshare2 + swjalager, data = df_votes)
summary(model)

### Variablen Wichtigkeit

## Level Importance (Durchschnittlicher Effekt)
coef(model)[-1] * colMeans(model.frame(model))[-1] 
# [-1]: Entfernung von beta_0 und der AV

## maximale Änderung
coef(model)[-1] * diff(apply(model.frame(model), 2, range))[-1]

## Dispersion Importance (Standarditisierte Koeffizienten)
library(QuantPsyc)
lm.beta(model)

## Anteil am R^2
library(relaimpo)
calc.relimp(model, type = "lmg")
calc.relimp(model, type = "lmg", rela = TRUE)