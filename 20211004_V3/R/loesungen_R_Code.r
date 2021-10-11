# Aufgabe 2
library(haven)
library(tidyverse)
df_votes = read_dta("aggregat2.dta")
model = lm(acceptpo ~ suppshare2 + swjalager + nationalrat + srf2ja,
           data = df_votes)
summary(model)

# Aufgabe 3 (Beispiel mit srf2ja)
df_votes = df_votes %>% 
  select(acceptpo, suppshare2, swjalager, nationalrat, srf2ja) %>% 
  na.omit()
model_kontrollvariablen = lm(acceptpo ~ suppshare2 + swjalager + nationalrat, data = df_votes)
model_hauptvariable = lm(srf2ja ~ suppshare2 + swjalager + nationalrat,
                   data = df_votes)
u = residuals(model_kontrollvariablen)
v = residuals(model_hauptvariable)
model_alles = lm(u ~ v)
summary(model_alles)

# Aufgabe 4
coef(model)[-1] * colMeans(model.frame(model))[-1] 
# -> srf2ja

# Aufgabe 5
coef(model)[-1] * diff(apply(model.frame(model), 2, range))[-1]
# -> srf2ja

# Aufgabe 6
library(QuantPsyc)
lm.beta(model)
# -> srf2ja

# Aufgabe 7
library(relaimpo)
calc.relimp(model, type = "lmg", rela = TRUE)
# -> srf2ja