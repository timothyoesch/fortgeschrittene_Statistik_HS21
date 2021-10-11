###########################################
### Fortgeschrittene Statistik: Woche 4 ###
###########################################

# Modell aus letzter Woche nochmals schätzen, 
# um danach damit arbeiten zu können

library(haven)
library(tidyverse)
df_votes = read_dta("aggregat2.dta")
model = lm(acceptpo ~ suppshare2 + swjalager, data = df_votes)

### Determinationskoeffizient R^2

#Varianzanalyse (Anova -> Analysis of variance)
v = anova(model)
v
SST = sum(v$`Sum Sq`)
SSR = sum(v$`Sum Sq`[-nrow(v)])
SSE = sum(v$`Sum Sq`[nrow(v)])

# R2 berechnen
R2 = SSR / SST
R2

MSE = v$`Mean Sq`[nrow(v)]
MST = SST / sum(v$Df)

# angepasstes R2 berechnen
adjR2 = 1 - MSE / MST
adjR2

# root mean squared error berechnen
RMSE = sqrt(MSE)
RMSE

# alles im Modelloutput ablesen
summary(model)

### AIC / BIC
df_votes = df_votes %>% filter(!is.na(suppshare2) & !is.na(swjalager) & !is.na(srf2ja))
model1 = lm(acceptpo ~ suppshare2 + swjalager, data = df_votes)
model2 = lm(acceptpo ~ suppshare2 + srf2ja, data = df_votes)

AIC(model1, model2)
BIC(model1, model2)

library(MuMIn)
AICc(model1, model2)
