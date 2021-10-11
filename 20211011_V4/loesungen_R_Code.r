# Aufgabe 2
df_votes = haven::read_dta("aggregat2.dta")
m1 = lm(acceptpo ~ suppshare2 + swjalager + nationalrat + srf2ja, data = df_votes)
summary(m1)
# 80,3% ; angepasstes R^2: 0.7894

# Aufgabe 3
m2 = lm(acceptpo ~ swjalager + nationalrat + srf2ja, data = df_votes)
nobs(m1)
nobs(m2)
# Achtung: nicht gleicher Stichprobenumfang => zuerst NAs entfernen von suppshare2

df_votes = df_votes %>% filter(!is.na(suppshare2))
m2 = lm(acceptpo ~ swjalager + nationalrat + srf2ja, data = df_votes)
nobs(m2) # jetzt gleich wie bei Modell 1
summary(m2)
# 79.65% ; 0.7841

# RMSE berechnen
a1 = anova(m1)
a2 = anova(m2)
MSE1 = a1$`Mean Sq`[nrow(a1)]
MSE2 = a2$`Mean Sq`[nrow(a2)]
RMSE1 = sqrt(MSE1)
RMSE2 = sqrt(MSE2)
RMSE1
RMSE2

# Modell 1 ist besser

# Aufgabe 5
BIC(m1, m2) # Modell 2 ist besser
AIC(m1, m2) # Modell 1 ist besser
