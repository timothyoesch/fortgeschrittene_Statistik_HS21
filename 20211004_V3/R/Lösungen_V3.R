df_votes = read_dta("aggregat2.dta")
model_1 = lm(acceptpo ~ suppshare2 + swjalager + nationalrat + srf2ja, data = df_votes)
summary(model_1)

### Übung 2
# Hypothese 1 "Je höher der Ja Anteil bei der zweiten SRF Umfrage, desto höher der Ja Anteil an der Urne"
model_h1 <- lm(acceptpo ~ srf2ja, data = df_votes)
confint(model_h1)

# Hypothese 2 "Je mehr Organisationen die Ja Parole zu einer Vorlage beschliessen, desto höher der Ja Anteil an der Urne."
model_h2 <- lm(acceptpo ~ swjalager, data = df_votes)
confint(model_h2)


### Übung 4- 7

df_votes_nrwm <- df_votes %>% 
  select(acceptpo, suppshare2, swjalager, nationalrat, srf2ja) %>% 
  na.ommit()

# Durchschnittlicher Effekt
coef(model_1)[-1] * colMeans(model.frame(model_1))[-1]

# Maximale Änderung
coef(model_1)[-1] * diff(apply(model.frame(model_1), 2, range))[-1]

# Standartisierter Koeffizient
library(QuantPsyc)
lm.beta(model_1)

# Anteil der erklärten Varianz
library(relaimpo)
calc.relimp(model_1, type = "lmg", rela = TRUE)
