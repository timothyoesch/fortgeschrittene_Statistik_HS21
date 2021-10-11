library(haven)
library(tidyverse)
df_votes_1 = read_dta("aggregat2.dta")

## AUFGABE 2
model1 = lm(acceptpo ~ suppshare2 + swjalager + nationalrat + srf2ja, data = df_votes_1)
summary(model1)

# Lösung: Multiple R-squared:  0.803 (Anteil der Varianz, welchen das Modell erklären kann),	Adjusted R-squared:  0.7894 (angepasstes R^2)

## AUFGABE 3
model2 = lm(acceptpo ~ swjalager + nationalrat + srf2ja, data = df_votes_1)
summary(model2)

# Lösung: 
# Wie stark verändert sich R^2? 0.7909 - 0.803 = **** -0.0121 ****
# Wie stark verändert sich adj. R^2? 0.7832 - 0.7894 = **** -0.0062 ****


## AUFGABE 5

df_votes_filtered = df_votes_1 %>% filter(!is.na(suppshare2) & !is.na(swjalager) & !is.na(nationalrat) & !is.na(srf2ja))
model1 = lm(acceptpo ~ suppshare2 + swjalager + nationalrat + srf2ja, data = df_votes_filtered)
model2 = lm(acceptpo ~ swjalager + nationalrat + srf2ja, data = df_votes_filtered)

AIC(model1, model2)
# df      AIC
# model1  6 437.3124
# model2  5 437.9445

BIC(model1, model2)
#        df      BIC
# model1  6 450.1712
# model2  5 448.6602

# Lösung: Model 2 ist besser.