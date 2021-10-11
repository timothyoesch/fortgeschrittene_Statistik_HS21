rm(list = ls())

library(tidyverse)
library(haven)

## Übung 1
df_votes = read_dta("aggregat2.dta")
df_votes = df_votes %>% filter(id > 333) # ab Ende 1985

ggplot(df_votes, aes(x = srf2ja, y = acceptpo)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  xlab("2. Welle Umfrage SRF") + 
  ylab("Anteil Ja-Stimmen an der Urne") +
  geom_smooth(method="lm", se = FALSE)


## Übung 2
df_FHindex <- read.csv("freedom_house_index.csv")

ggplot(df_FHindex, aes(x = Democratic_Score, y = GDPPC)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 50000)) +
  theme_minimal() +
  xlab("Demokratisierungsgrad") + 
  ylab("BIP / Kopf") +
  geom_smooth(method="lm", se = FALSE)


## Übung 10

model <- lm(acceptpo ~ srf2ja, data = df_votes)
model
summary(model)


# Übung 15
model_GDP_Demo <- lm(GDPPC ~ Democratic_Score, data = df_FHindex)
beta = coef(model_GDP_Demo)
se = sqrt(diag(vcov(model_GDP_Demo)))
lower = beta + se * qt(0.025, summary(model_GDP_Demo)$df[2])
upper = beta + se * qt(0.025, summary(model_GDP_Demo)$df[2], lower.tail = FALSE)
cbind(lower, beta, upper)
summary(model_GDP_Demo)