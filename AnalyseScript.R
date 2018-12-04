# Analyse Script ----

#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("psych")
#install.packages("esquisse")
#install.packages("ggthemes")
#install.packages("ggplot2")

#install.packages("devtools")
library(devtools)
devtools::install_github("HCIC/r-tools")

library(lubridate)
library(tidyverse)

source("surveymonkey.R")


# Datei laden ----

filename <- "data/smart_identification.csv"
raw <- load_surveymonkey_csv(filename)


# Data Cleaning ----
#print("Hier wird der Datensatz aufbereitet. Thema am 09.11.2018")

raw.short <- raw[ ,c(-1:-9, -24, -32:-89, -107:-127)]

#View(raw.short)

generate_codebook(raw.short, "codebook.csv")

codebook <- read_codebook("codebook_final.csv")

names(raw.short) <- codebook$variable

# raw.short %>% View()

# Schritt3: Variablen den richtigen Typen zuordnen
raw.short$gender <- as.factor(raw.short$gender)
raw.short$job <- as.factor(raw.short$job)
raw.short$socme <- as.factor(raw.short$socme)

scale.education <- c("Hauptschulabschluss",
                     "Realschulabschluss",
                     "Fachabitur/Abitur",
                     "Berufsausbildung",
                     "Studienabschluss (Bachelor, Master, Magister, Diplom, Promotion etc.)")

scale.residence <- c("Ich wohne auf dem Land.",
                     "Ich wohne im Vorort einer Stadt.",
                     "Ich wohne am Stadtrand.",
                     "Ich wohne zentral in einer Stadt.")



scale.zustimmung <- c("Stimme gar nicht zu",
                      "Stimme nicht zu",
                      "Stimme eher nicht zu",
                      "Stimme eher zu",
                      "Stimme zu",
                      "Stimme völlig zu")

scale.zustimmung2 <- c("stimme gar nicht zu",
                       "stimme nicht zu",
                       "stimme eher nicht zu",
                       "stimme eher zu",
                       "stimme zu",
                       "stimme völlig zu")



# kut1-8 = = scale.zustimmung
raw.short$edu <- ordered(raw.short$edu, levels = scale.education)

raw.short$residence <- ordered(raw.short$residence, levels = scale.residence)

raw.short$kut1 <- ordered(raw.short$kut1, levels = scale.zustimmung)
raw.short$kut2 <- ordered(raw.short$kut2, levels = scale.zustimmung)
raw.short$kut3 <- ordered(raw.short$kut3, levels = scale.zustimmung)
raw.short$kut4 <- ordered(raw.short$kut4, levels = scale.zustimmung)
raw.short$kut5 <- ordered(raw.short$kut5, levels = scale.zustimmung)
raw.short$kut6 <- ordered(raw.short$kut6, levels = scale.zustimmung)
raw.short$kut7 <- ordered(raw.short$kut7, levels = scale.zustimmung)
raw.short$kut8 <- ordered(raw.short$kut8, levels = scale.zustimmung)

raw.short$priv1 <- ordered(raw.short$priv1, levels = scale.zustimmung2)
raw.short$priv2 <- ordered(raw.short$priv2, levels = scale.zustimmung2)
raw.short$priv3 <- ordered(raw.short$priv3, levels = scale.zustimmung2)

raw.short$dat1 <- ordered(raw.short$dat1, levels = scale.zustimmung2)
raw.short$dat2 <- ordered(raw.short$dat2, levels = scale.zustimmung2)
raw.short$dat3 <- ordered(raw.short$dat3, levels = scale.zustimmung2)

raw.short$sich1 <- ordered(raw.short$sich1, levels = scale.zustimmung2)
raw.short$sich2 <- ordered(raw.short$sich2, levels = scale.zustimmung2)
raw.short$sich3 <- ordered(raw.short$sich3, levels = scale.zustimmung2)
raw.short$sich4 <- ordered(raw.short$sich4, levels = scale.zustimmung2)
raw.short$sich5 <- ordered(raw.short$sich5, levels = scale.zustimmung2)
raw.short$sich6 <- ordered(raw.short$sich6, levels = scale.zustimmung2)

raw.short$dperso1 <- ordered(raw.short$dperso1, levels = scale.zustimmung2)
raw.short$dperso2 <- ordered(raw.short$dperso2, levels = scale.zustimmung2)
raw.short$dperso3 <- ordered(raw.short$dperso3, levels = scale.zustimmung2)
raw.short$dperso4 <- ordered(raw.short$dperso4, levels = scale.zustimmung2)
raw.short$dperso5 <- ordered(raw.short$dperso5, levels = scale.zustimmung2)

raw.short$dsave1 <- ordered(raw.short$dsave1, levels = scale.zustimmung2)
raw.short$dsave2<- ordered(raw.short$dsave2, levels = scale.zustimmung2)
raw.short$dsave3 <- ordered(raw.short$dsave3, levels = scale.zustimmung2)

raw.short$dtype1 <- ordered(raw.short$dtype1, levels = scale.zustimmung2)
raw.short$dtype2 <- ordered(raw.short$dtype2, levels = scale.zustimmung2)
raw.short$dtype3 <- ordered(raw.short$dtype3, levels = scale.zustimmung2)


#### Skalenbildung ----
library(psych)

# 1. Schritt: Schlüsselliste > Liste enthält Variablen, die wir berechnen wollen.

keyslist <- list (KUT= c("kut1", "-kut2", "kut3", "kut4", "-kut5", "kut6", "-kut7", "-kut8"),
                  PRIV = c("priv1", "priv2", "-priv3"),
                  DAT = c("dat1", "dat2", "-dat3"),
                  SICH = c("-sich1", "-sich2", "sich3", "-sich4", "-sich5", "sich6"),
                  DPERSO = c("dperso1", "-dperso2", "dperso3", "-dperso4", "-dperso5"),
                  DSAVE = c("dsave1", "dsave2", "-dsave3"),
                  DTYPE = c("dtype1", "-dtype2", "dtype3"))

keyslist.edu <- list(EDU = c("edu"))

keyslist.residence <- list(RESIDENCE = c("residence"))
                  
        
                  
# Skalenberechnung ----
#print("Hier werden später Skalen berechnet. Thema am 09.11.2018")

scores <- scoreItems(keyslist, raw.short, missing = TRUE, min = 1, max = 6)
scores.edu <- scoreItems (keyslist.edu, raw.short, missing = TRUE, min = 1, max = 5)
scores.residence <- scoreItems (keyslist.residence, raw.short, missing = TRUE, min = 1, max = 4)


data <- bind_cols(raw.short, as.tibble(scores$scores))
data <- data %>% 
  select(-starts_with("kut", ignore.case = F)) %>% 
  select(-starts_with("priv", ignore.case = F)) %>%
  select(-starts_with("dat", ignore.case = F)) %>%
  select(-starts_with("sich", ignore.case = F)) %>%
  select(-starts_with("dperso", ignore.case = F)) %>%
  select(-starts_with("dsave", ignore.case = F)) %>%
  select(-starts_with("dtype", ignore.case = F))

data
raw.short

##### FEEDBACK: Sieht insgesamt schon ganz gut aus, ein paar Kleinigkeiten fehlen aber noch. Rufen Sie an dieser Stelle mal str(data) auf und schauen sich den Inhalt an. edu und residence sind z.B. auch ordinale Variablen, job und socme Faktoren.----
#str(data)
 
saveRDS(data, "data/smart_identification.rds")


# Analyse ----
#print("Hier werden später statistische Analysen durchgeführt. Thema ab dem 16.11.2018")

# Graphik erstellung ---- 
#print("Hier werden später Grafiken erstellt. Thema ab dem 16.11.2018")


