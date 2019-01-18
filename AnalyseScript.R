# Analyse Script ----

install.packages("tidyverse")
install.packages("lubridate")
install.packages("psych")
install.packages("esquisse")
install.packages("ggthemes")
install.packages("ggplot2")
install.packages("jmv")

install.packages("devtools")
library(devtools)
devtools::install_github("HCIC/r-tools")

library(lubridate)
library(tidyverse)

source("surveymonkey.R")


# Datei laden ----

filename <- "data/SmartIdentification.csv"
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

#keyslist.edu <- list(EDU = c("edu"))

#keyslist.residence <- list(RESIDENCE = c("residence"))
                  
        
                  
# Skalenberechnung ----
#print("Hier werden später Skalen berechnet. Thema am 09.11.2018")

scores <- scoreItems(keyslist, raw.short, missing = TRUE, min = 1, max = 6)
#scores.edu <- scoreItems (keyslist.edu, raw.short, missing = TRUE, min = 1, max = 5)
#scores.residence <- scoreItems (keyslist.residence, raw.short, missing = TRUE, min = 1, max = 4)


data <- bind_cols(raw.short, as.tibble(scores$scores))
data <- data %>% filter (age != 99)
data <- data %>% 
  select(-starts_with("kut", ignore.case = F)) %>% 
  select(-starts_with("priv", ignore.case = F)) %>%
  select(-starts_with("dat", ignore.case = F)) %>%
  select(-starts_with("sich", ignore.case = F)) %>%
  select(-starts_with("dperso", ignore.case = F)) %>%
  select(-starts_with("dsave", ignore.case = F)) %>%
  select(-starts_with("dtype", ignore.case = F))

library(ggplot2)


data
raw.short

##### FEEDBACK: Sieht insgesamt schon ganz gut aus, ein paar Kleinigkeiten fehlen aber noch. Rufen Sie an dieser Stelle mal str(data) auf und schauen sich den Inhalt an. edu und residence sind z.B. auch ordinale Variablen, job und socme Faktoren.----
#str(data)
 
saveRDS(data, "data/smart_identification.rds")



## Unterschiedshypothesen ----
# Hypothese 1: Es gibt einen geschlechtspezifischen Unterschied bei der Freigabe von personenbezogenen Daten.
# H0: Es gibt keinen geschlechtsspezifischen Unterschied bei der Freigabe von personenbezogenen Daten.

t.test( filter(data, gender == "Männlich")$DPERSO,
        filter(data, gender == "Weiblich")$DPERSO )

# Boxplot Unterschiedshypothese 1
data %>% 
  filter(gender != "Keine Angabe") %>%  
ggplot() +
  aes(x = gender, y = DPERSO) +
  geom_boxplot(fill = "#ffffff") +
  labs(title = "Geschlechtsspezifischer Unterschied bei der Freigabe personenbezogener Daten",
       x = "Geschlecht",
       y = "Freigabe personenbezogener Daten",
       caption = "n = 273, Punkte zeigen Ausreißer",
       subtitle = "Boxplot des Geschlecht nach Freigabe personenbezogener Daten") +
  theme_gray()

ggsave("dperso_histogramm.pdf", width = 7, height = 4)

#Likert Skala Unterschiedshypothese 1
raw.short$dperso1 <- factor(raw.short$dperso1, labels = scale.zustimmung2)
raw.short$dperso2 <- factor(raw.short$dperso2, labels = scale.zustimmung2)
raw.short$dperso3 <- factor(raw.short$dperso3, labels = scale.zustimmung2)
raw.short$dperso4 <- factor(raw.short$dperso4, labels = scale.zustimmung2)
raw.short$dperso5 <- factor(raw.short$dperso5, labels = scale.zustimmung2)

pl <- raw.short %>% 
  select(dperso1, dperso2, dperso3, dperso4, dperso5) %>% 
  as.data.frame() %>% 
  likert() %>% 
  plot() +
  labs(title = "Likert Diagramm Bereitschaft zur Preisgabe persönlicher Daten", y = "Prozent", x = "Preisgabe persönlicher Daten", fill = "Antwort")

pl

ggsave("Likert_DPERSO.pdf", width = 7, height = 4)

# Ergebnis: Es gibt keinen signifikanten, geschlechtsspezifischen Unterschied bei der Freigabe von personenbezogenen Daten (t(198.92) = 0.23, p = 0.8195). Die Nullhypothese H0 muss angenommen werden.
## FEEDBACK: Super! Das funktioniert so. Tipp für später: Da die Hypothese ungerichtet ist, ist es besonders wichtig, dass Sie die Mittelwerte für die einzelnen Gruppen nicht vergessen.

# Hypothese 2: Das subjektive Sicherheitsempfinden ist bei kontrolliertem Alterseinfluss abhängig vom Geschlecht.
# H0: Das subjektive Sicherheitsempfinden ist bei kontrolliertem Alterseinfluss nicht abhängig vom Geschlecht.

library(jmv)
res <- data %>% filter(gender != "Keine Angabe") %>% 
  ancova(dep = "SICH", factors = c("gender"), covs = "age", 
         emmPlots = T, emMeans = list("gender"), effectSize = "eta")
plot1 <- res$emm 

data %>% filter(gender != "Keine Angabe") %>% ggplot() + aes(x=gender, y=SICH) + stat_summary() +
  scale_y_continuous(limits=c(1,6)) +
  labs(title = "Geschlechtsspezifischer Unterschied beim subjektiven Sicherheitsempfinden",
       x = "Geschlecht",
       y = "subjektives Sicherheitsempfinden [1-6]",
       caption = "n = 272, Fehlerbalken zeigt Standardfehler",
       subtitle = "Punktdiagramm von Geschlecht nach Sicherheitsempfinden") +
  theme_gray()

ggsave("Punktdiagramm_SICH-Gender.pdf", width = 7, height = 4)

raw.short$sich1 <- factor(raw.short$sich1, labels = scale.zustimmung2)
raw.short$sich2 <- factor(raw.short$sich2, labels = scale.zustimmung2)
raw.short$sich3 <- factor(raw.short$sich3, labels = scale.zustimmung2)
raw.short$sich4 <- factor(raw.short$sich4, labels = scale.zustimmung2)
raw.short$sich5 <- factor(raw.short$sich5, labels = scale.zustimmung2)
raw.short$sich6 <- factor(raw.short$sich6, labels = scale.zustimmung2)

pl <- raw.short %>% 
  select(sich1, sich2, sich3, sich4, sich5, sich6) %>% 
  as.data.frame() %>% 
  likert() %>% 
  plot() +
  labs(title = "Likert Diagramm subjektives Sicherheitsempfinden", y = "Prozent", x = "subjektives Sicherheitsempfinden", fill = "Antwort")

pl

ggsave("Likert_SICH.pdf", width = 7, height = 4)
#Ergegbnis: Das Geschlecht hat einen signifikanten Einfluss auf das subjektive Sicherheitsempfinden, das Alter allerdings nicht.
# Richtige Formulierung?
## FEEDBACK: Wie sie selbst schon richtig kommentiert haben, ist hier die ANCOVA die richtige Methode. 

install.packages("plotrix")
library(plotrix)

# Hypothese 3: Es gibt einen geschlechtsspezifischen Unterschied hinsichtlich der Einstellung zur Dauer der Datenspeicherung unter dem kontrollierten Einfluss des KUT.
# H0: Es gibt keinen geschlechtsspezifischen Unterschied hinsichtlich der Einstellung zur Dauer der Datenspeicherung unter dem kontrollierten Einfluss des KUT.

ancova(data, dep = "DSAVE", factors = c("gender"), covs = "KUT")

#Ergebnis: Es gibt keinen signifikanten Unterschied. Die Nullhypothese wird beibehalten. 
## FEEDBACK: Die zweite Zeile funktioniert natürlich so nicht, da das keine Antwortmöglichkeit darstellt.
# Sie können mehrere logische Aussagen mit logischem oder verknüpfen: ||  <- das ist das Zeichen für logisches Oder.
# z.B. filter(data, edu == "Hauptschulabschluss" || edu == "Realschulabschluss")
# Grundsätzlich ist die Gruppeneinteilung in dieser Hypothese aber recht problematisch. Warum verläuft die Grenze ausgerechnet am Hauptschulabschluss?
# Besser wäre es, hier eine Zusammenhangshypothese zu machen (Variante 1): Die Einstellung zur usw. hängt mit dem Schulabschluss zusammen.
# Oder Sie regeln das mit Median-Split (Variante 2): Die gebildetere Hälfte der Stichprobe hat eine höhere xyz als die andere Hälfte.

# Zusammenhangshypothesen ----

### Zusammenhangshypothese 1: Alter und subjektives Sicherheitsempfinden  
## H1: Das subjektive Sicherheitsempfinden hängt ab vom Alter der Probanden. 
## H0: Es gibt keinen Zusammenhang zwischen dem subjektiven Sicherheitsempfinden und dem Alter der Probanden.

cor.test(data = data, ~ age+SICH)

library(ggplot2)



#Ergebnis: Es gibt keinen signifikanten Zusammenhang zwischen dem subjektiven Sicherheitsempfinden und dem Alter der Probanden. H0 wird beibehalten.

### Zusammenhangshypothese 2: KUT und Bereitschaft zur langfristigen Datenspeicherung
## H1: Es besteht ein Zusammenhang zwischen KUT und der Bereitschaft zur langfristigen Datenspeicherung.
## H0: Es besteht kein Zusammenhang zwischen KUT und der Bereitschaft zur langfristigen Datenspeicherung.

cor.test(data= data, ~ KUT+DSAVE)
install.packages("likert")
library(likert)

# Likert Skala Zusammenhangshypothese 2
raw.short$kut1 <- factor(raw.short$kut1, labels = scale.zustimmung)
raw.short$kut2 <- factor(raw.short$kut2, labels = scale.zustimmung)
raw.short$kut3 <- factor(raw.short$kut3, labels = scale.zustimmung)
raw.short$kut4 <- factor(raw.short$kut4, labels = scale.zustimmung)
raw.short$kut5 <- factor(raw.short$kut5, labels = scale.zustimmung)
raw.short$kut6 <- factor(raw.short$kut6, labels = scale.zustimmung)
raw.short$kut7 <- factor(raw.short$kut7, labels = scale.zustimmung)
raw.short$kut8 <- factor(raw.short$kut8, labels = scale.zustimmung)

raw.short$dsave1 <- factor(raw.short$dsave1, labels = scale.zustimmung2)
raw.short$dsave2<- factor(raw.short$dsave2, labels = scale.zustimmung2)
raw.short$dsave3 <- factor(raw.short$dsave3, labels = scale.zustimmung2)

pl <- raw.short %>% 
  select(kut1, kut2, kut3, kut4, kut5, kut6, kut7, kut8, dsave1, dsave2, dsave3) %>% 
  as.data.frame() %>% 
  likert() %>% 
  plot() +
  labs(title = "Likert Diagramm KUT und Bereitschaft zur langfristigen Datenspeicherung", y = "Prozent", x = "KUT und DSAVE", fill = "Antwort")

pl
ggsave("Likert_KUTDSAVE.pdf", width = 7, height = 4)

#Ergebnis: Es gibt keinen Zusammenhang zwischen dem KUT und der Bereitschaft zur langfristigen Datenspeicherung. H0 wird beibehalten.

### Zusammenhangshypothese 3: Einstellung zur Privatsphäre und Bereitschaft persönliche Daten preiszugeben
## H1: Es besteht ein Zusammenhang zwischen der Einstellung zur Privatsphäre und der Bereitschaft persönliche Daten preiszugeben. 
## H0: Es gibt keinen Zusammenhang zwischen der Einstellung zur Privatsphäre und der Bereiteschaft persönliche Daten preiszugeben. 

cor.test(data = data, ~PRIV+DPERSO)

#Likert Skala Zusammenhangshypothese 3
raw.short$priv1 <- factor(raw.short$priv1, labels = scale.zustimmung2)
raw.short$priv2 <- factor(raw.short$priv2, labels = scale.zustimmung2)
raw.short$priv3 <- factor(raw.short$priv3, labels = scale.zustimmung2)

raw.short$dperso1 <- factor(raw.short$dperso1, labels = scale.zustimmung2)
raw.short$dperso2 <- factor(raw.short$dperso2, labels = scale.zustimmung2)
raw.short$dperso3 <- factor(raw.short$dperso3, labels = scale.zustimmung2)
raw.short$dperso4 <- factor(raw.short$dperso4, labels = scale.zustimmung2)
raw.short$dperso5 <- factor(raw.short$dperso5, labels = scale.zustimmung2)

pl <- raw.short %>% 
  select(priv1, priv2, priv3, dperso1, dperso2, dperso3, dperso4, dperso5) %>% 
  as.data.frame() %>% 
  likert() %>% 
  plot() +
  labs(title = "Likert Diagramm Einstellung zur Privatsphäre und Preisgabe persönlicher Daten", y = "Prozent", x = "PRIV und DPERSO", fill = "Antwort")

pl

ggsave("Likert_PRIVDPERSO.pdf", width = 8, height = 4)

# Ergebnis: Es gibt keinen Zusammenhang zwischen der Einstellung zur Privatsphäre und der Bereiteschaft persönliche Daten preiszugeben. H0 wird beibehalten.

## Feedback: Ihr Datensatz heißt gar nicht df_multi ;-)
# Ob bei dem richtigen Datensatz so funktioniert weiß ich nicht, es kann nämlich sein dass die Methode cor.test() zwischen Groß- und Kleinschreibung unterscheidet. Lieber genau so schreiben wie in den Daten: Groß. 
# Warum wird bei H3 die round()-Methode benutzt? Und warum kendall-tau? Das muss beides nicht falsch sein, aber mir ist gerade nicht klar, warum Sie sich entschieden haben das so zu machen. 


