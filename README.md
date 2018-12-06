# PredictivePolicing
## In diesem Forschungsprojekt soll die Akzeptanz von Predictive Policing untersucht werden.

# Teammitglieder
## Susanne Gohr, Ines Güldenberg, Alexa Otte, Laura Schröder und Isabelle Wanders

# Forschungsfrage
## _Inwiefern beeinflussen Nutzerfaktoren die Akzeptanz von Predictive Policing in der Bevölkerung?_

# Faktorenraum

![tooltip](images/Faktorenraum_PredictivePolicing.png)
## Unterschiedshypothesen

1. Menschen mit einem hohen Bedürfnis nach Datensicherheit lehnen die Freigabe von personenbezogenen Daten für Predictive Policing eher ab, als Menschen mit einem niedrigen Bedürfnis nach Datensicherheit. 

2. Frauen sind eher bereit personenbezogene Daten freizugeben, wenn sich dadurch ihr subjektives Sicherheitsempfinden erhöht, als Männer.

3. Ältere Menschen sind eher zur permanenten Datenspeicherung bereit, als der jüngere Teil der Bevölkerung. 


## Data Cleaning
- 29.11.2018: Bereinigung des Datensatzes in R
- 27.11.2018: Erfassung des Dummy-Datensatzes

## neue Unterschiedshypothesen
Stand: 04.12.2018

1. Es gibt einen geschlechtspezifischen Unterschied bei der Freigabe von personenbezogenen Daten.

2. Es gibt einen Unterschied hinsichtlich des subjektiven Sicherheitsempfindens bei Männer und Frauen. 

3. Es gibt einen Unterschied hinsichtlich der Einstellung zur Dauer der Datenspeicherung bei Menschen mit höherem und niedrigerem Bildungsabschluss.

## Nullhypothesen
Stand: 06.12.2018 

1. Es gibt keinen geschlechtspezifischen Unterschied bei der Freigabe von personenbezogenen Daten.

2. Es gibt keinen Unterschied hinsichtlich des subjektiven Sicherheitsempfindens bei Männer und Frauen.

3. : Es gibt keinen Unterschied hinsichtlich der Einstellung zur Dauer der Datenspeicherung bei Männern und Frauen.




## Hinweise zum Umgang mit P-Values

Definition: "Die Wahrscheinlichkeit des beobachteten Ergebnisses, wenn die Nullhypothese stimmt."
- Der P-Wert hilft Ergebnisse von statistischen Tests zu interpretieren.
- Überschreitet der P-Wert 0,05, kann die Nullhypothese nicht verworfen werden.

- Problem: Häufige Überbewertung des P-Werts
1. Statistisch signifikantes Ergebnis kann irrelevant sein. Es werden Aussagen über die Daten getroffen und nicht über den Wahrheitsgehalt der Hypothesen.
2. Man braucht mehrere Studien um Aussagen zu treffen. Ein signifikanter P-Wert kann als Ausgangspunkt für weitere Forschung dienen. Daher ist es wichtig, den P-Wert zu betrachten.

- Quellen: http://www.perfendo.org/docs/BayesProbability/twelvePvaluemisconceptions.pdf
           http://blog.minitab.com/blog/adventures-in-statistics-2/not-all-p-values-are-created-equal
           https://www.youtube.com/watch?v=Gu_PGbINiXw



## _Frühere Versionen_
Am 29.11.2018 wurde diese Version aus folgenden Gründen angepasst:
- Aktualisierung zur Datensatzbereinigung
- Einfügen der Unterschiedshypothesen

Am 06.11.2018 wurde diese Version aus folgenden Gründen angepasst:
- Anpassung der Forschungsfrage
- Anpassung des Faktorenraums

