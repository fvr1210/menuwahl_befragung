
# Umbennung, Umcodierung, Überprüfung, Bereinigung und zusätzliche Variablen der Befragungsdaten --------

# Umlaute werden als ae, ue, oe geschrieben

# Dies ist eine Copy des Skripts 01_clean_recode_181102_05vori_de.R, bearbeitet für die teil anonymisierung die Vorgenommen wurde

## Status: 21.01.2021 // vori

# Benoetigte Packete -----------
library ("tidyverse") #Version 1.2.1
library ("lubridate") # Version 1.7.4

# Daten einlesen ---------

df_n <- read_delim("2017_ZHAW_vonRickenbach_survey_dataset_NOVANIMAL_anonym.csv", delim=';', locale = locale(encoding = 'ISO-8859-2')) 


# Dieser Datensatz beinhaltet alle Fragebogen die an der NOVANIMAL Befragung an den beiden Campus Grüental und Reidbach ausgefuellt und retourniert wurden. N = 1203


# Umbenennen und Umcodieren der Variablen auf der Grundlage des Codebogens-----------

# Frage 1: Welches Menue / Essen haben Sie heute gewaehlt?
df_n <- df_n %>% rename ("meal" = fr_230) 

df_n$meal = factor(df_n$meal, levels = 1:5, labels = c("Favorite", "World", "Kitchen", "Hot & Cold", "Selber")) 

# Frage 2: Wie treffen die folgenden Aussagen auf Ihre heutige Menue-Wahl in der Mensa zu? Ich habe dieses Menue heute gewaehlt, weil...

df_n <- df_n %>% rename("choice_1" = fr_233, "choice_2" = fr_235,  "choice_3" = fr_236,  "choice_4" = fr_237,  "choice_5" = fr_238,  "choice_6" = fr_239,  "choice_7" = fr_240,  "choice_8" = fr_242,
                        "choice_9" = fr_244, "choice_10" = fr_245)

# Die zugewiesenen Werte des Codebogens werden umgekehrt, damit 4 fuer "trifft zu", 3 fuer "trifft eher zu", 2 fuer "trifft eher nicht zu" und 1 fuer "trifft nicht zu" steht. 

df_n$choice_1 <- dplyr::recode(df_n$choice_1, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$choice_2 <- dplyr::recode(df_n$choice_2, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$choice_3 <- dplyr::recode(df_n$choice_3, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$choice_4 <- dplyr::recode(df_n$choice_4, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$choice_5 <- dplyr::recode(df_n$choice_5, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$choice_6 <- dplyr::recode(df_n$choice_6, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$choice_7 <- dplyr::recode(df_n$choice_7, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$choice_8 <- dplyr::recode(df_n$choice_8, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$choice_9 <- dplyr::recode(df_n$choice_9, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$choice_10 <- dplyr::recode(df_n$choice_10, '1'=4,'2'=3,'3'=2,'4'=1)

# Frage 3: Wie zufrieden sind Sie mit dem gewaehlten Menue?

df_n <- df_n %>% rename ("satis_1" = fr_249, "satis_2" = fr_250, "satis_3" = fr_251)

# Umcodieren, damit 4 fuer "trifft zu", 3 fuer "trifft eher zu", 2 fuer "trifft eher nicht zu" und 1 fuer "trifft nicht zu" steht. 

df_n$satis_1 <- dplyr::recode(df_n$satis_1, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$satis_2 <- dplyr::recode(df_n$satis_2, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$satis_3 <- dplyr::recode(df_n$satis_3, '1'=4,'2'=3,'3'=2,'4'=1)


# Frage 4: Wie wichtig waren Ihnen folgende Inhaltsstoffe Ihres heutigen Essens? Mein heutiges Essen...
df_n <- df_n %>% rename ("ing_1" = fr_253, "ing_2" = fr_276, "ing_3" = fr_277, "ing_4" = fr_267)

#Umcodierung damit 4 fuer "wichtig", 3 fuer "eher wichtig", 2 fuer "eher nicht wichtig", 1 fuer "nicht wichtig" und -99 fuer "kann ich nicht beurteilen" steht.

df_n$ing_1 <- dplyr::recode(df_n$ing_1, '1'=4,'2'=3,'3'=2,'4'=1,'-99' = -99)
df_n$ing_2 <- dplyr::recode(df_n$ing_2, '1'=4,'2'=3,'3'=2,'4'=1,'-99' = -99)
df_n$ing_3 <- dplyr::recode(df_n$ing_3, '1'=4,'2'=3,'3'=2,'4'=1,'-99' = -99)
df_n$ing_4 <- dplyr::recode(df_n$ing_4, '1'=4,'2'=3,'3'=2,'4'=1,'-99' = -99)

# Frage 5: Wie wichtig waren Ihnen folgende Eigenschaften Ihres heutigen Essens? Mein heutiges Essen...
df_n <- df_n %>% rename ("att_1" = fr_265, "att_2" = fr_260, "att_3" = fr_261, "att_4" = fr_266, "att_5" = fr_255, "att_6" = fr_256, "att_7" = fr_257, "att_8" = fr_264, 
                         "att_9" = fr_263, "att_10" = fr_268, "att_11" = fr_278)

#Umcodieren, damit 4 fuer "wichtig", 3 fuer "eher wichtig", 2 fuer "eher nicht wichtig", 1 fuer "nicht wichtig", -99 fuer "kann ich nicht beurteilen" steht. 

df_n$att_1 <- dplyr::recode(df_n$att_1, '1'=4,'2'=3,'3'=2,'4'=1,'-99' = -99)
df_n$att_2 <- dplyr::recode(df_n$att_2, '1'=4,'2'=3,'3'=2,'4'=1,'-99' = -99)
df_n$att_3 <- dplyr::recode(df_n$att_3, '1'=4,'2'=3,'3'=2,'4'=1,'-99' = -99)
df_n$att_4 <- dplyr::recode(df_n$att_4, '1'=4,'2'=3,'3'=2,'4'=1,'-99' = -99)
df_n$att_5 <- dplyr::recode(df_n$att_5, '1'=4,'2'=3,'3'=2,'4'=1,'-99' = -99)
df_n$att_6 <- dplyr::recode(df_n$att_6, '1'=4,'2'=3,'3'=2,'4'=1,'-99' = -99)
df_n$att_7 <- dplyr::recode(df_n$att_7, '1'=4,'2'=3,'3'=2,'4'=1,'-99' = -99)
df_n$att_8 <- dplyr::recode(df_n$att_8, '1'=4,'2'=3,'3'=2,'4'=1,'-99' = -99)
df_n$att_9 <- dplyr::recode(df_n$att_9, '1'=4,'2'=3,'3'=2,'4'=1,'-99' = -99)
df_n$att_10 <- dplyr::recode(df_n$att_10, '1'=4,'2'=3,'3'=2,'4'=1,'-99' = -99)
df_n$att_11 <- dplyr::recode(df_n$att_11, '1'=4,'2'=3,'3'=2,'4'=1,'-99' = -99)

# Frage 6: Was beeinflusste sonst noch, was Sie heute gegessen haben? (Mehrfachantworten moeglich)

df_n <- df_n %>% rename ("diet" = fr_269_1, "allerg" = fr_269_2, "relig" = fr_269_3, "meds" = fr_269_4 )

# Keine Kreuze werden bei dieser Frage nicht als fehlende Werte behandelt.  
df_n$allerg[is.na(df_n$allerg)] <- 0
df_n$diet[is.na(df_n$diet)] <- 0
df_n$relig[is.na(df_n$relig)] <- 0
df_n$meds[is.na(df_n$meds)] <- 0



# Frage 7: Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zum Essen zu? Ich mache mir allgemein Gedanken ueber die Folgen... 
df_n <- df_n %>% rename ("tho_1" = fr_271, "tho_2" = fr_272, "tho_3" = fr_273, "tho_4" = fr_274, "tho_5" = fr_275)

#Umcodieren, damit 4 fuer "stimme zu", 3 fuer "stimme eher zu", 2 fuer "stimme eher nicht zu", 1 fuer "stimme nicht zu" steht. 
df_n$tho_1 <- dplyr::recode(df_n$tho_1, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$tho_2 <- dplyr::recode(df_n$tho_2, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$tho_3 <- dplyr::recode(df_n$tho_3, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$tho_4 <- dplyr::recode(df_n$tho_4, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$tho_5 <- dplyr::recode(df_n$tho_5, '1'=4,'2'=3,'3'=2,'4'=1)

# Frage 8: Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zu? Mir ist es allgemein wichtig,...
df_n <- df_n %>% rename ("tra_1" = fr_280, "tra_2" = fr_281, "tra_3" = fr_282, "tra_4" = fr_283, "tra_5" = fr_284)

#Umcodieren, damit 4 fuer "stimme zu", 3 fuer "stimme eher zu", 2 fuer "stimme eher nicht zu", 1 fuer "stimme nicht zu" steht.
df_n$tra_1 <- dplyr::recode(df_n$tra_1, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$tra_2 <- dplyr::recode(df_n$tra_2, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$tra_3 <- dplyr::recode(df_n$tra_3, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$tra_4 <- dplyr::recode(df_n$tra_4, '1'=4,'2'=3,'3'=2,'4'=1)
df_n$tra_5 <- dplyr::recode(df_n$tra_5, '1'=4,'2'=3,'3'=2,'4'=1)



# Frage 9: Wie sehen Ihre Ernaehrungsgewohnheiten aus? 
df_n <- df_n %>% rename ("meat" = fr_286, "milk" = fr_287, "veget" = fr_288, "veg" = fr_289)


df_n$meat <- dplyr::recode(df_n$meat, '1'=7,'2'=6,'3'=5,'4'=4,'5'=3,'6'=2,'7'=1)
df_n$milk <- dplyr::recode(df_n$milk, '1'=7,'2'=6,'3'=5,'4'=4,'5'=3,'6'=2,'7'=1)
df_n$veget <- dplyr::recode(df_n$veget, '1'=6,'2'=5,'3'=4,'4'=3,'5'=2,'6'=1)
df_n$veg <- dplyr::recode(df_n$veg, '1'=6,'2'=5,'3'=4,'4'=3,'5'=2,'6'=1)


# Frage 10: An wie vielen Tagen pro Woche halten Sie sich im Campus Grueental/Reidbach auf? 

df_n <- df_n %>% rename("pensum" = fr_290)


# Frage 11: Wie verpflegen Sie sich im Campus Grueental/Reidbach

df_n <- df_n %>% rename ("cant" = fr_292, "home" = fr_293, "other" = fr_294)


df_n$cant <- dplyr::recode(df_n$cant, '1'=5,'2'=4,'3'=3,'4'=2,'5'=1)
df_n$home <- dplyr::recode(df_n$home, '1'=5,'2'=4,'3'=3,'4'=2,'5'=1)
df_n$other <- dplyr::recode(df_n$other, '1'=5,'2'=4,'3'=3,'4'=2,'5'=1)


# Frage 12: Angaben zur Person 

df_n <- df_n %>% rename("gender" = fr_296 ) %>% 
  mutate(gender = recode_factor(gender, "1"= "Frau", "2" = "Mann", "3" = "x"))


# df_n <- df_n %>% rename("birth" = fr_298) wurde fuer die Teil-Anonymisierung in Altersgruppen umgewandelt


# df_n <- df_n %>% rename("member" = fr_297 ) %>% 
#   mutate(member = recode_factor(member, "1"= "Student/in", "2" = "Lernende/r", "3"= "Mitarbeiter/in", "4" = "Externe/r, Gast", "5" = "Weiterbildungsteilnehmer/in"))
# wurde fuer die die Teil-Anonymisierung zusammengefasst, Lernende/r, Externe/r Gast und Weiterbildungsteilnehmer/in gehören nun zu "Andere"


# Frage 13: Seit wann kennen Sie die Mensa Grueental/Vista (Reidbach)

# df_n <- df_n %>% rename("since" = fr_299 ) %>% 
#   mutate(since = recode_factor(since, "1"= "seit HS 2017", "2" =  "weniger als 1 Jahr", "3"= "1-2 Jahre", "4" = "3-5 Jahre", "5" = "laenger als 5 Jahre"))
# Wurde für die Teilanonymisierung geloescht



# Frage 14: Haben Sie diesen Fragebogen bereits einmal ausgefuellt?

# df_n$fr_301[df_n$fr_301 == 1] <- 1
df_n <- df_n %>% rename("fill" = fr_301 ) %>% 
  mutate(fill = recode(fill, "1"= 1, "2" =  0)) 


# Korrekturen -----------

# Als zusaetzliche Kontrolle wurde der Datensatz nach Fragebogen durchsucht, die viele fehlende Antworten haben. 

# Nicht eingescannte Seiten

# Die Seite 3 des Fragebogens 2017_HS_S_0412 wurde nicht eingescannt. Die Antworten zu Frage 9 bis 14 wurden von Hand eingefuellt. 
df_n[df_n$Pseudonym == "2017_HS_S_0412", c ("meat", "milk", "veget", "veg", "pensum", "cant", "home", "other", "gender", "age_groups", "member", "fill")] <-  
  c(4, 6,  4, 1,  3, 2, 3, 3, "Mann", "26- bis 34-jaehrig", "Student/in", 0)

#Die Seite 1 und 2 des Fragebogens 2017_HS_K_0442   wurden nicht eingescannt. Die Antworten zu Frage 1 bis 8 wurden von Hand eingefuellt. 
df_n[df_n$Pseudonym == "2017_HS_K_0442", c ("choice_1", "choice_2", "choice_3", "choice_4", "choice_5", "choice_6", "choice_7", "choice_8", "choice_9", "choice_10", "satis_1", "satis_2",
                                            "satis_3", "ing_1", "ing_2", "ing_3", "ing_4", "att_1", "att_2", "att_3", "att_4", "att_5", "att_6", "att_7", "att_8", "att_9", "att_10", "att_11", "diet", "allerg", "relig", "meds",
                                            "tho_1", "tho_2", "tho_3", "tho_4", "tho_5", "tra_1", "tra_2", "tra_3", "tra_4", "tra_5")] <- 
  c(3, 4, 4, 4, 3, 4, 1, 1, 1, 1, 2, 3, 2, 4, 4, 2, 1, 3, 4, 3, 1, 4, 1, 1, 3, 1, 1, 4, 1, 0, 0, 0,  4, 2, 2, 4, 4, 4, 2, 1, 2, 3)

#Die Seite 2 des Fragebogens 2017_HS_W_0084  wurde nicht eingescannt. Die Antworten zu Frage 5 bis 8 wurden von Hand eingefuellt.
df_n[df_n$Pseudonym == "2017_HS_W_0084", c ("att_1", "att_2", "att_3", "att_4", "att_5", "att_6", "att_7", "att_8", "att_9", "att_10", "att_11", "diet", "allerg", "relig", "meds",
                                            "tho_1", "tho_2", "tho_3", "tho_4", "tho_5", "tra_1", "tra_2", "tra_3", "tra_4", "tra_5")] <- 
  c(3, 3, 3, 4, 4, 2, 2, 3, 3, 3, 3, 0, 1, 0, 0, 4, 3, 3, 3, 3, 4, 3, 3, 3, 4)

#Die Seite 1 des Fragebogens 2017_HS_F_0412  wurde nicht eingescannt. Die Antworten zu Frage 1 bis 4 wurden von Hand eingefuellt.
df_n[df_n$Pseudonym == "2017_HS_F_0412", c ("choice_1", "choice_2", "choice_3", "choice_4", "choice_5", "choice_6", "choice_7", "choice_8", "choice_9", "choice_10", "satis_1", "satis_2",
                                            "satis_3", "ing_1", "ing_2", "ing_3", "ing_4")] <- 
  c(4, 3, 4, 4, 3, 3, 1, 2, 3, 2, 4, 4, 2, 3, 1, 1, 3)




# Fuer das Programm nicht erkennbare Antworten

#Das Programm hatte Muehe, einige Antworten auf dem Fragebogen 2017_HS_F_0106 zu erkenne. Fehlende Werte wurden von Hand eingefuellt. 
df_n[df_n$Pseudonym == "2017_HS_F_0106", c ("choice_7", "choice_9", "satis_2", "satis_3", "ing_1", "ing_2", "ing_3", "ing_4",
                                            "att_3", "att_4", "att_5", "att_7", "att_9", "att_10", "diet", "allerg", "relig", "meds", "tho_1", "tho_2", "tho_4", 
                                            "tra_3", "tra_4", "tra_5", "meat", "milk", "veg", "pensum", "home", "other", "gender", "age_groups", "member")] <- 
  c(1, 2, 2, 2, 2, 2, 2, 3, 2, 3, 3, 1, 3, 3, 0, 0, 0, 0, 4, 3, 2, 3, 4, 3, 4, 6, 1, 5, 4, 1, "Mann", "17- bis 25-jaehrig", "Student/in")

#Das Programm hatte Muehe, einige Antworten auf dem Fragebogen 2017_HS_S_0127 zu erkenne. Fehlende Werte wurden von Hand eingefuellt. 
df_n[df_n$Pseudonym == "2017_HS_S_0127", c ("tho_3", "tho_4", "tra_1")] <-   c(3, 3, 4)

#Das Programm hatte Muehe, einige Antworten auf dem Fragebogen 2017_HS_W_0280 zu erkenne. Fehlende Werte wurden von Hand eingefuellt.
df_n[df_n$Pseudonym == "2017_HS_W_280", "member"] <- "Student/in"

#Das Programm hatte Muehe, einige Antworten auf dem Fragebogen 2017_HS_HC_0407 zu erkenne. Fehlende Werte wurden von Hand eingefuellt. 
df_n[df_n$Pseudonym == "2017_HS_HC_0407", c("choice_8")] <- c("4")

#Das Programm hatte Muehe, einge Antworten auf dem Fragebogen 2017_HS_S_0097 zu erkenne. Fehlende Werte wurden von Hand eingefuellt.
df_n[df_n$Pseudonym == "2017_HS_S_0097", c ("ing_1", "ing_2", "ing_3", "ing_4")] <- c (-99, -99, -99, -99)

#Die Antworten zu Frage 11 auf dem Fragebogen 2017_HS_S0267 wurden bei Mensa und anderswogekauftes Essen zwischen den Kreisen von "3-4x in der Woche" und "1-2x in der Woche" Angekreuzt. Wird von Hand bei beiden auf "1-2x in der Woche" gändert. 
df_n[df_n$Pseudonym == "2017_HS_S_0267", c ("cant", "other")] <- c (3, 3)

#Die Antwort zu Frage 11, Essen von Zuhause mitbringen wurde beim Fragebogen 2017_HS_S0125 die höchste Kategorie durchgestrichen und dann wieder angekreuzt. 
df_n[df_n$Pseudonym == "2017_HS_S_0125", c ("home")] <- c (5)

#Der Fragebogen 2017_HS_F_0371 wurde mit einem sehr dicken Stift ausgefüllt, darum waren einige Antworten für das Programm nicht erkennbar. Werden von Hand eingefüllt
df_n[df_n$Pseudonym == "2017_HS_S_0371", c ("choice_7", "choice_10", "att_2", "satis_3", "att_2", "att_4", "att_6", "att_7", "tho_2", "tho_3", "cant", "home", "gender")] <- c (1, 4, 1, 1, 3, "-99", 1, 1, 3, 3, 3, 3, "Frau")!
  
#Fehlende Antworten bei der letzten Frage
# Dieser Fragebogen 2017_HS_F_0007 wurde am ersten Befragungstag ausgeteilt. Ausdiesem Grund wurde die fehlend Antwort auf Frage 12 auf "nein" geaendert.  
df_n[df_n$Pseudonym == "2017_HS_F_0007", "fill"] <- 0 

# Die Fragebogen Software hatte bei einigen Fragebogen muehe, die Jahreszahlen zu erkennen. Die Fragebogen wurden daraufhin auf fehlende, sehr alte und sehr Junge Jahrgaeng durchsucht und 
# angepasst. 
# Gleich in Altersgruppen angepasst wegen Teil-Anonymisierung
df_n[df_n$Pseudonym == "2017_HS_K_0338", "age_groups"] <- NA
df_n[df_n$Pseudonym == "2017_HS_K_0029", "age_groups"] <- "35- bis 49-jaehrig"
df_n[df_n$Pseudonym == "2017_HS_HC_0333", "age_groups"] <- "26- bis 34-jaehrig"
df_n[df_n$Pseudonym == "2017_HS_K_0037", "age_groups"] <- "17- bis 25-jaehrig"
df_n[df_n$Pseudonym == "2017_HS_K_0073", "age_groups"] <- "17- bis 25-jaehrig"
df_n[df_n$Pseudonym == "2017_HS_K_0408", "age_groups"] <- "17- bis 25-jaehrig"
df_n[df_n$Pseudonym == "2017_HS_F_0100", "age_groups"] <- NA
df_n[df_n$Pseudonym == "2017_HS_S_0204", "age_groups"] <- "17- bis 25-jaehrig"
df_n[df_n$Pseudonym == "2017_HS_K_0204", "age_groups"] <- "17- bis 25-jaehrig"
df_n[df_n$Pseudonym == "2017_HS_K_0204", "age_groups"] <- "17- bis 25-jaehrig"
df_n[df_n$Pseudonym == "2017_HS_F_0024", "age_groups"] <- NA
df_n[df_n$Pseudonym == "2017_HS_S_0102", "age_groups"] <- NA
df_n[df_n$Pseudonym == "2017_HS_F_0267", "age_groups"] <- NA

# Bei 23 Fragebogen wurde bei der Frage zur Hochschulzugehoerigkeit sowohl Student/in als auch Mitarbeiter/in ausgewaehlt. 
# Aendern von NA zu Student/in. E. Kommentare lassen darauf schliessen, dass es sich dabei um Personen handelt, die an der ZHAW arbeiten aber auch noch ein Studium absolvieren. 
# Wir nehmen an, dass Personen welche an der ZHAW Teilzeit studieren und arbeiten, bei der Bezahlung an der Kasse ihre Studentenkarte benützen und somit für ihr Menü den Studentenpreis zahlen. 
# Im Datensatz werden solche Beobach-tungen darum als Student/in ausgewiesen. 
df_n[df_n$Pseudonym == "2017_HS_S_0397", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_K_0392", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_W_0074", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_K_0401", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_K_0074", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_HC_0313", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_HC_0145", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_F_0320", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_W_0312", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_S_0098", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_HC_0301", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_F_0434", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_K_0391", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_HC_0302", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_F_0273", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_S_0343", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_S_0334", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_S_0146", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_F_0288", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_W_0359", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_W_0318", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_HC_0233", "member"] <- "Student/in"
df_n[df_n$Pseudonym == "2017_HS_W_0002", "member"] <- "Student/in"



# Aenderungen aufgrund von Kommentaren

# Beim Fragebogen 2017_HS_F_0328 wurden aufgrund eines Kommentars auf Seite 4 alle Antworten zu Frage 5 zu NA geaendert.
df_n[df_n$Pseudonym == "2017_HS_F_0328", c ("att_1", "att_2", "att_3", "att_4", "att_5", "att_6", "att_7", "att_8", "att_9", "att_10", "att_11")] <- 
  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) 

# Beim Fragebogen 2017_HS_F_0328 wueden aufgrund eines Kommentars auf dem Fragebogen der Fleischkonsum zu "nie" geaendert.
df_n[df_n$Pseudonym == "2017_HS_W_0318", "meat"] <- 1

# Beim Fragebogen 2017_HS_W_0340 wurde aufgrund eines Kommentars auf dem Fragebogen die Antwort zur Aussage "Ich bringe anderswo gekauftes Essen mit (z. B. Sandwich)" (Frage 11)  zu "3-4x in der Woche" geaendert.
df_n[df_n$Pseudonym == "2017_HS_W_0340", "other"] <- 4

# Beim Fragebogen 2017_HS_S_0115 wurde aufgrund eines Kommentars auf dem Fragebogen die Antworten zur Aussage "Ich kaufe meine Essen in der Mensa." und "Ich bringe Essen von Zuhause mit." (Frage 11) 
# zu  "1-2x im Monat" und "5x in der Woche" geaendert.  
df_n[df_n$Pseudonym == "2017_HS_S_0115", c("cant", "home")] <- c(2, 5)

# Beim Fragebogen 2017_HS_S_0129 wurde aufgrund eines Kommentars auf dem Fragebogen die Antwort zur Hochschulzugehoerigkeit (Frage 12) in "Mitarbeiter/in" gaendert.  
df_n[df_n$Pseudonym == "2017_HS_S_0129", "member"] <- "Mitarbeiter/in"

# Beim Fragebogen 2017_HS_S_0084 wurde aufgrund eines Kommentars auf dem Fragebogen die Antwort zur Aussage "Ich ernaehre mich vegetarisch (ovo-lakto)" (Frage 9) zu NA geaendert. 
df_n[df_n$Pseudonym == "2017_HS_W_0084", c ("veget")] <- c(NA)

# Beim Fragebogen 2017_HS_W_0057 wurde aufgrund eines Kommentars auf dem Fragebogen die Antwort zur Hochschulzugehoerigkeit (Frage 12) in"Externe/r, Gast"  gaendert.  
df_n[df_n$Pseudonym == "2017_HS_W_0057", "member"] <- "Andere"





# Sich wiedersprechende Antworten -----------

# Die Fragebogen wurden nach sich wiedersprechenden Antworten durchsucht. Dies das Risiko zu verringern, dass nicht ernsthaft ausgefuellte Fragebogen die 
# Befragungsergebnisse beeinflussen. Fuer diesen Vorgang wurden die Antworten zu den Aussagen bei Frage 9 benutzt. 
# Es werden verschiedene Datensaetze mit Fragebogen erstellt, die wiederspruechliche Angaben zum Ernährungsverhalten enthalten

# Teilnehmer, die angeben "2x und mehr pro Tag" oder "1x pro Tag" Fleisch zu essen und gleichzeitig sich "immer" oder "meistens" vegetarisch zu ernaehren. 
df_meatveget <- df_n %>% filter(meat > 5 & veget > 4) #Anzahl Fragebogen: 2

# Teilnehmer, die angeben "2x und mehr pro Tag" oder "1x pro Tag" Fleisch zu essen und gleichzeitig sich "immer" oder "meistens" vegan zu ernaehren. 
df_meatveg <- df_n %>% filter(meat > 5 & veg > 4) #Anzahl Fragebogen: 1

# Teilnehmer, die angeben "3x und mehr pro Tag", "1-2x pro Tag" Milch/-produkte zu konsumieren und gleichzeitig sich "immer" oder "meistens" vegan zu ernaehren. 
df_milkveg <- df_n %>% filter(milk > 5 & veg > 5) #Anzahl Fragebogen: 2

# Teilnehmer, die angeben sich immer vegetarisch zu ernaehren aber auch einen hoeher Fleisch Konsum als 1-2x im Monat angeben. 
df_vegetmeat <- df_n %>% filter (veget == 6 & meat > 2) #Anzahl Fragebogen: 1  

# Teilnehmer, die angeben sich immer vegan zu ernaehren aber auch einen hoeher Fleisch Konsum als 1-2x im Monat angeben. 
df_vegmeat <- df_n %>% filter (veg == 6 & meat > 2) #Anzahl Fragebogen: 2  

# Teilnehmer, die angeben sich immer vegan zu ernaehren aber auch einen hoeher Milch/-produkte Konsum als 1-2x im Monat angeben.  
df_vegmilk <- df_n %>% filter (veg == 6 & milk > 2) #Anzahl Fragebogen: 2 

# Teilnehmer, die angeben "2x und mehr pro Tag" Fleisch zu essen aber auch angeben sich "immer", "meistens" oder "oft" vegetarisch oder vegan zu ernaehren
df_meatvegveget <- df_n %>% filter (meat == 7 & veget > 3 | meat == 7 & veg > 3 ) #Anzahl Fragebogen: 3

# Zusammenfuegen aller erstellten Datensaetze. Falls ein Fragebogen in mehreren Datensaetzen vorkommt wird er trotzdem nur einmal in diesen Datensatz aufgenommen. 
wiederspruch <- unique(rbind(df_meatveg, df_meatveget, df_milkveg, df_vegetmeat, df_vegmeat, df_vegmilk, df_meatvegveget)) # Anzahl Fragebogen: 6

# Die Fragebogen die im Datensatz "wiederspruch" vorkommen werden aus dem Datensatz df_n entfernt. 
df_n <- anti_join(df_n, wiederspruch) #Anzahl Fragebogen: 1195




# Angaben zum konsumierten Gericht  -----------

# Erstellen eines Datensatz, der die Fragebogen enthaelt, bei denen Frage 1 nicht beantwortet wurde
df_mealNA <- filter(df_n, is.na(meal)) #Anazhl Fragebogen 64

# Erstellen eines Datensatz, der die Fragebogen enthaelt, bei denen die Menuelinie in der Laufnummer und die Antwort auf Frage 1 nicht uebereinstimmen 
df_mealDI <- filter(df_n, substring(df_n$Pseudonym, 9, 9) != substring(df_n$meal, 1, 1)) #Anzahl Fragebogen: 109

# Speichern der beiden Datensaetze
write.csv2(df_mealNA, "03_bereinigung/02_alle/NA_meal_181102_vori.csv")
write.csv2(df_mealDI, "03_bereinigung/02_alle/unequal_meal_181102_vori.csv")


# Insgesamt wurde die Frage 1 64-mal (5 %) nicht beantwortet und 109-mal (9 %) wiedersprachen sich die Antwort zu bei Frage 1 und die Laufnummer. 
# Bei diesen Beobachtungen haben wir die Kommentare und das Antwortverhalten noch einmal genau angeschaut. 

# Dummy-Variable "mensa" erstellen. Die Auspraegung «1» bedeutet, dass sich dieser Fragebogen auf ein Gericht bezieht, das in der Mensa bezogen wurde. 
# Die Auspraegung «0» bedeutet, dass der Fragebogen zu einem Mittagsessen ausgefuellt wurde, das selber mitgebracht wurde. 
df_n$mensa <- ifelse(df_n$meal %in% c("Favorite", "World",  "Kitchen", "Hot & Cold"), 1, 0)


# Keine Antwort bei Frage 1: Bestimmung der Variable "meal" und "menas" aufgrund weiteren Antworten und Bemerkungen auf dem Fragebogen.
df_n[df_n$Pseudonym == "2017_HS_F_0063", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_F_0101", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_F_0141", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_F_0143", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_F_0145", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_F_0356", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_F_0407", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_F_0412", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0015", c("meal", "mensa") ] <- c("Hot & Cold", 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0026", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_HC_0050", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0051", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0061", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0062", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0175", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0188", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0197", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0247", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0350", c("meal", "mensa") ] <- c(NA, 1)

df_n[df_n$Pseudonym == "2017_HS_K_0013", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_K_0399", c("meal", "mensa") ] <- c("Kitchen", 1)
df_n[df_n$Pseudonym == "2017_HS_K_0404", c("meal", "mensa") ] <- c("Kitchen", 1)
df_n[df_n$Pseudonym == "2017_HS_K_0430", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_K_0440", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_K_0441", c("meal", "mensa") ] <- c("Kitchen", 1)
df_n[df_n$Pseudonym == "2017_HS_K_0442", c("meal", "mensa") ] <- c("Kitchen", 1)

df_n[df_n$Pseudonym == "2017_HS_S_0004", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_S_0011", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_S_0014", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_S_0015", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_S_0047", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_S_0106", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_S_0166", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_S_0192", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_S_0195", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_S_0218", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_S_0267", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_S_0268", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_S_0308", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_S_0318", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_S_0322", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_S_0350", c("meal", "mensa") ] <- c("Selber", 0)

df_n[df_n$Pseudonym == "2017_HS_W_0013", c("meal", "mensa") ] <- c("World", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0031", c("meal", "mensa") ] <- c("World", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0032", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_W_0061", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0090", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0135", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0157", c("meal", "mensa") ] <- c("World", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0264", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0306", c("meal", "mensa") ] <- c("World", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0307", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0334", c("meal", "mensa") ] <- c("World", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0362", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0371", c("meal", "mensa") ] <- c(NA, 1)


# Sich wiedersprechende Laufnummern und Antworten bei Frage 1: Bestimmung der Variable "meal" und "menas" aufgrund weiteren Antworten und Bemerkungen auf dem Fragebogen.
df_n[df_n$Pseudonym == "2017_HS_F_0001", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_F_0030", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_F_0032", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_F_0040", c("meal", "mensa") ] <- c("Kitchen", 1)
df_n[df_n$Pseudonym == "2017_HS_F_0085", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_F_0126", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_F_0137", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_F_0164", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_F_0175", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_F_0195", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_F_0216", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_F_0298", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_F_0361", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_F_0362", c("meal", "mensa") ] <- c("Favorite", 1)

df_n[df_n$Pseudonym == "2017_HS_HC_0037", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0039", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0040", c("meal", "mensa") ] <- c("Kitchen", 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0106", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_HC_0172", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0174", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0187", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0189", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0198", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0308", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0309", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0313", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0348", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_HC_0355", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_HC_0360", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_HC_0382", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_HC_0399", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0424", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_HC_0434", c("meal", "mensa") ] <- c("Selber", 0)

df_n[df_n$Pseudonym == "2017_HS_K_0037", c("meal", "mensa") ] <- c("Selber", 0)
df_n[df_n$Pseudonym == "2017_HS_K_0038", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_K_0052", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_K_0061", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_K_0156", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_K_0171", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_K_0319", c("meal", "mensa") ] <- c("Kitchen", 1)
df_n[df_n$Pseudonym == "2017_HS_K_0320", c("meal", "mensa") ] <- c("Kitchen", 1)
df_n[df_n$Pseudonym == "2017_HS_K_0340", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_K_0341", c("meal", "mensa") ] <- c("Kitchen", 1)
df_n[df_n$Pseudonym == "2017_HS_K_0348", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_K_0359", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_K_0360", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_K_0376", c("meal", "mensa") ] <- c("Kitchen", 1)
df_n[df_n$Pseudonym == "2017_HS_K_0388", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_K_0413", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_K_0414", c("meal", "mensa") ] <- c("Kitchen", 1)
df_n[df_n$Pseudonym == "2017_HS_K_0422", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_K_0428", c("meal", "mensa") ] <- c("Kitchen", 1)
df_n[df_n$Pseudonym == "2017_HS_K_0439", c("meal", "mensa") ] <- c(NA, 1)

df_n[df_n$Pseudonym == "2017_HS_S_0029", c("meal", "mensa") ] <- c("World", 1)
df_n[df_n$Pseudonym == "2017_HS_S_0037", c("meal", "mensa") ] <- c("Kitchen", 1)
df_n[df_n$Pseudonym == "2017_HS_S_0085", c("meal", "mensa") ] <- c("Hot & Cold", 1)
df_n[df_n$Pseudonym == "2017_HS_S_0110", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_S_0119", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_S_0124", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_S_0131", c("meal", "mensa") ] <- c("Hot & Cold", 1)
df_n[df_n$Pseudonym == "2017_HS_S_0200", c("meal", "mensa") ] <- c("Hot & Cold", 1)

df_n[df_n$Pseudonym == "2017_HS_W_0019", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0037", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0038", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0039", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0041", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0042", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0043", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0045", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0051", c("meal", "mensa") ] <- c("Kitchen", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0056", c("meal", "mensa") ] <- c("Kitchen", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0057", c("meal", "mensa") ] <- c("Kitchen", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0066", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0067", c("meal", "mensa") ] <- c("Kitchen", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0076", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0077", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0107", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0108", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0130", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0133", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0142", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0145", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0146", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0147", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0148", c("meal", "mensa") ] <- c("Favorite", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0175", c("meal", "mensa") ] <- c("World", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0190", c("meal", "mensa") ] <- c("World", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0209", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0210", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0211", c("meal", "mensa") ] <- c("World", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0235", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0303", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0313", c("meal", "mensa") ] <- c("World", 1)
df_n[df_n$Pseudonym == "2017_HS_W_0329", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0332", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0383", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0394", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0412", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0435", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0437", c("meal", "mensa") ] <- c(NA, 1)
df_n[df_n$Pseudonym == "2017_HS_W_0439", c("meal", "mensa") ] <- c("Kitchen", 1)


#Fragebogen bei denen nicht bestimmt werden konnte, ob Sie zu einem Mensa Gericht ausgefuellt wurden

df_n[df_n$Pseudonym == "2017_HS_F_0026", c("meal", "mensa") ] <- c(NA, NA)
df_n[df_n$Pseudonym == "2017_HS_F_0249", c("meal", "mensa") ] <- c(NA, NA)
df_n[df_n$Pseudonym == "2017_HS_HC_0332", c("meal", "mensa") ] <- c(NA, NA)
df_n[df_n$Pseudonym == "2017_HS_K_0321", c("meal", "mensa") ] <- c(NA, NA)
df_n[df_n$Pseudonym == "2017_HS_S_0013", c("meal", "mensa") ] <- c(NA, NA)
df_n[df_n$Pseudonym == "2017_HS_S_0294", c("meal", "mensa") ] <- c(NA, NA)
df_n[df_n$Pseudonym == "2017_HS_S_0361", c("meal", "mensa") ] <- c(NA, NA)
df_n[df_n$Pseudonym == "2017_HS_S_0432", c("meal", "mensa") ] <- c(NA, NA)
df_n[df_n$Pseudonym == "2017_HS_W_0001", c("meal", "mensa") ] <- c(NA, NA)
df_n[df_n$Pseudonym == "2017_HS_HC_0173", c("meal", "mensa") ] <- c(NA, NA)
df_n[df_n$Pseudonym == "2017_HS_HC_0303", c("meal", "mensa") ] <- c(NA, NA)
df_n[df_n$Pseudonym == "2017_HS_K_0403", c("meal", "mensa") ] <- c(NA, NA)
df_n[df_n$Pseudonym == "2017_HS_S_0444", c("meal", "mensa") ] <- c(NA, NA)
df_n[df_n$Pseudonym == "2017_HS_W_0040", c("meal", "mensa") ] <- c(NA, NA)
df_n[df_n$Pseudonym == "2017_HS_W_0085", c("meal", "mensa") ] <- c(NA, NA)
df_n[df_n$Pseudonym == "2017_HS_W_0132", c("meal", "mensa") ] <- c(NA, NA)
df_n[df_n$Pseudonym == "2017_HS_W_0136", c("meal", "mensa") ] <- c(NA, NA)


# Diese Fragebogen werden aus dem Datensatz entfernt. 

df_n <- subset(df_n, !is.na(mensa)) # Anzahl entfernter Fragebogen 17, Verbleibende Fragebogen 1178



# Fehlende Antworten -----------

# Fragebogen, die fuer Frage 9 bis 14 keine Antworten enthalten werden aus dem Datensatz entfernt. 

df_n_9_14 <- df_n %>% select(c (Pseudonym, meat, milk, veget, veg, pensum, cant, home, other, gender, age_groups, member))

cnt_na <- apply(df_n_9_14, 1, function(z) sum(is.na(z)))
NA_9_14 <- df_n %>% filter(cnt_na==11) # Anzahl Fragebogen: 2

df_n <- anti_join(df_n, NA_9_14)


# Mehr als 50% fehlende Antworten (ausgenommen Kommentare) -----------

# Entfernen der Kommentarspalten 
df_n <- df_n %>% select(- c(fr_302, fr_316))

# Variable mit der Anzahl fehlender Werte pro Beobachtung
df_n$sumNA <- rowSums(is.na(df_n))

# Umrechnung in Prozent. Dabei muss beachtet werden, das bei Fragebogen zu Selbstmitgebrachten Gerichten Frage 2 und 3 nicht beantwortet werden mussten

df_n$perNA <- ifelse(df_n$mensa == 1, df_n$sumNA/52*100, ((df_n$sumNA-13)/39)*100)


# Fragebogen die mehr als 50% fehlende Werte haben werden geloescht. 

df_n <- df_n %>% filter(df_n$perNA<=50) #Anzahl Fragebogen 1175 (1 Fragebogen wurde entfernt)

# Bei Fragebogen die zu einem selbstmitgebrachten Gericht ausgefuellt wurden, kann es sein dass die Fragen 2 und 3 trotzdem beantwortet wurden und darum der Anteil an fehlenden Werten unter 50% ist.
# Fuer dies wird hier noch kontrolliert
df_n_selbst <- df_n %>% 
  filter(mensa==0) %>% 
  select(-c("choice_1":"choice_10", "satis_1":"satis_3", "perNA"))

df_n_selbst$sumNA <- rowSums(is.na(df_n_selbst))

df_n_selbst$perNA <- (((df_n_selbst$sumNA)/39)*100)

Selbst_fehlend <- df_n_selbst %>% filter(df_n_selbst$perNA>=50) #Anzahl Fragebogen: 0

# Erstellen von Alter aufgrund des Geburtsdatums. Nicht mehr nötig, wurde für Teil-Anonymisierung schon gemacht. -----------

# df_n$age<-2017-as.numeric(df_n$birth)
# 
# # Erstellen von Altersgruppen -----------
# 
# df_n$age_groups <- cut(
#   df_n$age,
#   breaks = c(17, 26, 35, 50, 65, Inf),
#   labels = c(
#     "17- bis 25-jaehrig", "26- bis 34-jaehrig", "35- bis 49-jaehrig", "50- bis 64-jaehrig", "65-jaehrig und aelter"
#   ),
#   right = FALSE
# )
# 
# 

# Typenbildung ---- 

# Umcodieren von NA in 1 fuer "nie" fuer die Typenbildung, wird dann wieder rueckgaengig gemacht.
df_n$cant[is.na(df_n$cant)]<-1

df_n$home[is.na(df_n$home)]<-1

df_n$other[is.na(df_n$other)]<-1


df_n$Mensagaenger<-as.numeric(df_n$cant>df_n$home & df_n$cant>df_n$other)
df_n$Selbstverpfleger<-as.numeric(df_n$home>df_n$cant & df_n$home>df_n$other)
df_n$anderswo<-as.numeric(df_n$other>df_n$home & df_n$other>df_n$cant | df_n$other==df_n$home & df_n$other>df_n$cant)

df_n$Verpflegungstyp<-ifelse((df_n$Mensagaenger==1),"Mensagaenger",
                             ifelse((df_n$Selbstverpfleger==1),"Selbstverpfleger",
                                    ifelse((df_n$anderswo==1), "Einkaeufer",
                                           ifelse((df_n$cant==1 & df_n$home==1 & df_n$other==1), NA, "Abwechsler"))))

df_n$Mensagaenger<-NULL
df_n$Selbstverpfleger<-NULL
df_n$anderswo<-NULL


# # Rueckgaengi Machung von NA in 0
df_n$cant[df_n$cant==0]<-NA
df_n$home[df_n$home==0]<-NA
df_n$other[df_n$other==0]<-NA



# Datensatz place_date_181106_vori mit der Information welcher Fragebogen an welchem Befragungstag und -ort ausgeteilt wurde
df_date <- read_delim("S:/pools/n/N-IUNR-nova-data/01_befragung/quant/01_analysen/01_csv_Datenfiles/place_date_181106_vori.csv", delim=';', locale = locale(encoding = 'ISO-8859-2'))

# Hinzufügen der Daten zum Datensatz
df_n <- merge(df_n, df_date)

#Aenderung der Wochentage in Datumsformat  -----------
df_n$date <- as.Date(df_n$date, format = "%d.%m.%Y")

#Hinzufuegen der Information, ob der Fragebogen in einer Basis oder einer Interventionswoche ausgefuellt wurde. 
df_n$intervention <- ifelse(df_n$date=="2017-11-06" | df_n$date=="2017-11-08" | df_n$date=="2017-11-14" | df_n$date=="2017-11-16" | df_n$date=="2017-11-30", 1, 0) 

#Hinzufuegen des Menüinhalts (Fleisch/Fisch, vegetarisch, pflanzlich*, pflanzlich)  aufgrund des Menueplans

df_n$label_content <- ifelse(df_n$date=="2017-10-17" & df_n$meal=="Favorite", "Fleisch/Fisch", 
                             ifelse(df_n$date=="2017-10-17" & df_n$meal=="World", "vegetarisch", 
                                    ifelse(df_n$date=="2017-10-17" & df_n$meal=="Kitchen", "Fleisch/Fisch",
                                           ifelse(df_n$date=="2017-10-19" & df_n$meal=="Favorite", "vegetarisch",
                                                  ifelse(df_n$date=="2017-10-19" & df_n$meal=="World", "Fleisch/Fisch",
                                                         ifelse(df_n$date=="2017-10-19" & df_n$meal=="Kitchen", "Fleisch/Fisch",
                                                                ifelse(df_n$date=="2017-11-06" & df_n$meal=="Favorite", "rein pflanzlich*", 
                                                                       ifelse(df_n$date=="2017-11-06" & df_n$meal=="World", "Fleisch/Fisch", 
                                                                              ifelse(df_n$date=="2017-11-06" & df_n$meal=="Kitchen", "vegetarisch",
                                                                                     ifelse(df_n$date=="2017-11-08" & df_n$meal=="Favorite", "Fleisch/Fisch",
                                                                                            ifelse(df_n$date=="2017-11-08" & df_n$meal=="World", "vegetarisch",
                                                                                                   ifelse(df_n$date=="2017-11-08" & df_n$meal=="Kitchen", "rein pflanzlich*",
                                                                                                          ifelse(df_n$date=="2017-11-14" & df_n$meal=="Favorite", "rein pflanzlich*", 
                                                                                                                 ifelse(df_n$date=="2017-11-14" & df_n$meal=="World", "Fleisch/Fisch", 
                                                                                                                        ifelse(df_n$date=="2017-11-14" & df_n$meal=="Kitchen", "vegetarisch",
                                                                                                                               ifelse(df_n$date=="2017-11-16" & df_n$meal=="Favorite", "Fleisch/Fisch",
                                                                                                                                      ifelse(df_n$date=="2017-11-16" & df_n$meal=="World", "vegetarisch",
                                                                                                                                             ifelse(df_n$date=="2017-11-16" & df_n$meal=="Kitchen", "rein pflanzlich",
                                                                                                                                                    ifelse(df_n$date=="2017-11-21" & df_n$meal=="Favorite", "Fleisch/Fisch", 
                                                                                                                                                           ifelse(df_n$date=="2017-11-21" & df_n$meal=="World", "Fleisch/Fisch", 
                                                                                                                                                                  ifelse(df_n$date=="2017-11-21" & df_n$meal=="Kitchen", "vegetarisch",
                                                                                                                                                                         ifelse(df_n$date=="2017-11-30" & df_n$meal=="Favorite", "vegetarisch",
                                                                                                                                                                                ifelse(df_n$date=="2017-11-30" & df_n$meal=="World", "rein pflanzlich*",
                                                                                                                                                                                       ifelse(df_n$date=="2017-11-30" & df_n$meal=="Kitchen", "Fleisch/Fisch", "NA"
                                                                                                                                                                                       ))))))))))))))))))))))))

# Hinzufügen des Menünamens aufgrund des Menueplan
df_n$meal_name <- ifelse(df_n$date=="2017-10-17" & df_n$meal=="Favorite", "Kalbsbratwurst", 
                         ifelse(df_n$date=="2017-10-17" & df_n$meal=="World", "Aglio Olio", 
                                ifelse(df_n$date=="2017-10-17" & df_n$meal=="Kitchen", "Dorschfilet",
                                       ifelse(df_n$date=="2017-10-19" & df_n$meal=="Favorite", "Älpler Maggeronen",
                                              ifelse(df_n$date=="2017-10-19" & df_n$meal=="World", "Geräuchertes Rippli",
                                                     ifelse(df_n$date=="2017-10-19" & df_n$meal=="Kitchen", "Schweins-Piccata",
                                                            ifelse(df_n$date=="2017-11-06" & df_n$meal=="Favorite", "Fajita", 
                                                                   ifelse(df_n$date=="2017-11-06" & df_n$meal=="World", "Bauern Nudeln", 
                                                                          ifelse(df_n$date=="2017-11-06" & df_n$meal=="Kitchen", "Burro E Salvia",
                                                                                 ifelse(df_n$date=="2017-11-08" & df_n$meal=="Favorite", "Schweden-Braten",
                                                                                        ifelse(df_n$date=="2017-11-08" & df_n$meal=="World", "Spätzli-Gemüsepfanne",
                                                                                               ifelse(df_n$date=="2017-11-08" & df_n$meal=="Kitchen", "Friedrice",
                                                                                                      ifelse(df_n$date=="2017-11-14" & df_n$meal=="Favorite", "Vesuvio", 
                                                                                                             ifelse(df_n$date=="2017-11-14" & df_n$meal=="World", "Spanischer Schweins-Pfefferbraten", 
                                                                                                                    ifelse(df_n$date=="2017-11-14" & df_n$meal=="Kitchen", "Tartufo",
                                                                                                                           ifelse(df_n$date=="2017-11-16" & df_n$meal=="Favorite", "Zigeuner Cervelats",
                                                                                                                                  ifelse(df_n$date=="2017-11-16" & df_n$meal=="World", "Randen-Frischkäse-Risotto",
                                                                                                                                         ifelse(df_n$date=="2017-11-16" & df_n$meal=="Kitchen", "Vegi-Burger",
                                                                                                                                                ifelse(df_n$date=="2017-11-21" & df_n$meal=="Favorite", "Chickeria", 
                                                                                                                                                       ifelse(df_n$date=="2017-11-21" & df_n$meal=="World", "Panna E Pancetta", 
                                                                                                                                                              ifelse(df_n$date=="2017-10-17" & df_n$meal=="Kitchen", "Risotto Nonna",
                                                                                                                                                                     ifelse(df_n$date=="2017-11-30" & df_n$meal=="Favorite", "Broccoli- oder Spinatwähe",
                                                                                                                                                                            ifelse(df_n$date=="2017-11-30" & df_n$meal=="World", "Chili Sin Carne",
                                                                                                                                                                                   ifelse(df_n$date=="2017-11-30" & df_n$meal=="Kitchen", "Schweins Cordon Bleu", "NA"
                                                                                                                                                                                   ))))))))))))))))))))))))


# Kommentare ----

# Die Kommentare die bei Frage 2 unter "Sonstige Gruende" und auf Seite 4 bei "Kommentare und Anregungen" hingeschrieben wurden, haben wir abgeschrieben und codiert
# diese Informationen wird mit dem Datensatz kommentare_fb_190717_vori.csv dem Datensatz hinzugefuehgt. 

df_kom <- read_delim("S:/pools/n/N-IUNR-nova-data/01_befragung/quant/01_analysen/01_csv_Datenfiles/kommentare_fb_190717_vori.csv", delim=';', locale = locale(encoding = 'ISO-8859-2')) %>% 
  select(c("Pseudonym", "choice_add_d", "choice_add_v", "code_choice", "Bemerkungen_d", "Bemerkungen_v", "code_zu_Bemerkungen"))

df_n <- merge(df_n, df_kom, by="Pseudonym")

# Bei Fragebogen ohne Kommentare wird bei den Dummy-Variablen "choice_add_d" und "Bemerkungen_d" NA durch 0 ersetzt. 
df_n <- df_n %>% mutate(choice_add_d = ifelse(is.na(choice_add_d), 0, choice_add_d),
                        Bemerkungen_d = ifelse(is.na (Bemerkungen_d), 0, Bemerkungen_d))

# Entfernen von Variablen die nicht benütz werden -----------
df_n <- df_n %>% select ( -c("Welle", "rawid", "source", "zeit"))

write.csv2(df_n, "../../Zenodo/2019_ZHAW_vonRickenbach_cleaned_recorded_survey_dataset_NOVANIMAL_anonym.csv", row.names=FALSE)


