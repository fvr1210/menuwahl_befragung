library (formattable) #Version 0.2.0.1
library (reshape2) #Version 1.4.3
library (magrittr) #Version 1.5
library (gmodels) #Version 2.18.1
library (tidyverse) #Version 1.2.1
library (extrafont) # Version 0.17
loadfonts(device = "win")


# Themes laden ----
mytheme <- theme_bw()+ # definve theme for plot
  theme(plot.title = element_text(size = 6.6, face = "bold",  margin = margin(0, 0, .05, 0., "cm")),
        axis.text.x = element_text(size = 6.2),
        axis.text.y = element_text(size = 6.2),
        legend.text = element_text(size = 6.2),
        legend.title = element_text(size = 6.2),
        strip.text.x = element_text(size = 6.6,  face = "bold",  hjust = 0,  margin = margin(.05, 0, .05, 0.05, "cm")),
        axis.title.y = element_text(size = 6.2, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 6.2,  margin = margin(t = 0, r = 0, b = 0, l = 0)),
        plot.subtitle = element_text(margin=margin(b=15), size = 6.2),
        plot.caption = element_text(margin=margin(t=15), face="italic", size=  5),
        panel.spacing = unit(1, "lines"),
        legend.position = "bottom",
        legend.key.size = unit(0.4, "line"),
        legend.margin=margin(-0.5, 0, 0.05, 0, "cm"),
        text = element_text(family = "Akkurat Light"),
        plot.margin=unit(c(t = 0, r = 0, b = 0, l = 0),"cm"))


# Daten einlesen ---------

df_tot <- read_delim("2019_ZHAW_vonRickenbach_cleaned_recorded_survey_dataset_NOVANIMAL_anonym.csv", delim=';', locale = locale(encoding = 'ISO-8859-2'))

# Gesamtstichprobe ----

# Hochschulzugehoerigkeit und geschlecht
df_tot <- df_tot %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_tot <- df_tot %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_tot$gender, df_tot$member)


# Alter
# summary(df_tot$age)
# sd(df_tot$age, na.rm = T)

# Teilstichproben bilden  ----

# Nur Personen die zum ersten mal den Fragebogen ausgefuellt haben. Teilstichprobe A
df_tot_fill <- df_tot %>% 
  filter(fill==0)

df_tot_fill <- df_tot_fill %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_tot_fill <- df_tot_fill %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_tot_fill$gender, df_tot_fill$member)

# Alter
# summary(df_tot_fill$age)
# sd(df_tot_fill$age, na.rm = T)

# Nur Personen die ein Mensamenü konsumiert haben

df_mensa <- df_tot %>% 
  filter(mensa == 1)


# Frage 1: Antwortverteilung aller ausgefuellten Fragebogen
table(df_tot$meal, useNA = T)


# Frage 2: Antwortverteilung der Stichprobe mit Mensamenüs mit Mehrfachteilnahme -----

# Teildatensatz mit Antworten zu Frage 2
df_mensa_choice <- df_mensa %>% select(c(choice_1:choice_10))

# Umcodierung
df_mensa_choice$choice_1 = factor(df_mensa_choice$choice_1, levels = c (1:4,"NA"), labels = c("trifft nicht zu", "trifft eher nicht zu","trifft eher zu","trifft zu", "NA"))
df_mensa_choice$choice_1[is.na(df_mensa_choice$choice_1)]="NA"

df_mensa_choice$choice_2 = factor(df_mensa_choice$choice_2, levels = c (1:4,"NA"), labels = c("trifft nicht zu", "trifft eher nicht zu","trifft eher zu","trifft zu", "NA"))
df_mensa_choice$choice_2[is.na(df_mensa_choice$choice_2)]="NA"

df_mensa_choice$choice_3 = factor(df_mensa_choice$choice_3, levels = c (1:4,"NA"), labels = c("trifft nicht zu", "trifft eher nicht zu","trifft eher zu","trifft zu", "NA"))
df_mensa_choice$choice_3[is.na(df_mensa_choice$choice_3)]="NA"

df_mensa_choice$choice_4 = factor(df_mensa_choice$choice_4, levels = c (1:4,"NA"), labels = c("trifft nicht zu", "trifft eher nicht zu","trifft eher zu","trifft zu", "NA"))
df_mensa_choice$choice_4[is.na(df_mensa_choice$choice_4)]="NA"

df_mensa_choice$choice_5 = factor(df_mensa_choice$choice_5, levels = c (1:4,"NA"), labels = c("trifft nicht zu", "trifft eher nicht zu","trifft eher zu","trifft zu", "NA"))
df_mensa_choice$choice_5[is.na(df_mensa_choice$choice_5)]="NA"

df_mensa_choice$choice_6 = factor(df_mensa_choice$choice_6, levels = c (1:4,"NA"), labels = c("trifft nicht zu", "trifft eher nicht zu","trifft eher zu","trifft zu", "NA"))
df_mensa_choice$choice_6[is.na(df_mensa_choice$choice_6)]="NA"

df_mensa_choice$choice_7 = factor(df_mensa_choice$choice_7, levels = c (1:4,"NA"), labels = c("trifft nicht zu", "trifft eher nicht zu","trifft eher zu","trifft zu", "NA"))
df_mensa_choice$choice_7[is.na(df_mensa_choice$choice_7)]="NA"

df_mensa_choice$choice_8 = factor(df_mensa_choice$choice_8, levels = c (1:4,"NA"), labels = c("trifft nicht zu", "trifft eher nicht zu","trifft eher zu","trifft zu", "NA"))
df_mensa_choice$choice_8[is.na(df_mensa_choice$choice_8)]="NA"

df_mensa_choice$choice_9 = factor(df_mensa_choice$choice_9, levels = c (1:4,"NA"), labels = c("trifft nicht zu", "trifft eher nicht zu","trifft eher zu","trifft zu", "NA"))
df_mensa_choice$choice_9[is.na(df_mensa_choice$choice_9)]="NA"

df_mensa_choice$choice_10 = factor(df_mensa_choice$choice_10, levels = c (1:4,"NA"), labels = c("trifft nicht zu", "trifft eher nicht zu","trifft eher zu","trifft zu", "NA"))
df_mensa_choice$choice_10[is.na(df_mensa_choice$choice_10)]="NA"

# Datensatz mit Antwortverteilungen
df_mensa_choice1 <-df_mensa_choice %>% count(choice_1) %>%  rename("Wahl"="choice_1") %>% mutate (choice="... es gluschtig (schmackhaft) aussah.") 
df_mensa_choice2 <-df_mensa_choice %>% count(choice_2) %>%  rename("Wahl"="choice_2") %>% mutate (choice="... die Menü-Beschreibung mich \nangesprochen hat.")
df_mensa_choice3 <-df_mensa_choice %>% count(choice_3) %>%  rename("Wahl"="choice_3") %>% mutate (choice="... ich es kenne.") 
df_mensa_choice4 <-df_mensa_choice %>% count(choice_4) %>%  rename("Wahl"="choice_4") %>% mutate (choice="... ich grad Lust darauf hatte.") 
df_mensa_choice5 <-df_mensa_choice %>% count(choice_5) %>%  rename("Wahl"="choice_5") %>% mutate (choice="... es eines meiner Lieblingsmenüs ist.")  
df_mensa_choice6 <-df_mensa_choice %>% count(choice_6) %>%  rename("Wahl"="choice_6") %>% mutate (choice="... mir die anderen Menüs \nnoch weniger passten.")  
df_mensa_choice7 <-df_mensa_choice %>% count(choice_7) %>%  rename("Wahl"="choice_7") %>% mutate (choice="... ich etwas Neues ausprobieren wollte.") 
df_mensa_choice8 <-df_mensa_choice %>% count(choice_8) %>%  rename("Wahl"="choice_8") %>% mutate (choice="... ich das nicht selber koche.")  
df_mensa_choice9 <-df_mensa_choice %>% count(choice_9) %>%  rename("Wahl"="choice_9") %>% mutate (choice="... ich es beim Eingang zur Mensa \ngesehen habe (Menü-Aushang).")    
df_mensa_choice10 <-df_mensa_choice %>% count(choice_10) %>%  rename("Wahl"="choice_10") %>% mutate (choice="... das Preis-/Leistungsverhältnis stimmt.")  

#Für die Sotierung wird die Anzahl Antworten mit "trifft zu" benützt
df_mensa_choice1 <-df_mensa_choice1 %>% mutate (zustimmung = df_mensa_choice1$n[4])  
df_mensa_choice2 <-df_mensa_choice2 %>% mutate (zustimmung = df_mensa_choice2$n[4])  
df_mensa_choice3 <-df_mensa_choice3 %>% mutate (zustimmung = df_mensa_choice3$n[4])  
df_mensa_choice4 <-df_mensa_choice4 %>% mutate (zustimmung = df_mensa_choice4$n[4])  
df_mensa_choice5 <-df_mensa_choice5 %>% mutate (zustimmung = df_mensa_choice5$n[4])  
df_mensa_choice6 <-df_mensa_choice6 %>% mutate (zustimmung = df_mensa_choice6$n[4])  
df_mensa_choice7 <-df_mensa_choice7 %>% mutate (zustimmung = df_mensa_choice7$n[4])  
df_mensa_choice8 <-df_mensa_choice8 %>% mutate (zustimmung = df_mensa_choice8$n[4])  
df_mensa_choice9 <-df_mensa_choice9 %>% mutate (zustimmung = df_mensa_choice9$n[4])  
df_mensa_choice10 <-df_mensa_choice10 %>% mutate(zustimmung = df_mensa_choice10$n[4])  

#Hinzufügen von Prozent angaben
df_mensa_choice1 <-df_mensa_choice1 %>% mutate (pct = n/sum(n))
df_mensa_choice2 <-df_mensa_choice2 %>% mutate (pct = n/sum(n))
df_mensa_choice3 <-df_mensa_choice3 %>% mutate (pct = n/sum(n))
df_mensa_choice4 <-df_mensa_choice4 %>% mutate (pct = n/sum(n))
df_mensa_choice5 <-df_mensa_choice5 %>% mutate (pct = n/sum(n))
df_mensa_choice6 <-df_mensa_choice6 %>% mutate (pct = n/sum(n))
df_mensa_choice7 <-df_mensa_choice7 %>% mutate (pct = n/sum(n))
df_mensa_choice8 <-df_mensa_choice8 %>% mutate (pct = n/sum(n))
df_mensa_choice9 <-df_mensa_choice9 %>% mutate (pct = n/sum(n))
df_mensa_choice10 <-df_mensa_choice10 %>% mutate (pct = n/sum(n))



#Zusammenführung in ein Datenset 
df_mensa_choice_long <- bind_rows(df_mensa_choice1, df_mensa_choice2, df_mensa_choice3, df_mensa_choice4, df_mensa_choice5, df_mensa_choice6, df_mensa_choice7, df_mensa_choice8,df_mensa_choice9, df_mensa_choice10) 



ColsPerCat = c("NA" = "grey", "trifft nicht zu" = "#DC6413", "trifft eher nicht zu" = "#Fdd200", "trifft eher zu" = "#AFC410", "trifft zu"="#006885")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_mensa_choice_long <- merge(df_mensa_choice_long, df_color, by="Wahl")



# Plot
ggplot(df_mensa_choice_long, aes(x=reorder(choice, zustimmung), y=pct, fill=factor(Wahl, levels= c ("NA", "trifft nicht zu", "trifft eher nicht zu","trifft eher zu","trifft zu" )), color = label_color)) +
  geom_bar(stat = "identity", width = 0.5, color = NA) +
  ggtitle ("Wie treffen die folgenden Aussagen auf Ihre heutige Menü-Wahl in der Mensa zu?\n(Frage 2, Mensamenü, n = 875)\n\nIch habe dieses Menü heute gewählt, weil...") +
  geom_text(aes(label=(ifelse(pct<0.02, "",paste( round(pct*100), "%", sep = " ")))), size = 2, position = position_stack(vjust = 0.5))+
  scale_color_manual(values = levels(df_mensa_choice_long$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat)+
  scale_y_continuous(labels = scales::percent)+ 
  xlab("")+
  ylab("")+
  coord_flip() +
  mytheme +
  guides(color = F, fill = guide_legend("", reverse = T))

  ggsave(("04_plots/Frage_2_alle_190506_vori.pdf"),
         width = 14.5,
         height = 7,
         dpi=600,
         units="cm",
         device=cairo_pdf)




# Frage 3: Antwortverteilung der Stichprobe mit Mensamenüs mit Mehrfachteilnahme -----
df_mensa_satis <- df_mensa %>% select(c(satis_1:satis_3))

# Umcodierung
df_mensa_satis$satis_1 = factor(df_mensa_satis$satis_1, levels = c (1:4,"NA"), labels = c("trifft nicht zu", "trifft eher nicht zu","trifft eher zu","trifft zu", "NA"))
df_mensa_satis$satis_1[is.na(df_mensa_satis$satis_1)]="NA"

df_mensa_satis$satis_2 = factor(df_mensa_satis$satis_2, levels = c (1:4,"NA"), labels = c("trifft nicht zu", "trifft eher nicht zu","trifft eher zu","trifft zu", "NA"))
df_mensa_satis$satis_2[is.na(df_mensa_satis$satis_2)]="NA"

df_mensa_satis$satis_3 = factor(df_mensa_satis$satis_3, levels = c (1:4,"NA"), labels = c("trifft nicht zu", "trifft eher nicht zu","trifft eher zu","trifft zu", "NA"))
df_mensa_satis$satis_3[is.na(df_mensa_satis$satis_3)]="NA"



# Datensatz mit Antwortverteilungen
df_mensa_satis1 <-df_mensa_satis %>% count(satis_1) %>%  rename("Wahl"="satis_1") %>% mutate (satis="Meine Erwartungen an das Menü haben sich erfüllt.") 
df_mensa_satis2 <-df_mensa_satis %>% count(satis_2) %>%  rename("Wahl"="satis_2") %>% mutate (satis="Ich fand das Menü gut.")
df_mensa_satis3 <-df_mensa_satis %>% count(satis_3) %>%  rename("Wahl"="satis_3") %>% mutate (satis="Ich werde dieses Menü nicht mehr nehmen.") 

#Für die Sotierung wird die Anzahl Antworten mit "trifft zu"  benützt
df_mensa_satis1 <-df_mensa_satis1 %>% mutate (zustimmung = df_mensa_satis1$n[4])  
df_mensa_satis2 <-df_mensa_satis2 %>% mutate (zustimmung = df_mensa_satis2$n[4])  
df_mensa_satis3 <-df_mensa_satis3 %>% mutate (zustimmung = df_mensa_satis3$n[4])  


#Hinzufügen von Prozent angaben
df_mensa_satis1 <-df_mensa_satis1 %>% mutate (pct = n/sum(n))
df_mensa_satis2 <-df_mensa_satis2 %>% mutate (pct = n/sum(n))
df_mensa_satis3 <-df_mensa_satis3 %>% mutate (pct = n/sum(n))


#Zusammenführung in ein Datenset 
df_mensa_satis_long <- bind_rows(df_mensa_satis1, df_mensa_satis2, df_mensa_satis3)

# Farben für Plot

ColsPerCat = c("NA" = "grey", "trifft nicht zu" = "#FF0000", "trifft eher nicht zu" = "#FFA500", "trifft eher zu" = "#99f200", "trifft zu"="#006400")

source("09_function_is_dark_190114_egel.R", chdir = T)

label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_mensa_satis_long <- merge(df_mensa_satis_long, df_color, by="Wahl")


# Plot
ggplot(df_mensa_satis_long, aes(x=reorder(satis, zustimmung), y=pct, fill=factor(Wahl, levels= c ("NA", "trifft nicht zu", "trifft eher nicht zu","trifft eher zu","trifft zu" )), color = label_color)) +
  geom_bar(stat = "identity",color = NA, width = 0.5) +
  ylab("Prozent")+
  xlab("")+
  ggtitle("Frage 3: Wie zufrieden sind Sie mit dem gewählten Menü?\nMensamenü (n = 874)")+
  geom_text(aes(label=(ifelse(pct<0.02, "",paste( round(pct*100), "%", sep = " ")))), size = 5, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = levels(df_mensa_satis_long$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat)+
  guides(color = F, fill = guide_legend("", reverse = T)) +
  coord_flip() +
  mytheme 

  ggsave(("04_plots/Frage_3_alle_191104_vori.pdf"),
         width = 17,
         height = 8,
         dpi=600,
         units="cm",
         device="pdf")



# Frage 4 Antwortverteilung der Stichprobe alle mit Mehrfachteilnahme -----


# Teildatensatz mit Antworten zu Frage 4
df_tot_ing <- df_tot %>% select(c(ing_1:ing_4))

# Umcodierung
df_tot_ing$ing_1 = factor(df_tot_ing$ing_1, levels = c (1:4,-99, "NA"), labels = c("nicht wichtig", "eher nicht wichtig","eher wichtig","wichtig", "kann ich nicht beurteilen", "NA"))
df_tot_ing$ing_1[is.na(df_tot_ing$ing_1)]="NA"

df_tot_ing$ing_2 = factor(df_tot_ing$ing_2, levels = c (1:4,-99, "NA"), labels = c("nicht wichtig", "eher nicht wichtig","eher wichtig","wichtig", "kann ich nicht beurteilen", "NA"))
df_tot_ing$ing_2[is.na(df_tot_ing$ing_2)]="NA"

df_tot_ing$ing_3 = factor(df_tot_ing$ing_3, levels = c (1:4,-99, "NA"), labels = c("nicht wichtig", "eher nicht wichtig","eher wichtig","wichtig", "kann ich nicht beurteilen", "NA"))
df_tot_ing$ing_3[is.na(df_tot_ing$ing_3)]="NA"

df_tot_ing$ing_4 = factor(df_tot_ing$ing_4, levels = c (1:4,-99, "NA"), labels = c("nicht wichtig", "eher nicht wichtig","eher wichtig","wichtig", "kann ich nicht beurteilen", "NA"))
df_tot_ing$ing_4[is.na(df_tot_ing$ing_4)]="NA"



# Datensatz mit Antwortverteilungen
df_tot_ing1 <-df_tot_ing %>% count(ing_1) %>%  rename("Wahl"="ing_1") %>% mutate (att="... enthält Proteine\n(Fleisch, Fisch, Tofu etc.).")
df_tot_ing2 <-df_tot_ing %>% count(ing_2) %>%  rename("Wahl"="ing_2") %>% mutate (att="... enthält Fleisch.")
df_tot_ing3 <-df_tot_ing %>% count(ing_3) %>%  rename("Wahl"="ing_3") %>% mutate (att="... enthält Fisch.") 
df_tot_ing4 <-df_tot_ing %>% count(ing_4) %>%  rename("Wahl"="ing_4") %>% mutate (att="... enthält Produkte aus\nartgerechter Tierhaltung.") 

#Für die Sotierung wird die Anzahl Antworten mit "trifft zu"  benützt
df_tot_ing1 <-df_tot_ing1 %>% mutate (zustimmung = df_tot_ing1$n[4])  
df_tot_ing2 <-df_tot_ing2 %>% mutate (zustimmung = df_tot_ing2$n[4])  
df_tot_ing3 <-df_tot_ing3 %>% mutate (zustimmung = df_tot_ing3$n[4])  
df_tot_ing4 <-df_tot_ing4 %>% mutate (zustimmung = df_tot_ing4$n[4])  


#Hinzufügen von Prozent angaben
df_tot_ing1 <-df_tot_ing1 %>% mutate (pct = n/sum(n))
df_tot_ing2 <-df_tot_ing2 %>% mutate (pct = n/sum(n))
df_tot_ing3 <-df_tot_ing3 %>% mutate (pct = n/sum(n))
df_tot_ing4 <-df_tot_ing4 %>% mutate (pct = n/sum(n))


#Zusammenführung in ein Datenset 
df_tot_ing_long <- bind_rows(df_tot_ing1, df_tot_ing2, df_tot_ing3, df_tot_ing4) 

# add colors
ColsPerCat = c("NA" = "grey", "kann ich nicht beurteilen"="#D3C193",  "nicht wichtig" = "#DC6413", "eher nicht wichtig" = "#Fdd200", "eher wichtig" = "#AFC410", "wichtig"="#006885")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_tot_ing_long <- merge(df_tot_ing_long, df_color, by="Wahl")



# Plot ----
ggplot(df_tot_ing_long, aes(x=reorder(att, zustimmung), y=pct, fill=factor(Wahl, levels= c ( "NA", "kann ich nicht beurteilen",  "nicht wichtig", "eher nicht wichtig","eher wichtig", "wichtig" )), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.5)+
  ylab("")+
  xlab("")+
  ggtitle("Wie wichtig waren Ihnen folgende Inhaltsstoffe Ihres heutigen Essens?\n(Frage 4, alle Fragebogen, n =  1176)\n\nMein heutiges Essen...")+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = levels(df_tot_ing_long$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat)+ 
  coord_flip() +
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  mytheme 


ggsave(("04_plots/Frage_4_alle_191112_vori.pdf"),
       width = 14.5,
       height = 4.4,
       dpi=600,
       units="cm",
       device=cairo_pdf)



# Frage 5: Antwortverteilung der Stichprobe alle mit Mehrfachteilnahme -----

# Teildatensatz mit Antworten zu Frage 5
df_tot_att <- df_tot %>% select(c(att_1:att_11))

# Umcodierung
df_tot_att$att_1 = factor(df_tot_att$att_1, levels = c (1:4,-99, "NA"), labels = c("nicht wichtig", "eher nicht wichtig","eher wichtig","wichtig", "kann ich nicht beurteilen", "NA"))
df_tot_att$att_1[is.na(df_tot_att$att_1)]="NA"

df_tot_att$att_2 = factor(df_tot_att$att_2, levels = c (1:4,-99, "NA"), labels = c("nicht wichtig", "eher nicht wichtig","eher wichtig","wichtig", "kann ich nicht beurteilen", "NA"))
df_tot_att$att_2[is.na(df_tot_att$att_2)]="NA"

df_tot_att$att_3 = factor(df_tot_att$att_3, levels = c (1:4,-99, "NA"), labels = c("nicht wichtig", "eher nicht wichtig","eher wichtig","wichtig", "kann ich nicht beurteilen", "NA"))
df_tot_att$att_3[is.na(df_tot_att$att_3)]="NA"

df_tot_att$att_4 = factor(df_tot_att$att_4, levels = c (1:4,-99, "NA"), labels = c("nicht wichtig", "eher nicht wichtig","eher wichtig","wichtig", "kann ich nicht beurteilen", "NA"))
df_tot_att$att_4[is.na(df_tot_att$att_4)]="NA"

df_tot_att$att_5 = factor(df_tot_att$att_5, levels = c (1:4,-99, "NA"), labels = c("nicht wichtig", "eher nicht wichtig","eher wichtig","wichtig", "kann ich nicht beurteilen", "NA"))
df_tot_att$att_5[is.na(df_tot_att$att_5)]="NA"

df_tot_att$att_6 = factor(df_tot_att$att_6, levels = c (1:4,-99, "NA"), labels = c("nicht wichtig", "eher nicht wichtig","eher wichtig","wichtig", "kann ich nicht beurteilen", "NA"))
df_tot_att$att_6[is.na(df_tot_att$att_6)]="NA"

df_tot_att$att_7 = factor(df_tot_att$att_7, levels = c (1:4,-99, "NA"), labels = c("nicht wichtig", "eher nicht wichtig","eher wichtig","wichtig", "kann ich nicht beurteilen", "NA"))
df_tot_att$att_7[is.na(df_tot_att$att_7)]="NA"

df_tot_att$att_8 = factor(df_tot_att$att_8, levels = c (1:4,-99, "NA"), labels = c("nicht wichtig", "eher nicht wichtig","eher wichtig","wichtig", "kann ich nicht beurteilen", "NA"))
df_tot_att$att_8[is.na(df_tot_att$att_8)]="NA"

df_tot_att$att_9 = factor(df_tot_att$att_9, levels = c (1:4,-99, "NA"), labels = c("nicht wichtig", "eher nicht wichtig","eher wichtig","wichtig", "kann ich nicht beurteilen", "NA"))
df_tot_att$att_9[is.na(df_tot_att$att_9)]="NA"

df_tot_att$att_10 = factor(df_tot_att$att_10, levels = c (1:4,-99, "NA"), labels = c("nicht wichtig", "eher nicht wichtig","eher wichtig","wichtig", "kann ich nicht beurteilen", "NA"))
df_tot_att$att_10[is.na(df_tot_att$att_10)]="NA"

df_tot_att$att_11 = factor(df_tot_att$att_11, levels = c (1:4,-99, "NA"), labels = c("nicht wichtig", "eher nicht wichtig","eher wichtig","wichtig", "kann ich nicht beurteilen", "NA"))
df_tot_att$att_11[is.na(df_tot_att$att_11)]="NA"

# Datensatz mit Antwortverteilungen
df_tot_att1 <-df_tot_att %>% count(att_1) %>%  rename("Wahl"="att_1") %>% mutate (att="... ist gesund.")
df_tot_att2 <-df_tot_att %>% count(att_2) %>%  rename("Wahl"="att_2") %>% mutate (att="... ist sättigend.")
df_tot_att3 <-df_tot_att %>% count(att_3) %>%  rename("Wahl"="att_3") %>% mutate (att="... ist leicht.") 
df_tot_att4 <-df_tot_att %>% count(att_4) %>%  rename("Wahl"="att_4") %>% mutate (att="... ist wenig umweltbelastend.") 
df_tot_att5 <-df_tot_att %>% count(att_5) %>%  rename("Wahl"="att_5") %>% mutate (att="... ist natürlich (keine Zusatzstoffe).")  
df_tot_att6 <-df_tot_att %>% count(att_6) %>%  rename("Wahl"="att_6") %>% mutate (att="... ist vegetarisch (ovo-lakto).")  
df_tot_att7 <-df_tot_att %>% count(att_7) %>%  rename("Wahl"="att_7") %>% mutate (att="... ist rein pflanzlich (vegan).") 
df_tot_att8 <-df_tot_att %>% count(att_8) %>%  rename("Wahl"="att_8") %>% mutate (att="... ist frisch zubereitet.")  
df_tot_att9 <-df_tot_att %>% count(att_9) %>%  rename("Wahl"="att_9") %>% mutate (att="... verhindert, dass Nahrungsmittel\nweggeworfen werden.")    
df_tot_att10 <-df_tot_att %>% count(att_10) %>%  rename("Wahl"="att_10") %>% mutate (att="... stammt aus sozial\nverträglicher Produktion.")
df_tot_att11 <-df_tot_att %>% count(att_11) %>%  rename("Wahl"="att_11") %>% mutate (att="... schont mein Portemonnaie.")  

#Für die Sotierung wird die Anzahl Antworten mit "trifft zu"  benützt
df_tot_att1 <-df_tot_att1 %>% mutate (zustimmung = df_tot_att1$n[4])  
df_tot_att2 <-df_tot_att2 %>% mutate (zustimmung = df_tot_att2$n[4])  
df_tot_att3 <-df_tot_att3 %>% mutate (zustimmung = df_tot_att3$n[4])  
df_tot_att4 <-df_tot_att4 %>% mutate (zustimmung = df_tot_att4$n[4])  
df_tot_att5 <-df_tot_att5 %>% mutate (zustimmung = df_tot_att5$n[4])  
df_tot_att6 <-df_tot_att6 %>% mutate (zustimmung = df_tot_att6$n[4])  
df_tot_att7 <-df_tot_att7 %>% mutate (zustimmung = df_tot_att7$n[4])  
df_tot_att8 <-df_tot_att8 %>% mutate (zustimmung = df_tot_att8$n[4])  
df_tot_att9 <-df_tot_att9 %>% mutate (zustimmung = df_tot_att9$n[4])  
df_tot_att10 <-df_tot_att10 %>% mutate(zustimmung = df_tot_att10$n[4])
df_tot_att11 <-df_tot_att11 %>% mutate(zustimmung = df_tot_att11$n[4])

#Hinzufügen von Prozent angaben
df_tot_att1 <-df_tot_att1 %>% mutate (pct = n/sum(n))
df_tot_att2 <-df_tot_att2 %>% mutate (pct = n/sum(n))
df_tot_att3 <-df_tot_att3 %>% mutate (pct = n/sum(n))
df_tot_att4 <-df_tot_att4 %>% mutate (pct = n/sum(n))
df_tot_att5 <-df_tot_att5 %>% mutate (pct = n/sum(n))
df_tot_att6 <-df_tot_att6 %>% mutate (pct = n/sum(n))
df_tot_att7 <-df_tot_att7 %>% mutate (pct = n/sum(n))
df_tot_att8 <-df_tot_att8 %>% mutate (pct = n/sum(n))
df_tot_att9 <-df_tot_att9 %>% mutate (pct = n/sum(n))
df_tot_att10 <-df_tot_att10 %>% mutate (pct = n/sum(n))
df_tot_att11 <-df_tot_att11 %>% mutate (pct = n/sum(n))


#Zusammenführung in ein Datenset 
df_tot_att_long <- bind_rows(df_tot_att1, df_tot_att2, df_tot_att3, df_tot_att4, df_tot_att5, df_tot_att6, df_tot_att7, df_tot_att8,df_tot_att9, df_tot_att10, df_tot_att11) 

# add colors
ColsPerCat = c("NA" = "grey", "kann ich nicht beurteilen"="#D3C193",  "nicht wichtig" = "#DC6413", "eher nicht wichtig" = "#Fdd200", "eher wichtig" = "#AFC410", "wichtig"="#006885")

source("09_function_is_dark_190114_egel.R", chdir = T)

label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_tot_att_long <- merge(df_tot_att_long, df_color, by="Wahl")



# Plot ----
ggplot(df_tot_att_long, aes(x=reorder(att, zustimmung), y=pct, fill=factor(Wahl, levels= c ( "NA", "kann ich nicht beurteilen",  "nicht wichtig", "eher nicht wichtig","eher wichtig", "wichtig" )), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.5)+
  ylab("")+
  xlab("")+
  ggtitle("Wie wichtig waren Ihnen folgende Eigenschaften Ihres heutigen Essens?\n(Frage 5, alle Fragebogen, n =  1176)\n\nMein heutiges Essen...")+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = levels(df_tot_att_long$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                      values = ColsPerCat)+ 
  coord_flip() +
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  mytheme 


  ggsave(("04_plots/Frage_5_alle_191104_vori.pdf"),
         width = 14.5,
         height = 7,
         dpi=600,
         units="cm",
         device=cairo_pdf)



# Frage 6: Antwortverteilung der Stichprobe alle -----  
df_tot$table <- 1
CrossTable(df_tot$table, df_tot$diet)
CrossTable(df_tot$table, df_tot$allerg)
CrossTable(df_tot$table, df_tot$relig)
CrossTable(df_tot$table, df_tot$meds)
  
  
# Frage 7: Antwortverteilung der Stichprobe alle ohne Mehrfachteilnahme -----

# Teildatensatz mit Antworten zu Frage 5
df_tot_fill_tho <- df_tot_fill %>% select(c(tho_1:tho_5))

# Umcodierung
df_tot_fill_tho$tho_1 = factor(df_tot_fill_tho$tho_1, levels = c (1:4,"NA"), labels = c("stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu", "NA"))
df_tot_fill_tho$tho_1[is.na(df_tot_fill_tho$tho_1)]="NA"

df_tot_fill_tho$tho_2 = factor(df_tot_fill_tho$tho_2, levels = c (1:4,"NA"), labels = c("stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu", "NA"))
df_tot_fill_tho$tho_2[is.na(df_tot_fill_tho$tho_2)]="NA"

df_tot_fill_tho$tho_3 = factor(df_tot_fill_tho$tho_3, levels = c (1:4,"NA"), labels = c("stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu", "NA"))
df_tot_fill_tho$tho_3[is.na(df_tot_fill_tho$tho_3)]="NA"

df_tot_fill_tho$tho_4 = factor(df_tot_fill_tho$tho_4, levels = c (1:4,"NA"), labels = c("stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu", "NA"))
df_tot_fill_tho$tho_4[is.na(df_tot_fill_tho$tho_4)]="NA"

df_tot_fill_tho$tho_5 = factor(df_tot_fill_tho$tho_5, levels = c (1:4,"NA"), labels = c("stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu", "NA"))
df_tot_fill_tho$tho_5[is.na(df_tot_fill_tho$tho_5)]="NA"



# Datensatz mit Antwortverteilungen
df_tot_fill_tho1 <-df_tot_fill_tho %>% count(tho_1) %>%  rename("Wahl"="tho_1") %>% mutate (tho="... meiner Ernährungsweise\nfür meine Gesundheit.")
df_tot_fill_tho2 <-df_tot_fill_tho %>% count(tho_2) %>%  rename("Wahl"="tho_2") %>% mutate (tho="... meiner Ernährungsgewohnheiten\nfür die Umwelt.")
df_tot_fill_tho3 <-df_tot_fill_tho %>% count(tho_3) %>%  rename("Wahl"="tho_3") %>% mutate (tho="... der Produktion der Nahrungsmittel auf meinem Teller\nfür die Arbeitenden in der Wertschöpfungskette.") 
df_tot_fill_tho4 <-df_tot_fill_tho %>% count(tho_4) %>%  rename("Wahl"="tho_4") %>% mutate (tho="... meines Konsums von tierischen Nahrungsmittel\nfür die Tiere.") 
df_tot_fill_tho5 <-df_tot_fill_tho %>% count(tho_5) %>%  rename("Wahl"="tho_5") %>% mutate (tho="... meiner Ernährung für mein Portemonnaie.")  

#Für die Sotierung wird die Anzahl Antworten mit "trifft zu" und 
df_tot_fill_tho1 <-df_tot_fill_tho1 %>% mutate (zustimmung = df_tot_fill_tho1$n[4])  
df_tot_fill_tho2 <-df_tot_fill_tho2 %>% mutate (zustimmung = df_tot_fill_tho2$n[4])  
df_tot_fill_tho3 <-df_tot_fill_tho3 %>% mutate (zustimmung = df_tot_fill_tho3$n[4])  
df_tot_fill_tho4 <-df_tot_fill_tho4 %>% mutate (zustimmung = df_tot_fill_tho4$n[4])  
df_tot_fill_tho5 <-df_tot_fill_tho5 %>% mutate (zustimmung = df_tot_fill_tho5$n[4])  


#Hinzufügen von Prozent angaben
df_tot_fill_tho1 <-df_tot_fill_tho1 %>% mutate (pct = n/sum(n))
df_tot_fill_tho2 <-df_tot_fill_tho2 %>% mutate (pct = n/sum(n))
df_tot_fill_tho3 <-df_tot_fill_tho3 %>% mutate (pct = n/sum(n))
df_tot_fill_tho4 <-df_tot_fill_tho4 %>% mutate (pct = n/sum(n))
df_tot_fill_tho5 <-df_tot_fill_tho5 %>% mutate (pct = n/sum(n))


#Zusammenführung in ein Datenset 
df_tot_fill_tho_long <- bind_rows(df_tot_fill_tho1, df_tot_fill_tho2, df_tot_fill_tho3, df_tot_fill_tho4, df_tot_fill_tho5) 

#add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_tot_fill_tho_long <- merge(df_tot_fill_tho_long, df_color, by="Wahl")

# Plot ----
ggplot(df_tot_fill_tho_long, aes(x=reorder(tho, zustimmung), y=pct, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.5) +
  ylab("")+
  xlab("")+
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zum Essen zu?\n(Frage 7, ohne Mehrfachteilnahmen, n =  769)\n\nIch mache mir allgemein Gedanken über die Folgen ...")+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = levels(df_tot_fill_tho_long$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat)  +
  coord_flip() +
  guides(color = F, fill = guide_legend("", reverse = T)) +
  mytheme 

  ggsave(("04_plots/Frage_7_alle_191105_vori.pdf"),
         width = 14.5,
         height = 6,
         dpi=600,
         units="cm",
         device=cairo_pdf)


# Frage 8: Antwortverteilung der Stichprobe alle ohne Mehrfachteilnahme -----

# Teildatensatz mit Antworten zu Frage 8
df_tot_fill_tra <- df_tot_fill %>% select(c(tra_1:tra_5))

# Umcodierung
df_tot_fill_tra$tra_1 = factor(df_tot_fill_tra$tra_1, levels = c (1:4,"NA"), labels = c("stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu", "NA"))
df_tot_fill_tra$tra_1[is.na(df_tot_fill_tra$tra_1)]="NA"

df_tot_fill_tra$tra_2 = factor(df_tot_fill_tra$tra_2, levels = c (1:4,"NA"), labels = c("stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu", "NA"))
df_tot_fill_tra$tra_2[is.na(df_tot_fill_tra$tra_2)]="NA"

df_tot_fill_tra$tra_3 = factor(df_tot_fill_tra$tra_3, levels = c (1:4,"NA"), labels = c("stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu", "NA"))
df_tot_fill_tra$tra_3[is.na(df_tot_fill_tra$tra_3)]="NA"

df_tot_fill_tra$tra_4 = factor(df_tot_fill_tra$tra_4, levels = c (1:4,"NA"), labels = c("stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu", "NA"))
df_tot_fill_tra$tra_4[is.na(df_tot_fill_tra$tra_4)]="NA"

df_tot_fill_tra$tra_5 = factor(df_tot_fill_tra$tra_5, levels = c (1:4,"NA"), labels = c("stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu", "NA"))
df_tot_fill_tra$tra_5[is.na(df_tot_fill_tra$tra_5)]="NA"



# Datensatz mit Antwortverteilungen
df_tot_fill_tra1 <-df_tot_fill_tra %>% count(tra_1) %>%  rename("Wahl"="tra_1") %>% mutate (tra="... gesund zu leben.")
df_tot_fill_tra2 <-df_tot_fill_tra %>% count(tra_2) %>%  rename("Wahl"="tra_2") %>% mutate (tra="... mit meinem Verhalten die Umwelt\nmöglichst wenig zu belasten.")
df_tot_fill_tra3 <-df_tot_fill_tra %>% count(tra_3) %>%  rename("Wahl"="tra_3") %>% mutate (tra="... dass die Welt sozial\ngerechter wird.") 
df_tot_fill_tra4 <-df_tot_fill_tra %>% count(tra_4) %>%  rename("Wahl"="tra_4") %>% mutate (tra="... dass die Arbeitsbedingungen\nfür alle Menschen human sind.") 
df_tot_fill_tra5 <-df_tot_fill_tra %>% count(tra_5) %>%  rename("Wahl"="tra_5") %>% mutate (tra="... dass Tiere möglichst artgerecht\ngehalten werden")  

#Für die Sotierung wird die Anzahl Antworten mit "trifft zu" 
df_tot_fill_tra1 <-df_tot_fill_tra1 %>% mutate (zustimmung = df_tot_fill_tra1$n[4])  
df_tot_fill_tra2 <-df_tot_fill_tra2 %>% mutate (zustimmung = df_tot_fill_tra2$n[4])  
df_tot_fill_tra3 <-df_tot_fill_tra3 %>% mutate (zustimmung = df_tot_fill_tra3$n[4])  
df_tot_fill_tra4 <-df_tot_fill_tra4 %>% mutate (zustimmung = df_tot_fill_tra4$n[4])  
df_tot_fill_tra5 <-df_tot_fill_tra5 %>% mutate (zustimmung = df_tot_fill_tra5$n[4])  


#Hinzufügen von Prozent angaben
df_tot_fill_tra1 <-df_tot_fill_tra1 %>% mutate (pct = n/sum(n))
df_tot_fill_tra2 <-df_tot_fill_tra2 %>% mutate (pct = n/sum(n))
df_tot_fill_tra3 <-df_tot_fill_tra3 %>% mutate (pct = n/sum(n))
df_tot_fill_tra4 <-df_tot_fill_tra4 %>% mutate (pct = n/sum(n))
df_tot_fill_tra5 <-df_tot_fill_tra5 %>% mutate (pct = n/sum(n))


#Zusammenführung in ein Datenset 
df_tot_fill_tra_long <- bind_rows(df_tot_fill_tra1, df_tot_fill_tra2, df_tot_fill_tra3, df_tot_fill_tra4, df_tot_fill_tra5) 


#add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_tot_fill_tra_long <- merge(df_tot_fill_tra_long, df_color, by="Wahl")


# Plot ----
ggplot(df_tot_fill_tra_long, aes(x=reorder(tra, zustimmung), y=pct, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.5) +
  ylab("")+
  xlab("")+
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zu?\n(Frage 8, ohne Mehrfachteilnahmen, n =  769)\n\nMir ist es allgemein wichtig, ...")+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = levels(df_tot_fill_tra_long$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat)  +
  coord_flip() +
  guides(color = F, fill = guide_legend("", reverse = T)) +
  mytheme

  ggsave(("04_plots/Frage_8_alle_190506_vori.pdf"),
         width = 14.5,
         height = 6,
         dpi=600,
         units="cm",
         device=cairo_pdf)


# Frage 9: Antwortverteilung der Stichprobe alle ohne Mehrfachteilnahme  -----

# Infromations about Eating behaviors from the ZHAW 2017 survey are added (https://intra.zhaw.ch/?id=540)
  
df_tot_fill <-  df_tot_fill %>% 
  mutate(meat = ifelse(is.na(meat), "NA", meat)) %>%  
  mutate(milk = ifelse(is.na(milk), "NA", milk)) %>% 
  mutate(veget = ifelse(is.na(veget), "NA", veget)) %>% 
  mutate(veg = ifelse(is.na(veg), "NA", veg))  
  

df_tot_fill <- df_tot_fill %>% 
  mutate(meat = recode_factor(meat, "7"= "2x und mehr pro Tag", "6" = "1x pro Tag", "5" = "5-6x pro Woche", "4" = "3-4x pro Woche",
                                "3" = "1-2x pro Woche", "2" = "1-2x pro Monat", "1" = "nie")) %>% 
  mutate(milk = recode_factor(milk, "7"= "3x und mehr pro Tag", "6" = "1-2x pro Tag", "5" = "5-6x pro Woche", "4" = "3-4x pro Woche", "3" = "1-2x pro Woche", "2" = "1-2x pro Monat", "1" = "nie")) %>% 
  mutate(veget = recode_factor(veget, "6"= "immer", "5" = "meistens", "4" = "oft", "3" = "manchmal", "2" = "selten", "1" = "nie")) %>% 
  mutate(veg = recode_factor(veg, "6"= "immer", "5" = "meistens", "4" = "oft", "3" = "manchmal", "2" = "selten", "1" = "nie")) 




# Meat ----

df_tot_fill_meat <-df_tot_fill %>% count(meat) 

#Add percent, selecting and ordering
df_tot_fill_meat$per<-df_tot_fill_meat$n/sum(df_tot_fill_meat$n)


df_tot_fill_meat <- df_tot_fill_meat %>% select(c("meat", "per"))

#Reshape data for Plot
dfm_M <- melt(df_tot_fill_meat[,c("meat", "per")],id.vars = 1)
dfm_M$meat <- ordered(dfm_M$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "NA"))

# Grüental & Reidbach ZHAW  
meat <- df_tot_fill_meat$meat
zhaw_GR<-c(19, 32, 45, 65, 70, 18, 49, 0)
df_tot_fill_meat_GR <- data.frame(meat, zhaw_GR)
df_tot_fill_meat_GR$zhaw_GR_per<-df_tot_fill_meat_GR$zhaw_GR/sum(df_tot_fill_meat_GR$zhaw_GR)

dfm_GR_M <- melt(df_tot_fill_meat_GR[,c("meat", "zhaw_GR_per")],id.vars = 1)
dfm_GR_M$meat <- ordered(dfm_GR_M$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "NA"))


# Whole ZHAW  
meat <- df_tot_fill_meat$meat
zhaw_T<-c(74, 147, 208, 397, 324, 79, 149, 0)
df_tot_fill_meat_Z <- data.frame(meat, zhaw_T)
df_tot_fill_meat_Z$zhaw_T_per<-df_tot_fill_meat_Z$zhaw_T/sum(df_tot_fill_meat_Z$zhaw_T)

dfm_Z_M <- melt(df_tot_fill_meat_Z[,c("meat", "zhaw_T_per")],id.vars = 1)
dfm_Z_M$meat <- ordered(dfm_Z_M$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "NA"))


# merge the three datasets for one plot 
dfm_T_M <- rbind(dfm_M, dfm_GR_M, dfm_Z_M)
dfm_T_M$variable <- dplyr::recode(dfm_T_M$variable, "per"="NOVANIMAL Befragung Grüental & Reidbach\n(n = 769)", "zhaw_GR_per"="ZHAW Grüental & Reidbach \n(n = 298)",  "zhaw_T_per"="ZHAW Total\n(n = 1'378)")



# barplot for meat 
ggplot(dfm_T_M,aes(x = meat,y = value, width=0.7)) +
  geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
  labs(x = '',y='')+
  scale_fill_manual("",values = c("NOVANIMAL Befragung Grüental & Reidbach\n(n = 769)"="#fad60d", "ZHAW Grüental & Reidbach \n(n = 298)"="#505050", "ZHAW Total\n(n = 1'378)"="#c5b87c"))+
  mytheme+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0, 0.3))+
  geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100)), "%", sep = " "))), group=variable),
            size = 1.9, position = position_dodge(width = 0.8), vjust = -0.5)+  
  ggtitle("Ich esse Fleisch (alle Sorten inkl. Geflügel, Wurstwaren etc.):")

  ggsave(("04_plots/Frage_9_Fleisch_alle_190506_vori.pdf"),
         width = 14.5,
         height = 7,
         dpi=600,
         units="cm",
         device=cairo_pdf) 
  


# milk ----

df_tot_fill_milk <- df_tot_fill %>% count(milk) 

#Add percent, selecting and ordering
df_tot_fill_milk$per<-df_tot_fill_milk$n/sum(df_tot_fill_milk$n)


df_tot_fill_milk <- df_tot_fill_milk %>% select(c("milk", "per"))

#Reshape data for Plot
dfm_mi <- melt(df_tot_fill_milk[,c("milk", "per")],id.vars = 1)
dfm_mi$milk <- ordered(dfm_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "NA"))

# Grüental & Reidbach ZHAW  
milk <- df_tot_fill_milk$milk
zhaw_GR<-c(22, 87+99, 34, 27, 6, 12, 9, 0) #in der ZHAW Umfrage gab es die beiden AusWahloptionen: "1x pro Tag" und "2x pro Tag" 
df_tot_fill_milk_GR <- data.frame(milk, zhaw_GR)
df_tot_fill_milk_GR$zhaw_GR_per<-df_tot_fill_milk_GR$zhaw_GR/sum(df_tot_fill_milk_GR$zhaw_GR)

dfm_GR_mi <- melt(df_tot_fill_milk_GR[,c("milk", "zhaw_GR_per")],id.vars = 1)
dfm_GR_mi$milk <- ordered(dfm_GR_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "NA"))



# Whole ZHAW  
milk <- df_tot_fill_milk$milk
zhaw_T<- c (103, 433+387, 157, 125, 64, 36, 57, 0) #in der ZHAW Umfrage gab es die beiden AusWahloptionen: "1x pro Tag" und "2x pro Tag" 
df_tot_fill_milk_Z <- data.frame(milk, zhaw_T)
df_tot_fill_milk_Z$zhaw_T_per<-df_tot_fill_milk_Z$zhaw_T/sum(df_tot_fill_milk_Z$zhaw_T)

dfm_Z_mi <- melt(df_tot_fill_milk_Z[,c("milk", "zhaw_T_per")],id.vars = 1)
dfm_Z_mi$milk <- ordered(dfm_Z_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "NA"))



# merge the three datasets for one plot 
dfm_T_mi <- rbind(dfm_mi, dfm_GR_mi, dfm_Z_mi)
dfm_T_mi$variable <- dplyr::recode(dfm_T_mi$variable, "per"="NOVANIMAL Befragung Grüental & Reidbach\n(n = 769)", "zhaw_GR_per"="ZHAW Grüental & Reidbach \n(n = 296)","zhaw_T_per"="Total ZHAW\n(n = 1'362)")



# barplot for milk 

ggplot(dfm_T_mi,aes(x = milk,y = value, width=0.7)) +
  geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
  labs(x = '',y='Prozent')+
  scale_fill_manual("",values = c("NOVANIMAL Befragung Grüental & Reidbach\n(n = 769)"="#fad60d", "ZHAW Grüental & Reidbach \n(n = 296)"="#505050", "Total ZHAW\n(n = 1'362)"="#c5b87c"))+
  mytheme+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
            size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+
  ggtitle("Ich trinke/esse Milch/Milchprodukte, Käse:")

  ggsave(("04_plots/Frage_9_Milch_alle_190506_vori.pdf"),
         width = 17,
         height = 8,
         dpi=600,
         units="cm",
         device="pdf")



# Vegetarian ----


# Creating new Dataframe for veget
df_tot_fill_veget <-df_tot_fill %>% count(veget) 

# recode veget because of diffrent scales 
df_tot_fill_veget$veget <-  dplyr::recode(df_tot_fill_veget$veget, "meistens"="meistens /\n5- bis 6-mal\npro Woche", "oft"="oft /\n3- bis 4-mal\npro Woche", "manchmal"="manchmal /\n1- bis 2-mal\npro Woche", "selten"="selten /\n1- bis 2-mal\npro Monat")



# Adding the numbers from people who filled out the questionnair the first time
# df_tot_fill_veget$first <- df_first %>% count(veget)



#Add percent, selecting and ordering
df_tot_fill_veget$per<-df_tot_fill_veget$n/sum(df_tot_fill_veget$n)


# df_tot_fill_veget <- df_tot_fill_veget %>% select(c("veget", "Grue", "Grue_first", "Grue_zhaw"))
df_tot_fill_veget <- df_tot_fill_veget %>% select(c("veget", "per"))


#Reshape data for Plot
dfm_tot_fill_veget <- melt(df_tot_fill_veget[,c("veget", "per")],id.vars = 1)
dfm_tot_fill_veget$veget <- ordered(dfm_tot_fill_veget$veget, levels = c("immer", "meistens"="meistens /\n5- bis 6-mal\npro Woche", "oft"="oft /\n3- bis 4-mal\npro Woche", "manchmal"="manchmal /\n1- bis 2-mal\npro Woche", "selten"="selten /\n1- bis 2-mal\npro Monat", "nie", "NA"))

# Grüental & Reidbach ZHAW
veget <- df_tot_fill_veget$veget
zhaw_GR<-c(48, 55, 27, 67, 39, 59, 0)
df_tot_fill_veget_GR <- data.frame(veget, zhaw_GR)
df_tot_fill_veget_GR$zhaw_GR_per<-df_tot_fill_veget_GR$zhaw_GR/sum(df_tot_fill_veget_GR$zhaw_GR)

dfm_GR_tot_fill_veget <- melt(df_tot_fill_veget_GR[,c("veget", "zhaw_GR_per")],id.vars = 1)
dfm_GR_tot_fill_veget$veget <- ordered(dfm_GR_$veget, levels = c("immer", "meistens"="meistens /\n5- bis 6-mal\npro Woche", "oft"="oft /\n3- bis 4-mal\npro Woche", "manchmal"="manchmal /\n1- bis 2-mal\npro Woche", "selten"="selten /\n1- bis 2-mal\npro Monat", "nie", "NA"))


# Whole ZHAW
veget <- df_tot_fill_veget$veget
zhaw_T<-c(133, 171, 310, 367, 145, 230, 0)
df_tot_fill_veget_Z <- data.frame(veget, zhaw_T)
df_tot_fill_veget_Z$zhaw_T_per<-df_tot_fill_veget_Z$zhaw_T/sum(df_tot_fill_veget_Z$zhaw_T)

dfm_Z_tot_fill_veget <- melt(df_tot_fill_veget_Z[,c("veget", "zhaw_T_per")],id.vars = 1)
dfm_Z_tot_fill_veget$veget <- ordered(dfm_Z_tot_fill_veget$veget, levels = c("immer", "meistens"="meistens /\n5- bis 6-mal\npro Woche", "oft"="oft /\n3- bis 4-mal\npro Woche", "manchmal"="manchmal /\n1- bis 2-mal\npro Woche", "selten"="selten /\n1- bis 2-mal\npro Monat", "nie", "NA"))

#Reshape data for Plot
# merge the three datasets for one plot
dfm_T_tot_fill_veget <- rbind(dfm_tot_fill_veget, dfm_GR_tot_fill_veget, dfm_Z_tot_fill_veget)
dfm_T_tot_fill_veget$variable <- dplyr::recode(dfm_T_tot_fill_veget$variable, "per"="NOVANIMAL Befragung Grüental & Reidbach\n(n = 769)", "zhaw_GR_per"="Grüental & Reidbach \n(n = 295)", "zhaw_T_per"="Total ZHAW\n(n = 1'356)")



# barplot for veget

ggplot(dfm_T_tot_fill_veget,aes(x = veget,y = value, width=0.7)) +
  geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
  labs(x = '',y='Prozent')+
  scale_fill_manual("",values = c("NOVANIMAL Befragung Grüental & Reidbach\n(n = 769)"="#fad60d", "Grüental & Reidbach \n(n = 295)"="#505050", "Total ZHAW\n(n = 1'356)"="#c5b87c"))+
  mytheme+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=(ifelse(value<0.001, "", paste(format(round(value*100,1), nsmall = 1), "%", sep = " ")))  , group=variable),
            size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+
  ggtitle("Ich ernähre mich vegetarisch (ovo-lakto):")+
  ggsave(("04_plots/Frage_9_vegetarisch_alle_190506_vori.pdf"),
         width = 17,
         height = 8,
         dpi=600,
         units="cm",
         device="pdf")

# Vegan----

# Creating new Dataframe for veg
df_veg_tot_fill<-df_tot_fill %>% count(veg) 

# recode veg because of diffrent scales 
df_veg_tot_fill$veg <- dplyr:: recode(df_veg_tot_fill$veg, "meistens"="meistens /\n5- bis 6-mal\npro Woche", "oft"="oft /\n3- bis 4-mal\npro Woche", "manchmal"="manchmal /\n1- bis 2-mal\npro Woche", "selten"="selten /\n1- bis 2-mal\npro Monat")



# Adding the numbers from people who filled out the questionnair the first time
# df_veg_tot_fill$first <- df_first %>% count(veg)



#Add percent, selecting and ordering
df_veg_tot_fill$per<-df_veg_tot_fill$n/sum(df_veg_tot_fill$n)


# df_veg_tot_fill <- df_veg_tot_fill %>% select(c("veg", "Grue", "Grue_first", "Grue_zhaw"))
df_veg_tot_fill <- df_veg_tot_fill %>% select(c("veg", "per"))


#Reshape data for Plot
dfm_veg <- melt(df_veg_tot_fill[,c("veg", "per")],id.vars = 1)
dfm_veg$veg <- ordered(dfm_veg$veg, levels = c("immer", "meistens"="meistens /\n5- bis 6-mal\npro Woche", "oft"="oft /\n3- bis 4-mal\npro Woche", "manchmal"="manchmal /\n1- bis 2-mal\npro Woche", "selten"="selten /\n1- bis 2-mal\npro Monat", "nie", "NA"))

# Grüental & Reidbach ZHAW
veg <- df_veg_tot_fill$veg
zhaw_GR<-c(7, 15, 17, 39, 62, 155, 0)
df_veg_tot_fill_GR <- data.frame(veg, zhaw_GR)
df_veg_tot_fill_GR$zhaw_GR_per<-df_veg_tot_fill_GR$zhaw_GR/sum(df_veg_tot_fill_GR$zhaw_GR)

dfm_GR_veg <- melt(df_veg_tot_fill_GR[,c("veg", "zhaw_GR_per")],id.vars = 1)
dfm_GR_veg$veg <- ordered(dfm_GR_veg$veg, levels = c("immer", "meistens"="meistens /\n5- bis 6-mal\npro Woche", "oft"="oft /\n3- bis 4-mal\npro Woche", "manchmal"="manchmal /\n1- bis 2-mal\npro Woche", "selten"="selten /\n1- bis 2-mal\npro Monat", "nie", "NA"))


# Whole ZHAW 
veg <- df_veg_tot_fill$veg
zhaw_T<-c(29, 30, 48, 146, 232, 871, 0)
df_veg_tot_fill_Z <- data.frame(veg, zhaw_T)
df_veg_tot_fill_Z$zhaw_T_per<-df_veg_tot_fill_Z$zhaw_T/sum(df_veg_tot_fill_Z$zhaw_T)

dfm_Z_veg <- melt(df_veg_tot_fill_Z[,c("veg", "zhaw_T_per")],id.vars = 1)
dfm_Z_veg$veg <- ordered(dfm_Z_veg$veg, levels = c("immer", "meistens"="meistens /\n5- bis 6-mal\npro Woche", "oft"="oft /\n3- bis 4-mal\npro Woche", "manchmal"="manchmal /\n1- bis 2-mal\npro Woche", "selten"="selten /\n1- bis 2-mal\npro Monat", "nie", "NA"))

#Reshape data for Plot
# merge the three datasets for one plot
dfm_T_veg <- rbind(dfm_veg, dfm_GR_veg, dfm_Z_veg)
dfm_T_veg$variable <- dplyr::recode(dfm_T_veg$variable, "per"="NOVANIMAL Befragung Grüental & Reidbach\n(n = 769)", "zhaw_GR_per"="Grüental & Reidbach \n(n = 295)", "zhaw_T_per"="Total ZHAW\n(n = 1'356)")



# barplot for veg 

ggplot(dfm_T_veg,aes(x = veg,y = value, width=0.7)) +
  geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
  labs(x = '',y='Prozent')+
  scale_fill_manual("",values = c("NOVANIMAL Befragung Grüental & Reidbach\n(n = 769)"="#fad60d", "Grüental & Reidbach \n(n = 295)"="#505050", "Total ZHAW\n(n = 1'356)"="#c5b87c"))+
  mytheme+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
            size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+
  ggtitle("Ich ernähre mich rein pflanzlich (vegan):")+
  ggsave(("04_plots/Frage_9_vegan_alle_190506_vori.pdf"),
         width = 17,
         height = 20,
         dpi=600,
         units="cm",
         device="pdf")
