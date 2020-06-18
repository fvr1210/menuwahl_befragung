library (gmodels) #Version 2.18.1
library (formattable) #Version 0.2.0.1
library (reshape2) #Version 1.4.3
library (magrittr) #Version 1.5
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
# Nur Fragebogen bei das Geschlecht bekannt ist 

df_tot_gender <- read_delim("2019_ZHAW_vonRickenbach_cleaned_recorded_survey_dataset_NOVANIMAL_anonym.csv", delim=';', locale = locale(encoding = 'ISO-8859-2')) %>% 
  filter(gender=="Mann" | gender=="Frau") %>%  #Anzahl Fragebogen 1147
  mutate(gender_2 = if_else(gender=="Mann", "Mann (n = 652)", "Frau (n = 495)"))
  

# Teilstichproben bilden


# Männer
df_mann <- df_tot_gender %>% 
  filter(gender=="Mann") # Anzahl Fragebogen = 652


# Frauen
df_frau <- df_tot_gender %>% 
  filter(gender=="Frau") # Anzahl Fragebogen = 495

# Hochschulzugehoerigkeit
table(df_frau$member, exclude=NULL)
round(table(df_frau$member, exclude=NULL)/nrow(df_frau)*100,1)

# Alter
# summary(df_frau$age)
# sd(df_frau$age, na.rm = T)


# Männer und Frauen die ein Mensamenü konsumiert haben
df_mensa_gender <- df_tot_gender %>% 
  filter(mensa==1) %>% 
  mutate(gender_2 = if_else(gender=="Mann", "Mann (n = 526)", "Frau (n = 327)"))





# Frage 1: Mensa oder selber -----
gender_mensa <- df_tot_gender %>% 
  group_by(gender_2, mensa) %>% 
  summarize(count = n()) %>% 
  mutate(perc=count/sum(count)) %>% 
  ungroup() %>% 
  mutate(mensa = recode(.$mensa, "1" = "in Mensa gegessen", "0" = "Essen selber mitgebracht"))

ColsPerCat = c("Essen selber mitgebracht"="#823783", "in Mensa gegessen" = "#8AB5E1")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "mensa")

gender_mensa <- merge(gender_mensa, df_color, by="mensa")




ggplot(data = gender_mensa, aes(gender_2, y = perc, fill=factor(mensa, levels = c("Essen selber mitgebracht", "in Mensa gegessen")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(gender_mensa$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  geom_text(aes(label=(ifelse(perc<0.0249, "",paste( round(perc*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  ggtitle("Geschlecht\n(Art der Mittagsverpflegung, n = 1147)")+
  guides(color = F, fill = guide_legend("", reverse = T, nrow=1)) +
  coord_flip() +
  mytheme +
  scale_y_continuous(labels = scales::percent)

ggsave("04_plots/Frage_1_Geschlecht_191218.pdf",
       width = 14.5,
       height = 2,
       dpi=600,
       units="cm",
       device=cairo_pdf) 


# Frage 1: Menülinie -----
gender_linie <- df_tot_gender %>% 
  filter(mensa==1) %>% 
  filter(!is.na(meal)) %>%
  mutate(gender_2 = if_else(gender=="Mann", "Mann (n = 477)", "Frau (n = 304)")) %>% 
  mutate(meal = fct_recode(meal, "Favorite/World"="Favorite")) %>% 
  mutate(meal = fct_recode(meal, "Favorite/World"="World")) %>% 
  group_by(gender_2, meal) %>% 
  summarize(count = n()) %>% 
  mutate(perc=count/sum(count)) %>% 
  ungroup() 
  



ColsPerCat = c("Hot & Cold" = "#Fdd200", "Kitchen" = "#AFC410", "Favorite/World" = "#D3C193")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "meal")

gender_linie <- merge(gender_linie, df_color, by="meal")





ggplot(data = gender_linie, aes(gender_2, y = perc, fill=factor(meal, levels = c("Hot & Cold", "Kitchen", "Favorite/World")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(gender_linie$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  geom_text(aes(label=(ifelse(perc<0.0249, "",paste( round(perc*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  ggtitle("Geschlecht\n(Gewählte Menülinie, n = 781)") +
  guides(color = F, fill = guide_legend("", reverse = T, nrow=1)) +
  coord_flip() +
  mytheme +
  scale_y_continuous(labels = scales::percent)

ggsave("04_plots/Frage_1_linie_Geschlecht_191218.pdf",
       width = 14.5,
       height = 2.3,
       dpi=600,
       units="cm",
       device=cairo_pdf) 




# Frage 2: Direkter Gruppenvergleich ----

# Datensätze erstellen

df_gender_choice1 <- df_mensa_gender%>% 
  select(c(choice_1, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=choice_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_1")) %>% 
  mutate(question = "... es gluschtig (schmackhaft) aussah.")

df_gender_choice2 <- df_mensa_gender%>% 
  select(c(choice_2, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=choice_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_2")) %>% 
  mutate(question = "... die Menü-Beschreibung mich angesprochen hat.")

df_gender_choice3 <- df_mensa_gender%>% 
  select(c(choice_3, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=choice_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_3")) %>% 
  mutate(question = "... ich es kenne.")

df_gender_choice4 <- df_mensa_gender%>% 
  select(c(choice_4, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=choice_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_4")) %>% 
  mutate(question = "... ich grad Lust darauf hatte.")

df_gender_choice5 <- df_mensa_gender%>% 
  select(c(choice_5, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=choice_5) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_5")) %>% 
  mutate(question = "... es eines meiner Lieblingsmenüs ist.")

df_gender_choice6 <- df_mensa_gender%>% 
  select(c(choice_6, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=choice_6) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_6")) %>% 
  mutate(question = "... mir die anderen Menüs noch weniger passten.")

df_gender_choice7 <- df_mensa_gender%>% 
  select(c(choice_7, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=choice_7) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_7")) %>% 
  mutate(question = "... ich etwas Neues ausprobieren wollte.")

df_gender_choice8 <- df_mensa_gender%>% 
  select(c(choice_8, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=choice_8) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_8")) %>% 
  mutate(question = "... ich das nicht selber koche.")

df_gender_choice9 <- df_mensa_gender%>% 
  select(c(choice_9, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=choice_9) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_9")) %>% 
  mutate(question = "... ich es beim Eingang zur Mensa gesehen habe (Menü-Aushang).")

df_gender_choice10 <- df_mensa_gender%>% 
  select(c(choice_10, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=choice_10) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_10")) %>% 
  mutate(question = "... das Preis-/Leistungsverhältnis stimmt.")


# Datensätze zusammenführen
df_gender_choice <- bind_rows(df_gender_choice1, df_gender_choice2, df_gender_choice3, df_gender_choice4, df_gender_choice5, df_gender_choice6, df_gender_choice7, df_gender_choice8, df_gender_choice9, df_gender_choice10) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "trifft nicht zu", "2" = "trifft eher nicht zu","3" = "trifft eher zu", "4" = "trifft zu" )) %>% 
  group_by(question, Zustimmung, Inhalt, Wahl) %>%
  summarise(tot_answ = n()) %>%  
  mutate(pct = (tot_answ/sum(tot_answ))) 



# add colors
ColsPerCat = c("NA" = "grey", "trifft nicht zu" = "#DC6413", "trifft eher nicht zu" = "#Fdd200", "trifft eher zu" = "#AFC410", "trifft zu"="#006885")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_gender_choice <- merge(df_gender_choice, df_color, by="Wahl")


# für Plot Sortieren Augrund der Gesamtzustimmung
df_gender_choice$question <- reorder(df_gender_choice$question, -df_gender_choice$Zustimmung)


## plot ----
ggplot(data = df_gender_choice, aes(x=Inhalt, y=pct, fill=factor(Wahl, levels= c ("NA", "trifft nicht zu", "trifft eher nicht zu", "trifft eher zu", "trifft zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_gender_choice$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Mann (n = 652)", "Frau (n = 495)")) +
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Wie treffen die folgenden Aussagen auf Ihre heutige Menü-Wahl in der Mensa zu?\n(Frage 2, Geschlecht, n = 1147)\n\nIch habe dieses Menü heute gewählt, weil ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)




ggsave("04_plots/Frage_2_gender_unterteilt_191104_1.pdf",
       width = 14.5,
       height = 18.2,
       dpi=600,
       units="cm",
       device=cairo_pdf)


# Frage 3: Direkter Gruppenvergleich ----

# Datensätze erstellen

df_gender_satis1 <- df_mensa_gender%>% 
  select(c(satis_1, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=satis_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("satis_1")) %>% 
  mutate(question = "Meine Erwartungen an das Menü haben sich erfüllt.")

df_gender_satis2 <- df_mensa_gender%>% 
  select(c(satis_2, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=satis_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("satis_2")) %>% 
  mutate(question = "Ich fand das Menü gut.")

df_gender_satis3 <- df_mensa_gender%>% 
  select(c(satis_3, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=satis_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("satis_3")) %>% 
  mutate(question = "Ich werde dieses Menü nicht mehr nehmen.")



# Datensätze zusammenführen
df_gender_satis <- bind_rows(df_gender_satis1, df_gender_satis2, df_gender_satis3) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "trifft nicht zu", "2" = "trifft eher nicht zu","3" = "trifft eher zu", "4" = "trifft zu" )) %>% 
  group_by(question, Zustimmung, Inhalt, Wahl) %>%
  summarise(tot_answ = n()) %>%  
  mutate(pct = (tot_answ/sum(tot_answ))) 




# add colors
ColsPerCat = c("NA" = "grey", "trifft nicht zu" = "#DC6413", "trifft eher nicht zu" = "#Fdd200", "trifft eher zu" = "#AFC410", "trifft zu"="#006885")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_gender_satis <- merge(df_gender_satis, df_color, by="Wahl")


# für Plot Sortieren Augrund der Gesamtzustimmung
df_gender_satis$question <- reorder(df_gender_satis$question, -df_gender_satis$Zustimmung)


## plot ----
ggplot(data = df_gender_satis, aes(x=Inhalt, y=pct, fill=factor(Wahl, levels= c ("NA", "trifft nicht zu", "trifft eher nicht zu", "trifft eher zu", "trifft zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_gender_satis$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Mann (n = 652)", "Frau (n = 495)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "", paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Wie zufrieden sind Sie mit dem gewählten Menü? (Frage 3, Geschlecht, n = 1147)")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)

ggsave("04_plots/Frage_3_gender_unterteilt_191104.pdf",
       width = 14.5,
       height = 5.6,
       dpi=600,
       units="cm",
       device=cairo_pdf)

# Frage 4 ----

df_tot_gender_ing1 <- df_tot_gender%>% 
  select(c(ing_1, meal, gender_2)) %>% 
  mutate(Wahl=ing_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("ing_1", "meal")) %>% 
  mutate(question = "... enthält Proteine (Fleisch, Fisch, Tofu etc.).")

df_tot_gender_ing2 <- df_tot_gender%>% 
  select(c(ing_2, meal, gender_2)) %>% 
  mutate(Wahl=ing_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("ing_2", "meal")) %>% 
  mutate(question = "... enthält Fleisch.")

df_tot_gender_ing3 <- df_tot_gender%>% 
  select(c(ing_3, meal, gender_2)) %>% 
  mutate(Wahl=ing_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("ing_3", "meal")) %>% 
  mutate(question = "... enthält Fisch.")

df_tot_gender_ing4 <- df_tot_gender%>% 
  select(c(ing_4, meal, gender_2)) %>% 
  mutate(Wahl=ing_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("ing_4", "meal")) %>% 
  mutate(question = "... enhält Produkte aus artgerechter Tierhaltung.")



# Datensätze zusammenführen
df_tot_gender_ing <- bind_rows(df_tot_gender_ing1, df_tot_gender_ing2, df_tot_gender_ing3, df_tot_gender_ing4) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "nicht wichtig", "2" = "eher nicht wichtig","3" = "eher wichtig", "4" = "wichtig", "-99" = "kann ich nicht beurteilen" )) %>% 
  group_by(question, Zustimmung, gender_2, Wahl) %>%
  summarise(tot_answ = n()) %>%  
  mutate(pct = (tot_answ/sum(tot_answ))) 

# add colors
ColsPerCat = c("NA" = "grey", "kann ich nicht beurteilen"="#D3C193",  "nicht wichtig" = "#DC6413", "eher nicht wichtig" = "#Fdd200", "eher wichtig" = "#AFC410", "wichtig"="#006885")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_tot_gender_ing <- merge(df_tot_gender_ing, df_color, by="Wahl")

# für Plot Sortieren Augrund der Gesamtzustimmung
df_tot_gender_ing$question <- reorder(df_tot_gender_ing$question, -df_tot_gender_ing$Zustimmung)




## plot ----
ggplot(data = df_tot_gender_ing, aes(x=gender_2, y=pct, fill=factor(Wahl, levels= c ("NA", "kann ich nicht beurteilen", "nicht wichtig", "eher nicht wichtig", "eher wichtig","wichtig")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_tot_gender_ing$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Mann (n = 652)", "Frau (n = 495)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Wie wichtig waren Ihnen folgende Inhaltsstoffe Ihres heutigen Essens?\n(Frage 5, Geschlecht, n = 1147)\n\nMein heutiges Essen ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)

ggsave("04_plots/Frage_4_gender_unterteilt_191203.pdf",
       width = 14.5,
       height = 8,
       dpi=600,
       units="cm",
       device=cairo_pdf)



# Frage 5: Direkter Gruppenvergleich ----

# Datensätze erstellen

df_gender_att1 <- df_tot_gender%>% 
  select(c(att_1, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=att_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_1")) %>% 
  mutate(question = "... ist gesund.")

df_gender_att2 <- df_tot_gender%>% 
  select(c(att_2, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=att_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_2")) %>% 
  mutate(question = "...ist sättigend.")

df_gender_att3 <- df_tot_gender%>% 
  select(c(att_3, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=att_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_3")) %>% 
  mutate(question = "...ist leicht.")

df_gender_att4 <- df_tot_gender%>% 
  select(c(att_4, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=att_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_4")) %>% 
  mutate(question = "...ist wenig umweltbelastend.")

df_gender_att5 <- df_tot_gender%>% 
  select(c(att_5, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=att_5) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_5")) %>% 
  mutate(question = "...ist natürlich (keine Zusatzstoffe).")

df_gender_att6 <- df_tot_gender%>% 
  select(c(att_6, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=att_6) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_6")) %>% 
  mutate(question = "...ist vegetarisch (ovo-lakto).")

df_gender_att7 <- df_tot_gender%>% 
  select(c(att_7, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=att_7) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_7")) %>% 
  mutate(question = "...ist rein pflanzlich (vegan).")

df_gender_att8 <- df_tot_gender%>% 
  select(c(att_8, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=att_8) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_8")) %>% 
  mutate(question = "...ist frisch zubereitet.")

df_gender_att9 <- df_tot_gender%>% 
  select(c(att_9, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=att_9) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_9")) %>% 
  mutate(question = "...verhindert, dass Nahrungsmittel weggeworfen werden.")

df_gender_att10 <- df_tot_gender%>% 
  select(c(att_10, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=att_10) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_10")) %>% 
  mutate(question = "...stammt aus sozial verträglicher Produktion.")

df_gender_att11 <- df_tot_gender%>% 
  select(c(att_11, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=att_11) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_11")) %>% 
  mutate(question = "...schont mein Portemonnaie.")


# Datensätze zusammenführen
df_gender_att <- bind_rows(df_gender_att1, df_gender_att2, df_gender_att3, df_gender_att4, df_gender_att5, df_gender_att6, df_gender_att7, df_gender_att8, df_gender_att9, df_gender_att10, df_gender_att11) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "nicht wichtig", "2" = "eher nicht wichtig","3" = "eher wichtig", "4" = "wichtig", "-99" = "kann ich nicht beurteilen" )) %>% 
  group_by(question, Zustimmung, Inhalt, Wahl) %>%
  summarise(tot_answ = n()) %>%  
  mutate(pct = (tot_answ/sum(tot_answ))) #add empty coloms for spacing in the middle

# add colors
ColsPerCat = c("NA" = "grey", "kann ich nicht beurteilen"="#D3C193",  "nicht wichtig" = "#DC6413", "eher nicht wichtig" = "#Fdd200", "eher wichtig" = "#AFC410", "wichtig"="#006885")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_gender_att <- merge(df_gender_att, df_color, by="Wahl")


# für Plot Sortieren Augrund der Gesamtzustimmung
df_gender_att$question <- reorder(df_gender_att$question, -df_gender_att$Zustimmung)



## plot ----
ggplot(data = df_gender_att, aes(x=Inhalt, y=pct, fill=factor(Wahl, levels= c ("NA", "kann ich nicht beurteilen", "nicht wichtig", "eher nicht wichtig", "eher wichtig","wichtig")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_gender_att$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Mann (n = 652)", "Frau (n = 495)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Wie wichtig waren Ihnen folgende Eigenschaften Ihres heutigen Essens?\n(Frage 5, Geschlecht, n = 1147)\n\nMein heutiges Essen ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)



ggsave("04_plots/Frage_5_gender_unterteilt_191203.pdf",
       width = 14.5,
       height = 20,
       dpi=600,
       units="cm",
       device=cairo_pdf)





# Frage 6: Tabellen----

CrossTable(df_tot_gender$gender_2, df_tot_gender$diet)
CrossTable(df_tot_gender$gender_2, df_tot_gender$allerg)
CrossTable(df_tot_gender$gender_2, df_tot_gender$relig)
CrossTable(df_tot_gender$gender_2, df_tot_gender$meds)



# Frage 7: Direkter Gruppenvergleich ----

# Datensätze erstellen

df_gender_tho1 <- df_tot_gender%>% 
  select(c(tho_1, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=tho_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_1")) %>% 
  mutate(question = "...meiner Ernährungsweise für meine Gesundheit.")

df_gender_tho2 <- df_tot_gender%>% 
  select(c(tho_2, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=tho_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_2")) %>% 
  mutate(question = "...meiner Ernährungsgewohnheiten für die Umwelt.")

df_gender_tho3 <- df_tot_gender%>% 
  select(c(tho_3, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=tho_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_3")) %>% 
  mutate(question = "...der Produktion der Nahrungsmittel auf meinem Teller für die Arbeitenden in der Wertschöpfungskette.")

df_gender_tho4 <- df_tot_gender%>% 
  select(c(tho_4, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=tho_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_4")) %>% 
  mutate(question = "...meines Konsums von tierischen Nahrungsmitteln für die Tiere.")

df_gender_tho5 <- df_tot_gender%>% 
  select(c(tho_5, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=tho_5) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_5")) %>% 
  mutate(question = "...meiner Ernährung für mein Portemonnaie.")


# Datensätze zusammenführen
df_gender_tho <- bind_rows(df_gender_tho1, df_gender_tho2, df_gender_tho3, df_gender_tho4, df_gender_tho5) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(question, Zustimmung, Inhalt, Wahl) %>%
  summarise(tot_answ = n()) %>%  
  mutate(pct = (tot_answ/sum(tot_answ))) 


#add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_gender_tho <- merge(df_gender_tho, df_color, by="Wahl")


# für Plot Sortieren Augrund der Gesamtzustimmung
df_gender_tho$question <- reorder(df_gender_tho$question, -df_gender_tho$Zustimmung)


## plot ----
ggplot(data = df_gender_tho, aes(x=Inhalt, y=pct, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_gender_tho$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Mann (n = 652)", "Frau (n = 495)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zum Essen zu?\n(Frage 7, Geschlecht, n = 1147)\n\nIch mache mir allgemein Gedanken über die Folgen ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)



ggsave("04_plots/Frage_7_gender_unterteilt_190528.pdf",
       width = 14.5,
       height = 10.9,
       dpi=600,
       units="cm",
       device=cairo_pdf)



# Frage 8: Direkter Gruppenvergleich ----

# Datensätze erstellen

df_gender_tra1 <- df_tot_gender%>% 
  select(c(tra_1, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=tra_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_1")) %>% 
  mutate(question = "...gesund zu leben.")

df_gender_tra2 <- df_tot_gender%>% 
  select(c(tra_2, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=tra_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_2")) %>% 
  mutate(question = "...mit meinem Verhalten die Umwelt möglichst wenig zu belasten.")

df_gender_tra3 <- df_tot_gender%>% 
  select(c(tra_3, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=tra_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_3")) %>% 
  mutate(question = "...dass die Welt sozial gerechter wird.")

df_gender_tra4 <- df_tot_gender%>% 
  select(c(tra_4, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=tra_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_4")) %>% 
  mutate(question = "...dass die Arbeitsbedingungen für alle Menschen human sind.")

df_gender_tra5 <- df_tot_gender%>% 
  select(c(tra_5, gender_2)) %>% 
  mutate(Inhalt=gender_2) %>% 
  mutate(Wahl=tra_5) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_5")) %>% 
  mutate(question = "...dass Tiere möglichst argerecht gehalten werden.")


# Datensätze zusammenführen
df_gender_tra <- bind_rows(df_gender_tra1, df_gender_tra2, df_gender_tra3, df_gender_tra4, df_gender_tra5) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(question, Zustimmung, Inhalt, Wahl) %>%
  summarise(tot_answ = n()) %>%  
  mutate(pct = (tot_answ/sum(tot_answ)))  

#add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_gender_tra <- merge(df_gender_tra, df_color, by="Wahl")


# für Plot Sortieren Augrund der Gesamtzustimmung
df_gender_tra$question <- reorder(df_gender_tra$question, -df_gender_tra$Zustimmung)


## plot ----
ggplot(data = df_gender_tra, aes(x=Inhalt, y=pct,  fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_gender_tra$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Mann (n = 652)", "Frau (n = 495)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F,   fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zu?\n(Frage 8, Geschlecht, n = 1147)\n\nMir ist es allgemein wichtig, ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)


ggsave("04_plots/Frage_8_Geschlecht_unterteilt_191203.pdf",
       width = 14.5,
       height = 10.9,
       dpi=600,
       units="cm",
       device=cairo_pdf)

# Frage 9: Direkter Vergleich ----

# Datensatz bearbeiten 
df_mann_fill <- df_tot_gender %>% 
  filter(gender=="Mann") %>% 
  filter(fill==0) %>% 
  mutate(meat = ifelse(is.na(meat), "NA", meat)) %>% 
  mutate(milk = ifelse(is.na(milk), "NA", milk)) %>% 
  mutate(veget = ifelse(is.na(veget), "NA", veget)) %>% 
  mutate(veg = ifelse(is.na(veg), "NA", veg)) %>% 
  mutate(meat = recode_factor(meat, "7"= "2x und mehr pro Tag", "6" = "1x pro Tag", "5" = "5-6x pro Woche", "4" = "3-4x pro Woche", "3" = "1-2x pro Woche", "2" = "1-2x pro Monat", "1" = "nie")) %>% 
  mutate(milk = recode_factor(milk, "7"= "3x und mehr pro Tag", "6" = "1-2x pro Tag", "5" = "5-6x pro Woche", "4" = "3-4x pro Woche", "3" = "1-2x pro Woche", "2" = "1-2x pro Monat", "1" = "nie")) %>% 
  mutate(veget = recode_factor(veget, "6"= "immer", "5" = "meistens", "4" = "oft", "3" = "manchmal", "2" = "selten", "1" = "nie")) %>% 
  mutate(veg = recode_factor(veg, "6"= "immer", "5" = "meistens", "4" = "oft", "3" = "manchmal", "2" = "selten", "1" = "nie")) 


df_frau_fill <- df_tot_gender %>% 
  filter(gender=="Frau") %>% 
  filter(fill==0) %>% 
  mutate(meat = ifelse(is.na(meat), "NA", meat)) %>% 
  mutate(milk = ifelse(is.na(milk), "NA", milk)) %>% 
  mutate(veget = ifelse(is.na(veget), "NA", veget)) %>% 
  mutate(veg = ifelse(is.na(veg), "NA", veg)) %>% 
  mutate(meat = recode_factor(meat, "7"= "2x und mehr pro Tag", "6" = "1x pro Tag", "5" = "5-6x pro Woche", "4" = "3-4x pro Woche", "3" = "1-2x pro Woche", "2" = "1-2x pro Monat", "1" = "nie")) %>% 
  mutate(milk = recode_factor(milk, "7"= "3x und mehr pro Tag", "6" = "1-2x pro Tag", "5" = "5-6x pro Woche", "4" = "3-4x pro Woche", "3" = "1-2x pro Woche", "2" = "1-2x pro Monat", "1" = "nie")) %>% 
  mutate(veget = recode_factor(veget, "6"= "immer", "5" = "meistens", "4" = "oft", "3" = "manchmal", "2" = "selten", "1" = "nie")) %>% 
  mutate(veg = recode_factor(veg, "6"= "immer", "5" = "meistens", "4" = "oft", "3" = "manchmal", "2" = "selten", "1" = "nie")) 

# Fleischkonsumtyp ----

df_tot_gender_fill <-  df_tot_gender %>% 
  filter(fill == 1) %>% 
  mutate(meat_diet = case_when (meat==7 ~ "Fleischliebhaber",
                                meat==6 ~ "Fleischliebhaber",
                                meat==5 ~ "Fleischesser",
                                meat==4 ~ "Fleisch-Flexitarier",
                                meat==3 ~ "Vegi-Flexitarier",
                                meat==2 ~ "Fleischvermeider",
                                meat==1 ~ "Fleischvermeider", 
                                TRUE ~ as.character(NA))) %>% 
  group_by(gender) %>%
  count(meat_diet) %>%
  mutate(pct = n/sum(n))




# Fleisch ----
df_mann_fill_meat <- df_mann_fill %>% count(meat)

df_frau_fill_meat <- df_frau_fill %>% count(meat)

# Prozent
df_mann_fill_meat$pct_m<-df_mann_fill_meat$n/sum(df_mann_fill_meat$n)

df_frau_fill_meat$pct_f<-df_frau_fill_meat$n/sum(df_frau_fill_meat$n)

# Umwandeln der Datensätze für den Plot
df_m_m <- melt(df_mann_fill_meat[,c("meat", "pct_m")], id.vars = 1)
df_m_m$meat <- ordered(df_m_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "NA"))

df_f_m <- melt(df_frau_fill_meat[,c("meat", "pct_f")], id.vars = 1)
df_f_m$meat <- ordered(df_f_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "NA"))

#Zusammenführen der Datensätze
df_mnm_m <- rbind(df_m_m, df_f_m)
df_mnm_m$variable <- dplyr::recode(df_mnm_m$variable, "pct_f"="Frau (n=334)", "pct_m"="Mann (n=418)")

ggplot(df_mnm_m,aes(x = meat,y = value, width=0.7)) +
  geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
  labs(x = '',y='Prozent')+
  scale_fill_manual("",values = c("Frau (n=334)"="#fad60d", "Mann (n=418)"="#505050"))+
  mytheme+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
            size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
  ggtitle("Ich esse Fleisch (alle Sorten inkl. Geflügel, Wurstwaren etc.):")+
  ggsave(("04_plots/Frage_9_Fleisch_gender_190528_vori.pdf"),
         width = 17,
         height = 8,
         dpi=600,
         units="cm",
         device="pdf")

# Milch ----
df_mann_fill_milk <- df_mann_fill %>% count(milk)

df_frau_fill_milk <- df_frau_fill %>% count(milk)

# Prozent
df_mann_fill_milk$pct_m<-df_mann_fill_milk$n/sum(df_mann_fill_milk$n)

df_frau_fill_milk$pct_f<-df_frau_fill_milk$n/sum(df_frau_fill_milk$n)

# Umwandeln der Datensätze für den Plot
df_m_mi <- melt(df_mann_fill_milk[,c("milk", "pct_m")], id.vars = 1)
df_m_mi$milk <- ordered(df_m_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "NA"))

df_f_mi <- melt(df_frau_fill_milk[,c("milk", "pct_f")], id.vars = 1)
df_f_mi$milk <- ordered(df_f_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "NA"))

#Zusammenführen der Datensätze
df_mnm_mi <- rbind(df_m_mi, df_f_mi)
df_mnm_mi$variable <- dplyr::recode(df_mnm_mi$variable, "pct_f"="Frau (n=334)", "pct_m"="Mann (n=418)")

ggplot(df_mnm_mi,aes(x = milk,y = value, width=0.7)) +
  geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
  labs(x = '',y='Prozent')+
  scale_fill_manual("",values = c("Frau (n=334)"="#fad60d", "Mann (n=418)"="#505050"))+
  mytheme+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
            size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
  ggtitle("Ich esse Milch/Milchprodukte, Käse:")
  ggsave(("04_plots/Frage_9_Milch_gender_190528_vori.pdf"),
         width = 17,
         height = 8,
         dpi=600,
         units="cm",
         device="pdf")


  
# Verpfelgungstyp ----
  
df_mann_typ <- df_mann_fill %>% count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagaenger")
df_frau_typ <- df_frau_fill %>% count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagaenger")

# Prozent

df_mann_typ$pct_m <- df_mann_typ$n/sum(df_mann_typ$n)

df_frau_typ$pct_f <- df_frau_typ$n/sum(df_frau_typ$n)  

# Umwandeln für Plot
df_m_ver <- melt(df_mann_typ[,c("Verpflegungstyp", "pct_m")], id.vars = 1)

df_f_ver <- melt(df_frau_typ[,c("Verpflegungstyp", "pct_f")], id.vars = 1)

#Zusammenführen der Datensätze
df_mnm_ver <- rbind(df_m_ver, df_f_ver)
df_mnm_ver$variable <- dplyr::recode(df_mnm_ver$variable, "pct_f"="Frau (n=324)", "pct_m"="Mann (n=410)")

ggplot(df_mnm_ver,aes(x = Verpflegungstyp,y = value, width=0.7)) +
  geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
  labs(x = '',y='Prozent')+
  scale_fill_manual("",values = c("Frau (n=324)"="#fad60d", "Mann (n=410)"="#505050"))+
  mytheme+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
            size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
  ggtitle("Verpflegungstyp")

ggsave(("04_plots/Frage_11_gender_190925_vori.pdf"),
       width = 17,
       height = 8,
       dpi=600,
       units="cm",
       device="pdf")
