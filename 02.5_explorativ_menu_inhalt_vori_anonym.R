library (gmodels) #Version 2.18.1
library (formattable) #Version 0.2.0.1
library (reshape2) #Version 1.4.3
library (magrittr) #Version 1.5
library (gmodels) #Version 2.18.1
library (tidyverse) #Version 1.2.1s)
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

# Teilstichproben bilden


# Nur Personen bei denen der Menüinhalt bekannt ist. Teilstichprobe B
df_linie <- df_tot %>% 
  filter(mensa ==1) %>% 
  filter(!is.na(meal)) %>% 
  mutate(label_content = fct_recode(label_content, "vegan"="rein pflanzlich*")) %>% 
  mutate(label_content = fct_recode(label_content, "vegan"="rein pflanzlich")) %>% 
  mutate(label_content = case_when(meal == "Hot & Cold" ~ "Hot & Cold", TRUE ~ as.character(label_content))) %>% 
  mutate(Inhalt = case_when(label_content == "Fleisch/Fisch" ~ "Fleisch/Fisch (n = 350)", 
                            label_content == "vegetarisch" ~ "vegetarisch (n = 197)",
                            label_content == "vegan" ~ "vegan (n = 104)",
                            label_content == "Hot & Cold" ~ "Hot & Cold (n = 148)",
                            TRUE ~ as.character(label_content))) 
  


# Personen die ein Fleisch Gericht konsumiert haben. 
df_meat <- df_linie %>% 
  filter(label_content=="Fleisch/Fisch")

df_meat <- df_meat %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_meat <- df_meat %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_meat$gender, df_meat$member)


# summary(df_meat$age)
# sd(df_meat$age, na.rm = T)


# Personen die ein vegetarisches konsumiert haben. 
df_veget <- df_linie %>% 
  filter(label_content=="vegetarisch")

df_veget <- df_veget %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_veget <- df_veget %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_veget$gender, df_veget$member)


# summary(df_veget$age)
# sd(df_veget$age, na.rm = T)


# Personen die ein veganes Gericht konsumiert haben.
df_veg <- df_linie %>% 
  filter(label_content=="vegan")

df_veg <- df_veg %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_veg <- df_veg %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_veg$gender, df_veg$member)

# summary(df_veg$age)
# sd(df_veg$age, na.rm = T)

# Personen die ein Hot & Cold Teller genommen haben
df_HC <- df_linie %>% 
  filter(label_content=="Hot & Cold")

df_HC <- df_HC %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_HC <- df_HC %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_HC$gender, df_HC$member)

# summary(df_HC$age)
# sd(df_HC$age, na.rm = T)

# Frage 2: Direkter Gruppenvergleich ----

# Datensätze erstellen

df_linie_choice1 <- df_linie%>% 
  select(c(choice_1, meal, Inhalt)) %>% 
  mutate(Wahl=choice_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_1")) %>% 
  mutate(question = "... es gluschtig (schmackhaft) aussah.")

df_linie_choice2 <- df_linie%>% 
  select(c(choice_2, meal, Inhalt)) %>% 
  
  mutate(Wahl=choice_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_2")) %>% 
  mutate(question = "... die Menü-Beschreibung mich angesprochen hat.")

df_linie_choice3 <- df_linie%>% 
  select(c(choice_3, meal, Inhalt)) %>% 
  
  mutate(Wahl=choice_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_3")) %>% 
  mutate(question = "... ich es kenne.")

df_linie_choice4 <- df_linie%>% 
  select(c(choice_4, meal, Inhalt)) %>% 
  
  mutate(Wahl=choice_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_4")) %>% 
  mutate(question = "... ich grad Lust darauf hatte.")

df_linie_choice5 <- df_linie%>% 
  select(c(choice_5, meal, Inhalt)) %>% 
  
  mutate(Wahl=choice_5) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_5")) %>% 
  mutate(question = "... es eines meiner Lieblingsmenüs ist.")

df_linie_choice6 <- df_linie%>% 
  select(c(choice_6, meal, Inhalt)) %>% 
  
  mutate(Wahl=choice_6) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_6")) %>% 
  mutate(question = "... mir die anderen Menüs noch weniger passten.")

df_linie_choice7 <- df_linie%>% 
  select(c(choice_7, meal, Inhalt)) %>% 
  
  mutate(Wahl=choice_7) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_7")) %>% 
  mutate(question = "... ich etwas Neues ausprobieren wollte.")

df_linie_choice8 <- df_linie%>% 
  select(c(choice_8, meal, Inhalt)) %>% 
  
  mutate(Wahl=choice_8) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_8")) %>% 
  mutate(question = "... ich das nicht selber koche.")

df_linie_choice9 <- df_linie%>% 
  select(c(choice_9, meal, Inhalt)) %>% 
  
  mutate(Wahl=choice_9) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_9")) %>% 
  mutate(question = "... ich es beim Eingang zur Mensa gesehen habe (Menü-Aushang).")

df_linie_choice10 <- df_linie%>% 
  select(c(choice_10, meal, Inhalt)) %>% 
  
  mutate(Wahl=choice_10) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("choice_10")) %>% 
  mutate(question = "... das Preis-/Leistungsverhältnis stimmt.")


# Datensätze zusammenführen
df_linie_choice <- bind_rows(df_linie_choice1, df_linie_choice2, df_linie_choice3, df_linie_choice4, df_linie_choice5, df_linie_choice6, df_linie_choice7, df_linie_choice8, df_linie_choice9, df_linie_choice10) %>% # 
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

df_linie_choice <- merge(df_linie_choice, df_color, by="Wahl")


# für Plot Sortieren Augrund der Gesamtzustimmung
df_linie_choice$question <- reorder(df_linie_choice$question, -df_linie_choice$Zustimmung)


## plot ----
ggplot(data = df_linie_choice, aes(x=Inhalt, y=pct, fill=factor(Wahl, levels= c ("NA", "trifft nicht zu", "trifft eher nicht zu", "trifft eher zu", "trifft zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_linie_choice$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Hot & Cold (n = 148)", "vegan (n = 104)", "vegetarisch (n = 197)", "Fleisch/Fisch (n = 350)")) +
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Wie treffen die folgenden Aussagen auf Ihre heutige Menü-Wahl in der Mensa zu?\n(Frage 2, Menüinhalt, n = 799)\n\nIch habe dieses Menü heute gewählt, weil ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)+
  theme(panel.spacing = unit(0, "lines"))




ggsave("04_plots/Frage_2_Menuinhalt_unterteilt_191104.pdf",
       width = 14.5,
       height = 21,
       dpi=600,
       units="cm",
       device=cairo_pdf)


# Frage 3: Direkter Gruppenvergleich ----

# Datensätze erstellen

df_linie_satis1 <- df_linie%>% 
  select(c(satis_1, meal, Inhalt)) %>% 
  
  mutate(Wahl=satis_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("satis_1")) %>% 
  mutate(question = "Meine Erwartungen an das Menü haben sich erfüllt.")

df_linie_satis2 <- df_linie%>% 
  select(c(satis_2, meal, Inhalt)) %>% 
  
  mutate(Wahl=satis_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("satis_2")) %>% 
  mutate(question = "Ich fand das Menü gut.")

df_linie_satis3 <- df_linie%>% 
  select(c(satis_3, meal, Inhalt)) %>% 
  
  mutate(Wahl=satis_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("satis_3")) %>% 
  mutate(question = "Ich werde dieses Menü nicht mehr nehmen.")



# Datensätze zusammenführen
df_linie_satis <- bind_rows(df_linie_satis1, df_linie_satis2, df_linie_satis3) %>% # 
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

df_linie_satis <- merge(df_linie_satis, df_color, by="Wahl")


# für Plot Sortieren Augrund der Gesamtzustimmung
df_linie_satis$question <- reorder(df_linie_satis$question, -df_linie_satis$Zustimmung)


## plot ----
ggplot(data = df_linie_satis, aes(x=Inhalt, y=pct, fill=factor(Wahl, levels= c ("NA", "trifft nicht zu", "trifft eher nicht zu", "trifft eher zu", "trifft zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_linie_satis$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Hot & Cold (n = 148)", "vegan (n = 104)", "vegetarisch (n = 197)", "Fleisch/Fisch (n = 350)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Wie zufrieden sind Sie mit dem gewählten Menü?\n(Frage 3, Menüinhalt, n = 799)")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)

ggsave("04_plots/Frage_3_Menuinhalt_unterteilt_191104.pdf",
              width = 14.5,
              height = 8.4,
              dpi=600,
              units="cm",
              device=cairo_pdf) 

# Frage 4: Antwortverhalten alle Fragebogen ----

df_linie_ing1 <- df_linie%>% 
  select(c(ing_1, meal, Inhalt)) %>% 
  mutate(Wahl=ing_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("ing_1", "meal")) %>% 
  mutate(question = "... enthält Proteine (Fleisch, Fisch, Tofu etc.).")

df_linie_ing2 <- df_linie%>% 
  select(c(ing_2, meal, Inhalt)) %>% 
  mutate(Wahl=ing_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("ing_2", "meal")) %>% 
  mutate(question = "... enthält Fleisch.")

df_linie_ing3 <- df_linie%>% 
  select(c(ing_3, meal, Inhalt)) %>% 
  mutate(Wahl=ing_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("ing_3", "meal")) %>% 
  mutate(question = "... enthält Fisch.")

df_linie_ing4 <- df_linie%>% 
  select(c(ing_4, meal, Inhalt)) %>% 
  mutate(Wahl=ing_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("ing_4", "meal")) %>% 
  mutate(question = "... enhält Produkte aus artgerechter Tierhaltung.")



# Datensätze zusammenführen
df_linie_ing <- bind_rows(df_linie_ing1, df_linie_ing2, df_linie_ing3, df_linie_ing4) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "nicht wichtig", "2" = "eher nicht wichtig","3" = "eher wichtig", "4" = "wichtig", "-99" = "kann ich nicht beurteilen" )) %>% 
  group_by(question, Zustimmung, Inhalt, Wahl) %>%
  summarise(tot_answ = n()) %>%  
  mutate(pct = (tot_answ/sum(tot_answ))) 

# add colors
ColsPerCat = c("NA" = "grey", "kann ich nicht beurteilen"="#D3C193",  "nicht wichtig" = "#DC6413", "eher nicht wichtig" = "#Fdd200", "eher wichtig" = "#AFC410", "wichtig"="#006885")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_linie_ing <- merge(df_linie_ing, df_color, by="Wahl")

# für Plot Sortieren Augrund der Gesamtzustimmung
df_linie_ing$question <- reorder(df_linie_ing$question, -df_linie_ing$Zustimmung)




## plot ----
ggplot(data = df_linie_ing, aes(x=Inhalt, y=pct, fill=factor(Wahl, levels= c ("NA", "kann ich nicht beurteilen", "nicht wichtig", "eher nicht wichtig", "eher wichtig","wichtig")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_linie_ing$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Hot & Cold (n = 148)", "vegan (n = 104)", "vegetarisch (n = 197)", "Fleisch/Fisch (n = 350)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Wie wichtig waren Ihnen folgende Inhaltsstoffe Ihres heutigen Essens?\n(Frage 4, Menüinhalt, n = 799)\n\nMein heutiges Essen ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)

ggsave("04_plots/Frage_4_Menülinhalt_unterteilt_191203.pdf",
       width = 14.5,
       height = 9.6,
       dpi=600,
       units="cm",
       device=cairo_pdf)






# Frage 5: Direkter Gruppenvergleich ----

df_linie_att1 <- df_linie%>% 
  select(c(att_1, meal, Inhalt)) %>% 
  mutate(Wahl=att_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_1", "meal")) %>% 
  mutate(question = "... ist gesund.")

df_linie_att2 <- df_linie%>% 
  select(c(att_2, meal, Inhalt)) %>% 
  mutate(Wahl=att_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_2", "meal")) %>% 
  mutate(question = "... ist sättigend.")

df_linie_att3 <- df_linie%>% 
  select(c(att_3, meal, Inhalt)) %>% 
  
  mutate(Wahl=att_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_3", "meal")) %>% 
  mutate(question = "... ist leicht.")

df_linie_att4 <- df_linie%>% 
  select(c(att_4, meal, Inhalt)) %>% 
  
  mutate(Wahl=att_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_4", "meal")) %>% 
  mutate(question = "... ist wenig umweltbelastend.")

df_linie_att5 <- df_linie%>% 
  select(c(att_5, meal, Inhalt)) %>% 
  
  mutate(Wahl=att_5) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_5", "meal")) %>% 
  mutate(question = "... ist natürlich (keine Zusatzstoffe).")

df_linie_att6 <- df_linie%>% 
  select(c(att_6, meal, Inhalt)) %>% 
  
  mutate(Wahl=att_6) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_6", "meal")) %>% 
  mutate(question = "... ist vegetarisch (ovo-lakto).")

df_linie_att7 <- df_linie%>% 
  select(c(att_7, meal, Inhalt)) %>% 
  
  mutate(Wahl=att_7) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_7", "meal")) %>% 
  mutate(question = "... ist rein pflanzlich (vegan).")

df_linie_att8 <- df_linie%>% 
  select(c(att_8, meal, Inhalt)) %>% 
  
  mutate(Wahl=att_8) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_8", "meal")) %>% 
  mutate(question = "... ist frisch zubereitet.")

df_linie_att9 <- df_linie%>% 
  select(c(att_9, meal, Inhalt)) %>% 
  
  mutate(Wahl=att_9) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_9", "meal")) %>% 
  mutate(question = "... verhindert, dass Nahrungsmittel weggeworfen werden.")

df_linie_att10 <- df_linie%>% 
  select(c(att_10, meal, Inhalt)) %>% 
  
  mutate(Wahl=att_10) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_10", "meal")) %>% 
  mutate(question = "... stammt aus sozial verträglicher Produktion.")

df_linie_att11 <- df_linie%>% 
  select(c(att_11, meal, Inhalt)) %>% 
  
  mutate(Wahl=att_11) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_11", "meal")) %>% 
  mutate(question = "... schont mein Portemonnaie.")

# Datensätze zusammenführen
df_linie_att <- bind_rows(df_linie_att1, df_linie_att2, df_linie_att3, df_linie_att4, df_linie_att5, df_linie_att6, df_linie_att7, df_linie_att8, df_linie_att9, df_linie_att10, df_linie_att11) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "nicht wichtig", "2" = "eher nicht wichtig","3" = "eher wichtig", "4" = "wichtig", "-99" = "kann ich nicht beurteilen" )) %>% 
  group_by(question, Zustimmung, Inhalt, Wahl) %>%
  summarise(tot_answ = n()) %>%  
  mutate(pct = (tot_answ/sum(tot_answ)))




# add colors
ColsPerCat = c("NA" = "grey", "kann ich nicht beurteilen"="#D3C193",  "nicht wichtig" = "#DC6413", "eher nicht wichtig" = "#Fdd200", "eher wichtig" = "#AFC410", "wichtig"="#006885")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_linie_att <- merge(df_linie_att, df_color, by="Wahl")


# für Plot Sortieren Augrund der Gesamtzustimmung
df_linie_att$question <- reorder(df_linie_att$question, -df_linie_att$Zustimmung)



## plot ----
ggplot(data = df_linie_att, aes(x=Inhalt, y=pct, fill=factor(Wahl, levels= c ("NA", "kann ich nicht beurteilen", "nicht wichtig", "eher nicht wichtig", "eher wichtig","wichtig")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_linie_att$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Hot & Cold (n = 148)", "vegan (n = 104)", "vegetarisch (n = 197)", "Fleisch/Fisch (n = 350)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 1.8, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Wie wichtig waren Ihnen folgende Eigenschaften Ihres heutigen Essens?\n(Frage 5, Menüinhalt, n = 799)\n\nMein heutiges Essen ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") + 
  scale_y_continuous(labels = scales::percent) +
  theme(panel.spacing = unit(0, "lines"))



ggsave("04_plots/Frage_5_menu_inhalt_unterteilt_191206_11.pdf",
       width = 14.5,
       height = 21,
       dpi=600,
       units="cm",
       device=cairo_pdf)


# Frage 6: Tabellen----


CrossTable(df_linie$label_content, df_linie$diet)
CrossTable(df_linie$label_content, df_linie$allerg)
CrossTable(df_linie$label_content, df_linie$meds)
CrossTable(df_linie$label_content, df_linie$relig)



 
# Frage 7: Direkter Gruppenvergleich ----

# Datensätze erstellen

df_linie_tho1 <- df_linie%>%
  select(c(tho_1, meal, Inhalt)) %>%
  mutate(Wahl=tho_1) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tho_1", "meal")) %>%
  mutate(question = "meiner Ernährungsweise für meine Gesundheit.")

df_linie_tho2 <- df_linie%>%
  select(c(tho_2, meal, Inhalt)) %>%
  
  mutate(Wahl=tho_2) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tho_2", "meal")) %>%
  mutate(question = "... meiner Ernährungsgewohnheiten für die Umwelt.")

df_linie_tho3 <- df_linie%>%
  select(c(tho_3, meal, Inhalt)) %>%
  
  mutate(Wahl=tho_3) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tho_3", "meal")) %>%
  mutate(question = "... der Produktion der Nahrungsmittel auf meinem Teller für die Arbeitenden in der Wertschöpfungskette.")

df_linie_tho4 <- df_linie%>%
  select(c(tho_4, meal, Inhalt)) %>%
  
  mutate(Wahl=tho_4) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tho_4", "meal")) %>%
  mutate(question = "... meines Konsums von tierischen Nahrungsmitteln für die Tiere.")

df_linie_tho5 <- df_linie%>%
  select(c(tho_5, meal, Inhalt)) %>%
  
  mutate(Wahl=tho_5) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tho_5", "meal")) %>%
  mutate(question = "... meiner Ernährung für mein Portemonnaie.")


# Datensätze zusammenführen
df_linie_tho <- bind_rows(df_linie_tho1, df_linie_tho2, df_linie_tho3, df_linie_tho4, df_linie_tho5) %>% #
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

df_linie_tho <- merge(df_linie_tho, df_color, by="Wahl")


# für Plot Sortieren Augrund der Gesamtzustimmung
df_linie_tho$question <- reorder(df_linie_tho$question, -df_linie_tho$Zustimmung)


## plot ----
ggplot(data = df_linie_tho, aes(x=Inhalt, y=pct, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_linie_tho$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Hot & Cold (n = 148)", "vegan (n = 104)", "vegetarisch (n = 197)", "Fleisch/Fisch (n = 350)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zum Essen zu?\n(Frage 7, Menüinhalt, n = 799)\n\nIch mache mir allgemein Gedanken über die Folgen ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)



ggsave("04_plots/Frage_7_Menüinhalt_unterteilt_191203.pdf",
       width = 14.5,
       height = 12.5,
       dpi=600,
       units="cm",
       device=cairo_pdf)

# Frage 8: Antwortverteilung der Stichprobe  -----


# Datensätze erstellen

df_linie_tra1 <- df_linie %>%
  select(c(tra_1, meal, Inhalt)) %>%
  
  mutate(Wahl=tra_1) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tra_1", "meal")) %>%
  mutate(question = "... gesund zu leben.")

df_linie_tra2 <- df_linie %>%
  select(c(tra_2, meal, Inhalt)) %>%
  
  mutate(Wahl=tra_2) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tra_2", "meal")) %>%
  mutate(question = "... mit meinem Verhalten die Umwelt möglichst wenig zu belasten.")

df_linie_tra3 <- df_linie %>%
  select(c(tra_3, meal, Inhalt)) %>%
  
  mutate(Wahl=tra_3) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tra_3", "meal")) %>%
  mutate(question = "... dass die Welt sozial gerechter wird.")

df_linie_tra4 <- df_linie %>%
  select(c(tra_4, meal, Inhalt)) %>%
  
  mutate(Wahl=tra_4) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tra_4", "meal")) %>%
  mutate(question = "... dass die Arbeitsbedingungen für alle Menschen human sind.")

df_linie_tra5 <- df_linie %>%
  select(c(tra_5, meal, Inhalt)) %>%
  
  mutate(Wahl=tra_5) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tra_5", "meal")) %>%
  mutate(question = "... dass Tiere möglichst argerecht gehalten werden.")


# Datensätze zusammenführen
df_linie_tra <- bind_rows(df_linie_tra1, df_linie_tra2, df_linie_tra3, df_linie_tra4, df_linie_tra5) %>% #
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

df_linie_tra <- merge(df_linie_tra, df_color, by="Wahl")


# für Plot Sortieren Augrund der Gesamtzustimmung
df_linie_tra$question <- reorder(df_linie_tra$question, -df_linie_tra$Zustimmung)


## plot ----
ggplot(data = df_linie_tra, aes(x=Inhalt, y=pct,  fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_linie_tra$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Hot & Cold (n = 148)", "vegan (n = 104)", "vegetarisch (n = 197)", "Fleisch/Fisch (n = 350)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F,   fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zu?\n(Frage 8, Menüinhalt, n = 799)\n\nMir ist es allgemein wichtig, ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)



ggsave("04_plots/Frage_8_Menüinhalt_unterteilt_191203.pdf",
       width = 14.5,
       height = 12.5,
       dpi=600,
       units="cm",
       device=cairo_pdf)


# Fleischkonsumtypen ----

df_meattyp <- df_linie %>% 
  mutate(meat_diet = case_when (meat==7 ~ "Fleischliebhaber",
                                meat==6 ~ "Fleischliebhaber",
                                meat==5 ~ "Fleischesser",
                                meat==4 ~ "Fleisch-Flexitarier",
                                meat==3 ~ "Vegi-Flexitarier",
                                meat==2 ~ "Fleischvermeider",
                                meat==1 ~ "Fleischvermeider", 
                                TRUE ~ as.character(NA))) %>% 
  filter(!is.na(meat_diet)) %>%   # 794 Beobachungen
  mutate(Inhalt_2=case_when(label_content == "Fleisch/Fisch" ~ "Fleisch/Fisch (n = 350)", 
                            label_content == "vegetarisch" ~ "vegetarisch (n = 196)",
                            label_content == "vegan" ~ "vegan (n = 103)",
                            label_content == "Hot & Cold" ~ "Hot & Cold (n = 145)",
                            TRUE ~ as.character(label_content))) %>% 
  group_by(meat_diet, Inhalt_2) %>% 
  summarise(meattyp = n()) %>% 
  ungroup () %>% 
  group_by(Inhalt_2) %>%
  mutate(summe = sum(meattyp)) %>% 
  ungroup() %>% 
  mutate(per = meattyp/summe)


ColsPerCat = c("Fleischliebhaber" = "#DC6413", "Fleischesser" = "#Fdd200", "Fleisch-Flexitarier" = "#d3c193", "Vegi-Flexitarier" = "#AFC410", "Fleischvermeider"="#006885")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "meat_diet")

df_meattyp <- merge(df_meattyp, df_color, by="meat_diet")

# Plot -----
ggplot(df_meattyp, aes(y=per, x=Inhalt_2, fill= factor(meat_diet, levels = c("Fleischvermeider",  "Vegi-Flexitarier", "Fleisch-Flexitarier", "Fleischesser", "Fleischliebhaber")), color = label_color)) + 
  geom_bar(stat = "identity", position = position_stack(),  width = 0.6, color = NA) +
  scale_color_manual(values = levels(df_meattyp$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  scale_x_discrete(limits=c("Hot & Cold (n = 145)",
                            "vegan (n = 103)",
                            "vegetarisch (n = 196)",
                            "Fleisch/Fisch (n = 350)"
                            ))+
  xlab("") +
  ylab("") +
  ggtitle("Menüinhalt\n(Fleischkonsumtyp, n =  794)") +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme +
  guides(color = F, fill = guide_legend("", reverse = F, ncolum=1))+
  coord_flip()


ggsave(("04_plots/Fleischkonsumtyp_menueinhalt_201828_vori.pdf"),
       width = 14.5,
       height = 3,
       dpi=600,
       units="cm",
       device=cairo_pdf) 


# # Frage 9: Direkter Vergleich --

# df_linie_fill <- df_linie_fill  %>% 
#   mutate(meat = ifelse(is.na(meat), "keine Antwort", meat)) %>% 
#   mutate(milk = ifelse(is.na(milk), "keine Antwort", milk)) %>% 
#   mutate(veget = ifelse(is.na(veget), "keine Antwort", veget)) %>% 
#   mutate(veg = ifelse(is.na(veg), "keine Antwort", veg)) %>% 
#   mutate(meat = recode_factor(meat, "7"= "2x und mehr pro Tag", "6" = "1x pro Tag", "5" = "5-6x pro Woche", "4" = "3-4x pro Woche", "3" = "1-2x pro Woche", "2" = "1-2x pro Monat", "1" = "nie")) %>% 
#   mutate(milk = recode_factor(milk, "7"= "3x und mehr pro Tag", "6" = "1-2x pro Tag", "5" = "5-6x pro Woche", "4" = "3-4x pro Woche", "3" = "1-2x pro Woche", "2" = "1-2x pro Monat", "1" = "nie")) %>% 
#   mutate(veget = recode_factor(veget, "6"= "immer", "5" = "meistens", "4" = "oft", "3" = "manchmal", "2" = "selten", "1" = "nie")) %>% 
#   mutate(veg = recode_factor(veg, "6"= "immer", "5" = "meistens", "4" = "oft", "3" = "manchmal", "2" = "selten", "1" = "nie")) 
# 
# 
# # Fleisch ----
# df_meat_fill_meat <- df_linie_fill %>% 
#   filter(label_content=="Fleisch/Fisch") %>% 
#   count(meat)
# 
# df_veget_fill_meat <- df_linie_fill %>% 
#   filter(label_content=="vegetarisch") %>% 
#   count(meat)
# 
# df_veg_fill_meat <- df_linie_fill %>% 
#   filter(label_content=="vegan") %>% 
#   count(meat)
# 
# df_HC_fill_meat <- df_linie_fill %>% 
#   filter(label_content=="Hot & Cold") %>% 
#   count(meat)
# 
# 
# # Prozent
# df_meat_fill_meat$pct_meat<-df_meat_fill_meat$n/sum(df_meat_fill_meat$n)
# 
# df_veget_fill_meat$pct_veget<-df_veget_fill_meat$n/sum(df_veget_fill_meat$n)
# 
# df_veg_fill_meat$pct_veg<-df_veg_fill_meat$n/sum(df_veg_fill_meat$n)
# 
# df_HC_fill_meat$pct_HC<-df_HC_fill_meat$n/sum(df_HC_fill_meat$n)
# 
# 
# 
# # Umwandeln der Datensätze für den Plot
# df_meat_m <- melt(df_meat_fill_meat[,c("meat", "pct_meat")], id.vars = 1)
# df_meat_m$meat <- ordered(df_meat_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
# 
# df_veget_m <- melt(df_veget_fill_meat[,c("meat", "pct_veget")], id.vars = 1)
# df_veget_m$meat <- ordered(df_veget_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
# 
# df_veg_m <- melt(df_veg_fill_meat[,c("meat", "pct_veg")], id.vars = 1)
# df_veg_m$meat <- ordered(df_veg_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
# 
# df_HC_m <- melt(df_HC_fill_meat[,c("meat", "pct_HC")], id.vars = 1)
# df_HC_m$meat <- ordered(df_HC_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
# 
# #Zusammenführen der Datensätze
# df_T_m <- rbind(df_meat_m, df_veget_m, df_veg_m, df_HC_m)
# df_T_m$variable <- dplyr::recode(df_T_m$variable, "pct_meat"="Fleisch/Fisch (n=232)", "pct_veget"="vegetarisch (n=131)", "pct_veg"="vegan (n=51)", "pct_HC"="Hot & Cold (n=102)")
# 
# ggplot(df_T_m,aes(x = meat,y = value, width=0.7)) +
#   geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
#   labs(x = '',y='Prozent')+
#   scale_fill_manual("",values = c("Fleisch/Fisch (n=232)"="#fad60d", "vegetarisch (n=131)"="grey", "vegan (n=51)"="#505050" , "Hot & Cold (n=102)"="#c5b87c"))+
#   mytheme+ 
#   theme(legend.position = "bottom")+ 
#   scale_y_continuous(labels = scales::percent)+
#   geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
#             size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
#   ggtitle("Ich esse Fleisch (alle Sorten inkl. Geflügel, Wurstwaren etc.):") 
# 
#   ggsave(("04_plots/Frage_9_Fleisch_Menuinhalt_veg2_190703_vori.pdf"),
#          width = 17,
#          height = 8,
#          dpi=600,
#          units="cm",
#          device="pdf")
# 
# 
# # Milch ----
# 
# df_meat_fill_milk <- df_linie_fill %>% 
#   filter(label_content=="Fleisch/Fisch") %>% 
#   count(milk)
# 
# df_veget_fill_milk <- df_linie_fill %>% 
#   filter(label_content=="vegetarisch") %>% 
#   count(milk)  
#   
# df_veg_fill_milk <- df_linie_fill %>% 
#   filter(label_content=="vegan") %>% 
#   count(milk)
# 
# df_HC_fill_milk <- df_linie_fill %>% 
#   filter(label_content=="Hot & Cold") %>% 
#   count(milk)
# 
# 
# # Prozent
# df_meat_fill_milk$pct_meat<-df_meat_fill_milk$n/sum(df_meat_fill_milk$n)
# 
# df_veget_fill_milk$pct_veget<-df_veget_fill_milk$n/sum(df_veget_fill_milk$n)
# 
# df_veg_fill_milk$pct_veg<-df_veg_fill_milk$n/sum(df_veg_fill_milk$n)
# 
# df_HC_fill_milk$pct_HC<-df_HC_fill_milk$n/sum(df_HC_fill_milk$n)
# 
# 
# 
# # Umwandeln der Datensätze für den Plot
# df_meat_mi<- melt(df_meat_fill_milk[,c("milk", "pct_meat")], id.vars = 1)
# df_meat_mi$milk <- ordered(df_meat_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
# 
# df_veget_mi<- melt(df_veget_fill_milk[,c("milk", "pct_veget")], id.vars = 1)
# df_veget_mi$milk <- ordered(df_veget_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
# 
# 
# df_veg_mi<- melt(df_veg_fill_milk[,c("milk", "pct_veg")], id.vars = 1)
# df_veg_mi$milk <- ordered(df_veg_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
# 
# df_HC_mi<- melt(df_HC_fill_milk[,c("milk", "pct_HC")], id.vars = 1)
# df_HC_mi$milk <- ordered(df_HC_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
# 
# #Zusammenführen der Datensätze
# df_T_mi <- rbind(df_meat_mi, df_veget_mi, df_veg_mi, df_HC_mi)
# df_T_mi$variable <- dplyr::recode(df_T_mi$variable, "pct_meat"="Fleisch/Fisch (n=232)", "pct_veget"="vegetarisch (n=131)", "pct_veg"="vegan (n=51)", "pct_HC"="Hot & Cold (n=102)")
# 
# 
# 
# ggplot(df_T_mi,aes(x = milk,y = value, width=0.7)) +
#   geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
#   labs(x = '',y='Prozent')+
#   scale_fill_manual("",values = c("Fleisch/Fisch (n=232)"="#fad60d", "vegetarisch (n=131)"="grey", "vegan (n=51)"="#505050" , "Hot & Cold (n=102)"="#c5b87c"))+
#   mytheme+ 
#   theme(legend.position = "bottom")+ 
#   scale_y_continuous(labels = scales::percent)+
#   geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
#             size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
#   ggtitle("Ich trinke/esse Milch/Milchprodukte, Käse:")
# 
#   ggsave(("04_plots/Frage_9_Milch_Menuinhalt_veg2_190703_vori.pdf"),
#          width = 17,
#          height = 8,
#          dpi=600,
#          units="cm",
#          device="pdf")
# 
# 
# 
# 
# 
# # veget ----
# df_meat_fill_veget <- df_linie_fill %>% 
#   filter(label_content=="Fleisch/Fisch") %>% 
#   count(veget)
#   
# df_veget_fill_veget <- df_linie_fill %>% 
#   filter(label_content=="vegetarisch") %>% 
#   count(veget)
#   
# df_veg_fill_veget <- df_linie_fill %>% 
#   filter(label_content=="vegan") %>% 
#   count(veget)
# 
# df_HC_fill_veget <- df_linie_fill %>% 
#   filter(label_content=="Hot & Cold") %>% 
#   count(veget)
# 
# 
# # Prozent
# df_meat_fill_veget$pct_meat<-df_meat_fill_veget$n/sum(df_meat_fill_veget$n)
# 
# df_veget_fill_veget$pct_veget<-df_veget_fill_veget$n/sum(df_veget_fill_veget$n)
# 
# df_veg_fill_veget$pct_veg<-df_veg_fill_veget$n/sum(df_veg_fill_veget$n)
# 
# df_HC_fill_veget$pct_HC<-df_HC_fill_veget$n/sum(df_HC_fill_veget$n)
# 
# 
# # Umwandeln der Datensätze für den Plot
# df_meat_veget<- melt(df_meat_fill_veget[,c("veget", "pct_meat")], id.vars = 1)
# df_meat_veget$veget <- ordered(df_meat_veget$veget, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))
# 
# df_veget_veget<- melt(df_veget_fill_veget[,c("veget", "pct_veget")], id.vars = 1)
# df_veget_veget$veget <- ordered(df_veget_veget$veget, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))
# 
# df_veg_veget<- melt(df_veg_fill_veget[,c("veget", "pct_veg")], id.vars = 1)
# df_veg_veget$veget <- ordered(df_veg_veget$veget, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))
# 
# df_HC_veget<- melt(df_HC_fill_veget[,c("veget", "pct_HC")], id.vars = 1)
# df_HC_veget$veget <- ordered(df_HC_veget$veget, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))
# 
# #Zusammenführen der Datensätze
# df_T_veget <- rbind(df_meat_veget, df_veget_veget, df_veg_veget, df_HC_veget)
# df_T_veget$variable <- dplyr::recode(df_T_veget$variable, "pct_meat"="Fleisch/Fisch (n=232)", "pct_veget"="vegetarisch (n=131)", "pct_veg"="vegan (n=51)", "pct_HC"="Hot & Cold (n=102)")
# 
# 
# 
# ggplot(df_T_veget,aes(x = veget,y = value, width=0.7)) +
#   geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
#   labs(x = '',y='Prozent')+
#   scale_fill_manual("",values = c("Fleisch/Fisch (n=232)"="#fad60d", "vegetarisch (n=131)"="grey", "vegan (n=51)"="#505050" , "Hot & Cold (n=102)"="#c5b87c"))+
#   mytheme+ 
#   theme(legend.position = "bottom")+ 
#   scale_y_continuous(labels = scales::percent)+
#   geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
#             size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
#   ggtitle("Ich ernähre mich vegetarisch (ovo-lakto):")
# 
#   ggsave(("04_plots/Frage_9_veget_Menuinhalt_veg2_190703_vori.pdf"),
#          width = 17,
#          height = 8,
#          dpi=600,
#          units="cm",
#          device="pdf")
# 
# 
# 
# 
# 
# # veg ----
# df_meat_fill_veg <- df_linie_fill %>% 
#   filter(label_content=="Fleisch/Fisch") %>% 
#   count(veg)
#   
# df_veget_fill_veg <- df_linie_fill %>% 
#   filter(label_content=="vegetarisch") %>% 
#   count(veg)
#   
# df_veg_fill_veg <- df_linie_fill %>% 
#   filter(label_content=="vegan") %>% 
#   count(veg)
# 
# df_HC_fill_veg <- df_linie_fill %>% 
#   filter(label_content=="Hot & Cold") %>% 
#   count(veg)
# 
# 
# # Prozent
# df_meat_fill_veg$pct_meat<-df_meat_fill_veg$n/sum(df_meat_fill_veg$n)
# 
# df_veget_fill_veg$pct_veget<-df_veget_fill_veg$n/sum(df_veget_fill_veg$n)
# 
# df_veg_fill_veg$pct_veg<-df_veg_fill_veg$n/sum(df_veg_fill_veg$n)
# 
# df_HC_fill_veg$pct_HC<-df_HC_fill_veg$n/sum(df_HC_fill_veg$n)
# 
# 
# # Umwandeln der Datensätze für den Plot
# df_meat_veg<- melt(df_meat_fill_veg[,c("veg", "pct_meat")], id.vars = 1)
# df_meat_veg$veg <- ordered(df_meat_veg$veg, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))
# 
# df_veget_veg<- melt(df_veget_fill_veg[,c("veg", "pct_veget")], id.vars = 1)
# df_veget_veg$veg <- ordered(df_veget_veg$veg, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))
# 
# df_veg_veg<- melt(df_veg_fill_veg[,c("veg", "pct_veg")], id.vars = 1)
# df_veg_veg$veg <- ordered(df_veg_veg$veg, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))
# 
# df_HC_veg<- melt(df_HC_fill_veg[,c("veg", "pct_HC")], id.vars = 1)
# df_HC_veg$veg <- ordered(df_HC_veg$veg, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))
# 
# #Zusammenführen der Datensätze
# df_T_veg <- rbind(df_meat_veg, df_veget_veg, df_veg_veg, df_HC_veg)
# df_T_veg$variable <- dplyr::recode(df_T_veg$variable, "pct_meat"="Fleisch/Fisch (n=232)", "pct_veget"="vegetarisch (n=131)", "pct_veg"="vegan (n=51)", "pct_HC"="Hot & Cold (n=102)")
# 
# 
# 
# ggplot(df_T_veg,aes(x = veg,y = value, width=0.7)) +
#   geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
#   labs(x = '',y='Prozent')+
#   scale_fill_manual("",values = c("Fleisch/Fisch (n=232)"="#fad60d", "vegetarisch (n=131)"="grey", "vegan (n=51)"="#505050" , "Hot & Cold (n=102)"="#c5b87c"))+
#   mytheme+ 
#   theme(legend.position = "bottom")+ 
#   scale_y_continuous(labels = scales::percent)+
#   geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
#             size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
#   ggtitle("Ich ernähre mich rein pflanzlich (vegan):")
# 
#   ggsave(("04_plots/Frage_9_veg_Menüinhalt_veg2_190703_vori.pdf"),
#          width = 17,
#          height = 8,
#          dpi=600,
#          units="cm",
#          device="pdf")
#   
# # Das gleiche aber nur für Personen die einen Fleischkonsum von 5-6x pro Woche und 1-2x pro Woche Fleisch Essen ----
#   
#   
# # Daten filtern ----  
# df_linie_flex <- df_linie %>% filter(meat>=3 & meat<=5)
#   
#     
#   df_inhalt_choice1 <- df_linie_flex%>% 
#     select(c(choice_1, label_content)) %>% 
#     mutate(Inhalt=label_content) %>% 
#     mutate(Wahl=choice_1) %>% 
#     mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#     select(-c("choice_1")) %>% 
#     mutate(question = "es gluschtig (schmackhaft) aussah.")
#   
#   df_inhalt_choice2 <- df_linie_flex%>% 
#     select(c(choice_2, label_content)) %>% 
#     mutate(Inhalt=label_content) %>% 
#     mutate(Wahl=choice_2) %>% 
#     mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#     mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#     select(-c("choice_2")) %>% 
#     mutate(question = "die Menü-Beschreibung mich angesprochen hat.")
#   
#   df_inhalt_choice3 <- df_linie_flex%>% 
#     select(c(choice_3, label_content)) %>% 
#     mutate(Inhalt=label_content) %>% 
#     mutate(Wahl=choice_3) %>% 
#     mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#     select(-c("choice_3")) %>% 
#     mutate(question = "ich es kenne.")
#   
#   df_inhalt_choice4 <- df_linie_flex%>% 
#     select(c(choice_4, label_content)) %>% 
#     mutate(Inhalt=label_content) %>% 
#     mutate(Wahl=choice_4) %>% 
#     mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#     select(-c("choice_4")) %>% 
#     mutate(question = "ich grad Lust darauf hatte.")
#   
#   df_inhalt_choice5 <- df_linie_flex%>% 
#     select(c(choice_5, label_content)) %>% 
#     mutate(Inhalt=label_content) %>% 
#     mutate(Wahl=choice_5) %>% 
#     mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#     select(-c("choice_5")) %>% 
#     mutate(question = "es eines meiner Lieblingsmenüs ist.")
#   
#   df_inhalt_choice6 <- df_linie_flex%>% 
#     select(c(choice_6, label_content)) %>% 
#     mutate(Inhalt=label_content) %>% 
#     mutate(Wahl=choice_6) %>% 
#     mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#     select(-c("choice_6")) %>% 
#     mutate(question = "mir die anderen Menüs noch weniger passten.")
#   
#   df_inhalt_choice7 <- df_linie_flex%>% 
#     select(c(choice_7, label_content)) %>% 
#     mutate(Inhalt=label_content) %>% 
#     mutate(Wahl=choice_7) %>% 
#     mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#     select(-c("choice_7")) %>% 
#     mutate(question = "ich etwas Neues ausprobieren wollte.")
#   
#   df_inhalt_choice8 <- df_linie_flex%>% 
#     select(c(choice_8, label_content)) %>% 
#     mutate(Inhalt=label_content) %>% 
#     mutate(Wahl=choice_8) %>% 
#     mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#     select(-c("choice_8")) %>% 
#     mutate(question = "ich das nicht selber koche.")
#   
#   df_inhalt_choice9 <- df_linie_flex%>% 
#     select(c(choice_9, label_content)) %>% 
#     mutate(Inhalt=label_content) %>% 
#     mutate(Wahl=choice_9) %>% 
#     mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#     select(-c("choice_9")) %>% 
#     mutate(question = "ich es beim Eingang zur meal gesehen habe (Menü-Aushang).")
#   
#   df_inhalt_choice10 <- df_linie_flex%>% 
#     select(c(choice_10, label_content)) %>% 
#     mutate(Inhalt=label_content) %>% 
#     mutate(Wahl=choice_10) %>% 
#     mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#     select(-c("choice_10")) %>% 
#     mutate(question = "das Preis-/Leistungsverhältnis stimmt.")
#   
#   
#   # Datensätze zusammenführen
#   df_inhalt_choice <- bind_rows(df_inhalt_choice1, df_inhalt_choice2, df_inhalt_choice3, df_inhalt_choice4, df_inhalt_choice5, df_inhalt_choice6, df_inhalt_choice7, df_inhalt_choice8, df_inhalt_choice9, df_inhalt_choice10) %>% # 
#     mutate(Wahl = recode(.$Wahl, .missing = "keine Antwort", "1" = "trifft nicht zu", "2" = "trifft eher nicht zu","3" = "trifft eher zu", "4" = "trifft zu" )) %>% 
#     group_by(question, Zustimmung, Inhalt, Wahl) %>%
#     summarise(tot_answ = n()) %>%  
#     mutate(pct = (tot_answ/sum(tot_answ))) %>% 
#     mutate(xlab_ = paste("(", Inhalt, ")", sep = ""),
#            xlab = paste(Inhalt, question , sep = ": ")) #add empty coloms for spacing in the middle
#   
#   df_inhalt_choice[nrow(df_inhalt_choice) + 1,] <-  list("space1", NA, NA, NA, NA, NA, NA)
#   df_inhalt_choice[nrow(df_inhalt_choice) + 1,] <-  list("space2", NA, NA, NA, NA, NA, NA)
#   df_inhalt_choice[nrow(df_inhalt_choice) + 1,] <-  list("space3", NA, NA, NA, NA, NA, NA)
#   df_inhalt_choice[nrow(df_inhalt_choice) + 1,] <-  list("space4", NA, NA, NA, NA, NA, NA)
#   df_inhalt_choice[nrow(df_inhalt_choice) + 1,] <-  list("space5", NA, NA, NA, NA, NA, NA)
#   df_inhalt_choice[nrow(df_inhalt_choice) + 1,] <-  list("space6", NA, NA, NA, NA, NA, NA)
#   df_inhalt_choice[nrow(df_inhalt_choice) + 1,] <-  list("space7", NA, NA, NA, NA, NA, NA)
#   df_inhalt_choice[nrow(df_inhalt_choice) + 1,] <-  list("space8", NA, NA, NA, NA, NA, NA)
#   df_inhalt_choice[nrow(df_inhalt_choice) + 1,] <-  list("space9", NA, NA, NA, NA, NA, NA)
#   
#   # Sortieren Augrund der Gesamtzustimmung
#   df_inhalt_choice <- df_inhalt_choice[order(df_inhalt_choice$Zustimmung),]
#   
#   
#   # add colors
#   ColsPerCat = c("keine Antwort" = "grey", "trifft nicht zu" = "#FF0000", "trifft eher nicht zu" = "#FFA500", "trifft eher zu" = "#99f200", "trifft zu"="#006400")
#   
#   source("09_function_is_dark_190114_egel.R", chdir = T)
#   
#   # df_inhalt_choice$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_inhalt_choice$Wahl], # takes every label and their belonged color
#   #                                              function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"
#   
#   label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"
#   
#   label_color <- as.data.frame(label_color)
#   
#   df_color <- tibble::rownames_to_column(label_color, var = "Wahl")
#   
#   df_inhalt_choice <- merge(df_inhalt_choice, df_color, by="Wahl")
#   
#   # Sortieren Augrund der Gesamtzustimmung
#   df_inhalt_choice <- df_inhalt_choice[order(df_inhalt_choice$Zustimmung, df_inhalt_choice$Wahl, df_inhalt_choice$Inhalt),]
#   
#   
#   
#   ggplot(df_inhalt_choice, aes(x=xlab, y=pct, fill=factor(Wahl, levels= c ("keine Antwort", "trifft nicht zu", "trifft eher nicht zu", "trifft eher zu", "trifft zu")), color = label_color)) +
#     geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.8) +
#     scale_y_continuous(labels = scales::percent)+
#     scale_color_manual(values = levels(df_inhalt_choice$label_color)) +
#     scale_fill_manual(breaks = attributes(ColsPerCat)$name,
#                       values = ColsPerCat)+
#     scale_x_discrete(limits=c((df_inhalt_choice$xlab)[c(10, 11, 12, 13)],
#                               "", 
#                               (df_inhalt_choice$xlab)[c(30, 31, 32, 33)],
#                               "",
#                               (df_inhalt_choice$xlab)[c(50, 51, 52, 53)],
#                               "", 
#                               (df_inhalt_choice$xlab)[c(70, 71, 72, 73)],
#                               "",
#                               (df_inhalt_choice$xlab)[c(90, 91, 92, 93)],
#                               "",
#                               (df_inhalt_choice$xlab)[c(110, 111, 112, 113)],
#                               "", 
#                               (df_inhalt_choice$xlab)[c(130, 131, 132, 133)],
#                               "",
#                               (df_inhalt_choice$xlab)[c(150, 151, 152, 153)],
#                               "",
#                               (df_inhalt_choice$xlab)[c(170, 171, 172, 173)], 
#                               "",
#                               (df_inhalt_choice$xlab)[c(190, 191, 192, 193)]))+
#     #                     guide_legend("")) +
#     geom_text(aes(label=(ifelse(pct<0.02, "",paste( round(pct*100), "%", sep = " ")))), size = 5, position = position_stack(vjust = 0.5))+
#     ylab("Antworten in Prozent")+
#     xlab("")+
#     guides(color = F, fill = guide_legend("", reverse = T)) +
#     ggtitle("Frage 2: Wie treffen die folgenden Aussagen auf Ihre heutige Menü Wahl in der meal zu?\n(Fleisch/Fisch (n=350), vegetarisch (n=197), vegan (n=104), Hot & Cold (n=148))")+
#     xlab("Ich habe dieses Menü heute gewählt, weil ...")+
#     coord_flip() +
#     mytheme +
#     
#     theme(legend.position = "bottom")
#   
#   ggsave("04_plots/Frage_2_Menuinhalt_veg2_190702.pdf",
#          width = 27,
#          height = 13,
#          dpi=600,
#          units="cm",
#          device=cairo_pdf)
