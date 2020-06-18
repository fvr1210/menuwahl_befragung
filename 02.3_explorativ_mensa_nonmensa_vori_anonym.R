library (magrittr) #Version 1.5
library (gmodels) #Version 2.18.1
library (tidyverse) #Version 1.2.1
library (extrafont) # Version 0.17
loadfonts(device = "win")

# Themes laden
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

df_tot <- read_delim("2019_ZHAW_vonRickenbach_cleaned_recorded_survey_dataset_NOVANIMAL_anonym.csv", delim=';', locale = locale(encoding = 'ISO-8859-2')) %>% 
  mutate(Inhalt=ifelse(mensa==1, "Mensamenü (n = 875)", "selber mitgebracht (n = 301)"))

# Teilstichproben bilden

# Nur Personen die ein Mensamenü konsumiert haben
df_mensa <- df_tot %>% 
  filter(mensa==1)


df_mensa <- df_mensa %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_mensa <- df_mensa %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_mensa$gender, df_mensa$member)


# summary(df_mensa$age)
# sd(df_mensa$age, na.rm = T)



# Nur Personen die ein Selbermitgebrachtes Menü konsumiert haben
df_selber <- df_tot %>% 
  filter(mensa==0)


df_selber <- df_selber %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_selber <- df_selber %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_selber$gender, df_selber$member)


# summary(df_selber$age)
# sd(df_selber$age, na.rm = T)


# Frage 4: Antwortverhalten alle Fragebogen ----

df_where_ing1 <- df_tot%>% 
  select(c(ing_1, mensa, Inhalt)) %>% 
  mutate(Wahl=ing_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("ing_1", "mensa")) %>% 
  mutate(question = "... enthält Proteine (Fleisch, Fisch. Tofu etc)")

df_where_ing2 <- df_tot%>% 
  select(c(ing_2, mensa, Inhalt)) %>% 
  mutate(Wahl=ing_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("ing_2", "mensa")) %>% 
  mutate(question = "... enthält Fleisch")

df_where_ing3 <- df_tot%>% 
  select(c(ing_3, mensa, Inhalt)) %>% 
  mutate(Wahl=ing_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("ing_3", "mensa")) %>% 
  mutate(question = "... enthält Fisch")

df_where_ing4 <- df_tot%>% 
  select(c(ing_4, mensa, Inhalt)) %>% 
  mutate(Wahl=ing_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("ing_4", "mensa")) %>% 
  mutate(question = "... enhält Produkte aus artgerechter Tierhaltung")



# Datensätze zusammenführen
df_where_ing <- bind_rows(df_where_ing1, df_where_ing2, df_where_ing3, df_where_ing4) %>% # 
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

df_where_ing <- merge(df_where_ing, df_color, by="Wahl")

# für Plot Sortieren Augrund der Gesamtzustimmung
df_where_ing$question <- reorder(df_where_ing$question, -df_where_ing$Zustimmung)

## plot ----
ggplot(data = df_where_ing, aes(x=Inhalt, y=pct, fill=factor(Wahl, levels= c ("NA", "kann ich nicht beurteilen", "nicht wichtig", "eher nicht wichtig", "eher wichtig","wichtig")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_where_ing$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("selber mitgebracht (n = 301)", "Mensamenü (n = 875)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 5, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Wie wichtig waren Ihnen folgende Inhaltsstoffe Ihres heutigen Essens? (Frage 4, Art der Mittagsverpflegung, n = 1176)\n\nMein heutiges Essen ...")+
    coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)

ggsave("04_plots/Frage_4_mensa_nonmensa_unterteilt_191203.pdf",
       width = 27,
       height = 13,
       dpi=600,
       units="in",
       device=cairo_pdf)




# Frage 5: Direkter Gruppenvergleich ----

# Datensätze erstellen

df_where_att1 <- df_tot%>% 
  select(c(att_1, mensa, Inhalt)) %>% 
  mutate(Wahl=att_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_1", "mensa")) %>% 
  mutate(question = "... ist gesund.")

df_where_att2 <- df_tot%>% 
  select(c(att_2, mensa, Inhalt)) %>% 
  mutate(Wahl=att_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_2", "mensa")) %>% 
  mutate(question = "... ist sättigend.")

df_where_att3 <- df_tot%>% 
  select(c(att_3, mensa, Inhalt)) %>% 
  
  mutate(Wahl=att_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_3", "mensa")) %>% 
  mutate(question = "... ist leicht.")

df_where_att4 <- df_tot%>% 
  select(c(att_4, mensa, Inhalt)) %>% 
  
  mutate(Wahl=att_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_4", "mensa")) %>% 
  mutate(question = "... ist wenig umweltbelastend.")

df_where_att5 <- df_tot%>% 
  select(c(att_5, mensa, Inhalt)) %>% 
  
  mutate(Wahl=att_5) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_5", "mensa")) %>% 
  mutate(question = "... ist natürlich (keine Zusatzstoffe).")

df_where_att6 <- df_tot%>% 
  select(c(att_6, mensa, Inhalt)) %>% 
  
  mutate(Wahl=att_6) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_6", "mensa")) %>% 
  mutate(question = "... ist vegetarisch (ovo-lakto).")

df_where_att7 <- df_tot%>% 
  select(c(att_7, mensa, Inhalt)) %>% 
  
  mutate(Wahl=att_7) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_7", "mensa")) %>% 
  mutate(question = "... ist rein pflanzlich (vegan).")

df_where_att8 <- df_tot%>% 
  select(c(att_8, mensa, Inhalt)) %>% 
  
  mutate(Wahl=att_8) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_8", "mensa")) %>% 
  mutate(question = "... ist frisch zubereitet.")

df_where_att9 <- df_tot%>% 
  select(c(att_9, mensa, Inhalt)) %>% 
  
  mutate(Wahl=att_9) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_9", "mensa")) %>% 
  mutate(question = "... verhindert, dass Nahrungsmittel weggeworfen werden.")

df_where_att10 <- df_tot%>% 
  select(c(att_10, mensa, Inhalt)) %>% 
  
  mutate(Wahl=att_10) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_10", "mensa")) %>% 
  mutate(question = "... stammt aus sozial verträglicher Produktion.")

df_where_att11 <- df_tot%>% 
  select(c(att_11, mensa, Inhalt)) %>% 
  
  mutate(Wahl=att_11) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("att_11", "mensa")) %>% 
  mutate(question = "... schont mein Portemonnaie.")

# Datensätze zusammenführen
df_where_att <- bind_rows(df_where_att1, df_where_att2, df_where_att3, df_where_att4, df_where_att5, df_where_att6, df_where_att7, df_where_att8, df_where_att9, df_where_att10, df_where_att11) %>% # 
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

df_where_att <- merge(df_where_att, df_color, by="Wahl")


# für Plot Sortieren Augrund der Gesamtzustimmung
df_where_att$question <- reorder(df_where_att$question, -df_where_att$Zustimmung)



## plot ----
ggplot(data = df_where_att, aes(x=Inhalt, y=pct, fill=factor(Wahl, levels= c ("NA", "kann ich nicht beurteilen", "nicht wichtig", "eher nicht wichtig", "eher wichtig","wichtig")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_where_att$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("selber mitgebracht (n = 301)", "Mensamenü (n = 875)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Wie wichtig waren Ihnen folgende Eigenschaften Ihres heutigen Essens?\n(Frage 5, Art der Mittagsverpflegung, n = 1176)\n\nMein heutiges Essen ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent) 



ggsave("04_plots/Frage_5_mensa_nonmensa_unterteilt_191203.pdf",
              width = 14.5,
              height = 20,
              dpi=600,
              units="cm",
              device=cairo_pdf)
       


# Frage 6: Tabellen----

#Mensa Gericht mit Mehrfachteilnahmen


CrossTable(df_tot$mensa, df_tot$diet)
CrossTable(df_tot$mensa, df_tot$allerg)
CrossTable(df_tot$mensa, df_tot$relig)
CrossTable(df_tot$mensa, df_tot$meds)





# Frage 7: Direkter Gruppenvergleich ----

# Datensätze erstellen

df_where_tho1 <- df_tot%>%
  select(c(tho_1, mensa, Inhalt)) %>%
  mutate(Wahl=tho_1) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tho_1", "mensa")) %>%
  mutate(question = "meiner Ernährungsweise für meine Gesundheit.")

df_where_tho2 <- df_tot%>%
  select(c(tho_2, mensa, Inhalt)) %>%

  mutate(Wahl=tho_2) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tho_2", "mensa")) %>%
  mutate(question = "... meiner Ernährungsgewohnheiten für die Umwelt.")

df_where_tho3 <- df_tot%>%
  select(c(tho_3, mensa, Inhalt)) %>%

  mutate(Wahl=tho_3) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tho_3", "mensa")) %>%
  mutate(question = "... der Produktion der Nahrungsmittel auf meinem Teller für die Arbeitenden in der Wertschöpfungskette.")

df_where_tho4 <- df_tot%>%
  select(c(tho_4, mensa, Inhalt)) %>%

  mutate(Wahl=tho_4) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tho_4", "mensa")) %>%
  mutate(question = "... meines Konsums von tierischen Nahrungsmitteln für die Tiere.")

df_where_tho5 <- df_tot%>%
  select(c(tho_5, mensa, Inhalt)) %>%

  mutate(Wahl=tho_5) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tho_5", "mensa")) %>%
  mutate(question = "... meiner Ernährung für mein Portemonnaie.")


# Datensätze zusammenführen
df_where_tho <- bind_rows(df_where_tho1, df_where_tho2, df_where_tho3, df_where_tho4, df_where_tho5) %>% #
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

df_where_tho <- merge(df_where_tho, df_color, by="Wahl")


# für Plot Sortieren Augrund der Gesamtzustimmung
df_where_tho$question <- reorder(df_where_tho$question, -df_where_tho$Zustimmung)


## plot ----
ggplot(data = df_where_tho, aes(x=Inhalt, y=pct, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_where_tho$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("selber mitgebracht (n = 301)", "Mensamenü (n = 875)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zum Essen zu?\n(Frage 7, Art der Mittagsverpflegung, n = 1176)\n\nIch mache mir allgemein Gedanken über die Folgen ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)



ggsave("04_plots/Frage_7_mensa_nonmensa_unterteilt_191203.pdf",
       width = 14.5,
       height = 10.9,
       dpi=600,
       units="cm",
       device=cairo_pdf)

# Frage 8: Antwortverteilung der Stichprobe  -----

 
# Datensätze erstellen

df_where_tra1 <- df_tot %>%
  select(c(tra_1, mensa, Inhalt)) %>%

  mutate(Wahl=tra_1) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tra_1", "mensa")) %>%
  mutate(question = "... gesund zu leben.")

df_where_tra2 <- df_tot %>%
  select(c(tra_2, mensa, Inhalt)) %>%

  mutate(Wahl=tra_2) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tra_2", "mensa")) %>%
  mutate(question = "... mit meinem Verhalten die Umwelt möglichst wenig zu belasten.")

df_where_tra3 <- df_tot %>%
  select(c(tra_3, mensa, Inhalt)) %>%

  mutate(Wahl=tra_3) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tra_3", "mensa")) %>%
  mutate(question = "... dass die Welt sozial gerechter wird.")

df_where_tra4 <- df_tot %>%
  select(c(tra_4, mensa, Inhalt)) %>%

  mutate(Wahl=tra_4) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tra_4", "mensa")) %>%
  mutate(question = "... dass die Arbeitsbedingungen für alle Menschen human sind.")

df_where_tra5 <- df_tot %>%
  select(c(tra_5, mensa, Inhalt)) %>%

  mutate(Wahl=tra_5) %>%
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>%
  select(-c("tra_5", "mensa")) %>%
  mutate(question = "... dass Tiere möglichst argerecht gehalten werden.")


# Datensätze zusammenführen
df_where_tra <- bind_rows(df_where_tra1, df_where_tra2, df_where_tra3, df_where_tra4, df_where_tra5) %>% #
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

df_where_tra <- merge(df_where_tra, df_color, by="Wahl")


# für Plot Sortieren Augrund der Gesamtzustimmung
df_where_tra$question <- reorder(df_where_tra$question, -df_where_tra$Zustimmung)


## plot ----
ggplot(data = df_where_tra, aes(x=Inhalt, y=pct,  fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_where_tra$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("selber mitgebracht (n = 301)", "Mensamenü (n = 875)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F,   fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zu? (Frage 8, Art der Mittagsverpflegung, n = 1176)\n\nMir ist es allgemein wichtig, ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)



ggsave("04_plots/Frage_8_mensa_nonmensa_unterteilt_191203.pdf",
       width = 14.5,
       height = 10.9,
       dpi=600,
       units="cm",
       device=cairo_pdf)


 # Frage 9: Direkter Vergleich ----

# Fleischkonsumtypen ------

df_meattyp <- df_tot %>% 
  mutate(meat_diet = case_when (meat==7 ~ "Fleischliebhaber",
                                  meat==6 ~ "Fleischliebhaber",
                                  meat==5 ~ "Fleischesser",
                                  meat==4 ~ "Fleisch-Flexitarier",
                                  meat==3 ~ "Vegi-Flexitarier",
                                  meat==2 ~ "Fleischvermeider",
                                  meat==1 ~ "Fleischvermeider", 
                                  TRUE ~ as.character(NA))) %>% 
  filter(!is.na(meat_diet)) %>%   # 1169 Beobachungen
  mutate(Inhalt_2=ifelse(mensa==1, "Mensamenü (n = 869)", "selber mitgebracht (n = 300)")) %>% 
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
ggplot(df_meattyp, aes(y=per, x=Inhalt_2, fill= factor(meat_diet, levels = c("Fleischvermeider", "Vegi-Flexitarier", "Fleisch-Flexitarier",  "Fleischesser", "Fleischliebhaber")), color = label_color)) + 
  geom_bar(stat = "identity", position = position_stack(),  width = 0.6, color = NA) +
  scale_color_manual(values = levels(df_meattyp$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  scale_x_discrete(limits=c("selber mitgebracht (n = 300)", "Mensamenü (n = 869)"))+
  xlab("") +
  ylab("") +
  ggtitle("Art der Mittagsverpflegung\n(Fleischkonsumtyp, n =  1169)") +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme +
  guides(color = F, fill = guide_legend("", reverse = F, ncolum=1))+
  coord_flip()


ggsave(("04_plots/Fleischkonsumtyp_mensa_nonmensa_201828_vori.pdf"),
       width = 14.5,
       height = 2,
       dpi=600,
       units="cm",
       device=cairo_pdf)  

# # Datensatz bearbeiten 
# df_mensa_fill <- df_mensa_fill  %>% 
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
# df_nonmensa_fill <- df_nonmensa_fill  %>% 
#   mutate(meat = ifelse(is.na(meat), "keine Antwort", meat)) %>% 
#   mutate(milk = ifelse(is.na(milk), "keine Antwort", milk)) %>% 
#   mutate(veget = ifelse(is.na(veget), "keine Antwort", veget)) %>% 
#   mutate(veg = ifelse(is.na(veg), "keine Antwort", veg)) %>% 
#   mutate(meat = recode_factor(meat, "7"= "2x und mehr pro Tag", "6" = "1x pro Tag", "5" = "5-6x pro Woche", "4" = "3-4x pro Woche", "3" = "1-2x pro Woche", "2" = "1-2x pro Monat", "1" = "nie")) %>% 
#   mutate(milk = recode_factor(milk, "7"= "3x und mehr pro Tag", "6" = "1-2x pro Tag", "5" = "5-6x pro Woche", "4" = "3-4x pro Woche", "3" = "1-2x pro Woche", "2" = "1-2x pro Monat", "1" = "nie")) %>% 
#   mutate(veget = recode_factor(veget, "6"= "immer", "5" = "meistens", "4" = "oft", "3" = "manchmal", "2" = "selten", "1" = "nie")) %>% 
#   mutate(veg = recode_factor(veg, "6"= "immer", "5" = "meistens", "4" = "oft", "3" = "manchmal", "2" = "selten", "1" = "nie")) 
# 
# # Fleisch ----
# df_mensa_fill_meat <- df_mensa_fill %>% count(meat)
# 
# df_nonmensa_fill_meat <- df_nonmensa_fill %>% count(meat)
# 
# # Prozent
# df_mensa_fill_meat$pct_m<-df_mensa_fill_meat$n/sum(df_mensa_fill_meat$n)
# 
# df_nonmensa_fill_meat$pct_nm<-df_nonmensa_fill_meat$n/sum(df_nonmensa_fill_meat$n)
# 
# # Umwandeln der Datensätze für den Plot
# df_m_m <- melt(df_mensa_fill_meat[,c("meat", "pct_m")], id.vars = 1)
# df_m_m$meat <- ordered(df_m_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
# 
# df_nm_m <- melt(df_nonmensa_fill_meat[,c("meat", "pct_nm")], id.vars = 1)
# df_nm_m$meat <- ordered(df_nm_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
# 
# #Zusammenführen der Datensätze
# df_mnm_m <- rbind(df_m_m, df_nm_m)
# df_mnm_m$variable <- dplyr::recode(df_mnm_m$variable, "pct_m"="Mensa Gericht (n=574)", "pct_nm"="selber mitgebracht (n=195)")
# 
# ggplot(df_mnm_m,aes(x = meat,y = value, width=0.7)) +
#   geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
#   labs(x = '',y='Prozent')+
#   scale_fill_manual("",values = c("Mensa Gericht (n=574)"="#fad60d", "selber mitgebracht (n=195)"="#505050"))+
#   mytheme+ 
#   theme(legend.position = "bottom")+ 
#   scale_y_continuous(labels = scales::percent)+
#   geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
#             size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
#   ggtitle("Ich esse Fleisch (alle Sorten inkl. Geflügel, Wurstwaren etc.):")+
#   ggsave(("04_plots/Frage_9_Fleisch_mensa_nonmensa_190507_vori.pdf"),
#          width = 17,
#          height = 8,
#          dpi=600,
#          units="in",
#          device="pdf")
# 
# # Milch ----
# df_mensa_fill_milk <- df_mensa_fill %>% count(milk)
# 
# df_nonmensa_fill_milk <- df_nonmensa_fill %>% count(milk)
# 
# # Prozent
# df_mensa_fill_milk$pct_m<-df_mensa_fill_milk$n/sum(df_mensa_fill_milk$n)
# 
# df_nonmensa_fill_milk$pct_nm<-df_nonmensa_fill_milk$n/sum(df_nonmensa_fill_milk$n)
# 
# # Umwandeln der Datensätze für den Plot
# df_m_mi <- melt(df_mensa_fill_milk[,c("milk", "pct_m")], id.vars = 1)
# df_m_mi$milk <- ordered(df_m_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
# 
# df_nm_mi <- melt(df_nonmensa_fill_milk[,c("milk", "pct_nm")], id.vars = 1)
# df_nm_mi$milk <- ordered(df_nm_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
# 
# #Zusammenführen der Datensätze
# df_mnm_mi <- rbind(df_m_mi, df_nm_mi)
# df_mnm_mi$variable <- dplyr::recode(df_mnm_mi$variable, "pct_m"="Mensa Gericht (n=574)", "pct_nm"="selber mitgebracht (n=195)")
# 
# ggplot(df_mnm_mi,aes(x = milk,y = value, width=0.7)) +
#   geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
#   labs(x = '',y='Prozent')+
#   scale_fill_manual("",values = c("Mensa Gericht (n=574)"="#fad60d", "selber mitgebracht (n=195)"="#505050"))+
#   mytheme+ 
#   theme(legend.position = "bottom")+ 
#   scale_y_continuous(labels = scales::percent)+
#   geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
#             size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
#   ggtitle("Ich esse Milch/Milchprodukte, Käse:")
#   ggsave(("04_plots/Frage_9_Milch_mensa_nonmensa_190507_vori.pdf"),
#          width = 17,
#          height = 8,
#          dpi=600,
#          units="in",
#          device="pdf")
# 
# 
#   
#   # veget ----
#   df_mensa_fill_veget <- df_mensa_fill %>% count(veget)
#   
#   df_nonmensa_fill_veget <- df_nonmensa_fill %>% count(veget)
#   
#   # Prozent
#   df_mensa_fill_veget$pct_MG<-df_MG_fill_veget$n/sum(df_MG_fill_veget$n)
#   
#   df_nonmensa_fill_veget$pct_SV<-df_SV_fill_veget$n/sum(df_SV_fill_veget$n)
#   
#   
#   # Prozent
#   df_mensa_fill_veget$pct_m<-df_mensa_fill_veget$n/sum(df_mensa_fill_veget$n)
#   
#   df_nonmensa_fill_veget$pct_nm<-df_nonmensa_fill_veget$n/sum(df_nonmensa_fill_veget$n)
#   
#   df_mensa_veget<- melt(df_mensa_fill_veget[,c("veget", "pct_m")], id.vars = 1)
#   df_mensa_veget$veget <- ordered(df_mensa_veget$veget, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))
#   
#   df_nonmensa_veget<- melt(df_nonmensa_fill_veget[,c("veget", "pct_nm")], id.vars = 1)
#   df_nonmensa_veget$veget <- ordered(df_nonmensa_veget$veget, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))
#   
# 
#   #Zusammenführen der Datensätze
#   df_T_veget <- rbind(df_mensa_veget, df_nonmensa_veget)
#   df_T_veget$variable <- dplyr::recode(df_T_veget$variable, "pct_m"="Mensa Gericht (n=574)", "pct_nm"="selber mitgebracht (n=195)")
#   
#   
#   
#   ggplot(df_T_veget,aes(x = veget,y = value, width=0.7)) +
#     geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
#     labs(x = '',y='Prozent')+
#     scale_fill_manual("",values = c("Mensa Gericht (n=574)"="#fad60d", "selber mitgebracht (n=195)"="#505050"))+
#     mytheme+ 
#     theme(legend.position = "bottom")+ 
#     scale_y_continuous(labels = scales::percent)+
#     geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
#               size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
#     ggtitle("Ich ernähre mich vegetarisch (ovo-lakto):")+
#     ggsave(("04_plots/Frage_9_veget_mensa_nonmensa_190507_vori.pdf"),
#            width = 17,
#            height = 8,
#            dpi=600,
#            units="in",
#            device="pdf")
# 
#   
#   
#   # veg ----
#   df_mensa_fill_veg <- df_mensa_fill %>% count(veg)
#   
#   df_nonmensa_fill_veg <- df_nonmensa_fill %>% count(veg)
#   
#   # Prozent
#   df_mensa_fill_veg$pct_MG<-df_MG_fill_veg$n/sum(df_MG_fill_veg$n)
#   
#   df_nonmensa_fill_veg$pct_SV<-df_SV_fill_veg$n/sum(df_SV_fill_veg$n)
#   
#   
#   # Prozent
#   df_mensa_fill_veg$pct_m<-df_mensa_fill_veg$n/sum(df_mensa_fill_veg$n)
#   
#   df_nonmensa_fill_veg$pct_nm<-df_nonmensa_fill_veg$n/sum(df_nonmensa_fill_veg$n)
#   
#   df_mensa_veg<- melt(df_mensa_fill_veg[,c("veg", "pct_m")], id.vars = 1)
#   df_mensa_veg$veg <- ordered(df_mensa_veg$veg, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))
#   
#   df_nonmensa_veg<- melt(df_nonmensa_fill_veg[,c("veg", "pct_nm")], id.vars = 1)
#   df_nonmensa_veg$veg <- ordered(df_nonmensa_veg$veg, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))
#   
#   
#   #Zusammenführen der Datensätze
#   df_T_veg <- rbind(df_mensa_veg, df_nonmensa_veg)
#   df_T_veg$variable <- dplyr::recode(df_T_veg$variable, "pct_m"="Mensa Gericht (n=574)", "pct_nm"="selber mitgebracht (n=195)")
#   
#   
#   
#   ggplot(df_T_veg,aes(x = veg,y = value, width=0.7)) +
#     geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
#     labs(x = '',y='Prozent')+
#     scale_fill_manual("",values = c("Mensa Gericht (n=574)"="#fad60d", "selber mitgebracht (n=195)"="#505050"))+
#     mytheme+ 
#     theme(legend.position = "bottom")+ 
#     scale_y_continuous(labels = scales::percent)+
#     geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
#               size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
#     ggtitle("Ich ernähre mich rein pflanzlich (vegan):")+
#     ggsave(("04_plots/Frage_9_veg_mensa_nonmensa_190507_vori.pdf"),
#            width = 17,
#            height = 8,
#            dpi=600,
#            units="in",
#            device="pdf")
#   
#   