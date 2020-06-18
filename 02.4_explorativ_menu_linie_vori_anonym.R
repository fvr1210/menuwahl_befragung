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

df_tot <- read_delim("2019_ZHAW_vonRickenbach_cleaned_recorded_survey_dataset_NOVANIMAL_anonym.csv", delim=';', locale = locale(encoding = 'ISO-8859-2'))

# Teilstichproben bilden
# Nur Personen bei denen der Menülinie bekannt ist. Ohne Hot & Cold Menues. Diese sind bei den Menueinhalten. Teilstichprobe B
df_linie <- df_tot %>% 
  filter(mensa==1) %>% 
  filter(!is.na(meal) & meal!="Hot & Cold") %>% 
  mutate(meal = fct_recode(meal, "Favorite/World"="Favorite")) %>% 
  mutate(meal = fct_recode(meal, "Favorite/World"="World")) %>% 
  mutate(Inhalt=ifelse(meal == "Favorite/World", "Favorite/Word (n = 513)", "Kitchen (n = 138)"))
 

# Fragebogen zu Kitchen.
df_kitch <- df_linie %>% 
  filter(meal=="Kitchen")


df_kitch <- df_kitch %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_kitch <- df_kitch %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_kitch$gender, df_kitch$member)

# summary(df_kitch$age)
# sd(df_kitch$age, na.rm = T)

# Fragebogen zu Favorite oder World
df_f_w <- df_linie %>% 
  filter(meal=="Favorite/World")

df_f_w <- df_f_w %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_f_w <- df_f_w %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_f_w$gender, df_f_w$member)

# summary(df_f_w$age)
# sd(df_f_w$age, na.rm = T)



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
  scale_x_discrete(limits=c("Kitchen (n = 138)", "Favorite/Word (n = 513)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 5, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Wie treffen die folgenden Aussagen auf Ihre heutige Menü-Wahl in der Mensa zu?\n(Frage 2, Menülinie, n = 651)\n\nIch habe dieses Menü heute gewählt, weil ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)




ggsave("04_plots/Frage_2_Menulinie_unterteilt_191104.pdf",
       width = 14.5,
       height = 20,
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
  scale_x_discrete(limits=c("Kitchen (n = 138)", "Favorite/Word (n = 513)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Wie zufrieden sind Sie mit dem gewählten Menü?\n(Frage 3, Menülinie, n = 651)")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)

ggsave("04_plots/Frage_3_Menulinie_unterteilt_191104.pdf",
       width = 14.5,
       height = 6,
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
  scale_x_discrete(limits=c("Kitchen (n = 138)", "Favorite/Word (n = 513)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 5, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Wie wichtig waren Ihnen folgende Inhaltsstoffe Ihres heutigen Essens? (Frage 4, Menülinie, n = 651)\n\nMein heutiges Essen ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)

ggsave("04_plots/Frage_4_Menülinie_unterteilt_191203.pdf",
       width = 27,
       height = 13,
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
  scale_x_discrete(limits=c("Kitchen (n = 138)", "Favorite/Word (n = 513)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 5, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Wie wichtig waren Ihnen folgende Eigenschaften Ihres heutigen Essens?\n(Frage 5, Menülinie, n = 651)\n\nMein heutiges Essen ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)



ggsave("04_plots/Frage_5_Menülinie_unterteilt_191203.pdf",
       width = 27,
       height = 13,
       dpi=600,
       units="cm",
       device=cairo_pdf)

# Datensätze erstellen



# Frage 6: Tabellen----


CrossTable(df_linie$meal, df_linie$diet)
CrossTable(df_linie$meal, df_linie$allerg)
CrossTable(df_linie$meal, df_linie$relig)
CrossTable(df_linie$meal, df_linie$meds)
 


# Wird nicht gebraucht für Working Paper, stand 04.11.2019

# # Frage 7: Direkter Gruppenvergleich ----
# 
# # Datensätze erstellen
# 
# df_linie_tho1 <- df_linie_fill%>% 
#   select(c(tho_1, meal, Inhalt)) %>% 
#   mutate(Wahl=tho_1) %>% 
#   mutate(linie=meal) %>%
#   mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#   select(-c("tho_1")) %>% 
#   mutate(question = "meiner Ernährungsweise für meine Gesundheit.")
# 
# df_linie_tho2 <- df_linie_fill%>% 
#   select(c(tho_2, meal, Inhalt)) %>% 
#   mutate(Wahl=tho_2) %>% 
#   mutate(linie=meal) %>%
#   mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#   mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#   select(-c("tho_2")) %>% 
#   mutate(question = "meiner Ernährungsgewohnheiten für die Umwelt.")
# 
# df_linie_tho3 <- df_linie_fill%>% 
#   select(c(tho_3, meal, Inhalt)) %>% 
#   mutate(Wahl=tho_3) %>% 
#   mutate(linie=meal) %>%
#   mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#   select(-c("tho_3")) %>% 
#   mutate(question = "der Produktion der Nahrungsmittel auf meinem Teller\nfür die Arbeitenden in der Wertschöpfungskette.")
# 
# df_linie_tho4 <- df_linie_fill%>% 
#   select(c(tho_4, meal, Inhalt)) %>% 
#   mutate(Wahl=tho_4) %>% 
#   mutate(linie=meal) %>%
#   mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#   select(-c("tho_4")) %>% 
#   mutate(question = "meines Konsums von tierischen Nahrungsmitteln\nfür die Tiere.")
# 
# df_linie_tho5 <- df_linie_fill%>% 
#   select(c(tho_5, meal, Inhalt)) %>% 
#   mutate(Wahl=tho_5) %>% 
#   mutate(linie=meal) %>%
#   mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#   select(-c("tho_5")) %>% 
#   mutate(question = "meiner Ernährung für mein Portemonnaie.")
# 
# 
# # Datensätze zusammenführen
# df_linie_tho <- bind_rows(df_linie_tho1, df_linie_tho2, df_linie_tho3, df_linie_tho4, df_linie_tho5) %>% # 
#   mutate(Wahl = recode(.$Wahl, .missing = "keine Antwort", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
#   group_by(question, Zustimmung, linie, Wahl) %>%
#   summarise(tot_answ = n()) %>%  
#   mutate(pct = (tot_answ/sum(tot_answ))) %>% 
#   mutate(xlab_ = paste("(", linie, ")", sep = ""),
#          xlab = paste(linie, question , sep = ": ")) #add empty coloms for spacing in the middle
# 
# df_linie_tho[nrow(df_linie_tho) + 1,] <-  list("space1", NA, NA, NA, NA, NA, NA)
# df_linie_tho[nrow(df_linie_tho) + 1,] <-  list("space2", NA, NA, NA, NA, NA, NA)
# df_linie_tho[nrow(df_linie_tho) + 1,] <-  list("space3", NA, NA, NA, NA, NA, NA)
# df_linie_tho[nrow(df_linie_tho) + 1,] <-  list("space4", NA, NA, NA, NA, NA, NA)
# 
# # Sortieren Augrund der Gesamtzustimmung
# df_linie_tho <- df_linie_tho[order(df_linie_tho$Zustimmung),]
# 
# 
# # add colors
# ColsPerCat = c("keine Antwort" = "grey", "stimme nicht zu" = "#FF0000", "stimme eher nicht zu" = "#FFA500", "stimme eher zu" = "#99f200", "stimme zu"="#006400")
# 
# # source("S:\\pools\\n\\N-IUNR-nova-data\\02_kassendaten\\02_tilldata_2017\\09_function_is_dark_190114_egel.R", chdir = T)
# # 
# # df_linie_tho$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_linie_tho$Wahl], # takes every label and their belonged color
# #                                           function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"
# 
# label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"
# 
# label_color <- as.data.frame(label_color)
# 
# df_color <- tibble::rownames_to_column(label_color, var = "Wahl")
# 
# df_linie_tho <- merge(df_linie_tho, df_color, by="Wahl")
# 
# # Sortieren Augrund der Gesamtzustimmung
# df_linie_tho <- df_linie_tho[order(df_linie_tho$Zustimmung, df_linie_tho$Wahl, df_linie_tho$linie),]
# 
# 
# 
# 
# ggplot(df_linie_tho, aes(x=xlab, y=pct, fill=factor(Wahl, levels= c ("keine Antwort", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
#   geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.8) +
#   scale_y_continuous(labels = scales::percent)+
#   scale_color_manual(values = levels(df_linie_tho$label_color)) +
#   scale_fill_manual(breaks = attributes(ColsPerCat)$name,
#                     values = ColsPerCat)+
#   scale_x_discrete(limits=c((df_linie_tho$xlab)[c(2, 3, 1)],
#                             "", 
#                             (df_linie_tho$xlab)[c(16, 17, 18)],
#                             "",
#                             (df_linie_tho$xlab)[c(31, 32, 33)],
#                             "", 
#                             (df_linie_tho$xlab)[c(50, 51, 52)],
#                             "",
#                             (df_linie_tho$xlab)[c(61, 62, 63)]))+
#   #                     guide_legend("")) +
#   geom_text(aes(label=(ifelse(pct<0.02, "",paste( round(pct*100), "%", sep = " ")))), size = 5, position = position_stack(vjust = 0.5))+
#   ylab("Antworten in Prozent")+
#   xlab("")+
#   guides(color = F, fill = guide_legend("", reverse = T)) +
#   ggtitle("Frage 7: Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zum Essen zu?\n(Favorite/Word (n=331), Kitchen (n=83), H&C (n=102))")+
#   xlab("Ich mache mir allgemein Gedanken über die Folgen ...")+
#   coord_flip() +
#   mytheme +
# 
#   theme(legend.position = "bottom")
# 
# ggsave("04_plots/Frage_7_Menulinie_190715.pdf",
#        width = 27,
#        height = 13,
#        dpi=600,
#        units="cm",
#        device=cairo_pdf)
# 
# 
# 
# 
# # Frage 8: Direkter Gruppenvergleich ----
# 
# # Datensätze erstellen
# 
# df_linie_tra1 <- df_linie_fill%>% 
#   select(c(tra_1, meal, Inhalt)) %>% 
#   mutate(Wahl=tra_1) %>% 
#   mutate(linie=meal) %>%
#   mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#   select(-c("tra_1")) %>% 
#   mutate(question = "gesund zu leben.")
# 
# df_linie_tra2 <- df_linie_fill%>% 
#   select(c(tra_2, meal, Inhalt)) %>% 
#   mutate(Wahl=tra_2) %>% 
#   mutate(linie=meal) %>%
#   mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#   mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#   select(-c("tra_2")) %>% 
#   mutate(question = "mit meinem Verhalten die Umwelt möglichst wenig zu belasten.")
# 
# df_linie_tra3 <- df_linie_fill%>% 
#   select(c(tra_3, meal, Inhalt)) %>% 
#   mutate(Wahl=tra_3) %>% 
#   mutate(linie=meal) %>%
#   mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#   select(-c("tra_3")) %>% 
#   mutate(question = "dass die Welt sozial gerechter wird.")
# 
# df_linie_tra4 <- df_linie_fill%>% 
#   select(c(tra_4, meal, Inhalt)) %>% 
#   mutate(Wahl=tra_4) %>% 
#   mutate(linie=meal) %>%
#   mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#   select(-c("tra_4")) %>% 
#   mutate(question = "dass die Arbeitsbedingungen für alle Menschen human sind.")
# 
# df_linie_tra5 <- df_linie_fill%>% 
#   select(c(tra_5, meal, Inhalt)) %>% 
#   mutate(Wahl=tra_5) %>% 
#   mutate(linie=meal) %>%
#   mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
#   select(-c("tra_5")) %>% 
#   mutate(question = "dass Tiere möglichst artgerecht gehalten werden.")
# 
# 
# # Datensätze zusammenführen
# df_linie_tra <- bind_rows(df_linie_tra1, df_linie_tra2, df_linie_tra3, df_linie_tra4, df_linie_tra5) %>% # 
#   mutate(Wahl = recode(.$Wahl, .missing = "keine Antwort", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
#   group_by(question, Zustimmung, linie, Wahl) %>%
#   summarise(tot_answ = n()) %>%  
#   mutate(pct = (tot_answ/sum(tot_answ))) %>% 
#   mutate(xlab_ = paste("(", linie, ")", sep = ""),
#          xlab = paste(linie, question , sep = ": ")) #add empty coloms for spacing in the middle
# 
# df_linie_tra[nrow(df_linie_tra) + 1,] <-  list("space1", NA, NA, NA, NA, NA, NA)
# df_linie_tra[nrow(df_linie_tra) + 1,] <-  list("space2", NA, NA, NA, NA, NA, NA)
# df_linie_tra[nrow(df_linie_tra) + 1,] <-  list("space3", NA, NA, NA, NA, NA, NA)
# df_linie_tra[nrow(df_linie_tra) + 1,] <-  list("space4", NA, NA, NA, NA, NA, NA)
# 
# # Sortieren Augrund der Gesamtzustimmung
# df_linie_tra <- df_linie_tra[order(df_linie_tra$Zustimmung),]
# 
# 
# # add colors
# ColsPerCat = c("keine Antwort" = "grey", "stimme nicht zu" = "#FF0000", "stimme eher nicht zu" = "#FFA500", "stimme eher zu" = "#99f200", "stimme zu"="#006400")
# 
# source("S:\\pools\\n\\N-IUNR-nova-data\\02_kassendaten\\02_tilldata_2017\\09_function_is_dark_190114_egel.R", chdir = T)
# 
# # df_linie_tra$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_linie_tra$Wahl], # takes every label and their belonged color
# #                                               function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"
# label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"
# 
# label_color <- as.data.frame(label_color)
# 
# df_color <- tibble::rownames_to_column(label_color, var = "Wahl")
# 
# df_linie_tra <- merge(df_linie_tra, df_color, by="Wahl")
# 
# # Sortieren Augrund der Gesamtzustimmung
# df_linie_tra <- df_linie_tra[order(df_linie_tra$Zustimmung, df_linie_tra$Wahl, df_linie_tra$linie),]
# 
# 
# ggplot(df_linie_tra, aes(x=xlab, y=pct, fill=factor(Wahl, levels= c ("keine Antwort", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
#   geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.8) +
#   scale_y_continuous(labels = scales::percent)+
#   scale_color_manual(values = levels(df_linie_tra$label_color)) +
#   scale_fill_manual(breaks = attributes(ColsPerCat)$name,
#                     values = ColsPerCat)+
#   scale_x_discrete(limits=c((df_linie_tra$xlab)[c(2, 3, 1)],
#                             "", 
#                             (df_linie_tra$xlab)[c(17, 18, 16)],
#                             "",
#                             (df_linie_tra$xlab)[c(33, 34, 32)],
#                             "", 
#                             (df_linie_tra$xlab)[c(47, 48, 46)],
#                             "",
#                             (df_linie_tra$xlab)[c(59, 60, 58)]))+
#   #                     guide_legend("")) +
#   geom_text(aes(label=(ifelse(pct<0.02, "",paste( round(pct*100), "%", sep = " ")))), size = 5, position = position_stack(vjust = 0.5))+
#   ylab("Antworten in Prozent")+
#   xlab("")+
#   guides(color = F, fill = guide_legend("", reverse = T)) +
#   ggtitle("Frage 8: Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zum Essen zu?\n(Favorite/Word (n=331), Kitchen (n=83), H&C (n=102))")+
#   xlab("Mir ist es allgemein wichtig, ...")+
#   coord_flip() +
#   mytheme +
# 
#   theme(legend.position = "bottom")
# 
# ggsave("04_plots/Frage_8_Menulinie_190715.pdf",
#        width = 27,
#        height = 13,
#        dpi=600,
#        units="cm",
#        device=cairo_pdf)
# 
# 
# # Frage 9: Direkter Vergleich ----
# 
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
# # Fleisch ----
# df_f_w_fill_meat <- df_linie_fill %>% 
#   filter(meal=="Favorite/World") %>% 
#   count(meat)
# 
# df_kitch_fill_meat <- df_linie_fill %>% 
#   filter(meal=="Kitchen") %>% 
#   count(meat)
# 
# df_HC_fill_meat <- df_linie_fill %>% 
#   filter(meal=="Hot & Cold") %>% 
#   count(meat)
# 
# 
# # Prozent
# df_f_w_fill_meat$pct_meat<-df_f_w_fill_meat$n/sum(df_f_w_fill_meat$n)
# 
# df_kitch_fill_meat$pct_veg<-df_kitch_fill_meat$n/sum(df_kitch_fill_meat$n)
# 
# df_HC_fill_meat$pct_HC<-df_HC_fill_meat$n/sum(df_HC_fill_meat$n)
# 
# 
# 
# # Umwandeln der Datensätze für den Plot
# df_f_w_m <- melt(df_f_w_fill_meat[,c("meat", "pct_meat")], id.vars = 1)
# df_f_w_m$meat <- ordered(df_f_w_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
# 
# df_kitch_m <- melt(df_kitch_fill_meat[,c("meat", "pct_veg")], id.vars = 1)
# df_kitch_m$meat <- ordered(df_kitch_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
# 
# df_HC_m <- melt(df_HC_fill_meat[,c("meat", "pct_HC")], id.vars = 1)
# df_HC_m$meat <- ordered(df_HC_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
# 
# #Zusammenführen der Datensätze
# df_T_m <- rbind(df_f_w_m, df_kitch_m, df_HC_m)
# df_T_m$variable <- dplyr::recode(df_T_m$variable, "pct_meat"="Favorite/Word (n=331)", "pct_veg"="Kitchen (n=83)", "pct_HC"="H&C (n=102)")
# 
# ggplot(df_T_m,aes(x = meat,y = value, width=0.7)) +
#   geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
#   labs(x = '',y='Prozent')+
#   scale_fill_manual("",values = c("Favorite/Word (n=331)"="#fad60d", "Kitchen (n=83)"="#505050" , "H&C (n=102)"="#c5b87c"))+
#   mytheme+ 
#   theme(legend.position = "bottom")+ 
#   scale_y_continuous(labels = scales::percent)+
#   geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
#             size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
#   ggtitle("Ich esse Fleisch (alle Sorten inkl. Geflügel, Wurstwaren etc.):")+
#   ggsave(("04_plots/Frage_9_Fleisch_Menulinie_190715_vori.pdf"),
#          width = 17,
#          height = 8,
#          dpi=600,
#          units="cm",
#          device="pdf")
# 
# 
# # Milch ----
# 
# df_f_w_fill_milk <- df_linie_fill %>% 
#   filter(meal=="Favorite/World") %>% 
#   count(milk)
# 
# df_kitch_fill_milk <- df_linie_fill %>% 
#   filter(meal=="Kitchen") %>% 
#   count(milk)
# 
# df_HC_fill_milk <- df_linie_fill %>% 
#   filter(meal=="Hot & Cold") %>% 
#   count(milk)
# 
# 
# # Prozent
# df_f_w_fill_milk$pct_meat<-df_f_w_fill_milk$n/sum(df_f_w_fill_milk$n)
# 
# df_kitch_fill_milk$pct_veg<-df_kitch_fill_milk$n/sum(df_kitch_fill_milk$n)
# 
# df_HC_fill_milk$pct_HC<-df_HC_fill_milk$n/sum(df_HC_fill_milk$n)
# 
# 
# 
# # Umwandeln der Datensätze für den Plot
# df_f_w_mi<- melt(df_f_w_fill_milk[,c("milk", "pct_meat")], id.vars = 1)
# df_f_w_mi$milk <- ordered(df_f_w_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
# 
# df_kitch_mi<- melt(df_kitch_fill_milk[,c("milk", "pct_veg")], id.vars = 1)
# df_kitch_mi$milk <- ordered(df_kitch_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
# 
# df_HC_mi<- melt(df_HC_fill_milk[,c("milk", "pct_HC")], id.vars = 1)
# df_HC_mi$milk <- ordered(df_HC_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
# 
# #Zusammenführen der Datensätze
# df_T_mi <- rbind(df_f_w_mi, df_kitch_mi, df_HC_mi)
# df_T_mi$variable <- dplyr::recode(df_T_mi$variable, "pct_meat"="Favorite/Word (n=331)", "pct_veg"="Kitchen (n=83)", "pct_HC"="H&C (n=102)")
# 
# 
# 
# ggplot(df_T_mi,aes(x = milk,y = value, width=0.7)) +
#   geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
#   labs(x = '',y='Prozent')+
#   scale_fill_manual("",values = c("Favorite/Word (n=331)"="#fad60d", "Kitchen (n=83)"="#505050" , "H&C (n=102)"="#c5b87c"))+
#   mytheme+ 
#   theme(legend.position = "bottom")+ 
#   scale_y_continuous(labels = scales::percent)+
#   geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
#             size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
#   ggtitle("Ich trinke/esse Milch/Milchprodukte, Käse:")+
#   ggsave(("04_plots/Frage_9_Milch_Menulinie_190715_vori.pdf"),
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
# df_f_w_fill_veget <- df_linie_fill %>% 
#   filter(meal=="Favorite/World") %>% 
#   count(veget)
# 
# df_kitch_fill_veget <- df_linie_fill %>% 
#   filter(meal=="Kitchen") %>% 
#   count(veget)
# 
# df_HC_fill_veget <- df_linie_fill %>% 
#   filter(meal=="Hot & Cold") %>% 
#   count(veget)
# 
# 
# # Prozent
# df_f_w_fill_veget$pct_meat<-df_f_w_fill_veget$n/sum(df_f_w_fill_veget$n)
# 
# df_kitch_fill_veget$pct_veg<-df_kitch_fill_veget$n/sum(df_kitch_fill_veget$n)
# 
# df_HC_fill_veget$pct_HC<-df_HC_fill_veget$n/sum(df_HC_fill_veget$n)
# 
# 
# # Umwandeln der Datensätze für den Plot
# df_f_w_veget<- melt(df_f_w_fill_veget[,c("veget", "pct_meat")], id.vars = 1)
# df_f_w_veget$veget <- ordered(df_f_w_veget$veget, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))
# 
# df_kitch_veget<- melt(df_kitch_fill_veget[,c("veget", "pct_veg")], id.vars = 1)
# df_kitch_veget$veget <- ordered(df_kitch_veget$veget, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))
# 
# df_HC_veget<- melt(df_HC_fill_veget[,c("veget", "pct_HC")], id.vars = 1)
# df_HC_veget$veget <- ordered(df_HC_veget$veget, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))
# 
# #Zusammenführen der Datensätze
# df_T_veget <- rbind(df_f_w_veget, df_kitch_veget, df_HC_veget)
# df_T_veget$variable <- dplyr::recode(df_T_veget$variable, "pct_meat"="Favorite/Word (n=331)", "pct_veg"="Kitchen (n=83)", "pct_HC"="H&C (n=102)")
# 
# 
# 
# ggplot(df_T_veget,aes(x = veget,y = value, width=0.7)) +
#   geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
#   labs(x = '',y='Prozent')+
#   scale_fill_manual("",values = c("Favorite/Word (n=331)"="#fad60d", "Kitchen (n=83)"="#505050" , "H&C (n=102)"="#c5b87c"))+
#   mytheme+ 
#   theme(legend.position = "bottom")+ 
#   scale_y_continuous(labels = scales::percent)+
#   geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
#             size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
#   ggtitle("Ich ernähre mich vegetarisch (ovo-lakto):")+
#   ggsave(("04_plots/Frage_9_veget_Menulinie_190715_vori.pdf"),
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
# df_f_w_fill_veg <- df_linie_fill %>% 
#   filter(meal=="Favorite/World") %>% 
#   count(veg)
# 
# df_kitch_fill_veg <- df_linie_fill %>% 
#   filter(meal=="Kitchen") %>% 
#   count(veg)
# 
# df_HC_fill_veg <- df_linie_fill %>% 
#   filter(meal=="Hot & Cold") %>% 
#   count(veg)
# 
# 
# # Prozent
# df_f_w_fill_veg$pct_meat<-df_f_w_fill_veg$n/sum(df_f_w_fill_veg$n)
# 
# df_kitch_fill_veg$pct_veg<-df_kitch_fill_veg$n/sum(df_kitch_fill_veg$n)
# 
# df_HC_fill_veg$pct_HC<-df_HC_fill_veg$n/sum(df_HC_fill_veg$n)
# 
# 
# # Umwandeln der Datensätze für den Plot
# df_f_w_veg<- melt(df_f_w_fill_veg[,c("veg", "pct_meat")], id.vars = 1)
# df_f_w_veg$veg <- ordered(df_f_w_veg$veg, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))
# 
# df_kitch_veg<- melt(df_kitch_fill_veg[,c("veg", "pct_veg")], id.vars = 1)
# df_kitch_veg$veg <- ordered(df_kitch_veg$veg, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))
# 
# df_HC_veg<- melt(df_HC_fill_veg[,c("veg", "pct_HC")], id.vars = 1)
# df_HC_veg$veg <- ordered(df_HC_veg$veg, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))
# 
# #Zusammenführen der Datensätze
# df_T_veg <- rbind(df_f_w_veg, df_kitch_veg, df_HC_veg)
# df_T_veg$variable <- dplyr::recode(df_T_veg$variable, "pct_meat"="Favorite/Word (n=331)", "pct_veg"="Kitchen (n=83)", "pct_HC"="H&C (n=102)")
# 
# 
# 
# ggplot(df_T_veg,aes(x = veg,y = value, width=0.7)) +
#   geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
#   labs(x = '',y='Prozent')+
#   scale_fill_manual("",values = c("Favorite/Word (n=331)"="#fad60d", "Kitchen (n=83)"="#505050" , "H&C (n=102)"="#c5b87c"))+
#   mytheme+ 
#   theme(legend.position = "bottom")+ 
#   scale_y_continuous(labels = scales::percent)+
#   geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
#             size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
#   ggtitle("Ich ernähre mich rein pflanzlich (vegan):")+
#   ggsave(("04_plots/Frage_9_veg_Menülinie_190715_vori.pdf"),
#          width = 17,
#          height = 8,
#          dpi=600,
#          units="cm",
#          device="pdf")
