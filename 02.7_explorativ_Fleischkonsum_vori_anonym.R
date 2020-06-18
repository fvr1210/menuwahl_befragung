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
        plot.margin=unit(c(t = 0, r = 0.1, b = 0, l = 0),"cm"))



# Daten einlesen ---------

df_tot <- read_delim("2019_ZHAW_vonRickenbach_cleaned_recorded_survey_dataset_NOVANIMAL_anonym.csv", delim=';', locale = locale(encoding = 'ISO-8859-2'))

# Teilstichproben bilden
df_tot_fill <- df_tot %>% 
  filter(fill==0)

# Bilden von Fleisch ernährungstypen, evtl. dann in Skript von Datensatz clean_record verschieben
df_diet_fill <- df_tot_fill %>%
  filter(!is.na(meat)) %>% 
  mutate(meat_diet = case_when (meat==7 ~ "Fleischliebhaber (n = 146)",
                              meat==6 ~ "Fleischliebhaber (n = 146)",
                              meat==5 ~ "Fleischesser (n = 118)",
                              meat==4 ~ "Fleisch-Flexitarier (n = 177)",
                              meat==3 ~ "Vegi-Flexitarier (n = 183)",
                              meat==2 ~ "Fleischvermeider (n = 140)",
                              meat==1 ~ "Fleischvermeider (n = 140)", 
                              TRUE ~ as.character(NA))) %>% 
   mutate(meat_diet_2 = case_when (meat==7 ~ "Fleischliebhaber",
                                meat==6 ~ "Fleischliebhaber",
                                meat==5 ~ "Fleischesser",
                                meat==4 ~ "Fleisch-Flexitarier",
                                meat==3 ~ "Vegi-Flexitarier",
                                meat==2 ~ "Fleischvermeider",
                                meat==1 ~ "Fleischvermeider", 
                                TRUE ~ as.character(NA)))

# Mit Fleischliebhaber
df_FL <- df_diet_fill %>% 
  filter(meat_diet_2=="Fleischliebhaber")

df_FL <- df_FL %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_FL <- df_FL %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_FL$gender, df_FL$member)

# summary(df_FL$age)
# sd(df_FL$age, na.rm = T)

# Mit Fleischesser
df_FE <- df_diet_fill %>% 
  filter(meat_diet_2=="Fleischesser")

df_FE <- df_FE %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_FE <- df_FE %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_FE$gender, df_FE$member)

# summary(df_FE$age)
# sd(df_FE$age, na.rm = T)

# Mit Fleischess-Flexitarier
df_FF <- df_diet_fill %>% 
  filter(meat_diet_2=="Fleisch-Flexitarier")

df_FF <- df_FF %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_FF <- df_FF %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_FF$gender, df_FF$member)

# summary(df_FF$age)
# sd(df_FF$age, na.rm = T)

# Mit Vegi-Flexitarier
df_VF <- df_diet_fill %>% 
  filter(meat_diet_2=="Vegi-Flexitarier")

df_VF <- df_VF %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_VF <- df_VF %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_VF$gender, df_VF$member)

# summary(df_VF$age)
# sd(df_VF$age, na.rm = T)

# Mit Fleischvermeider
df_NF <- df_diet_fill %>% 
  filter(meat_diet_2=="Fleischvermeider")

df_NF <- df_NF %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_NF <- df_NF %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_NF$gender, df_NF$member)

# summary(df_NF$age)
# sd(df_NF$age, na.rm = T)



# Frage 7: Direkter Gruppenvergleich ----

# Datensätze erstellen

df_VT_tho1 <- df_diet_fill%>% 
  select(c(tho_1, meat_diet)) %>% 
  mutate(Wahl=tho_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_1")) %>% 
  mutate(question = "... meiner Ernährungsweise für meine Gesundheit.")

df_VT_tho2 <- df_diet_fill%>% 
  select(c(tho_2, meat_diet)) %>% 
  mutate(Wahl=tho_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_2")) %>% 
  mutate(question = "... meiner Ernährungsgewohnheiten für die Umwelt.")

df_VT_tho3 <- df_diet_fill%>% 
  select(c(tho_3, meat_diet)) %>% 
  mutate(Wahl=tho_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_3")) %>% 
  mutate(question = "... der Produktion der Nahrungsmittel auf meinem Teller für die Arbeitenden in der Wertschöpfungskette.")

df_VT_tho4 <- df_diet_fill%>% 
  select(c(tho_4, meat_diet)) %>% 
  mutate(Wahl=tho_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_4")) %>% 
  mutate(question = "... meines Konsums von tierischen Nahrungsmitteln für die Tiere.")

df_VT_tho5 <- df_diet_fill%>% 
  select(c(tho_5, meat_diet)) %>% 
  mutate(Wahl=tho_5) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_5")) %>% 
  mutate(question = "... meiner Ernährung für mein Portemonnaie.")


# Datensätze zusammenführen
df_VT_tho <- bind_rows(df_VT_tho1, df_VT_tho2, df_VT_tho3, df_VT_tho4, df_VT_tho5) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(question, Zustimmung, meat_diet, Wahl) %>%
  summarise(tot_answ = n()) %>%  
  mutate(pct = (tot_answ/sum(tot_answ)))



# add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_VT_tho <- merge(df_VT_tho, df_color, by="Wahl")


# für Plot Sortieren Augrund der Gesamtzustimmung
df_VT_tho$question <- reorder(df_VT_tho$question, -df_VT_tho$Zustimmung)

## plot ----
ggplot(data = df_VT_tho, aes(x=meat_diet, y=pct, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_VT_tho$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Fleischvermeider (n = 140)", "Vegi-Flexitarier (n = 183)", "Fleisch-Flexitarier (n = 177)", "Fleischesser (n = 118)", "Fleischliebhaber (n = 146)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T, nrow=1)) +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zum Essen zu?\n(Frage 7, Fleischkonsumtyp, n = 764)\n\nIch mache mir allgemein Gedanken über die Folgen ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)


ggsave("04_plots/Frage_7_meat_diet_erweitert_unterteilt_191206_.pdf",
       width = 14.5,
       height = 13.5,
       dpi=600,
       units="cm",
       device=cairo_pdf)

# Frage 8: Direkter Gruppenvergleich ----

# Datensätze erstellen

df_VT_tra1 <- df_diet_fill%>% 
  select(c(tra_1, meat_diet)) %>% 
  mutate(Wahl=tra_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_1")) %>% 
  mutate(question = "... gesund zu leben.")

df_VT_tra2 <- df_diet_fill%>% 
  select(c(tra_2, meat_diet)) %>% 
  mutate(Wahl=tra_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_2")) %>% 
  mutate(question = "... mit meinem Verhalten die Umwelt möglichst wenig zu belasten.")

df_VT_tra3 <- df_diet_fill%>% 
  select(c(tra_3, meat_diet)) %>% 
  mutate(Wahl=tra_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_3")) %>% 
  mutate(question = "... dass die Welt sozial gerechter wird.")

df_VT_tra4 <- df_diet_fill%>% 
  select(c(tra_4, meat_diet)) %>% 
  mutate(Wahl=tra_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_4")) %>% 
  mutate(question = "... das die Arbeitsbedingungen für alle Menschen human sind.")

df_VT_tra5 <- df_diet_fill%>% 
  select(c(tra_5, meat_diet)) %>% 
  mutate(Wahl=tra_5) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_5")) %>% 
  mutate(question = "... dass Tiere möglichst artgerecht gehalten werden.")


# Datensätze zusammenführen
df_VT_tra <- bind_rows(df_VT_tra1, df_VT_tra2, df_VT_tra3, df_VT_tra4, df_VT_tra5) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(question, Zustimmung, meat_diet, Wahl) %>%
  summarise(tot_answ = n()) %>%  
  mutate(pct = (tot_answ/sum(tot_answ)))



# add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_VT_tra <- merge(df_VT_tra, df_color, by="Wahl")


# für Plot Sortieren Augrund der Gesamtzustimmung
df_VT_tra$question <- reorder(df_VT_tra$question, -df_VT_tra$Zustimmung)

## plot ----
ggplot(data = df_VT_tra, aes(x=meat_diet, y=pct, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_VT_tra$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Fleischvermeider (n = 140)", "Vegi-Flexitarier (n = 183)", "Fleisch-Flexitarier (n = 177)", "Fleischesser (n = 118)", "Fleischliebhaber (n = 146)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zu?\n(Frage 8, Fleischkonsumtyp, n = 764)\n\nMir ist es allgemein wichtig, ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)


ggsave("04_plots/Frage_8_meat_diet_erweitert_unterteilt_191206.pdf",
       width = 14.5,
       height = 13.5,
       dpi=600,
       units="cm",
       device=cairo_pdf)


# Frage 9 ----
# Fleisch vs. vegi ----
df_meatveg <- df_diet_fill %>% 
  mutate(veget = ifelse(is.na(veget), "NA", veget)) %>%
  mutate(veget = recode_factor(veget, "6"= "immer", "5" = "meistens", "4" = "oft", "3" = "manchmal", "2" = "selten", "1" = "nie")) %>% 
  group_by(meat_diet, veget) %>% 
  summarise(meatveg = n()) %>% 
  ungroup() %>% 
  mutate(meat_diet = ordered(meat_diet, levels = c("Fleischvermeider (n = 140)",
                                                   "Vegi-Flexitarier (n = 183)",
                                                   "Fleisch-Flexitarier (n = 177)",
                                                   "Fleischesser (n = 118)",
                                                   "Fleischliebhaber (n = 146)"
  ))) %>% 
  group_by(meat_diet) %>%
  mutate(summe = sum(meatveg)) %>% 
  ungroup() %>% 
  mutate(per = meatveg/summe)

ColsPerCat = c("NA" = "grey" ,"nie" = "#DC6413", "selten" = "#Fdd200", "manchmal" = "#d3c193", "oft" = "#AFC410", "meistens"="#006885", "immer" = "#823783")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "veget")

df_meatveg <- merge(df_meatveg, df_color, by="veget")


# Plot -----

ggplot(df_meatveg, aes(y=per, x=meat_diet, fill=factor(veget, levels = c("NA", "nie" , "selten" , "manchmal", "oft", "meistens", "immer")), color = label_color)) + 
  geom_bar(stat = "identity", position = position_stack(),  width = 0.6, color =NA) +
  scale_color_manual(values = levels(df_meatveg$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  xlab("") +
  ylab("")+
  ggtitle("Ich ernähre mich vegetarisch (ovo-lakto)\n(ohne Mehrfachteilnahmen, n =  764)") +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  guides(fill = guide_legend("")) +
  guides(color = F, fill = guide_legend("", reverse = T, ncolum=1, nrow = 1)) +
  mytheme +
  coord_flip()
 


ggsave(("04_plots/Frage_9_Fleischtyp_veget_191210_vori.pdf"),
       width = 14.5,
       height = 10,
       dpi=600,
       units="cm",
       device=cairo_pdf)

# Vegi vs. Fleisch----
df_vegmeat <- df_diet_fill %>% 
  filter(!is.na(veget)) %>%
  mutate(veget = recode_factor(veget, "6"= "immer (n =69)", "5" = "meistens (n = 86)", "4" = "oft (n = 128)", "3" = "manchmal (n = 217)", "2" = "selten (n = 128)", "1" = "nie (n = 124)")) %>% 
  group_by(meat_diet_2, veget) %>% 
  summarise(meatveg = n()) %>% 
  ungroup() %>% 
  mutate(meat_diet_2 = ordered(meat_diet_2, levels = c("Fleischvermeider",
                                                   "Vegi-Flexitarier",
                                                   "Fleisch-Flexitarier",
                                                   "Fleischesser",
                                                   "Fleischliebhaber"
  ))) %>% 
  group_by(veget) %>%
  mutate(summe = sum(meatveg)) %>% 
  ungroup() %>% 
  mutate(per = meatveg/summe)

ColsPerCat = c("Fleischliebhaber" = "#DC6413", "Fleischesser" = "#Fdd200", "Fleisch-Flexitarier" = "#d3c193", "Vegi-Flexitarier" = "#AFC410", "Fleischvermeider"="#006885")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "meat_diet_2")

df_vegmeat <- merge(df_vegmeat, df_color, by="meat_diet_2")


# Plot -----

ggplot(df_vegmeat, aes(y=per, x = reorder(veget, desc(veget)), fill=factor(meat_diet_2, levels = c("Fleischvermeider", "Vegi-Flexitarier", "Fleisch-Flexitarier", "Fleischesser", "Fleischliebhaber")), color = label_color)) + 
  geom_bar(stat = "identity", position = position_stack(),  width = 0.6, color =NA) +
  scale_color_manual(values = levels(df_vegmeat$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  xlab("") +
  ylab("")+
  ggtitle("Ich ernähre mich vegetarisch (ovo-lakto)\n(Fleischkonsumtyp, n =  764)") +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  guides(fill = guide_legend("")) +
  guides(color = F, fill = guide_legend("", reverse = F, ncolum=1, nrow = 1)) +
  mytheme +
  coord_flip()



ggsave(("04_plots/Fleischtyp_veget_200128_vori.pdf"),
       width = 14.5,
       height = 3.5,
       dpi=600,
       units="cm",
       device=cairo_pdf)



# Fleisch vs. Mensa oder selber ------
df_typeat <- df_diet_fill %>% 
  group_by(meat_diet, mensa) %>% 
  summarise(mealtyp = n()) %>% 
  ungroup () %>% 
  group_by(meat_diet) %>%
  mutate(summe = sum(mealtyp)) %>% 
  ungroup() %>% 
  mutate(per = mealtyp/summe) %>% 
  mutate(meat_diet = ordered(meat_diet, levels = c("Fleischvermeider (n = 140)",
                                                   "Vegi-Flexitarier (n = 183)",
                                                   "Fleisch-Flexitarier (n = 177)",
                                                   "Fleischesser (n = 118)",
                                                   "Fleischliebhaber (n = 146)"
                                                   ))) %>% 
  mutate(mensa = ifelse(mensa == 1, "Mensamenü", "selber mitgebracht"))




ColsPerCat = c("selber mitgebracht" = "#DC6413", "Mensamenü" = "#006885")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "mensa")

df_typeat <- merge(df_typeat, df_color, by="mensa")


# Plot -----
ggplot(df_typeat, aes(y=per, x=meat_diet, fill= factor(mensa, levels = c("Mensamenü", "selber mitgebracht")), color = label_color)) + 
  geom_bar(stat = "identity", position = position_stack(),  width = 0.6, color = NA) +
  scale_color_manual(values = levels(df_typeat$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  xlab("") +
  ylab("") +
  ggtitle("Art der Mittagsverpflegung bei den Fleischkonsumtypen\n(ohne Mehrfachteilnahmen, n =  764)") +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme +
  guides(color = F, fill = guide_legend("", reverse = F, ncolum=1))+
  coord_flip()


ggsave(("04_plots/Frage_9_Fleischtyp_mensa_selber_191210_vori.pdf"),
       width = 14.5,
       height = 4,
       dpi=600,
       units="cm",
       device=cairo_pdf)  





# Fleisch vs. Menüinhalt ----
df_inmeat <- df_diet_fill %>% 
  mutate(eat = case_when(label_content=="Fleisch/Fisch" ~ "Fleisch/Fisch",
                         label_content=="rein pflanzlich" ~ "vegan",
                         label_content=="rein pflanzlich*" ~ "vegan",
                         label_content=="vegetarisch" ~ "vegetarisch",
                         meal=="Hot & Cold" ~ "Hot & Cold",
                         TRUE ~ as.character(""))) %>%
  filter(eat != "") %>% 
  mutate(meat_diet_3 = case_when (meat==7 ~ "Fleischliebhaber (n = 105)",
                                  meat==6 ~ "Fleischliebhaber (n = 105)",
                                  meat==5 ~ "Fleischesser (n = 83)",
                                  meat==4 ~ "Fleisch-Flexitarier (n = 119)",
                                  meat==3 ~ "Vegi-Flexitarier (n = 124)",
                                  meat==2 ~ "Fleischvermeider (n = 81)",
                                  meat==1 ~ "Fleischvermeider (n = 81)", 
                                  TRUE ~ as.character(NA))) %>% 
  group_by(meat_diet_3, eat) %>% 
  summarise(eatmeat = n()) %>% 
  ungroup () %>% 
  group_by(meat_diet_3) %>%
  mutate(summe = sum(eatmeat)) %>% 
  ungroup() %>% 
  mutate(per = eatmeat/summe) %>% 
  mutate(meat_diet_3 = ordered(meat_diet_3, levels = c("Fleischvermeider (n = 81)",
                                                       "Vegi-Flexitarier (n = 124)",
                                                       "Fleisch-Flexitarier (n = 119)",
                                                       "Fleischesser (n = 83)",
                                                       "Fleischliebhaber (n = 105)"
  ))) %>% 
  mutate (eat =  factor(eat, levels = c("Fleisch/Fisch",
                                        "vegetarisch",
                                        "vegan",
                                        "Hot & Cold"
  ), exclude = "", ordered = T))


ColsPerCat = c("Hot & Cold" = "#Fdd200",  "vegan" = "#AFC410", "vegetarisch" ="#d3c193", "Fleisch/Fisch" = "#DC6413")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "eat")

df_inmeat <- merge(df_inmeat, df_color, by="eat")


#Plot ----
ggplot(df_inmeat, aes(y=per, x=meat_diet_3, fill= factor(eat, levels = c("Hot & Cold", "vegan", "vegetarisch", "Fleisch/Fisch")), color = label_color)) + 
  geom_bar(stat = "identity", position = position_stack(),  width = 0.6, color =NA) +
  scale_color_manual(values = levels(df_inmeat$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme +
  guides(color = F, fill = guide_legend("", reverse = T, ncolum=1))+
  ggtitle("Fleischkonsumtyp\n(Gewählter Menüinhalt, n =  512)")+
  xlab("") +
  ylab("") +
  coord_flip()  

ggsave("04_plots/Menueinhalt_Fleischkonsumtyp_191210_vori.pdf",
       width = 14.5,
       height = 7.5,
       dpi=600,
       units="cm",
       device=cairo_pdf)



# Fleisch vs. alter und Geschlecht ------

df_agegenmeat <- df_diet_fill %>% 
  filter(gender == "Mann" | gender == "Frau") %>% 
  filter(!is.na(age_groups)) %>% 
  mutate(meat_diet_4 = case_when (meat==7 ~ "Fleischliebhaber (n = 144)",
                                  meat==6 ~ "Fleischliebhaber (n = 144)",
                                  meat==5 ~ "Fleischesser (n = 114)",
                                  meat==4 ~ "Fleisch-Flexitarier (n = 172)",
                                  meat==3 ~ "Vegi-Flexitarier (n = 175)",
                                  meat==2 ~ "Fleischvermeider (n = 136)",
                                  meat==1 ~ "Fleischvermeider (n = 136)", 
                                  TRUE ~ as.character(NA))) %>% 
  mutate(meat_diet_4 = ordered(meat_diet_4, levels = c("Fleischliebhaber (n = 144)",
                                                   "Fleischesser (n = 114)",
                                                   "Fleisch-Flexitarier (n = 172)",
                                                   "Vegi-Flexitarier (n = 175)",
                                                   "Fleischvermeider (n = 136)"))) %>% 
  
  mutate(age_groups = recode(age_groups, "50- bis 64-jaehrig"="50- bis 64-jährig",  "35- bis 49-jaehrig"="35- bis 49-jährig", "17- bis 25-jaehrig" = "17- bis 25-jährig", "26- bis 34-jaehrig"="26- bis 34-jährig")) %>%
  group_by(meat_diet_4, gender, age_groups) %>% 
  summarise(agegender = n()) %>% 
  ungroup () %>% 
  group_by(age_groups, meat_diet_4) %>%
  mutate(summe = sum(agegender)) %>% 
  ungroup() %>% 
  mutate(per = agegender/summe) 


df_agegenmeat %<>% 
  mutate(age_groups_2 = case_when (meat_diet_4=="Fleischliebhaber (n = 144)" & age_groups == "17- bis 25-jährig" ~ "17- bis 25-jährig\n(n = 192)",
                             meat_diet_4=="Fleischliebhaber (n = 144)"& age_groups == "26- bis 34-jährig" ~ "26- bis 34-jährig\n(n = 33)",
                             meat_diet_4=="Fleischliebhaber (n = 144)" & age_groups == "35- bis 49-jährig" ~ "35- bis 49-jährig\n(n = 15)",
                             meat_diet_4=="Fleischliebhaber (n = 144)" & age_groups == "50- bis 64-jährig" ~ "50- bis 64-jährig\n(n = 4)",
                             meat_diet_4=="Fleischesser (n = 114)" & age_groups == "17- bis 25-jährig" ~ "17- bis 25-jährig\n(n = 61)",
                             meat_diet_4=="Fleischesser (n = 114)"& age_groups == "26- bis 34-jährig" ~ "26- bis 34-jährig\n(n = 32)",
                             meat_diet_4=="Fleischesser (n = 114)" & age_groups == "35- bis 49-jährig" ~ "35- bis 49-jährig\n(n = 14)",
                             meat_diet_4=="Fleischesser (n = 114)" & age_groups == "50- bis 64-jährig" ~ "50- bis 64-jährig\n(n = 7)",
                             meat_diet_4=="Fleisch-Flexitarier (n = 172)" & age_groups == "17- bis 25-jährig" ~ "17- bis 25-jährig\n(n = 83)",
                             meat_diet_4=="Fleisch-Flexitarier (n = 172)" & age_groups == "26- bis 34-jährig" ~ "26- bis 34-jährig\n(n = 52)",
                             meat_diet_4=="Fleisch-Flexitarier (n = 172)" & age_groups == "35- bis 49-jährig" ~ "35- bis 49-jährig\n(n = 24)",
                             meat_diet_4=="Fleisch-Flexitarier (n = 172)" & age_groups == "50- bis 64-jährig" ~ "50- bis 64-jährig\n(n = 13)",
                             meat_diet_4=="Vegi-Flexitarier (n = 175)" & age_groups == "17- bis 25-jährig" ~ "17- bis 25-jährig\n(n = 80)",
                             meat_diet_4=="Vegi-Flexitarier (n = 175)" & age_groups == "26- bis 34-jährig" ~ "26- bis 34-jährig\n(n = 49)",
                             meat_diet_4=="Vegi-Flexitarier (n = 175)" & age_groups == "35- bis 49-jährig" ~ "35- bis 49-jährig\n(n = 28)",
                             meat_diet_4=="Vegi-Flexitarier (n = 175)" & age_groups == "50- bis 64-jährig" ~ "50- bis 64-jährig\n(n = 18)",
                             meat_diet_4=="Fleischvermeider (n = 136)" & age_groups == "17- bis 25-jährig" ~ "17- bis 25-jährig\n(n = 69)",
                             meat_diet_4=="Fleischvermeider (n = 136)" & age_groups == "26- bis 34-jährig" ~ "26- bis 34-jährig\n(n = 48)",
                             meat_diet_4=="Fleischvermeider (n = 136)" & age_groups == "35- bis 49-jährig" ~ "35- bis 49-jährig\n(n = 12)",
                             meat_diet_4=="Fleischvermeider (n = 136)" & age_groups == "50- bis 64-jährig" ~ "50- bis 64-jährig\n(n = 7)",
                             TRUE ~ as.character(NA)))



# ColsPerCat = c("17- bis 25-jährig" = "#8AB5E1","26- bis 34-jährig" = "#823783" , "35- bis 49-jährig" = "#AFC410", "50- bis 64-jährig" = "#FDD200")
ColsPerCat = c("Frau" = "#8AB5E1", "Mann" = "#DC6413")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "gender")

df_agegenmeat <- merge(df_agegenmeat, df_color, by="gender") 


# Plot -----
ggplot(df_agegenmeat, aes(y=per, x = reorder(age_groups_2, desc(age_groups_2)), fill= factor(gender, levels = c( "Frau", "Mann")), color = label_color)) + 
  geom_bar(stat = "identity", position = position_stack(),  width = 0.4, color = NA) +
  scale_color_manual(values = levels(df_agegenmeat$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  
  facet_wrap(~meat_diet_4, nrow = 5, ncol = 1, scales = "free") +
  xlab("") +
  ylab("") +
  ggtitle("Alter und Geschlecht nach Fleischkonsumtypen\n(ohne Mehrfachteilnahmen, n =  741)") +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme +
  guides(color = F, fill = guide_legend("", reverse = T, ncolum=1)) +
  coord_flip()

ggsave(("04_plots/Frage_9_Fleischtyp_age_gender_191210_vori.pdf"),
       width = 14.5,
       height = 20,
       dpi=600,
       units="cm",
       device=cairo_pdf)  

# Fleisch vs. Verpflegungstyp ----

df_typmeat <- df_diet_fill %>% 
  filter(!is.na(Verpflegungstyp)) %>% # 10 Beobachtungen weniger
  mutate(meat_diet = case_when (meat==7 ~ "Fleischliebhaber (n = 145)",
                                meat==6 ~ "Fleischliebhaber (n = 145)",
                                meat==5 ~ "Fleischesser (n = 116)",
                                meat==4 ~ "Fleisch-Flexitarier (n = 173)",
                                meat==3 ~ "Vegi-Flexitarier (n = 180)",
                                meat==2 ~ "Fleischvermeider (n = 140)",
                                meat==1 ~ "Fleischvermeider (n = 140)", 
                                TRUE ~ as.character(NA))) %>% 
  group_by(meat_diet, Verpflegungstyp) %>% 
  summarise(typmeat = n()) %>% 
  ungroup () %>% 
  group_by(meat_diet) %>%
  mutate(summe = sum(typmeat)) %>% 
  ungroup() %>% 
  mutate(per = typmeat/summe) %>% 
  mutate(meat_diet = ordered(meat_diet, levels = c("Fleischvermeider (n = 140)",
                                                   "Vegi-Flexitarier (n = 180)",
                                                   "Fleisch-Flexitarier (n = 173)",
                                                   "Fleischesser (n = 116)",
                                                   "Fleischliebhaber (n = 145)"
                                                    ))) %>% 
  mutate(Verpflegungstyp = recode(Verpflegungstyp, "Mensagaenger"="Mensagänger", "Einkaeufer"="Einkäufer")) %>% 
  mutate(Verpflegungstyp = ordered(Verpflegungstyp, levels = c("Selbstverpfleger",
                                                               "Einkäufer",
                                                               "Abwechsler",
                                                               "Mensagänger"))) 

ColsPerCat = c("Selbstverpfleger" = "#AFC410", "Einkäufer" = "#823783", "Abwechsler" ="#006885", "Mensagänger" = "#DC6413")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Verpflegungstyp")

df_typmeat <- merge(df_typmeat, df_color, by="Verpflegungstyp") 


# Plot ----

ggplot(df_typmeat, aes(y=per, x=meat_diet, fill= factor(Verpflegungstyp, levels = c("Selbstverpfleger", "Einkäufer" , "Abwechsler", "Mensagänger")), color = label_color)) + 
  geom_bar(stat = "identity", position = position_stack(),  width = 0.6, color = NA) +
  scale_color_manual(values = levels(df_typmeat$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  ggtitle("Verpflegungstyp nach Fleischkonsumtypen\n(ohne Mehrfachteilnahmen, n =  754)") +
  labs(x = "", y = "") +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme +
  guides(color = F, fill = guide_legend("", reverse = T, ncolum=1))+
  coord_flip()



ggsave(("04_plots/Frage_9_x_Fleischtyp_verpflegungstyp_191210_vori.pdf"),
       width = 14.5,
       height = 7,
       dpi=600,
       units="cm",
       device=cairo_pdf)  


