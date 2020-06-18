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

df_tot <- read_delim("2019_ZHAW_vonRickenbach_cleaned_recorded_survey_dataset_NOVANIMAL_anonym.csv", delim=';', locale = locale(encoding = 'ISO-8859-2'))

# Stichprob nur mit Personen die einmal mitgemacht haben und einen Verpflegungstyp haben
df_typ_fill <- df_tot %>% 
  filter(fill==0) %>% 
  filter(!is.na(Verpflegungstyp)) %>% 
  mutate(Verpflegungstyp = recode(Verpflegungstyp, "Mensagaenger"="Mensagänger", "Einkaeufer"="Einkäufer")) %>% 
  mutate(Verpflegungstyp_2 = case_when (Verpflegungstyp=="Mensagänger" ~ "Mensagänger (n = 379)",
                                        Verpflegungstyp=="Selbstverpfleger" ~ "Selbstverpfleger (n = 268)",
                                        Verpflegungstyp=="Abwechsler" ~ "Abwechsler (n = 85)",
                                        Verpflegungstyp=="Einkäufer" ~ "Einkäufer (n = 27)",
                                        TRUE ~ as.character(NA)))


#Verpflegunstypn (Frage 11, selbst gebildet vergleiche Skript 01_clean_recode_181102_05vori_de.R) 
#Mensagänger
df_MG_fill <- df_typ_fill %>% 
  filter(Verpflegungstyp=="Mensagänger") 

df_MG_fill <- df_MG_fill %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_MG_fill <- df_MG_fill %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_MG_fill$gender, df_MG_fill$member)

# summary(df_MG_fill$age)
# sd(df_MG_fill$age, na.rm = T)


# Selbstverpfleger
df_SV_fill <- df_typ_fill %>% 
  filter(Verpflegungstyp=="Selbstverpfleger")
  
df_SV_fill <- df_SV_fill %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_SV_fill <- df_SV_fill %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_SV_fill$gender, df_SV_fill$member)

# summary(df_SV_fill$age)
# sd(df_SV_fill$age, na.rm = T)


# Abwechsler 
df_AW_fill <- df_typ_fill %>% 
  filter(Verpflegungstyp=="Abwechsler") 

df_AW_fill <- df_AW_fill %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_AW_fill <- df_AW_fill %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_AW_fill$gender, df_AW_fill$member)

# summary(df_AW_fill$age)
# sd(df_AW_fill$age, na.rm = T)

# Einkäufer
df_EK_fill <- df_typ_fill %>% 
  filter(Verpflegungstyp=="Einkäufer") 

df_EK_fill <- df_EK_fill %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_EK_fill <- df_EK_fill %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_EK_fill$gender, df_EK_fill$member)

# summary(df_EK_fill$age)
# sd(df_EK_fill$age, na.rm = T)


# Frage 7: Direkter Gruppenvergleich ----

# Datensätze erstellen

df_VT_tho1 <- df_typ_fill%>% 
  select(c(tho_1, Verpflegungstyp_2)) %>% 
  mutate(Wahl=tho_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_1")) %>% 
  mutate(question = "... meiner Ernährungsweise für meine Gesundheit.")

df_VT_tho2 <- df_typ_fill%>% 
  select(c(tho_2, Verpflegungstyp_2)) %>% 
  mutate(Wahl=tho_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_2")) %>% 
  mutate(question = "... meiner Ernährungsgewohnheiten für die Umwelt.")

df_VT_tho3 <- df_typ_fill%>% 
  select(c(tho_3, Verpflegungstyp_2)) %>% 
  mutate(Wahl=tho_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_3")) %>% 
  mutate(question = "... der Produktion der Nahrungsmittel auf meinem Teller für die Arbeitenden in der Wertschöpfungskette.")

df_VT_tho4 <- df_typ_fill%>% 
  select(c(tho_4, Verpflegungstyp_2)) %>% 
  mutate(Wahl=tho_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_4")) %>% 
  mutate(question = "... meines Konsums von tierischen Nahrungsmitteln für die Tiere.")

df_VT_tho5 <- df_typ_fill%>% 
  select(c(tho_5, Verpflegungstyp_2)) %>% 
  mutate(Wahl=tho_5) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_5")) %>% 
  mutate(question = "... meiner Ernährung für mein Portemonnaie.")


# Datensätze zusammenführen
df_VT_tho <- bind_rows(df_VT_tho1, df_VT_tho2, df_VT_tho3, df_VT_tho4, df_VT_tho5) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(question, Zustimmung, Verpflegungstyp_2, Wahl) %>%
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
ggplot(data = df_VT_tho, aes(x=Verpflegungstyp_2, y=pct, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_VT_tho$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Einkäufer (n = 27)", "Abwechsler (n = 85)", "Selbstverpfleger (n = 268)",  "Mensagänger (n = 379)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T, nrow=1)) +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zum Essen zu?\n(Frage 7, Verpflegungstyp, n = 759)\n\nIch mache mir allgemein Gedanken über die Folgen ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)


ggsave("04_plots/Frage_7_Verpflegungstyp_2_erweitert_unterteilt_191206.pdf",
       width = 14.5,
       height = 12.5,
       dpi=600,
       units="cm",
       device=cairo_pdf)



# Frage 8: Direkter Gruppenvergleich ----

# Datensätze erstellen

df_VT_tra1 <- df_typ_fill%>% 
  select(c(tra_1, Verpflegungstyp_2)) %>% 
  mutate(Wahl=tra_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_1")) %>% 
  mutate(question = "... gesund zu leben.")

df_VT_tra2 <- df_typ_fill%>% 
  select(c(tra_2, Verpflegungstyp_2)) %>% 
  mutate(Wahl=tra_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_2")) %>% 
  mutate(question = "... mit meinem Verhalten die Umwelt möglichst wenig zu belasten.")

df_VT_tra3 <- df_typ_fill%>% 
  select(c(tra_3, Verpflegungstyp_2)) %>% 
  mutate(Wahl=tra_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_3")) %>% 
  mutate(question = "... dass die Welt sozial gerechter wird.")

df_VT_tra4 <- df_typ_fill%>% 
  select(c(tra_4, Verpflegungstyp_2)) %>% 
  mutate(Wahl=tra_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_4")) %>% 
  mutate(question = "... das die Arbeitsbedingungen für alle Menschen human sind.")

df_VT_tra5 <- df_typ_fill%>% 
  select(c(tra_5, Verpflegungstyp_2)) %>% 
  mutate(Wahl=tra_5) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_5")) %>% 
  mutate(question = "... dass Tiere möglichst artgerecht gehalten werden.")


# Datensätze zusammenführen
df_VT_tra <- bind_rows(df_VT_tra1, df_VT_tra2, df_VT_tra3, df_VT_tra4, df_VT_tra5) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(question, Zustimmung, Verpflegungstyp_2, Wahl) %>%
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
ggplot(data = df_VT_tra, aes(x=Verpflegungstyp_2, y=pct, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_VT_tra$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Einkäufer (n = 27)", "Abwechsler (n = 85)", "Selbstverpfleger (n = 268)",  "Mensagänger (n = 379)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 1.5, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zu?\n(Frage 8, Verpflegungstyp, n = 759)\n\nMir ist es allgemein wichtig, ... ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1) +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)


ggsave("04_plots/Frage_8_Verpflegungstyp_unterteilt_191206.pdf",
       width = 14.5,
       height = 11,
       dpi=600,
       units="cm",
       device=cairo_pdf)

# Frage 9: Direkter Vergleich ----
# df_typ_fill <-  df_typ_fill  %>% 
#   mutate(meat = ifelse(is.na(meat), "keine Antwort", meat)) %>% 
#   mutate(milk = ifelse(is.na(milk), "keine Antwort", milk)) %>% 
#   mutate(veget = ifelse(is.na(veget), "keine Antwort", veget)) %>% 
#   mutate(veg = ifelse(is.na(veg), "keine Antwort", veg)) %>% 
#   mutate(meat = recode_factor(meat, "7"= "2x und mehr pro Tag", "6" = "1x pro Tag", "5" = "5-6x pro Woche", "4" = "3-4x pro Woche", "3" = "1-2x pro Woche", "2" = "1-2x pro Monat", "1" = "nie")) %>% 
#   mutate(milk = recode_factor(milk, "7"= "3x und mehr pro Tag", "6" = "1-2x pro Tag", "5" = "5-6x pro Woche", "4" = "3-4x pro Woche", "3" = "1-2x pro Woche", "2" = "1-2x pro Monat", "1" = "nie")) %>% 
#   mutate(veget = recode_factor(veget, "6"= "immer", "5" = "meistens", "4" = "oft", "3" = "manchmal", "2" = "selten", "1" = "nie")) %>% 
#   mutate(veg = recode_factor(veg, "6"= "immer", "5" = "meistens", "4" = "oft", "3" = "manchmal", "2" = "selten", "1" = "nie")) 



# Fleischverpflegungstyp x = Verpflegungstyp----
df_typ_fill_meat <- df_typ_fill %>% 
  filter(!is.na(meat)) %>% 
  mutate(meat_diet_2 = case_when (meat==7 ~ "Fleischliebhaber",
                                  meat==6 ~ "Fleischliebhaber",
                                  meat==5 ~ "Fleischesser",
                                  meat==4 ~ "Fleisch-Flexitarier",
                                  meat==3 ~ "Vegi-Flexitarier",
                                  meat==2 ~ "Fleischvermeider",
                                  meat==1 ~ "Fleischvermeider", 
                                  TRUE ~ as.character(NA)))


df_typmeat <- df_typ_fill_meat %>% 
  mutate(Verpflegungstyp_3 = case_when (Verpflegungstyp=="Mensagänger" ~ "Mensagänger (n = 375)",
                                        Verpflegungstyp=="Selbstverpfleger" ~ "Selbstverpfleger (n = 268)",
                                        Verpflegungstyp=="Abwechsler" ~ "Abwechsler (n = 85)",
                                        Verpflegungstyp=="Einkäufer" ~ "Einkäufer (n = 26)",
                                        TRUE ~ as.character(NA))) %>% 
  group_by( Verpflegungstyp_3, meat_diet_2) %>% 
  summarise(typVer = n()) %>% 
  ungroup () %>% 
  group_by(Verpflegungstyp_3) %>%
  mutate(summe = sum(typVer)) %>% 
  ungroup() %>% 
  mutate(per = typVer/summe) %>%  
  mutate(meat_diet_2 = ordered(meat_diet_2, levels = c("Fleischvermeider",
                                                     "Vegi-Flexitarier",
                                                     "Fleisch-Flexitarier",
                                                     "Fleischesser",
                                                     "Fleischliebhaber"
                                                     ))) %>% 

  mutate(Verpflegungstyp_3 = ordered(Verpflegungstyp_3, levels = c("Selbstverpfleger (n = 268)",
                                                                   "Einkäufer (n = 26)",
                                                                   "Abwechsler (n = 85)",
                                                                   "Mensagänger (n = 375)"))) 

ColsPerCat = c("Fleischliebhaber" = "#DC6413", "Fleischesser" = "#Fdd200", "Fleisch-Flexitarier" = "#d3c193", "Vegi-Flexitarier" = "#AFC410", "Fleischvermeider"="#006885")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "meat_diet_2")

df_typmeat <- merge(df_typmeat, df_color, by="meat_diet_2")


# Plot ----
ggplot(df_typmeat, aes(y=per, x=Verpflegungstyp_3, fill= factor(meat_diet_2, levels = c("Fleischvermeider", "Vegi-Flexitarier", "Fleisch-Flexitarier",  "Fleischesser", "Fleischliebhaber")), color = label_color)) + 
  geom_bar(stat = "identity", position = position_stack(),  width = 0.6, color = NA) +
  scale_color_manual(values = levels(df_typmeat$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Einkäufer (n = 26)", "Abwechsler (n = 85)", "Selbstverpfleger (n = 268)",  "Mensagänger (n = 375)"))+
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  labs(x = "", y = "") +
  ggtitle("Verpflegungstyp\n(Fleischkonsumtyp, n = 754)") +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme +
  coord_flip() +
  guides(color = F, fill = guide_legend("", reverse = F, ncolum=1))

ggsave(("04_plots/Fleischkonsumtyp_verpflegungstyp_200120_vori.pdf"),
       width = 14.5,
       height = 3,
       dpi=600,
       units="cm",
       device=cairo_pdf)  


# Milch ----
df_typ_fill %<>%
  filter(!is.na(milk)) %>% 
  mutate(milk_2 = case_when (milk==7 ~ "2x und mehr pro Tag",
                                  milk==6 ~ "1x pro Tag",
                                  milk==5 ~ "5-6x pro Woche",
                                  milk==4 ~ "3-4x pro Woche",
                                  milk==3 ~  "1-2x pro Woche",
                                  milk==2 ~ "1-2x pro Monat",
                                  milk==1 ~ "nie", 
                                  TRUE ~ as.character(NA)))

df_typmilk <- df_typ_fill %>% 
  group_by( Verpflegungstyp_2, milk_2) %>% 
  summarise(milkVer = n()) %>% 
  ungroup () %>% 
  group_by(Verpflegungstyp_2) %>%
  mutate(summe = sum(milkVer)) %>% 
  ungroup() %>% 
  mutate(per = milkVer/summe) %>% 
  mutate(milk_2 = ordered(milk_2, levels = c("nie",
                                             "1-2x pro Monat",
                                             "1-2x pro Woche",
                                             "3-4x pro Woche",
                                             "5-6x pro Woche",
                                             "1x pro Tag",
                                             "2x und mehr pro Tag"
  ))) %>% 
  mutate(Verpflegungstyp_2 = ordered(Verpflegungstyp_2, levels = c("Selbstverpfleger (n = 268)",
                                                                   "Einkäufer (n = 27)",
                                                                   "Abwechsler (n = 85)",
                                                                   "Mensagänger (n = 379)"))) 

ColsPerCat = c("2x und mehr pro Tag" ="#DC6413",
               "1x pro Tag" = "#Fdd200",
               "5-6x pro Woche" = "#AFC410",
               "3-4x pro Woche" = "#006885",
               "1-2x pro Woche" = "#8AB5E1",
               "1-2x pro Monat" = "#823783",
               "nie" = "#D3C193")



# Plot ----
ggplot(df_typmilk, aes(y=per, x=Verpflegungstyp_2, fill= factor(milk_2))) + 
  geom_bar(stat = "identity", position = position_stack(),  width = 0.6) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Einkäufer (n = 27)", "Abwechsler (n = 85)", "Selbstverpfleger (n = 268)",  "Mensagänger (n = 379)"))+
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  labs(x = "", y = "") +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme +
  coord_flip() +
  guides(color = F, fill = guide_legend("", reverse = F, nrow=1))

ggsave(("04_plots/Frage_9_Milk_x_verpflegungstyp_200120_vori.pdf"),
       width = 14.5,
       height = 7,
       dpi=600,
       units="cm",
       device=cairo_pdf)  

ggplot(df_T_mi,aes(x = milk,y = value, width=0.7)) +
  geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
  labs(x = '',y='Prozent')+
  scale_fill_manual("",values = c("Mensagänger (n=379)"="#fad60d", "Selbstverpfleger (n=268)"="#505050", "Einkäufer (n=27)"="grey", "Abwechsler (n=85)"="#c5b87c"))+
  mytheme+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
            size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
  ggtitle("Ich trinke/esse Milch/Milchprodukte, Käse:\nVerpflegunstypn n = 759: Mensagänger n = 379, Selbstverpfleger n = 268, Einkäufer n = 27, Abwechsler n = 85")

ggsave(("04_plots/Frage_9_Milch_Verpflegungstyp_erweitert_191030_vori.pdf"),
         width = 17,
         height = 8,
         dpi=600,
         units="in",
         device="pdf")




  
# veget ----
  df_MG_fill_veget <- df_typ_fill %>% 
    filter(Verpflegungstyp=="Mensagänger") %>% 
    count(veget)
  
  df_SV_fill_veget <- df_typ_fill %>% 
    filter(Verpflegungstyp=="Selbstverpfleger") %>% 
    count(veget)
  
  df_EK_fill_veget <- df_typ_fill %>% 
    filter(Verpflegungstyp=="Einkäufer") %>% 
    count(veget)
    
  df_AW_fill_veget <- df_typ_fill %>% 
    filter(Verpflegungstyp=="Abwechsler") %>% 
    count(veget)
  
  
  # Prozent
  df_MG_fill_veget$pct_MG<-df_MG_fill_veget$n/sum(df_MG_fill_veget$n)
  
  df_SV_fill_veget$pct_SV<-df_SV_fill_veget$n/sum(df_SV_fill_veget$n)
  
  df_EK_fill_veget$pct_EK<-df_EK_fill_veget$n/sum(df_EK_fill_veget$n)
  
  df_AW_fill_veget$pct_AW<-df_AW_fill_veget$n/sum(df_AW_fill_veget$n)
  

# Umwandeln der Datensätze für den Plot
df_MG_veget<- melt(df_MG_fill_veget[,c("veget", "pct_MG")], id.vars = 1)
df_MG_veget$veget <- ordered(df_MG_veget$veget, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))

df_SV_veget<- melt(df_SV_fill_veget[,c("veget", "pct_SV")], id.vars = 1)
df_SV_veget$veget <- ordered(df_SV_veget$veget, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))

df_EK_veget<- melt(df_EK_fill_veget[,c("veget", "pct_EK")], id.vars = 1)
df_EK_veget$veget <- ordered(df_EK_veget$veget, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))

df_AW_veget<- melt(df_AW_fill_veget[,c("veget", "pct_AW")], id.vars = 1)
df_AW_veget$veget <- ordered(df_AW_veget$veget, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))

#Zusammenführen der Datensätze
df_T_veget <- rbind(df_MG_veget, df_SV_veget, df_AW_veget, df_EK_veget)
df_T_veget$variable <- dplyr::recode(df_T_veget$variable, "pct_MG"="Mensagänger (n=379)", "pct_SV"="Selbstverpfleger (n=268)", "pct_AW"="Abwechsler (n=85)", "pct_EK"="Einkäufer (n=27)")

#NA in 0 für Plot
df_T_veget  %<>%  
  complete(veget, variable, fill= list(per=0))

ggplot(df_T_veget,aes(x = veget,y = value, width=0.7)) +
  geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
  labs(x = '',y='Prozent')+
  scale_fill_manual("",values = c("Mensagänger (n=379)"="#fad60d", "Selbstverpfleger (n=268)"="#505050", "Einkäufer (n=27)"="grey", "Abwechsler (n=85)"="#c5b87c"))+
  mytheme+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
            size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
  ggtitle("Ich ernähre mich vegetarisch (ovo-lakto)\nVerpflegunstypn n = 759: Mensagänger n = 379, Selbstverpfleger n = 268, Einkäufer n = 27, Abwechsler n = 85")+
  ggsave(("04_plots/Frage_9_veget_Verpflegungstyp_erweitert_191607_vori.pdf"),
         width = 17,
         height = 8,
         dpi=600,
         units="in",
         device="pdf")



  
  
# veg ----
df_MG_fill_veg <- df_typ_fill %>% 
  filter(Verpflegungstyp=="Mensagänger") %>% 
  count(veg)

df_SV_fill_veg <- df_typ_fill %>% 
  filter(Verpflegungstyp=="Selbstverpfleger") %>% 
  count(veg)

df_EK_fill_veg <- df_typ_fill %>% 
  filter(Verpflegungstyp=="Einkäufer") %>% 
  count(veg)

df_AW_fill_veg <- df_typ_fill %>% 
  filter(Verpflegungstyp=="Abwechsler") %>% 
  count(veg)


# Prozent
df_MG_fill_veg$pct_MG<-df_MG_fill_veg$n/sum(df_MG_fill_veg$n)

df_SV_fill_veg$pct_SV<-df_SV_fill_veg$n/sum(df_SV_fill_veg$n)

df_EK_fill_veg$pct_EK<-df_EK_fill_veg$n/sum(df_EK_fill_veg$n)

df_AW_fill_veg$pct_AW<-df_AW_fill_veg$n/sum(df_AW_fill_veg$n)


# Umwandeln der Datensätze für den Plot
df_MG_veg<- melt(df_MG_fill_veg[,c("veg", "pct_MG")], id.vars = 1)
df_MG_veg$veg <- ordered(df_MG_veg$veg, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))

df_SV_veg<- melt(df_SV_fill_veg[,c("veg", "pct_SV")], id.vars = 1)
df_SV_veg$veg <- ordered(df_SV_veg$veg, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))

df_EK_veg<- melt(df_EK_fill_veg[,c("veg", "pct_EK")], id.vars = 1)
df_EK_veg$veg <- ordered(df_EK_veg$veg, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))

df_AW_veg<- melt(df_AW_fill_veg[,c("veg", "pct_AW")], id.vars = 1)
df_AW_veg$veg <- ordered(df_AW_veg$veg, levels = c("immer", "meistens", "oft", "manchmal", "selten", "nie", "keine Antwort"))

#Zusammenführen der Datensätze
df_T_veg <- rbind(df_MG_veg, df_SV_veg, df_EK_veg, df_AW_veg)
df_T_veg$variable <- dplyr::recode(df_T_veg$variable, "pct_MG"="Mensagänger (n=379)", "pct_SV"="Selbstverpfleger (n=268)", "pct_EK"="Einkäufer (n=18)", "pct_AW"="Abwechsler (n=101)")

#NA in 0 für Plot
df_T_veg  %<>%  
  complete(veg, variable, fill= list(per=0))

ggplot(df_T_veg,aes(x = veg,y = value, width=0.7)) +
  geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
  labs(x = '',y='Prozent')+
  scale_fill_manual("",values = c("Mensagänger (n=379)"="#fad60d", "Selbstverpfleger (n=268)"="#505050", "Einkäufer (n=18)"="grey",  "Abwechsler (n=101)"="#c5b87c"))+
  mytheme+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
            size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
  ggtitle("Ich ernähre mich rein pflanzlich (vegan):\nVerpflegunstypn n = 759: Mensagänger n = 379, Selbstverpfleger n = 268, Einkäufer n = 27, Abwechsler n = 85") +
  ggsave(("04_plots/Frage_9_veg_Verpflegungstyp_erweitert_191607_vori.pdf"),
         width = 17,
         height = 8,
         dpi=600,
         units="in",
         device="pdf")
