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

df_tot_age_gr_fill <- read_delim("2019_ZHAW_vonRickenbach_cleaned_recorded_survey_dataset_NOVANIMAL_anonym.csv", delim=';', locale = locale(encoding = 'ISO-8859-2')) %>% 
  drop_na(age_groups) %>% 
  filter(fill==0) #763 Beobachtungen

# # Datensatz bearbeiten-----
 df_tot_age_gr_fill <- df_tot_age_gr_fill  %>%
#   mutate(meat = ifelse(is.na(meat), "keine Antwort", meat)) %>%
#   mutate(milk = ifelse(is.na(milk), "keine Antwort", milk)) %>%
#   mutate(veget = ifelse(is.na(veget), "keine Antwort", veget)) %>%
#   mutate(veg = ifelse(is.na(veg), "keine Antwort", veg)) %>%
#   mutate(meat = recode_factor(meat, "7"= "2x und mehr pro Tag", "6" = "1x pro Tag", "5" = "5-6x pro Woche", "4" = "3-4x pro Woche", "3" = "1-2x pro Woche", "2" = "1-2x pro Monat", "1" = "nie")) %>%
#   mutate(milk = recode_factor(milk, "7"= "3x und mehr pro Tag", "6" = "1-2x pro Tag", "5" = "5-6x pro Woche", "4" = "3-4x pro Woche", "3" = "1-2x pro Woche", "2" = "1-2x pro Monat", "1" = "nie")) %>%
#   mutate(veget = recode_factor(veget, "6"= "immer", "5" = "meistens", "4" = "oft", "3" = "manchmal", "2" = "selten", "1" = "nie")) %>%
#   mutate(veg = recode_factor(veg, "6"= "immer", "5" = "meistens", "4" = "oft", "3" = "manchmal", "2" = "selten", "1" = "nie")) %>%
   mutate(age_groups = recode(age_groups, "17- bis 25-jaehrig" = "17- bis 25-jährig", "26- bis 34-jaehrig"="26- bis 34-jährig", "35- bis 49-jaehrig"="35- bis 49-jährig", "50- bis 64-jaehrig"="50- bis 64-jährig")) %>%
  mutate(Verpflegungstyp = recode(Verpflegungstyp, "Mensagaenger"="Mensagänger", "Einkaeufer"="Einkäufer"))


df_tot_age_gr_mae_fill <- df_tot_age_gr_fill %>% filter(gender=="Mann") # 412 Fragebogen

df_tot_age_gr_fr_fill <- df_tot_age_gr_fill %>% filter(gender=="Frau") # 334 Fragebogen


# Maenner der verschiednen Altersgruppen Teilstichproben 

df_m1725 <- df_tot_age_gr_fill %>% 
  filter(gender=="Mann" & age_groups=="17- bis 25-jährig")

df_m2634 <- df_tot_age_gr_fill %>% 
  filter(gender=="Mann" & age_groups=="26- bis 34-jährig")

df_m3549 <- df_tot_age_gr_fill %>% 
  filter(gender=="Mann" & age_groups=="35- bis 49-jährig")

df_m5064 <- df_tot_age_gr_fill %>% 
  filter(gender=="Mann" & age_groups=="50- bis 64-jährig")

# Hochschulzugehoerigkeit
table(df_m1725$member, exclude=NULL)
round(table(df_m1725$member, exclude=NULL)/nrow(df_m1725)*100,1)

table(df_m2634$member, exclude=NULL)
round(table(df_m2634$member, exclude=NULL)/nrow(df_m2634)*100,1)

table(df_m3549$member, exclude=NULL)
round(table(df_m3549$member, exclude=NULL)/nrow(df_m3549)*100,1)

table(df_m5064$member, exclude=NULL)
round(table(df_m5064$member, exclude=NULL)/nrow(df_m5064)*100,1)




# Frauen der verschiednen Altersgruppen Teilstichproben 

df_f1725 <- df_tot_age_gr_fill %>% 
  filter(gender=="Frau" & age_groups=="17- bis 25-jährig")

df_f2634 <- df_tot_age_gr_fill %>% 
  filter(gender=="Frau" & age_groups=="26- bis 34-jährig")

df_f3549 <- df_tot_age_gr_fill %>% 
  filter(gender=="Frau" & age_groups=="35- bis 49-jährig")

df_f5064 <- df_tot_age_gr_fill %>% 
  filter(gender=="Frau" & age_groups=="50- bis 64-jährig")


# Hochschulzugehoerigkeit
table(df_f1725$member, exclude=NULL)
round(table(df_f1725$member, exclude=NULL)/nrow(df_f1725)*100,1)

table(df_f2634$member, exclude=NULL)
round(table(df_f2634$member, exclude=NULL)/nrow(df_f2634)*100,1)

table(df_f3549$member, exclude=NULL)
round(table(df_f3549$member, exclude=NULL)/nrow(df_f3549)*100,1)

table(df_f5064$member, exclude=NULL)
round(table(df_f5064$member, exclude=NULL)/nrow(df_f5064)*100,1)




# Frage 7: Direkter Gruppenvergleich Männer----

# Datensätze erstellen

df_VT_tho1 <- df_tot_age_gr_mae_fill%>% 
  select(c(tho_1, age_groups)) %>% 
  mutate(Wahl=tho_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_1")) %>% 
  mutate(question = "meiner Ernährungsweise für meine Gesundheit.")

df_VT_tho2 <- df_tot_age_gr_mae_fill%>% 
  select(c(tho_2, age_groups)) %>% 
  mutate(Wahl=tho_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_2")) %>% 
  mutate(question = "meiner Ernährungsgewohnheiten für die Umwelt.")

df_VT_tho3 <- df_tot_age_gr_mae_fill%>% 
  select(c(tho_3, age_groups)) %>% 
  mutate(Wahl=tho_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_3")) %>% 
  mutate(question = "der Produktion der Nahrungsmittel auf meinem Teller für die Arbeitenden in der Wertschöpfungskette.")

df_VT_tho4 <- df_tot_age_gr_mae_fill%>% 
  select(c(tho_4, age_groups)) %>% 
  mutate(Wahl=tho_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_4")) %>% 
  mutate(question = "meines Konsums von tierischen Nahrungsmitteln für die Tiere.")

df_VT_tho5 <- df_tot_age_gr_mae_fill%>% 
  select(c(tho_5, age_groups)) %>% 
  mutate(Wahl=tho_5) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_5")) %>% 
  mutate(question = "meiner Ernährung für mein Portemonnaie.")


# Datensätze zusammenführen
df_VT_tho <- bind_rows(df_VT_tho1, df_VT_tho2, df_VT_tho3, df_VT_tho4, df_VT_tho5) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(question, Zustimmung, age_groups, Wahl) %>%
  summarise(tot_answ = n()) %>%  
  mutate(pct = (tot_answ/sum(tot_answ))) 



#add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_VT_tho <- merge(df_VT_tho, df_color, by="Wahl")


df_VT_tho %<>% mutate(age_groups = recode(age_groups, 
                                          "17- bis 25-jährig" = "17- bis 25-jährig (n = 208)",
                                          "26- bis 34-jährig" = "26- bis 34-jährig (n = 130)",
                                          "35- bis 49-jährig" = "35- bis 49-jährig (n = 51)",
                                          "50- bis 64-jährig" = "50- bis 64-jährig (n = 23)"))

# für Plot Sortieren Augrund der Gesamtzustimmung
df_VT_tho$question <- reorder(df_VT_tho$question, -df_VT_tho$Zustimmung)

## plot ----
ggplot(df_VT_tho, aes(x=age_groups, y=pct, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_VT_tho$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("50- bis 64-jährig (n = 23)",
                            "35- bis 49-jährig (n = 51)",
                            "26- bis 34-jährig (n = 130)",
                            "17- bis 25-jährig (n = 208)"
                            ))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zum Essen zu?\n(Frage 7, Männer nach Altersgruppen, n = 412)\n\nIch mache mir allgemein Gedanken über die Folgen ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)



ggsave("04_plots/Frage_7_Altersgruppen_M_191105.pdf",
       width = 14.5,
       height = 12.5,
       dpi=600,
       units="cm",
       device=cairo_pdf)


# Frage 7: Direkter Gruppenvergleich Frauen----

# Datensätze erstellen

df_VT_tho1 <- df_tot_age_gr_fr_fill%>% 
  select(c(tho_1, age_groups)) %>% 
  mutate(Wahl=tho_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_1")) %>% 
  mutate(question = "meiner Ernährungsweise für meine Gesundheit.")

df_VT_tho2 <- df_tot_age_gr_fr_fill%>% 
  select(c(tho_2, age_groups)) %>% 
  mutate(Wahl=tho_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_2")) %>% 
  mutate(question = "meiner Ernährungsgewohnheiten für die Umwelt.")

df_VT_tho3 <- df_tot_age_gr_fr_fill%>% 
  select(c(tho_3, age_groups)) %>% 
  mutate(Wahl=tho_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_3")) %>% 
  mutate(question = "der Produktion der Nahrungsmittel auf meinem Teller für die Arbeitenden in der Wertschöpfungskette.")

df_VT_tho4 <- df_tot_age_gr_fr_fill%>% 
  select(c(tho_4, age_groups)) %>% 
  mutate(Wahl=tho_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_4")) %>% 
  mutate(question = "meines Konsums von tierischen Nahrungsmitteln für die Tiere.")

df_VT_tho5 <- df_tot_age_gr_fr_fill%>% 
  select(c(tho_5, age_groups)) %>% 
  mutate(Wahl=tho_5) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_5")) %>% 
  mutate(question = "meiner Ernährung für mein Portemonnaie.")


# Datensätze zusammenführen
df_VT_tho <- bind_rows(df_VT_tho1, df_VT_tho2, df_VT_tho3, df_VT_tho4, df_VT_tho5) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(question, Zustimmung, age_groups, Wahl) %>%
  summarise(tot_answ = n()) %>%  
  mutate(pct = (tot_answ/sum(tot_answ))) 


#add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_VT_tho <- merge(df_VT_tho, df_color, by="Wahl")


df_VT_tho %<>% mutate(age_groups = recode(age_groups, 
                                          "17- bis 25-jährig" = "17- bis 25-jährig (n = 178)",
                                          "26- bis 34-jährig" = "26- bis 34-jährig (n = 86)",
                                          "35- bis 49-jährig" = "35- bis 49-jährig (n = 44)",
                                          "50- bis 64-jährig" = "50- bis 64-jährig (n = 26)"))

# für Plot Sortieren Augrund der Gesamtzustimmung
df_VT_tho$question <- reorder(df_VT_tho$question, -df_VT_tho$Zustimmung)

## plot ----
ggplot(df_VT_tho, aes(x=age_groups, y=pct, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_VT_tho$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("50- bis 64-jährig (n = 26)",
                            "35- bis 49-jährig (n = 44)",
                            "26- bis 34-jährig (n = 86)",
                            "17- bis 25-jährig (n = 178)"
  ))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zum Essen zu?\n(Frage 7, Frauen nach Altersgruppen, n = 334)\n\nIch mache mir allgemein Gedanken über die Folgen ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)

ggsave("04_plots/Frage_7_Altersgruppen_F_191030.pdf",
       width = 14.5,
       height = 12.5,
       dpi=600,
       units="cm",
       device=cairo_pdf)

# Frage 7: Vergleich Geschlechter und Alter für einzellne Fragen ----

# Gesundheit ----
df_VT_tho1 <- df_tot_age_gr_fill%>% 
  select(c(tho_1, age_groups, gender)) %>% 
  filter(!is.na(age_groups)) %>% 
  mutate(Wahl=tho_1) %>% 
  select(-c("tho_1")) %>% 
  mutate(question = "meiner Ernährungsweise für meine Gesundheit.") %>% 
  filter(gender == "Mann" | gender == "Frau") %>% 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(Wahl, gender, age_groups) %>% 
  summarise(agegender = n()) %>% 
  ungroup () %>% 
  group_by(age_groups, gender) %>%
  mutate(summe = sum(agegender)) %>% 
  ungroup() %>% 
  mutate(per = agegender/summe) %>% 
  mutate(gender = case_when (gender=="Frau" & age_groups == "17- bis 25-jährig" ~ "Frau\n(n = 178)",
                             gender=="Frau" & age_groups == "26- bis 34-jährig" ~ "Frau\n(n = 86)",
                             gender=="Frau" & age_groups == "35- bis 49-jährig" ~ "Frau\n(n = 44)",
                             gender=="Frau" & age_groups == "50- bis 64-jährig" ~ "Frau\n(n = 26)",
                             gender=="Mann" & age_groups == "17- bis 25-jährig" ~ "Mann\n(n = 208)",
                             gender=="Mann" & age_groups == "26- bis 34-jährig" ~ "Mann\n(n = 130)",
                             gender=="Mann" & age_groups == "35- bis 49-jährig" ~ "Mann\n(n = 51)",
                             gender=="Mann" & age_groups == "50- bis 64-jährig" ~ "Mann\n(n = 23)",
                             TRUE ~ as.character(NA)))


#add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_VT_tho1 <- merge(df_VT_tho1, df_color, by="Wahl") 

# Plot ----
ggplot(df_VT_tho1, aes(x = gender, y = per, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_VT_tho1$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  facet_wrap(~age_groups, nrow = NULL, ncol = 4, scales = "free") +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  ylab("") +
  xlab("") +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zum Essen zu?\n(Frage 7, Altersgruppen und Geschlecht, n = 746)\n\nIch mache mir allgemein Gedanken über die Folgen meiner Ernährungsweise für meine Gesundheit.")+
  guides(color = F, fill = guide_legend("", reverse = T, nrow=1)) +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme

ggsave(("04_plots/Frage_7_gesundheit_altersgruppen_geschlecht_191209_vori.pdf"),
       width = 14.5,
       height = 7,
       dpi=600,
       units="cm",
       device=cairo_pdf)

# Umwelt ----
df_VT_tho2 <- df_tot_age_gr_fill %>% 
  select(c(tho_2, age_groups, gender)) %>% 
  filter(!is.na(age_groups)) %>% 
  mutate(Wahl=tho_2) %>% 
  select(-c("tho_2")) %>% 
  mutate(question = "meiner Ernährungsgewohnheiten für die Umwelt.") %>% 
  filter(gender == "Mann" | gender == "Frau") %>% 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(Wahl, gender, age_groups) %>% 
  summarise(agegender = n()) %>% 
  ungroup () %>% 
  group_by(age_groups, gender) %>%
  mutate(summe = sum(agegender)) %>% 
  ungroup() %>% 
  mutate(per = agegender/summe) %>% 
  mutate(gender = case_when (gender=="Frau" & age_groups == "17- bis 25-jährig" ~ "Frau\n(n = 178)",
                             gender=="Frau" & age_groups == "26- bis 34-jährig" ~ "Frau\n(n = 86)",
                             gender=="Frau" & age_groups == "35- bis 49-jährig" ~ "Frau\n(n = 44)",
                             gender=="Frau" & age_groups == "50- bis 64-jährig" ~ "Frau\n(n = 26)",
                             gender=="Mann" & age_groups == "17- bis 25-jährig" ~ "Mann\n(n = 208)",
                             gender=="Mann" & age_groups == "26- bis 34-jährig" ~ "Mann\n(n = 130)",
                             gender=="Mann" & age_groups == "35- bis 49-jährig" ~ "Mann\n(n = 51)",
                             gender=="Mann" & age_groups == "50- bis 64-jährig" ~ "Mann\n(n = 23)",
                             TRUE ~ as.character(NA)))


#add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_VT_tho2 <- merge(df_VT_tho2, df_color, by="Wahl") 

# Plot ----
ggplot(df_VT_tho2, aes(x = gender, y = per, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_VT_tho2$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  facet_wrap(~age_groups, nrow = NULL, ncol = 4, scales = "free") +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  ylab("") +
  xlab("") +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zum Essen zu?\n(Frage 7, Altersgruppen und Geschlecht, n = 746)\n\nIch mache mir allgemein Gedanken über die Folgen meiner Ernährungsgewohnheiten für die Umwelt.")+
  guides(color = F, fill = guide_legend("", reverse = T, nrow=1)) +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme

ggsave(("04_plots/Frage_7_umwelt_altersgruppen_geschlecht_191209_vori.pdf"),
       width = 14.5,
       height = 7,
       dpi=600,
       units="cm",
       device=cairo_pdf)

# Wertschöpfungskette ----
df_VT_tho3 <- df_tot_age_gr_fill %>% 
  select(c(tho_3, age_groups, gender)) %>% 
  filter(!is.na(age_groups)) %>% 
  mutate(Wahl=tho_3) %>% 
  select(-c("tho_3")) %>% 
  mutate(question = "der Produktion der Nahrungsmittel auf meinem Teller für die Arbeitenden in der Wertschöpfungskette.") %>% 
  filter(gender == "Mann" | gender == "Frau") %>% 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(Wahl, gender, age_groups) %>% 
  summarise(agegender = n()) %>% 
  ungroup () %>% 
  group_by(age_groups, gender) %>%
  mutate(summe = sum(agegender)) %>% 
  ungroup() %>% 
  mutate(per = agegender/summe) %>% 
  mutate(gender = case_when (gender=="Frau" & age_groups == "17- bis 25-jährig" ~ "Frau\n(n = 178)",
                             gender=="Frau" & age_groups == "26- bis 34-jährig" ~ "Frau\n(n = 86)",
                             gender=="Frau" & age_groups == "35- bis 49-jährig" ~ "Frau\n(n = 44)",
                             gender=="Frau" & age_groups == "50- bis 64-jährig" ~ "Frau\n(n = 26)",
                             gender=="Mann" & age_groups == "17- bis 25-jährig" ~ "Mann\n(n = 208)",
                             gender=="Mann" & age_groups == "26- bis 34-jährig" ~ "Mann\n(n = 130)",
                             gender=="Mann" & age_groups == "35- bis 49-jährig" ~ "Mann\n(n = 51)",
                             gender=="Mann" & age_groups == "50- bis 64-jährig" ~ "Mann\n(n = 23)",
                             TRUE ~ as.character(NA)))


#add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_VT_tho3 <- merge(df_VT_tho3, df_color, by="Wahl") 

# Plot ----
ggplot(df_VT_tho3, aes(x = gender, y = per, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_VT_tho3$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  facet_wrap(~age_groups, nrow = NULL, ncol = 4, scales = "free") +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  ylab("") +
  xlab("") +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zum Essen zu?\n(Frage 7, Altersgruppen und Geschlecht, n = 746)\n\nIch mache mir allgemein Gedanken über die Folgen der Produktion der Nahrungsmittel\nauf meinem Teller für die Arbeitenden in der Wertschöpfungskette.")+
  guides(color = F, fill = guide_legend("", reverse = T, nrow=1)) +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme

ggsave(("04_plots/Frage_7_wertschoepfung_altersgruppen_geschlecht_191209_vori.pdf"),
       width = 14.5,
       height = 7,
       dpi=600,
       units="cm",
       device=cairo_pdf)

# Tiere ----
df_VT_tho4 <- df_tot_age_gr_fill %>% 
  select(c(tho_4, age_groups, gender)) %>% 
  filter(!is.na(age_groups)) %>% 
  mutate(Wahl=tho_4) %>% 
  select(-c("tho_4")) %>% 
  mutate(question = "meines Konsums von tierischen Nahrungsmitteln für die Tiere.") %>% 
  filter(gender == "Mann" | gender == "Frau") %>% 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(Wahl, gender, age_groups) %>% 
  summarise(agegender = n()) %>% 
  ungroup () %>% 
  group_by(age_groups, gender) %>%
  mutate(summe = sum(agegender)) %>% 
  ungroup() %>% 
  mutate(per = agegender/summe) %>% 
  mutate(gender = case_when (gender=="Frau" & age_groups == "17- bis 25-jährig" ~ "Frau\n(n = 178)",
                             gender=="Frau" & age_groups == "26- bis 34-jährig" ~ "Frau\n(n = 86)",
                             gender=="Frau" & age_groups == "35- bis 49-jährig" ~ "Frau\n(n = 44)",
                             gender=="Frau" & age_groups == "50- bis 64-jährig" ~ "Frau\n(n = 26)",
                             gender=="Mann" & age_groups == "17- bis 25-jährig" ~ "Mann\n(n = 208)",
                             gender=="Mann" & age_groups == "26- bis 34-jährig" ~ "Mann\n(n = 130)",
                             gender=="Mann" & age_groups == "35- bis 49-jährig" ~ "Mann\n(n = 51)",
                             gender=="Mann" & age_groups == "50- bis 64-jährig" ~ "Mann\n(n = 23)",
                             TRUE ~ as.character(NA)))


#add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_VT_tho4 <- merge(df_VT_tho4, df_color, by="Wahl") 

# Plot ----
ggplot(df_VT_tho4, aes(x = gender, y = per, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_VT_tho4$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  facet_wrap(~age_groups, nrow = NULL, ncol = 4, scales = "free") +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  ylab("") +
  xlab("") +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zum Essen zu?\n(Frage 7, Altersgruppen und Geschlecht, n = 746)\n\nIch mache mir allgemein Gedanken über die Folgen meines Konsums von tierischen Nahrungsmitteln für die Tiere.")+
  guides(color = F, fill = guide_legend("", reverse = T, nrow=1)) +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme

ggsave(("04_plots/Frage_7_tiere_altersgruppen_geschlecht_191209_vori.pdf"),
       width = 14.5,
       height = 7,
       dpi=600,
       units="cm",
       device=cairo_pdf)

# Portemonnaie ----
df_VT_tho5 <- df_tot_age_gr_fill %>% 
  select(c(tho_5, age_groups, gender)) %>% 
  filter(!is.na(age_groups)) %>% 
  mutate(Wahl=tho_5) %>% 
  select(-c("tho_5")) %>% 
  mutate(question ="meiner Ernährung für mein Portemonnaie.") %>% 
  filter(gender == "Mann" | gender == "Frau") %>% 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(Wahl, gender, age_groups) %>% 
  summarise(agegender = n()) %>% 
  ungroup () %>% 
  group_by(age_groups, gender) %>%
  mutate(summe = sum(agegender)) %>% 
  ungroup() %>% 
  mutate(per = agegender/summe) %>% 
  mutate(gender = case_when (gender=="Frau" & age_groups == "17- bis 25-jährig" ~ "Frau\n(n = 178)",
                             gender=="Frau" & age_groups == "26- bis 34-jährig" ~ "Frau\n(n = 86)",
                             gender=="Frau" & age_groups == "35- bis 49-jährig" ~ "Frau\n(n = 44)",
                             gender=="Frau" & age_groups == "50- bis 64-jährig" ~ "Frau\n(n = 26)",
                             gender=="Mann" & age_groups == "17- bis 25-jährig" ~ "Mann\n(n = 208)",
                             gender=="Mann" & age_groups == "26- bis 34-jährig" ~ "Mann\n(n = 130)",
                             gender=="Mann" & age_groups == "35- bis 49-jährig" ~ "Mann\n(n = 51)",
                             gender=="Mann" & age_groups == "50- bis 64-jährig" ~ "Mann\n(n = 23)",
                             TRUE ~ as.character(NA)))


#add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_VT_tho5 <- merge(df_VT_tho5, df_color, by="Wahl") 

# Plot ----
ggplot(df_VT_tho5, aes(x = gender, y = per, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_VT_tho5$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  facet_wrap(~age_groups, nrow = NULL, ncol = 4, scales = "free") +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  ylab("") +
  xlab("") +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zum Essen zu?\n(Frage 7, Altersgruppen und Geschlecht, n = 746)\n\nIch mache mir allgemein Gedanken über die Folgen meiner Ernährung für mein Portemonnaie.")+
  guides(color = F, fill = guide_legend("", reverse = T, nrow=1)) +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme

ggsave(("04_plots/Frage_7_portemonnaie_altersgruppen_geschlecht_191209_vori.pdf"),
       width = 14.5,
       height = 7,
       dpi=600,
       units="cm",
       device=cairo_pdf)





# Frage 8: Direkter Gruppenvergleich Männer----

# Datensätze erstellen

df_VT_tra1 <- df_tot_age_gr_mae_fill%>% 
  select(c(tra_1, age_groups)) %>% 
  mutate(Wahl=tra_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_1")) %>% 
  mutate(question = "gesund zu leben.")

df_VT_tra2 <- df_tot_age_gr_mae_fill%>% 
  select(c(tra_2, age_groups)) %>% 
  mutate(Wahl=tra_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_2")) %>% 
  mutate(question = "mit meinem Verhalten die Umwelt möglichst wenig zu belasten.")

df_VT_tra3 <- df_tot_age_gr_mae_fill%>% 
  select(c(tra_3, age_groups)) %>% 
  mutate(Wahl=tra_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_3")) %>% 
  mutate(question = "dass die Welt sozial gerechter wird.")

df_VT_tra4 <- df_tot_age_gr_mae_fill%>% 
  select(c(tra_4, age_groups)) %>% 
  mutate(Wahl=tra_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_4")) %>% 
  mutate(question = "das die Arbeitsbedingungen für alle Menschen human sind.")

df_VT_tra5 <- df_tot_age_gr_mae_fill%>% 
  select(c(tra_5, age_groups)) %>% 
  mutate(Wahl=tra_5) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_5")) %>% 
  mutate(question = "dass Tiere möglichst artgerecht gehalten werden.")


# Datensätze zusammenführen
df_VT_tra <- bind_rows(df_VT_tra1, df_VT_tra2, df_VT_tra3, df_VT_tra4, df_VT_tra5) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(question, Zustimmung, age_groups, Wahl) %>%
  summarise(tot_answ = n()) %>%  
  mutate(pct = (tot_answ/sum(tot_answ))) 
# Sortieren Augrund der Gesamtzustimmung
df_VT_tra <- df_VT_tra[order(df_VT_tra$Zustimmung),]


#add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_VT_tra <- merge(df_VT_tra, df_color, by="Wahl")


df_VT_tra %<>% mutate(age_groups = recode(age_groups, 
                                          "17- bis 25-jährig" = "17- bis 25-jährig (n = 208)",
                                          "26- bis 34-jährig" = "26- bis 34-jährig (n = 130)",
                                          "35- bis 49-jährig" = "35- bis 49-jährig (n = 51)",
                                          "50- bis 64-jährig" = "50- bis 64-jährig (n = 23)"))
# für Plot Sortieren Augrund der Gesamtzustimmung
df_VT_tra$question <- reorder(df_VT_tra$question, -df_VT_tra$Zustimmung)


## plot ----
ggplot(df_VT_tra, aes(x=age_groups, y=pct, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_VT_tra$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("50- bis 64-jährig (n = 23)",
                            "35- bis 49-jährig (n = 51)",
                            "26- bis 34-jährig (n = 130)",
                            "17- bis 25-jährig (n = 208)"
  ))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zu?\n(Frage 8, Männer nach Altersgruppen, n = 412)\n\nMir ist es allgemein wichtig, ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)

ggsave("04_plots/Frage_8_Altersgruppen_M_193010.pdf",
       width = 14.5,
       height = 12.5,
       dpi=600,
       units="cm",
       device=cairo_pdf)

# Frage 8: Direkter Gruppenvergleich Frauen----

df_VT_tra1 <- df_tot_age_gr_fr_fill%>% 
  select(c(tra_1, age_groups)) %>% 
  mutate(Wahl=tra_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_1")) %>% 
  mutate(question = "gesund zu leben.")

df_VT_tra2 <- df_tot_age_gr_fr_fill%>% 
  select(c(tra_2, age_groups)) %>% 
  mutate(Wahl=tra_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_2")) %>% 
  mutate(question = "mit meinem Verhalten die Umwelt möglichst wenig zu belasten.")

df_VT_tra3 <- df_tot_age_gr_fr_fill%>% 
  select(c(tra_3, age_groups)) %>% 
  mutate(Wahl=tra_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_3")) %>% 
  mutate(question = "dass die Welt sozial gerechter wird.")

df_VT_tra4 <- df_tot_age_gr_fr_fill%>% 
  select(c(tra_4, age_groups)) %>% 
  mutate(Wahl=tra_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_4")) %>% 
  mutate(question = "das die Arbeitsbedingungen für alle Menschen human sind.")

df_VT_tra5 <- df_tot_age_gr_fr_fill%>% 
  select(c(tra_5, age_groups)) %>% 
  mutate(Wahl=tra_5) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_5")) %>% 
  mutate(question = "dass Tiere möglichst artgerecht gehalten werden.")


# Datensätze zusammenführen
df_VT_tra <- bind_rows(df_VT_tra1, df_VT_tra2, df_VT_tra3, df_VT_tra4, df_VT_tra5) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(question, Zustimmung, age_groups, Wahl) %>%
  summarise(tot_answ = n()) %>%  
  mutate(pct = (tot_answ/sum(tot_answ))) 

#add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_VT_tra <- merge(df_VT_tra, df_color, by="Wahl")


df_VT_tra %<>% mutate(age_groups = recode(age_groups, 
                                          "17- bis 25-jährig" = "17- bis 25-jährig (n = 178)",
                                          "26- bis 34-jährig" = "26- bis 34-jährig (n = 86)",
                                          "35- bis 49-jährig" = "35- bis 49-jährig (n = 44)",
                                          "50- bis 64-jährig" = "50- bis 64-jährig (n = 26)"))

# für Plot Sortieren Augrund der Gesamtzustimmung
df_VT_tra$question <- reorder(df_VT_tra$question, -df_VT_tra$Zustimmung)

## plot ----
ggplot(df_VT_tra, aes(x=age_groups, y=pct, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_VT_tra$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("50- bis 64-jährig (n = 26)",
                            "35- bis 49-jährig (n = 44)",
                            "26- bis 34-jährig (n = 86)",
                            "17- bis 25-jährig (n = 178)"
  ))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zu?\n(Frage 8, Frauen nach Altersgruppen, n = 334)\n\nMir ist es allgemein wichtig, ...")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)

ggsave("04_plots/Frage_8_Altersgruppen_F_193010.pdf",
       width = 14.5,
       height = 12.5,
       dpi=600,
       units="cm",
       device=cairo_pdf)

# Frage 8: Vergleich Geschlechter und Alter für einzellne Fragen ----

# Gesundheit ----
df_VT_tra1 <- df_tot_age_gr_fill%>% 
  select(c(tra_1, age_groups, gender)) %>% 
  filter(!is.na(age_groups)) %>% 
  mutate(Wahl=tra_1) %>% 
  select(-c("tra_1")) %>% 
  mutate(question = "gesund zu leben.") %>% 
  filter(gender == "Mann" | gender == "Frau") %>% 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(Wahl, gender, age_groups) %>% 
  summarise(agegender = n()) %>% 
  ungroup () %>% 
  group_by(age_groups, gender) %>%
  mutate(summe = sum(agegender)) %>% 
  ungroup() %>% 
  mutate(per = agegender/summe) %>% 
  mutate(gender = case_when (gender=="Frau" & age_groups == "17- bis 25-jährig" ~ "Frau\n(n = 178)",
                             gender=="Frau" & age_groups == "26- bis 34-jährig" ~ "Frau\n(n = 86)",
                             gender=="Frau" & age_groups == "35- bis 49-jährig" ~ "Frau\n(n = 44)",
                             gender=="Frau" & age_groups == "50- bis 64-jährig" ~ "Frau\n(n = 26)",
                             gender=="Mann" & age_groups == "17- bis 25-jährig" ~ "Mann\n(n = 208)",
                             gender=="Mann" & age_groups == "26- bis 34-jährig" ~ "Mann\n(n = 130)",
                             gender=="Mann" & age_groups == "35- bis 49-jährig" ~ "Mann\n(n = 51)",
                             gender=="Mann" & age_groups == "50- bis 64-jährig" ~ "Mann\n(n = 23)",
                             TRUE ~ as.character(NA)))


#add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_VT_tra1 <- merge(df_VT_tra1, df_color, by="Wahl") 

# Plot ----
ggplot(df_VT_tra1, aes(x = gender, y = per, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_VT_tra1$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  facet_wrap(~age_groups, nrow = NULL, ncol = 4, scales = "free") +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  ylab("") +
  xlab("") +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zu?\n(Frage 8, Altersgruppen und Geschlecht, n = 746)\n\nMir ist es allgemein wichtig, gesund zu leben.")+
  guides(color = F, fill = guide_legend("", reverse = T, nrow=1)) +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme

ggsave(("04_plots/Frage_8_gesundheit_altersgruppen_geschlecht_191209_vori.pdf"),
       width = 14.5,
       height = 7,
       dpi=600,
       units="cm",
       device=cairo_pdf)

# Umwelt ----
df_VT_tra2 <- df_tot_age_gr_fill %>% 
  select(c(tra_2, age_groups, gender)) %>% 
  filter(!is.na(age_groups)) %>% 
  mutate(Wahl=tra_2) %>% 
  select(-c("tra_2")) %>% 
  mutate(question = "mit meinem Verhalten die Umwelt möglichst wenig zu belasten.") %>% 
  filter(gender == "Mann" | gender == "Frau") %>% 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(Wahl, gender, age_groups) %>% 
  summarise(agegender = n()) %>% 
  ungroup () %>% 
  group_by(age_groups, gender) %>%
  mutate(summe = sum(agegender)) %>% 
  ungroup() %>% 
  mutate(per = agegender/summe) %>% 
  mutate(gender = case_when (gender=="Frau" & age_groups == "17- bis 25-jährig" ~ "Frau\n(n = 178)",
                             gender=="Frau" & age_groups == "26- bis 34-jährig" ~ "Frau\n(n = 86)",
                             gender=="Frau" & age_groups == "35- bis 49-jährig" ~ "Frau\n(n = 44)",
                             gender=="Frau" & age_groups == "50- bis 64-jährig" ~ "Frau\n(n = 26)",
                             gender=="Mann" & age_groups == "17- bis 25-jährig" ~ "Mann\n(n = 208)",
                             gender=="Mann" & age_groups == "26- bis 34-jährig" ~ "Mann\n(n = 130)",
                             gender=="Mann" & age_groups == "35- bis 49-jährig" ~ "Mann\n(n = 51)",
                             gender=="Mann" & age_groups == "50- bis 64-jährig" ~ "Mann\n(n = 23)",
                             TRUE ~ as.character(NA)))


#add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_VT_tra2 <- merge(df_VT_tra2, df_color, by="Wahl") 

# Plot ----
ggplot(df_VT_tra2, aes(x = gender, y = per, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_VT_tra2$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  facet_wrap(~age_groups, nrow = NULL, ncol = 4, scales = "free") +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  ylab("") +
  xlab("") +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zu?\n(Frage 8, Altersgruppen und Geschlecht, n = 746)\n\nMir ist es allgemein wichtig, mit meinem Verhalten die Umwelt möglichst wenig zu belasten.")+
  guides(color = F, fill = guide_legend("", reverse = T, nrow=1)) +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme

ggsave(("04_plots/Frage_8_umwelt_altersgruppen_geschlecht_191209_vori.pdf"),
       width = 14.5,
       height = 7,
       dpi=600,
       units="cm",
       device=cairo_pdf)

# Sozial ----
df_VT_tra3 <- df_tot_age_gr_fill %>% 
  select(c(tra_3, age_groups, gender)) %>% 
  filter(!is.na(age_groups)) %>% 
  mutate(Wahl=tra_3) %>% 
  select(-c("tra_3")) %>% 
  mutate(question = "dass die Welt sozial gerechter wird.") %>% 
  filter(gender == "Mann" | gender == "Frau") %>% 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(Wahl, gender, age_groups) %>% 
  summarise(agegender = n()) %>% 
  ungroup () %>% 
  group_by(age_groups, gender) %>%
  mutate(summe = sum(agegender)) %>% 
  ungroup() %>% 
  mutate(per = agegender/summe) %>% 
  mutate(gender = case_when (gender=="Frau" & age_groups == "17- bis 25-jährig" ~ "Frau\n(n = 178)",
                             gender=="Frau" & age_groups == "26- bis 34-jährig" ~ "Frau\n(n = 86)",
                             gender=="Frau" & age_groups == "35- bis 49-jährig" ~ "Frau\n(n = 44)",
                             gender=="Frau" & age_groups == "50- bis 64-jährig" ~ "Frau\n(n = 26)",
                             gender=="Mann" & age_groups == "17- bis 25-jährig" ~ "Mann\n(n = 208)",
                             gender=="Mann" & age_groups == "26- bis 34-jährig" ~ "Mann\n(n = 130)",
                             gender=="Mann" & age_groups == "35- bis 49-jährig" ~ "Mann\n(n = 51)",
                             gender=="Mann" & age_groups == "50- bis 64-jährig" ~ "Mann\n(n = 23)",
                             TRUE ~ as.character(NA)))


#add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_VT_tra3 <- merge(df_VT_tra3, df_color, by="Wahl") 

# Plot ----
ggplot(df_VT_tra3, aes(x = gender, y = per, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_VT_tra3$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  facet_wrap(~age_groups, nrow = NULL, ncol = 4, scales = "free") +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  ylab("") +
  xlab("") +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zu?\n(Frage 8, Altersgruppen und Geschlecht, n = 746)\n\nMir ist es allgemein wichtig, dass die Welt sozial gerechter wird.")+
  guides(color = F, fill = guide_legend("", reverse = T, nrow=1)) +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme

ggsave(("04_plots/Frage_8_sozial_altersgruppen_geschlecht_191209_vori.pdf"),
       width = 14.5,
       height = 7,
       dpi=600,
       units="cm",
       device=cairo_pdf)



# Arbeitsbedingungen ----
df_VT_tra4 <- df_tot_age_gr_fill %>% 
  select(c(tra_4, age_groups, gender)) %>% 
  filter(!is.na(age_groups)) %>% 
  mutate(Wahl=tra_4) %>% 
  select(-c("tra_4")) %>% 
  mutate(question = "das die Arbeitsbedingungen für alle Menschen human sind.") %>% 
  filter(gender == "Mann" | gender == "Frau") %>% 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(Wahl, gender, age_groups) %>% 
  summarise(agegender = n()) %>% 
  ungroup () %>% 
  group_by(age_groups, gender) %>%
  mutate(summe = sum(agegender)) %>% 
  ungroup() %>% 
  mutate(per = agegender/summe) %>% 
  mutate(gender = case_when (gender=="Frau" & age_groups == "17- bis 25-jährig" ~ "Frau\n(n = 178)",
                             gender=="Frau" & age_groups == "26- bis 34-jährig" ~ "Frau\n(n = 86)",
                             gender=="Frau" & age_groups == "35- bis 49-jährig" ~ "Frau\n(n = 44)",
                             gender=="Frau" & age_groups == "50- bis 64-jährig" ~ "Frau\n(n = 26)",
                             gender=="Mann" & age_groups == "17- bis 25-jährig" ~ "Mann\n(n = 208)",
                             gender=="Mann" & age_groups == "26- bis 34-jährig" ~ "Mann\n(n = 130)",
                             gender=="Mann" & age_groups == "35- bis 49-jährig" ~ "Mann\n(n = 51)",
                             gender=="Mann" & age_groups == "50- bis 64-jährig" ~ "Mann\n(n = 23)",
                             TRUE ~ as.character(NA)))


#add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_VT_tra4 <- merge(df_VT_tra4, df_color, by="Wahl") 

# Plot ----
ggplot(df_VT_tra4, aes(x = gender, y = per, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_VT_tra4$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  facet_wrap(~age_groups, nrow = NULL, ncol = 4, scales = "free") +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  ylab("") +
  xlab("") +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zu?\n(Frage 8, Altersgruppen und Geschlecht, n = 746)\n\nMir ist es allgemein wichtig, das die Arbeitsbedingungen für alle Menschen human sind.")+
  guides(color = F, fill = guide_legend("", reverse = T, nrow=1)) +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme

ggsave(("04_plots/Frage_8_arbeitsbedingungen_altersgruppen_geschlecht_191209_vori.pdf"),
       width = 14.5,
       height = 7,
       dpi=600,
       units="cm",
       device=cairo_pdf)

# Tiere----
df_VT_tra5 <- df_tot_age_gr_fill %>% 
  select(c(tra_5, age_groups, gender)) %>% 
  filter(!is.na(age_groups)) %>% 
  mutate(Wahl=tra_5) %>% 
  select(-c("tra_5")) %>% 
  mutate(question ="dass Tiere möglichst artgerecht gehalten werden.") %>% 
  filter(gender == "Mann" | gender == "Frau") %>% 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(Wahl, gender, age_groups) %>% 
  summarise(agegender = n()) %>% 
  ungroup () %>% 
  group_by(age_groups, gender) %>%
  mutate(summe = sum(agegender)) %>% 
  ungroup() %>% 
  mutate(per = agegender/summe) %>% 
  mutate(gender = case_when (gender=="Frau" & age_groups == "17- bis 25-jährig" ~ "Frau\n(n = 178)",
                             gender=="Frau" & age_groups == "26- bis 34-jährig" ~ "Frau\n(n = 86)",
                             gender=="Frau" & age_groups == "35- bis 49-jährig" ~ "Frau\n(n = 44)",
                             gender=="Frau" & age_groups == "50- bis 64-jährig" ~ "Frau\n(n = 26)",
                             gender=="Mann" & age_groups == "17- bis 25-jährig" ~ "Mann\n(n = 208)",
                             gender=="Mann" & age_groups == "26- bis 34-jährig" ~ "Mann\n(n = 130)",
                             gender=="Mann" & age_groups == "35- bis 49-jährig" ~ "Mann\n(n = 51)",
                             gender=="Mann" & age_groups == "50- bis 64-jährig" ~ "Mann\n(n = 23)",
                             TRUE ~ as.character(NA)))


#add colors
ColsPerCat = c("NA" = "grey", "stimme nicht zu" = "#DC6413", "stimme eher nicht zu" = "#Fdd200", "stimme eher zu" = "#AFC410", "stimme zu"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_VT_tra5 <- merge(df_VT_tra5, df_color, by="Wahl") 

# Plot ----
ggplot(df_VT_tra5, aes(x = gender, y = per, fill=factor(Wahl, levels= c ("NA", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_VT_tra5$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  facet_wrap(~age_groups, nrow = NULL, ncol = 4, scales = "free") +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  ylab("") +
  xlab("") +
  ggtitle("Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zu?\n(Frage 8, Altersgruppen und Geschlecht, n = 746)\n\nMir ist es allgemein wichtig, dass Tiere möglichst artgerecht gehalten werden.")+
  guides(color = F, fill = guide_legend("", reverse = T, nrow=1)) +
  geom_text(aes(label=(ifelse(per<0.0249, "",paste( round(per*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme

ggsave(("04_plots/Frage_8_tiere_altersgruppen_geschlecht_191209_vori.pdf"),
       width = 14.5,
       height = 7,
       dpi=600,
       units="cm",
       device=cairo_pdf)



# Fleischverpflegungstyp und Altersgruppen und Geschlecht ein Plot ----

df_tot_age_gr_fill_gen <- df_tot_age_gr_fill  %>%
  filter(gender=="Mann" | gender=="Frau") %>%
  filter(!is.na(meat)) %>%
  mutate(meat_diet = case_when (meat==7 ~ "Fleischliebhaber",
                                meat==6 ~ "Fleischliebhaber",
                                meat==5 ~ "Fleischesser",
                                meat==4 ~ "Fleisch-Flexitarier",
                                meat==3 ~ "Vegi-Flexitarier",
                                meat==2 ~ "Fleischvermeider",
                                meat==1 ~ "Fleischvermeider",
                                TRUE ~ as.character(NA))) # 741 Beobachtungen


df_1725_fill_meat_gen <- df_tot_age_gr_fill_gen %>%
  filter(age_groups=="17- bis 25-jährig") %>%
  group_by(gender) %>%
  count(meat_diet) %>%
  mutate(pct_1725=n/sum(n))

df_2634_fill_meat_gen <- df_tot_age_gr_fill_gen %>%
  filter(age_groups=="26- bis 34-jährig") %>%
  group_by(gender) %>%
  count(meat_diet) %>%
  mutate(pct_2634=n/sum(n))

df_3549_fill_meat_gen <- df_tot_age_gr_fill_gen %>%
  filter(age_groups=="35- bis 49-jährig") %>%
  group_by(gender) %>%
  count(meat_diet) %>%
  mutate(pct_3549=n/sum(n))

df_5064_fill_meat_gen <- df_tot_age_gr_fill_gen %>%
  filter(age_groups=="50- bis 64-jährig") %>%
  group_by(gender) %>%
  count(meat_diet) %>%
  mutate(pct_5064=n/sum(n))

#
# # Umwandeln der Datensätze für den Plot
 df_1725_mg <- melt(df_1725_fill_meat_gen[,c("meat_diet", "gender", "pct_1725")], id.vars = c("meat_diet", "gender"))
# df_1725_mg$meat_diet <- ordered(df_1725_mg$meat_diet, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
#
 df_2634_mg <- melt(df_2634_fill_meat_gen[,c("meat_diet", "pct_2634", "gender")], id.vars = c("meat_diet", "gender"))
# df_2634_mg$meat_diet <- ordered(df_2634_mg$meat_diet, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
#
 df_3549_mg <- melt(df_3549_fill_meat_gen[,c("meat_diet", "pct_3549", "gender")], id.vars = c("meat_diet", "gender"))
# df_3549_mg$meat_diet <- ordered(df_3549_mg$meat_diet, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
#
df_5064_mg <- melt(df_5064_fill_meat_gen[,c("meat_diet", "pct_5064", "gender")], id.vars = c("meat_diet", "gender"))
# df_5064_mg$meat_diet <- ordered(df_5064_mg$meat_diet, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))

#Zusammenführen der Datensätze
df_T_mg <- rbind(df_1725_mg, df_2634_mg, df_3549_mg, df_5064_mg)
df_T_mg$variable <- dplyr::recode(df_T_mg$variable, "pct_1725"="17- bis 25-jährig", "pct_2634"="26- bis 34-jährig", "pct_3549"="35- bis 49-jährig", "pct_5064"="50- bis 64-jährig")
df_T_mg$meat_diet <- ordered(df_T_mg$meat_diet, levels = c("Fleischliebhaber",
                                                             "Fleischesser",
                                                              "Fleisch-Flexitarier",
                                                              "Vegi-Flexitarier",
                                                              "Fleischvermeider"))


df_T_mg %<>%
  mutate(gender = case_when (gender=="Frau" & variable == "17- bis 25-jährig" ~ "Frau\n(n = 178)",
                             gender=="Frau" & variable == "26- bis 34-jährig" ~ "Frau\n(n = 86)",
                             gender=="Frau" & variable == "35- bis 49-jährig" ~ "Frau\n(n = 43)",
                             gender=="Frau" & variable == "50- bis 64-jährig" ~ "Frau\n(n = 26)",
                             gender=="Mann" & variable == "17- bis 25-jährig" ~ "Mann\n(n = 207)",
                             gender=="Mann" & variable == "26- bis 34-jährig" ~ "Mann\n(n = 128)",
                             gender=="Mann" & variable == "35- bis 49-jährig" ~ "Mann\n(n = 50)",
                             gender=="Mann" & variable == "50- bis 64-jährig" ~ "Mann\n(n = 23)",
                            TRUE ~ as.character(NA)))

ColsPerCat = c("Fleischliebhaber" = "#DC6413", "Fleischesser" = "#Fdd200", "Fleisch-Flexitarier" = "#d3c193", "Vegi-Flexitarier" = "#AFC410", "Fleischvermeider"="#006885")


source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "meat_diet")


df_T_mg <- merge(df_T_mg, df_color, by="meat_diet") 

#Plot ----
ggplot(df_T_mg, aes(x = gender,y = value, fill=factor(meat_diet, levels= c ("Fleischvermeider", "Vegi-Flexitarier", "Fleisch-Flexitarier",  "Fleischesser", "Fleischliebhaber")), color = label_color)) + 
  geom_bar(stat = "identity", position = position_stack(),  color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_T_mg$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  facet_wrap(~variable, nrow = NULL, ncol = 4, scales = "free") +
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0.01)) +
  ylab("") +
  xlab("") +
  ggtitle("Altersgruppen und Geschlecht\n(Fleischkonsumtyp, n = 741)") +
  guides(color = F, fill = guide_legend("", reverse = F,nrow=1)) +
  geom_text(aes(label=(ifelse(value<0.0249, "",paste( round(value*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  mytheme

  ggsave(("04_plots/Frage_9_Fleisch_Altersgruppen_geschlecht_191209_vori.pdf"),
         width = 14.5,
         height = 7,
         dpi=600,
         units="cm",
         device=cairo_pdf)

# 
#   # Milch Altersgruppen Frauen----
#   
#   
#   df_fr_1725_fill_milk <- df_tot_age_gr_fr_fill %>% 
#     filter(age_groups=="17- bis 25-jährig") %>% 
#     count(milk)
#   
#   df_fr_2634_fill_milk <- df_tot_age_gr_fr_fill %>% 
#     filter(age_groups=="26- bis 34-jährig") %>% 
#     count(milk)
#   
#   df_fr_3549_fill_milk <- df_tot_age_gr_fr_fill %>% 
#     filter(age_groups=="35- bis 49-jährig") %>% 
#     count(milk)
#   
#   df_fr_5064_fill_milk <- df_tot_age_gr_fr_fill %>% 
#     filter(age_groups=="50- bis 64-jährig") %>% 
#     count(milk)
#   
#   
#   # Prozent
#   df_fr_1725_fill_milk$pct_1725<-df_fr_1725_fill_milk$n/sum(df_fr_1725_fill_milk$n)
#   
#   df_fr_2634_fill_milk$pct_2634<-df_fr_2634_fill_milk$n/sum(df_fr_2634_fill_milk$n)
#   
#   df_fr_3549_fill_milk$pct_3549<-df_fr_3549_fill_milk$n/sum(df_fr_3549_fill_milk$n)
#   
#   df_fr_5064_fill_milk$pct_5064<-df_fr_5064_fill_milk$n/sum(df_fr_5064_fill_milk$n)
#   
#   
#   
#   # Umwandeln der Datensätze für den Plot
#   df_fr_1725_mi <- melt(df_fr_1725_fill_milk[,c("milk", "pct_1725")], id.vars = 1)
#   df_fr_1725_mi$milk <- ordered(df_fr_1725_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
#   
#   df_fr_2634_mi <- melt(df_fr_2634_fill_milk[,c("milk", "pct_2634")], id.vars = 1)
#   df_fr_2634_mi$milk <- ordered(df_fr_2634_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
#   
#   df_fr_3549_mi <- melt(df_fr_3549_fill_milk[,c("milk", "pct_3549")], id.vars = 1)
#   df_fr_3549_mi$milk <- ordered(df_fr_3549_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
#   
#   df_fr_5064_mi <- melt(df_fr_5064_fill_milk[,c("milk", "pct_5064")], id.vars = 1)
#   df_fr_5064_mi$milk <- ordered(df_fr_5064_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
#   
#   #Zusammenführen der Datensätze
#   df_fr_T_mi <- rbind(df_fr_1725_mi, df_fr_2634_mi, df_fr_3549_mi, df_fr_5064_mi)
#   df_fr_T_mi$variable <- dplyr::recode(df_fr_T_mi$variable, "pct_1725"="17- bis 25-jährig (n=178)", "pct_2634"="26- bis 34-jährig (n=86)", "pct_3549"="35- bis 49-jährig (n=44)", "pct_5064"="50- bis 64-jährig (n=26)")
#   
#   #NA in 0 für Plot
#   df_fr_T_mi  %<>%  
#     complete(milk, variable, fill= list(per=0))
#   
#   # Plot
#   ggplot(df_fr_T_mi,aes(x = milk,y = value, width=0.7)) +
#     geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
#     labs(x = '',y='Prozent')+
#     scale_fill_manual("",values = c("17- bis 25-jährig (n=178)"="#fad60d", "26- bis 34-jährig (n=86)"="#505050", "35- bis 49-jährig (n=44)"="grey", "50- bis 64-jährig (n=26)"="#c5b87c"))+
#     mytheme+ 
#     theme(legend.position = "bottom")+ 
#     scale_y_continuous(labels = scales::percent)+
#     geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
#               size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
#     ggtitle("Ich trinke/esse Milch/Milchprodukte, Käse:\nFrauen unterschieden nach Altersgruppen n = 334") 
#     
#     ggsave(("04_plots/Frage_9_Milch_Altersgruppen_Frauen_191105_vori.pdf"),
#            width = 17,
#            height = 8,
#            dpi=600,
#            units="in",
#            device="pdf")
#   
#   # Milch Altersgruppen Männer----
#   
#   df_mae_1725_fill_milk <- df_tot_age_gr_mae_fill %>% 
#     filter(age_groups=="17- bis 25-jährig") %>% 
#     count(milk)
#   
#   df_mae_2634_fill_milk <- df_tot_age_gr_mae_fill %>% 
#     filter(age_groups=="26- bis 34-jährig") %>% 
#     count(milk)
#   
#   df_mae_3549_fill_milk <- df_tot_age_gr_mae_fill %>% 
#     filter(age_groups=="35- bis 49-jährig") %>% 
#     count(milk)
#   
#   df_mae_5064_fill_milk <- df_tot_age_gr_mae_fill %>% 
#     filter(age_groups=="50- bis 64-jährig") %>% 
#     count(milk)
#   
#   
#   # Prozent
#   df_mae_1725_fill_milk$pct_1725<-df_mae_1725_fill_milk$n/sum(df_mae_1725_fill_milk$n)
#   
#   df_mae_2634_fill_milk$pct_2634<-df_mae_2634_fill_milk$n/sum(df_mae_2634_fill_milk$n)
#   
#   df_mae_3549_fill_milk$pct_3549<-df_mae_3549_fill_milk$n/sum(df_mae_3549_fill_milk$n)
#   
#   df_mae_5064_fill_milk$pct_5064<-df_mae_5064_fill_milk$n/sum(df_mae_5064_fill_milk$n)
#   
#   
#   
#   # Umwandeln der Datensätze für den Plot
#   df_mae_1725_mi <- melt(df_mae_1725_fill_milk[,c("milk", "pct_1725")], id.vars = 1)
#   df_mae_1725_mi$milk <- ordered(df_mae_1725_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
#   
#   df_mae_2634_mi <- melt(df_mae_2634_fill_milk[,c("milk", "pct_2634")], id.vars = 1)
#   df_mae_2634_mi$milk <- ordered(df_mae_2634_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
#   
#   df_mae_3549_mi <- melt(df_mae_3549_fill_milk[,c("milk", "pct_3549")], id.vars = 1)
#   df_mae_3549_mi$milk <- ordered(df_mae_3549_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
#   
#   df_mae_5064_mi <- melt(df_mae_5064_fill_milk[,c("milk", "pct_5064")], id.vars = 1)
#   df_mae_5064_mi$milk <- ordered(df_mae_5064_mi$milk, levels = c("3x und mehr pro Tag", "1-2x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "keine Antwort"))
#   
#   #Zusammenführen der Datensätze
#   df_mae_T_mi <- rbind(df_mae_1725_mi, df_mae_2634_mi, df_mae_3549_mi, df_mae_5064_mi)
#   df_mae_T_mi$variable <- dplyr::recode(df_mae_T_mi$variable, "pct_1725"="17- bis 25-jährig (n=208)", "pct_2634"="26- bis 34-jährig (n=130)", "pct_3549"="35- bis 49-jährig (n=51)", "pct_5064"="50- bis 64-jährig (n=23)")
#   
#   #NA in 0 für Plot
#   df_mae_T_mi  %<>%  
#     complete(milk, variable, fill= list(per=0))
#   
#   # Plot
#   ggplot(df_mae_T_mi,aes(x = milk,y = value, width=0.7)) +
#     geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
#     labs(x = '',y='Prozent')+
#     scale_fill_manual("",values = c("17- bis 25-jährig (n=208)"="#fad60d", "26- bis 34-jährig (n=130)"="#505050", "35- bis 49-jährig (n=51)"="grey", "50- bis 64-jährig (n=23)"="#c5b87c"))+
#     mytheme+ 
#     theme(legend.position = "bottom")+ 
#     scale_y_continuous(labels = scales::percent)+
#     geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
#               size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
#     ggtitle("Ich trinke/esse Milch/Milchprodukte, Käse:\nMänner unterschieden nach Altersgruppen n = 412") +
#     
#     ggsave(("04_plots/Frage_9_Milch_Altersgruppen_Maenner_191016_vori.pdf"),
#            width = 17,
#            height = 8,
#            dpi=600,
#            units="in",
#            device="pdf")
#   
#   
#   
# 
#   
#   # Verpfelgungstyp Frauen----
#   
#   
#   df_fr_1725_typ <- df_tot_age_gr_fr_fill %>% filter(age_groups=="17- bis 25-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger" | Verpflegungstyp=="Einkäufer")
#   
#   df_fr_2634_typ <- df_tot_age_gr_fr_fill %>% filter(age_groups=="26- bis 34-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger" | Verpflegungstyp=="Einkäufer")
#   
#   df_fr_3549_typ <- df_tot_age_gr_fr_fill %>% filter(age_groups=="35- bis 49-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger" | Verpflegungstyp=="Einkäufer")
#   
#   df_fr_5064_typ <- df_tot_age_gr_fr_fill %>% filter(age_groups=="50- bis 64-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger" | Verpflegungstyp=="Einkäufer")
#   
#   
#   # Prozent
#   
#   df_fr_1725_typ$pct_1725 <- df_fr_1725_typ$n/sum(df_fr_1725_typ$n)
#   df_fr_2634_typ$pct_2634 <- df_fr_2634_typ$n/sum(df_fr_2634_typ$n)
#   df_fr_3549_typ$pct_3549 <- df_fr_3549_typ$n/sum(df_fr_3549_typ$n)
#   df_fr_5064_typ$pct_5064 <- df_fr_5064_typ$n/sum(df_fr_5064_typ$n)
#   
#   # Umwandeln für Plot
#   df_fr_1725_ver <- melt(df_fr_1725_typ[,c("Verpflegungstyp", "pct_1725")], id.vars = 1)
#   df_fr_2634_ver <- melt(df_fr_2634_typ[,c("Verpflegungstyp", "pct_2634")], id.vars = 1)
#   df_fr_3549_ver <- melt(df_fr_3549_typ[,c("Verpflegungstyp", "pct_3549")], id.vars = 1)
#   df_fr_5064_ver <- melt(df_fr_5064_typ[,c("Verpflegungstyp", "pct_5064")], id.vars = 1)
#   
#   
#   
#   #Zusammenführen der Datensätze
#   df_fr_mnm_ver <- rbind(df_fr_1725_ver, df_fr_2634_ver, df_fr_3549_ver, df_fr_5064_ver)
#   df_fr_mnm_ver$variable <- dplyr::recode(df_fr_mnm_ver$variable, "pct_1725"="17- bis 25-jährig (n=173)", "pct_2634"="26- bis 34-jährig (n=85)", "pct_3549"="35- bis 49-jährig (n=41)", "pct_5064"="50- bis 64-jährig (n=25)"   )
#   
#   # Mensagänger zuerst, gefolgt von Selbstverpfleger
#   df_fr_mnm_ver$Verpflegungstyp <- ordered(df_fr_mnm_ver$Verpflegungstyp, levels = c("Mensagänger", "Selbstverpfleger", "Abwechsler", "Einkäufer"))
#  
#   #NA in 0 für Plot
#   df_fr_mnm_ver  %<>%  
#     complete(Verpflegungstyp, variable, fill= list(per=0))
#    
#   # Plot
#   ggplot(df_fr_mnm_ver,aes(x = Verpflegungstyp,y = value, width=0.7)) +
#     geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
#     labs(x = '',y='Prozent')+
#     scale_fill_manual("",values = c("17- bis 25-jährig (n=173)"="#fad60d", "26- bis 34-jährig (n=85)"="#505050", "35- bis 49-jährig (n=41)"="grey", "50- bis 64-jährig (n=25)"="#c5b87c"))+
#     mytheme+ 
#     theme(legend.position = "bottom")+ 
#     scale_y_continuous(labels = scales::percent)+
#     geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
#               size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
#     ggtitle("Verpflegungstyp\nFrauen unterschieden nach Altersgruppen n = 334")
#   
#   ggsave(("04_plots/Frage_11_altersgruppen_frauen_191018_vori.pdf"),
#          width = 17,
#          height = 8,
#          dpi=600,
#          units="in",
#          device="pdf")  
#   
#   # Verpfelgungstyp Männer----
#   
#   
#   df_mae_1725_typ <- df_tot_age_gr_mae_fill %>% filter(age_groups=="17- bis 25-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger" | Verpflegungstyp=="Einkäufer")
#   
#   df_mae_2634_typ <- df_tot_age_gr_mae_fill %>% filter(age_groups=="26- bis 34-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger" | Verpflegungstyp=="Einkäufer")
#   
#   df_mae_3549_typ <- df_tot_age_gr_mae_fill %>% filter(age_groups=="35- bis 49-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger" | Verpflegungstyp=="Einkäufer")
#   
#   df_mae_5064_typ <- df_tot_age_gr_mae_fill %>% filter(age_groups=="50- bis 64-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger" | Verpflegungstyp=="Einkäufer")
#   
#   
#   # Prozent
#   
#   df_mae_1725_typ$pct_1725 <- df_mae_1725_typ$n/sum(df_mae_1725_typ$n)
#   df_mae_2634_typ$pct_2634 <- df_mae_2634_typ$n/sum(df_mae_2634_typ$n)
#   df_mae_3549_typ$pct_3549 <- df_mae_3549_typ$n/sum(df_mae_3549_typ$n)
#   df_mae_5064_typ$pct_5064 <- df_mae_5064_typ$n/sum(df_mae_5064_typ$n)
#   
#   # Umwandeln für Plot
#   df_mae_1725_ver <- melt(df_mae_1725_typ[,c("Verpflegungstyp", "pct_1725")], id.vars = 1)
#   df_mae_2634_ver <- melt(df_mae_2634_typ[,c("Verpflegungstyp", "pct_2634")], id.vars = 1)
#   df_mae_3549_ver <- melt(df_mae_3549_typ[,c("Verpflegungstyp", "pct_3549")], id.vars = 1)
#   df_mae_5064_ver <- melt(df_mae_5064_typ[,c("Verpflegungstyp", "pct_5064")], id.vars = 1)
# 
#   
#   #Zusammenführen der Datensätze
#   df_mae_mnm_ver <- rbind(df_mae_1725_ver, df_mae_2634_ver, df_mae_3549_ver, df_mae_5064_ver)
#   df_mae_mnm_ver$variable <- dplyr::recode(df_mae_mnm_ver$variable, "pct_1725"="17- bis 25-jährig (n=206)", "pct_2634"="26- bis 34-jährig (n=126)", "pct_3549"="35- bis 49-jährig (n=49)", "pct_5064"="50- bis 64-jährig (n=23)"   )
#   
#   #NA in 0 für Plot
#   df_mae_mnm_ver  %<>%  
#     complete(Verpflegungstyp, variable, fill= list(per=0))
#   
#   # Mensagänger zuerst, gefolgt von Selbstverpfleger
#   df_mae_mnm_ver$Verpflegungstyp <- ordered(df_mae_mnm_ver$Verpflegungstyp, levels = c("Mensagänger", "Selbstverpfleger", "Abwechsler", "Einkäufer"))
#   
# 
#   #Plot
#   
#   ggplot(df_mae_mnm_ver,aes(x = Verpflegungstyp,y = value, width=0.7)) +
#     geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
#     labs(x = '',y='Prozent')+
#     scale_fill_manual("",values = c("17- bis 25-jährig (n=206)"="#fad60d", "26- bis 34-jährig (n=126)"="#505050", "35- bis 49-jährig (n=49)"="grey", "50- bis 64-jährig (n=23)"="#c5b87c"))+
#     mytheme+ 
#     theme(legend.position = "bottom")+ 
#     scale_y_continuous(labels = scales::percent)+
#     geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
#               size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
#     ggtitle("Verpflegungstyp\nMänner unterschieden nach Altersgruppen n = 412")
#   
#   ggsave(("04_plots/Frage_11_altersgruppen_maenner_191018_vori.pdf"),
#          width = 17,
#          height = 8,
#          dpi=600,
#          units="in",
#          device="pdf")  
#     
#   