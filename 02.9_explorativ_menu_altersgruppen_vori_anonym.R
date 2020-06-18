library (tidyverse) #Version 1.2.1
library (formattable) #Version 0.2.0.1
library (wesanderson) #Version 0.3.6
library (reshape2) #Version 1.4.3
library (magrittr) #Version 1.5

# Themes laden
mytheme <- theme_bw()+ # definve theme for plot
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=18, face = "plain"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size =15),
        legend.position = "bottom",
        axis.title.y = element_text(size = 18, margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(size = 18,  margin = margin(t = 15, r = 0, b = 0, l = 0)),
        plot.subtitle=element_text(margin=margin(b=15),size = 15),
        plot.caption=element_text(margin=margin(t=15), face="italic", size=15))




# Daten einlesen ---------

df_tot_age_gr_fill <- read_delim("2019_ZHAW_vonRickenbach_cleaned_recorded_survey_dataset_NOVANIMAL_anonym.csv", delim=';', locale = locale(encoding = 'ISO-8859-2')) %>% 
  drop_na(age_groups) %>% 
  filter(fill==0) #763 Beobachtungen

# Datensatz bearbeiten-----
df_tot_age_gr_fill <- df_tot_age_gr_fill  %>% 
  mutate(meat = ifelse(is.na(meat), "Unbekannt", meat)) %>% 
  mutate(milk = ifelse(is.na(milk), "Unbekannt", milk)) %>% 
  mutate(veget = ifelse(is.na(veget), "Unbekannt", veget)) %>% 
  mutate(veg = ifelse(is.na(veg), "Unbekannt", veg)) %>% 
  mutate(meat = recode_factor(meat, "7"= "2x und mehr pro Tag", "6" = "1x pro Tag", "5" = "5-6x pro Woche", "4" = "3-4x pro Woche", "3" = "1-2x pro Woche", "2" = "1-2x pro Monat", "1" = "nie")) %>% 
  mutate(milk = recode_factor(milk, "7"= "3x und mehr pro Tag", "6" = "1-2x pro Tag", "5" = "5-6x pro Woche", "4" = "3-4x pro Woche", "3" = "1-2x pro Woche", "2" = "1-2x pro Monat", "1" = "nie")) %>% 
  mutate(veget = recode_factor(veget, "6"= "immer", "5" = "meistens", "4" = "oft", "3" = "manchmal", "2" = "selten", "1" = "nie")) %>% 
  mutate(veg = recode_factor(veg, "6"= "immer", "5" = "meistens", "4" = "oft", "3" = "manchmal", "2" = "selten", "1" = "nie")) %>% 
  mutate(age_groups = recode(age_groups, "17- bis 25-jaehrig" = "17- bis 25-jährig", "26- bis 34-jaehrig"="26- bis 34-jährig", "35- bis 49-jaehrig"="35- bis 49-jährig", "50- bis 64-jaehrig"="50- bis 69-jährig")) %>% 
  mutate(Verpflegungstyp = recode(Verpflegungstyp, "Mensagaenger"="Mensagänger", "Einkaeufer"="Einkäufer"))

df_tot_age_gr_mae_fill <- df_tot_age_gr_fill %>% filter(gender=="Mann") # 412 Fragebogen

df_tot_age_gr_fr_fill <- df_tot_age_gr_fill %>% filter(gender=="Frau") # 334 Fragebogen

# Frage 7: Direkter Gruppenvergleich ----

# Datensätze erstellen

df_VT_tho1 <- df_tot_age_gr_fill%>% 
  select(c(tho_1, age_groups)) %>% 
  mutate(Wahl=tho_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_1")) %>% 
  mutate(question = "meiner Ernährungsweise für meine Gesundheit.")

df_VT_tho2 <- df_tot_age_gr_fill%>% 
  select(c(tho_2, age_groups)) %>% 
  mutate(Wahl=tho_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_2")) %>% 
  mutate(question = "meiner Ernährungsgewohnheiten für die Umwelt.")

df_VT_tho3 <- df_tot_age_gr_fill%>% 
  select(c(tho_3, age_groups)) %>% 
  mutate(Wahl=tho_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_3")) %>% 
  mutate(question = "der Produktion der Nahrungsmittel auf meinem Teller\nfür die Arbeitenden in der Wertschöpfungskette.")

df_VT_tho4 <- df_tot_age_gr_fill%>% 
  select(c(tho_4, age_groups)) %>% 
  mutate(Wahl=tho_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_4")) %>% 
  mutate(question = "meines Konsums von tierischen Nahrungsmitteln für die Tiere.")

df_VT_tho5 <- df_tot_age_gr_fill%>% 
  select(c(tho_5, age_groups)) %>% 
  mutate(Wahl=tho_5) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tho_5")) %>% 
  mutate(question = "meiner Ernährung für mein Portemonnaie.")


# Datensätze zusammenführen
df_VT_tho <- bind_rows(df_VT_tho1, df_VT_tho2, df_VT_tho3, df_VT_tho4, df_VT_tho5) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "Unbekannt", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(question, Zustimmung, age_groups, Wahl) %>%
  summarise(tot_answ = n()) %>%  
  mutate(pct = (tot_answ/sum(tot_answ))) %>% 
  mutate(xlab_ = paste("(", age_groups, ")", sep = ""),
         xlab = paste(age_groups, question , sep = ": ")) #add empty coloms for spacing in the middle

df_VT_tho[nrow(df_VT_tho) + 1,] <-  list("space1", NA, NA, NA, NA, NA, NA)
df_VT_tho[nrow(df_VT_tho) + 1,] <-  list("space2", NA, NA, NA, NA, NA, NA)
df_VT_tho[nrow(df_VT_tho) + 1,] <-  list("space3", NA, NA, NA, NA, NA, NA)
df_VT_tho[nrow(df_VT_tho) + 1,] <-  list("space4", NA, NA, NA, NA, NA, NA)

# Sortieren Augrund der Gesamtzustimmung
df_VT_tho <- df_VT_tho[order(df_VT_tho$Zustimmung),]


# add colors
ColsPerCat = c("Unbekannt" = "grey", "stimme nicht zu" = "#FF0000", "stimme eher nicht zu" = "#FFA500", "stimme eher zu" = "#99f200", "stimme zu"="#006400")

source("S:\\pools\\n\\N-IUNR-nova-data\\02_kassendaten\\02_tilldata_2017\\09_function_is_dark_190114_egel.R", chdir = T)

# df_VT_tho$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_VT_tho$Wahl], # takes every label and their belonged color
#                                              function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_VT_tho <- merge(df_VT_tho, df_color, by="Wahl")

# Sortieren Augrund der Gesamtzustimmung
df_VT_tho <- df_VT_tho[order(df_VT_tho$Zustimmung, df_VT_tho$Wahl, df_VT_tho$question),]

df_VT_tho$ps <- (1:91)



ggplot(df_VT_tho, aes(x=xlab, y=pct, fill=factor(Wahl, levels= c ("Unbekannt", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.8) +
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = levels(df_VT_tho$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat)+
  scale_x_discrete(limits=c((df_VT_tho$xlab)[c(3, 2, 1, 4)],
                            "",
                            (df_VT_tho$xlab)[c(22, 23, 21, 20)],
                            "", 
                            (df_VT_tho$xlab)[c(42, 41, 39, 40)],
                            "",
                            (df_VT_tho$xlab)[c(60, 58, 57, 59)],
                            "",
                            (df_VT_tho$xlab)[c(78, 76, 75, 77)]))+
  #                     guide_legend("")) +
  geom_text(aes(label=(ifelse(pct<0.02, "",paste( round(pct*100), "%", sep = " ")))), size = 5, position = position_stack(vjust = 0.5))+
  ylab("Antworten in Prozent")+
  xlab("")+
  guides(color = F, fill = guide_legend("", reverse = T)) +
  ggtitle("Frage 7: Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zum Essen zu?\n(17- bis 25-jährig (n=398), 26- bis 34-jährig (n=221), 35- bis 49-jährig (n=95), 50- bis 64-jährig (n=49))")+
  xlab("Ich mache mir allgemein Gedanken über die Folgen ...")+
  coord_flip() +
  mytheme +
  
  theme(legend.position = "bottom")

ggsave("04_plots/Frage_7_Altersgruppen_190924.pdf",
       width = 27,
       height = 13,
       dpi=600,
       units="in",
       device=cairo_pdf)

# Frage 8: Direkter Gruppenvergleich ----

# Datensätze erstellen

df_VT_tra1 <- df_tot_age_gr_fill%>% 
  select(c(tra_1, age_groups)) %>% 
  mutate(Wahl=tra_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_1")) %>% 
  mutate(question = "gesund zu leben.")

df_VT_tra2 <- df_tot_age_gr_fill%>% 
  select(c(tra_2, age_groups)) %>% 
  mutate(Wahl=tra_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_2")) %>% 
  mutate(question = "mit meinem Verhalten die Umwelt möglichst wenig zu belasten.")

df_VT_tra3 <- df_tot_age_gr_fill%>% 
  select(c(tra_3, age_groups)) %>% 
  mutate(Wahl=tra_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_3")) %>% 
  mutate(question = "dass die Welt sozial gerechter wird.")

df_VT_tra4 <- df_tot_age_gr_fill%>% 
  select(c(tra_4, age_groups)) %>% 
  mutate(Wahl=tra_4) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_4")) %>% 
  mutate(question = "das die Arbeitsbedingungen für alle Menschen human sind.")

df_VT_tra5 <- df_tot_age_gr_fill%>% 
  select(c(tra_5, age_groups)) %>% 
  mutate(Wahl=tra_5) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("tra_5")) %>% 
  mutate(question = "dass Tiere möglichst artgerecht gehalten werden.")


# Datensätze zusammenführen
df_VT_tra <- bind_rows(df_VT_tra1, df_VT_tra2, df_VT_tra3, df_VT_tra4, df_VT_tra5) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "Unbekannt", "1" = "stimme nicht zu", "2" = "stimme eher nicht zu","3" = "stimme eher zu", "4" = "stimme zu" )) %>% 
  group_by(question, Zustimmung, age_groups, Wahl) %>%
  summarise(tot_answ = n()) %>%  
  mutate(pct = (tot_answ/sum(tot_answ))) %>% 
  mutate(xlab_ = paste("(", age_groups, ")", sep = ""),
         xlab = paste(age_groups, question , sep = ": ")) #add empty coloms for spacing in the middle

df_VT_tra[nrow(df_VT_tra) + 1,] <-  list("space1", NA, NA, NA, NA, NA, NA)
df_VT_tra[nrow(df_VT_tra) + 1,] <-  list("space2", NA, NA, NA, NA, NA, NA)
df_VT_tra[nrow(df_VT_tra) + 1,] <-  list("space3", NA, NA, NA, NA, NA, NA)
df_VT_tra[nrow(df_VT_tra) + 1,] <-  list("space4", NA, NA, NA, NA, NA, NA)

# Sortieren Augrund der Gesamtzustimmung
df_VT_tra <- df_VT_tra[order(df_VT_tra$Zustimmung),]


# add colors
ColsPerCat = c("Unbekannt" = "grey", "stimme nicht zu" = "#FF0000", "stimme eher nicht zu" = "#FFA500", "stimme eher zu" = "#99f200", "stimme zu"="#006400")

# source("S:\\pools\\n\\N-IUNR-nova-data\\02_kassendaten\\02_tilldata_2017\\09_function_is_dark_190114_egel.R", chdir = T)
# 
# df_VT_tra$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_VT_tra$Wahl], # takes every label and their belonged color
#                                           function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_VT_tra <- merge(df_VT_tra, df_color, by="Wahl")

# Sortieren Augrund der Gesamtzustimmung
df_VT_tra <- df_VT_tra[order(df_VT_tra$Zustimmung, df_VT_tra$Wahl, df_VT_tra$question),]

df_VT_tra$ps <- (1:88)

ggplot(df_VT_tra, aes(x=xlab, y=pct, fill=factor(Wahl, levels= c ("Unbekannt", "stimme nicht zu", "stimme eher nicht zu","stimme eher zu","stimme zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.8) +
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = levels(df_VT_tra$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat)+
  scale_x_discrete(limits=c((df_VT_tra$xlab)[c(1, 4, 2, 3)],
                            "",
                            (df_VT_tra$xlab)[c(22, 20, 19, 21)],
                            "", 
                            (df_VT_tra$xlab)[c(37, 36, 39, 38)],
                            "",
                            (df_VT_tra$xlab)[c(54, 55, 56, 57)],
                            "",
                            (df_VT_tra$xlab)[c(78, 76, 75, 74)]))+
  #                     guide_legend("")) +
  geom_text(aes(label=(ifelse(pct<0.02, "",paste( round(pct*100), "%", sep = " ")))), size = 5, position = position_stack(vjust = 0.5))+
  ylab("Antworten in Prozent")+
  xlab("")+
  guides(color = F, fill = guide_legend("", reverse = T)) +
  ggtitle("Frage 8: Inwiefern stimmen Sie im Allgemeinen folgenden Aussagen zu?\n(17- bis 25-jährig (n=398), 26- bis 34-jährig (n=221), 35- bis 49-jährig (n=95), 50- bis 64-jährig (n=49))")+
  xlab("Mir ist es allgemein wichtig, ...")+
  coord_flip() +
  mytheme +
  
  theme(legend.position = "bottom")

ggsave("04_plots/Frage_8_Altersgruppen_190924.pdf",
       width = 27,
       height = 13,
       dpi=600,
       units="in",
       device=cairo_pdf)



# Fleisch Altersgruppen----
df_1725_fill_meat <- df_tot_age_gr_fill %>% 
  filter(age_groups=="17- bis 25-jährig") %>% 
  count(meat)

df_2634_fill_meat <- df_tot_age_gr_fill %>% 
  filter(age_groups=="26- bis 34-jährig") %>% 
  count(meat)

df_3549_fill_meat <- df_tot_age_gr_fill %>% 
  filter(age_groups=="35- bis 49-jährig") %>% 
  count(meat)

df_5064_fill_meat <- df_tot_age_gr_fill %>% 
  filter(age_groups=="50- bis 64-jährig") %>% 
  count(meat)


# Prozent
df_1725_fill_meat$pct_1725<-df_1725_fill_meat$n/sum(df_1725_fill_meat$n)

df_2634_fill_meat$pct_2634<-df_2634_fill_meat$n/sum(df_2634_fill_meat$n)

df_3549_fill_meat$pct_3549<-df_3549_fill_meat$n/sum(df_3549_fill_meat$n)

df_5064_fill_meat$pct_5064<-df_5064_fill_meat$n/sum(df_5064_fill_meat$n)



# Umwandeln der Datensätze für den Plot
df_1725_m <- melt(df_1725_fill_meat[,c("meat", "pct_1725")], id.vars = 1)
df_1725_m$meat <- ordered(df_1725_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "Unbekannt"))

df_2634_m <- melt(df_2634_fill_meat[,c("meat", "pct_2634")], id.vars = 1)
df_2634_m$meat <- ordered(df_2634_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "Unbekannt"))

df_3549_m <- melt(df_3549_fill_meat[,c("meat", "pct_3549")], id.vars = 1)
df_3549_m$meat <- ordered(df_3549_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "Unbekannt"))

df_5064_m <- melt(df_5064_fill_meat[,c("meat", "pct_5064")], id.vars = 1)
df_5064_m$meat <- ordered(df_5064_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "Unbekannt"))

#Zusammenführen der Datensätze
df_T_m <- rbind(df_1725_m, df_2634_m, df_3549_m, df_5064_m)
df_T_m$variable <- dplyr::recode(df_T_m$variable, "pct_1725"="17- bis 25-jährig (n=398)", "pct_2634"="26- bis 34-jährig (n=221)", "pct_3549"="35- bis 49-jährig (n=95)", "pct_5064"="50- bis 64-jährig (n=49)")

ggplot(df_T_m,aes(x = meat,y = value, width=0.7)) +
  geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
  labs(x = '',y='Prozent')+
  scale_fill_manual("",values = c("17- bis 25-jährig (n=398)"="#fad60d", "26- bis 34-jährig (n=221)"="#505050", "35- bis 49-jährig (n=95)"="grey", "50- bis 64-jährig (n=49)"="#c5b87c"))+
  mytheme+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
            size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
  ggtitle("Ich esse Fleisch (alle Sorten inkl. Geflügel, Wurstwaren etc.):") +
  
  ggsave(("04_plots/Frage_9_Fleisch_Altersgruppen_190924_vori.pdf"),
         width = 17,
         height = 8,
         dpi=600,
         units="in",
         device="pdf")

# Fleisch Altersgruppen Frauen----


df_fr_1725_fill_meat <- df_tot_age_gr_fr_fill %>% 
  filter(age_groups=="17- bis 25-jährig") %>% 
  count(meat)

df_fr_2634_fill_meat <- df_tot_age_gr_fr_fill %>% 
  filter(age_groups=="26- bis 34-jährig") %>% 
  count(meat)

df_fr_3549_fill_meat <- df_tot_age_gr_fr_fill %>% 
  filter(age_groups=="35- bis 49-jährig") %>% 
  count(meat)

df_fr_5064_fill_meat <- df_tot_age_gr_fr_fill %>% 
  filter(age_groups=="50- bis 64-jährig") %>% 
  count(meat)


# Prozent
df_fr_1725_fill_meat$pct_1725<-df_fr_1725_fill_meat$n/sum(df_fr_1725_fill_meat$n)

df_fr_2634_fill_meat$pct_2634<-df_fr_2634_fill_meat$n/sum(df_fr_2634_fill_meat$n)

df_fr_3549_fill_meat$pct_3549<-df_fr_3549_fill_meat$n/sum(df_fr_3549_fill_meat$n)

df_fr_5064_fill_meat$pct_5064<-df_fr_5064_fill_meat$n/sum(df_fr_5064_fill_meat$n)



# Umwandeln der Datensätze für den Plot
df_fr_1725_m <- melt(df_fr_1725_fill_meat[,c("meat", "pct_1725")], id.vars = 1)
df_fr_1725_m$meat <- ordered(df_fr_1725_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "Unbekannt"))

df_fr_2634_m <- melt(df_fr_2634_fill_meat[,c("meat", "pct_2634")], id.vars = 1)
df_fr_2634_m$meat <- ordered(df_fr_2634_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "Unbekannt"))

df_fr_3549_m <- melt(df_fr_3549_fill_meat[,c("meat", "pct_3549")], id.vars = 1)
df_fr_3549_m$meat <- ordered(df_fr_3549_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "Unbekannt"))

df_fr_5064_m <- melt(df_fr_5064_fill_meat[,c("meat", "pct_5064")], id.vars = 1)
df_fr_5064_m$meat <- ordered(df_fr_5064_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "Unbekannt"))

#Zusammenführen der Datensätze
df_fr_T_m <- rbind(df_fr_1725_m, df_fr_2634_m, df_fr_3549_m, df_fr_5064_m)
df_fr_T_m$variable <- dplyr::recode(df_fr_T_m$variable, "pct_1725"="17- bis 25-jährig (n=178)", "pct_2634"="26- bis 34-jährig (n=86)", "pct_3549"="35- bis 49-jährig (n=44)", "pct_5064"="50- bis 64-jährig (n=26)")

#NA in 0 für Plot
df_fr_T_m  %<>%  
  complete(meat, variable, fill= list(per=0))

# Plot
ggplot(df_fr_T_m,aes(x = meat,y = value, width=0.7)) +
  geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
  labs(x = '',y='Prozent')+
  scale_fill_manual("",values = c("17- bis 25-jährig (n=178)"="#fad60d", "26- bis 34-jährig (n=86)"="#505050", "35- bis 49-jährig (n=44)"="grey", "50- bis 64-jährig (n=26)"="#c5b87c"))+
  mytheme+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
            size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
  ggtitle("Ich esse Fleisch (alle Sorten inkl. Geflügel, Wurstwaren etc.):\nFrauen") +
  
  ggsave(("04_plots/Frage_9_Fleisch_Altersgruppen_Frauen_191016_vori.pdf"),
         width = 17,
         height = 8,
         dpi=600,
         units="in",
         device="pdf")

# Fleisch Altersgruppen Männer----

df_mae_1725_fill_meat <- df_tot_age_gr_mae_fill %>% 
  filter(age_groups=="17- bis 25-jährig") %>% 
  count(meat)

df_mae_2634_fill_meat <- df_tot_age_gr_mae_fill %>% 
  filter(age_groups=="26- bis 34-jährig") %>% 
  count(meat)

df_mae_3549_fill_meat <- df_tot_age_gr_mae_fill %>% 
  filter(age_groups=="35- bis 49-jährig") %>% 
  count(meat)

df_mae_5064_fill_meat <- df_tot_age_gr_mae_fill %>% 
  filter(age_groups=="50- bis 64-jährig") %>% 
  count(meat)


# Prozent
df_mae_1725_fill_meat$pct_1725<-df_mae_1725_fill_meat$n/sum(df_mae_1725_fill_meat$n)

df_mae_2634_fill_meat$pct_2634<-df_mae_2634_fill_meat$n/sum(df_mae_2634_fill_meat$n)

df_mae_3549_fill_meat$pct_3549<-df_mae_3549_fill_meat$n/sum(df_mae_3549_fill_meat$n)

df_mae_5064_fill_meat$pct_5064<-df_mae_5064_fill_meat$n/sum(df_mae_5064_fill_meat$n)



# Umwandeln der Datensätze für den Plot
df_mae_1725_m <- melt(df_mae_1725_fill_meat[,c("meat", "pct_1725")], id.vars = 1)
df_mae_1725_m$meat <- ordered(df_mae_1725_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "Unbekannt"))

df_mae_2634_m <- melt(df_mae_2634_fill_meat[,c("meat", "pct_2634")], id.vars = 1)
df_mae_2634_m$meat <- ordered(df_mae_2634_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "Unbekannt"))

df_mae_3549_m <- melt(df_mae_3549_fill_meat[,c("meat", "pct_3549")], id.vars = 1)
df_mae_3549_m$meat <- ordered(df_mae_3549_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "Unbekannt"))

df_mae_5064_m <- melt(df_mae_5064_fill_meat[,c("meat", "pct_5064")], id.vars = 1)
df_mae_5064_m$meat <- ordered(df_mae_5064_m$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "Unbekannt"))

#Zusammenführen der Datensätze
df_mae_T_m <- rbind(df_mae_1725_m, df_mae_2634_m, df_mae_3549_m, df_mae_5064_m)
df_mae_T_m$variable <- dplyr::recode(df_mae_T_m$variable, "pct_1725"="17- bis 25-jährig (n=208)", "pct_2634"="26- bis 34-jährig (n=130)", "pct_3549"="35- bis 49-jährig (n=51)", "pct_5064"="50- bis 64-jährig (n=23)")

#NA in 0 für Plot
df_mae_T_m  %<>%  
  complete(meat, variable, fill= list(per=0))

# Plot
ggplot(df_mae_T_m,aes(x = meat,y = value, width=0.7)) +
  geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
  labs(x = '',y='Prozent')+
  scale_fill_manual("",values = c("17- bis 25-jährig (n=208)"="#fad60d", "26- bis 34-jährig (n=130)"="#505050", "35- bis 49-jährig (n=51)"="grey", "50- bis 64-jährig (n=23)"="#c5b87c"))+
  mytheme+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
            size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
  ggtitle("Ich esse Fleisch (alle Sorten inkl. Geflügel, Wurstwaren etc.):\nMänner") +
  
  ggsave(("04_plots/Frage_9_Fleisch_Altersgruppen_Maenner_191016_vori.pdf"),
         width = 17,
         height = 8,
         dpi=600,
         units="in",
         device="pdf")



# Fleisch Altersgruppen und Geschlecht ein Plot ----

df_tot_age_gr_fill_gen <- df_tot_age_gr_fill  %>%
  filter(gender=="Mann" | gender=="Frau") # 746 Beobachtungen


df_1725_fill_meat_gen <- df_tot_age_gr_fill_gen %>%
  filter(age_groups=="17- bis 25-jährig") %>%
  group_by(gender) %>%
  count(meat) %>%
  mutate(pct_1725=n/sum(n))

df_2634_fill_meat_gen <- df_tot_age_gr_fill_gen %>%
  filter(age_groups=="26- bis 34-jährig") %>%
  group_by(gender) %>%
  count(meat) %>%
  mutate(pct_2634=n/sum(n))

df_3549_fill_meat_gen <- df_tot_age_gr_fill_gen %>%
  filter(age_groups=="35- bis 49-jährig") %>%
  group_by(gender) %>%
  count(meat) %>%
  mutate(pct_3549=n/sum(n))

df_5064_fill_meat_gen <- df_tot_age_gr_fill_gen %>%
  filter(age_groups=="50- bis 64-jährig") %>%
  group_by(gender) %>%
  count(meat) %>%
  mutate(pct_5064=n/sum(n))


# Umwandeln der Datensätze für den Plot
df_1725_mg <- melt(df_1725_fill_meat_gen[,c("meat", "gender", "pct_1725")], id.vars = c("meat", "gender"))
df_1725_mg$meat <- ordered(df_1725_mg$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "Unbekannt"))

df_2634_mg <- melt(df_2634_fill_meat_gen[,c("meat", "pct_2634", "gender")], id.vars = c("meat", "gender"))
df_2634_mg$meat <- ordered(df_2634_mg$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "Unbekannt"))

df_3549_mg <- melt(df_3549_fill_meat_gen[,c("meat", "pct_3549", "gender")], id.vars = c("meat", "gender"))
df_3549_mg$meat <- ordered(df_3549_mg$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "Unbekannt"))

df_5064_mg <- melt(df_5064_fill_meat_gen[,c("meat", "pct_5064", "gender")], id.vars = c("meat", "gender"))
df_5064_mg$meat <- ordered(df_5064_mg$meat, levels = c("2x und mehr pro Tag", "1x pro Tag", "5-6x pro Woche", "3-4x pro Woche", "1-2x pro Woche", "1-2x pro Monat", "nie", "Unbekannt"))

#Zusammenführen der Datensätze
df_T_mg <- rbind(df_1725_mg, df_2634_mg, df_3549_mg, df_5064_mg)
df_T_mg$variable <- dplyr::recode(df_T_mg$variable, "pct_1725"="17- bis 25-jährig (n=398)", "pct_2634"="26- bis 34-jährig (n=221)", "pct_3549"="35- bis 49-jährig (n=65)", "pct_5064"="50- bis 64-jährig (n=49)")

ggplot(df_T_mg,aes(x = meat,y = value, width=0.7)) +
  geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
  labs(x = '',y='Prozent')+
  scale_fill_manual("",values = c("17- bis 25-jährig (n=398)"="#fad60d", "26- bis 34-jährig (n=221)"="#505050", "35- bis 49-jährig (n=65)"="grey", "50- bis 64-jährig (n=49)"="#c5b87c"))+
  mytheme+
  theme(legend.position = "bottom")+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
            size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+
  ggtitle("Ich esse Fleisch (alle Sorten inkl. Geflügel, Wurstwaren etc.):") +
  facet_wrap(~gender)


  ggsave(("04_plots/Frage_9_Fleisch_Altersgruppen_geschlecht_190924_vori.pdf"),
         width = 17,
         height = 8,
         dpi=600,
         units="in",
         device="pdf")

  
  # Verpfelgungstyp ----
  
  df_1725_typ <- df_tot_age_gr_fill %>% filter(age_groups=="17- bis 25-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Einkäufer" | Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger")
  
  df_2634_typ <- df_tot_age_gr_fill %>% filter(age_groups=="26- bis 34-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Einkäufer" | Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger")
  
  df_3549_typ <- df_tot_age_gr_fill %>% filter(age_groups=="35- bis 49-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Einkäufer" | Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger")
  
  df_5064_typ <- df_tot_age_gr_fill %>% filter(age_groups=="50- bis 64-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Einkäufer" | Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger")

  
  # Prozent
  
  df_1725_typ$pct_1725 <- df_1725_typ$n/sum(df_1725_typ$n)
  df_2634_typ$pct_2634 <- df_2634_typ$n/sum(df_2634_typ$n)
  df_3549_typ$pct_3549 <- df_3549_typ$n/sum(df_3549_typ$n)
  df_5064_typ$pct_5064 <- df_5064_typ$n/sum(df_5064_typ$n)
  
  # Umwandeln für Plot
  df_1725_ver <- melt(df_1725_typ[,c("Verpflegungstyp", "pct_1725")], id.vars = 1)
  df_2634_ver <- melt(df_2634_typ[,c("Verpflegungstyp", "pct_2634")], id.vars = 1)
  df_3549_ver <- melt(df_3549_typ[,c("Verpflegungstyp", "pct_3549")], id.vars = 1)
  df_5064_ver <- melt(df_5064_typ[,c("Verpflegungstyp", "pct_5064")], id.vars = 1)
  
  #Zusammenführen der Datensätze
  df_mnm_ver <- rbind(df_1725_ver, df_2634_ver, df_3549_ver, df_5064_ver)
  df_mnm_ver$variable <- dplyr::recode(df_mnm_ver$variable, "pct_1725"="17- bis 25-jährig (n=388)", "pct_2634"="26- bis 34-jährig (n=216)", "pct_3549"="35- bis 49-jährig (n=90)", "pct_5064"="50- bis 64-jährig (n=48)"   )
  
  # Mensagänger zuerst, gefolgt von Selbstverpfleger
  df_mnm_ver$Verpflegungstyp <- ordered(df_mnm_ver$Verpflegungstyp, levels = c("Mensagänger", "Selbstverpfleger", "Abwechsler"))
  
  
  # Plot
  ggplot(df_mnm_ver,aes(x = Verpflegungstyp,y = value, width=0.7)) +
    geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
    labs(x = '',y='Prozent')+
    scale_fill_manual("",values = c("17- bis 25-jährig (n=388)"="#fad60d", "26- bis 34-jährig (n=216)"="#505050", "35- bis 49-jährig (n=90)"="grey", "50- bis 64-jährig (n=48)"="#c5b87c"))+
    mytheme+ 
    theme(legend.position = "bottom")+ 
    scale_y_continuous(labels = scales::percent)+
    geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
              size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
    ggtitle("Verpflegungstyp")
  
  ggsave(("04_plots/Frage_11_altersgruppen_191016_vori.pdf"),
         width = 17,
         height = 8,
         dpi=600,
         units="in",
         device="pdf")
  
  # Verpfelgungstyp Frauen----
  
  
  df_fr_1725_typ <- df_tot_age_gr_fr_fill %>% filter(age_groups=="17- bis 25-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Einkäufer" | Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger")
  
  df_fr_2634_typ <- df_tot_age_gr_fr_fill %>% filter(age_groups=="26- bis 34-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Einkäufer" | Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger")
  
  df_fr_3549_typ <- df_tot_age_gr_fr_fill %>% filter(age_groups=="35- bis 49-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Einkäufer" | Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger")
  
  df_fr_5064_typ <- df_tot_age_gr_fr_fill %>% filter(age_groups=="50- bis 64-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Einkäufer" | Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger")
  
  
  # Prozent
  
  df_fr_1725_typ$pct_1725 <- df_fr_1725_typ$n/sum(df_fr_1725_typ$n)
  df_fr_2634_typ$pct_2634 <- df_fr_2634_typ$n/sum(df_fr_2634_typ$n)
  df_fr_3549_typ$pct_3549 <- df_fr_3549_typ$n/sum(df_fr_3549_typ$n)
  df_fr_5064_typ$pct_5064 <- df_fr_5064_typ$n/sum(df_fr_5064_typ$n)
  
  # Umwandeln für Plot
  df_fr_1725_ver <- melt(df_fr_1725_typ[,c("Verpflegungstyp", "pct_1725")], id.vars = 1)
  df_fr_2634_ver <- melt(df_fr_2634_typ[,c("Verpflegungstyp", "pct_2634")], id.vars = 1)
  df_fr_3549_ver <- melt(df_fr_3549_typ[,c("Verpflegungstyp", "pct_3549")], id.vars = 1)
  df_fr_5064_ver <- melt(df_fr_5064_typ[,c("Verpflegungstyp", "pct_5064")], id.vars = 1)
  
  
  
  #Zusammenführen der Datensätze
  df_fr_mnm_ver <- rbind(df_fr_1725_ver, df_fr_2634_ver, df_fr_3549_ver, df_fr_5064_ver)
  df_fr_mnm_ver$variable <- dplyr::recode(df_fr_mnm_ver$variable, "pct_1725"="17- bis 25-jährig (n=173)", "pct_2634"="26- bis 34-jährig (n=85)", "pct_3549"="35- bis 49-jährig (n=41)", "pct_5064"="50- bis 64-jährig (n=25)"   )
  
  # Mensagänger zuerst, gefolgt von Selbstverpfleger
  df_fr_mnm_ver$Verpflegungstyp <- ordered(df_mae_mnm_ver$Verpflegungstyp, levels = c("Mensagänger", "Selbstverpfleger", "Abwechsler", "Einkäufer"))
  
  # Plot
  ggplot(df_fr_mnm_ver,aes(x = Verpflegungstyp,y = value, width=0.7)) +
    geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
    labs(x = '',y='Prozent')+
    scale_fill_manual("",values = c("17- bis 25-jährig (n=173)"="#fad60d", "26- bis 34-jährig (n=85)"="#505050", "35- bis 49-jährig (n=41)"="grey", "50- bis 64-jährig (n=25)"="#c5b87c"))+
    mytheme+ 
    theme(legend.position = "bottom")+ 
    scale_y_continuous(labels = scales::percent)+
    geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
              size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
    ggtitle("Verpflegungstyp\nFrauen")
  
  ggsave(("04_plots/Frage_11_altersgruppen_frauen_191018_vori.pdf"),
         width = 17,
         height = 8,
         dpi=600,
         units="in",
         device="pdf")  
  
  # Verpfelgungstyp Männer----
  
  
  df_mae_1725_typ <- df_tot_age_gr_mae_fill %>% filter(age_groups=="17- bis 25-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Einkäufer" | Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger")
  
  df_mae_2634_typ <- df_tot_age_gr_mae_fill %>% filter(age_groups=="26- bis 34-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Einkäufer" | Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger")
  
  df_mae_3549_typ <- df_tot_age_gr_mae_fill %>% filter(age_groups=="35- bis 49-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Einkäufer" | Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger")
  
  df_mae_5064_typ <- df_tot_age_gr_mae_fill %>% filter(age_groups=="50- bis 64-jährig") %>%  count(Verpflegungstyp) %>% filter(Verpflegungstyp=="Einkäufer" | Verpflegungstyp=="Selbstverpfleger" | Verpflegungstyp=="Abwechsler" | Verpflegungstyp=="Mensagänger")
  
  
  # Prozent
  
  df_mae_1725_typ$pct_1725 <- df_mae_1725_typ$n/sum(df_mae_1725_typ$n)
  df_mae_2634_typ$pct_2634 <- df_mae_2634_typ$n/sum(df_mae_2634_typ$n)
  df_mae_3549_typ$pct_3549 <- df_mae_3549_typ$n/sum(df_mae_3549_typ$n)
  df_mae_5064_typ$pct_5064 <- df_mae_5064_typ$n/sum(df_mae_5064_typ$n)
  
  # Umwandeln für Plot
  df_mae_1725_ver <- melt(df_mae_1725_typ[,c("Verpflegungstyp", "pct_1725")], id.vars = 1)
  df_mae_2634_ver <- melt(df_mae_2634_typ[,c("Verpflegungstyp", "pct_2634")], id.vars = 1)
  df_mae_3549_ver <- melt(df_mae_3549_typ[,c("Verpflegungstyp", "pct_3549")], id.vars = 1)
  df_mae_5064_ver <- melt(df_mae_5064_typ[,c("Verpflegungstyp", "pct_5064")], id.vars = 1)

  
  #Zusammenführen der Datensätze
  df_mae_mnm_ver <- rbind(df_mae_1725_ver, df_mae_2634_ver, df_mae_3549_ver, df_mae_5064_ver)
  df_mae_mnm_ver$variable <- dplyr::recode(df_mae_mnm_ver$variable, "pct_1725"="17- bis 25-jährig (n=206)", "pct_2634"="26- bis 34-jährig (n=126)", "pct_3549"="35- bis 49-jährig (n=49)", "pct_5064"="50- bis 64-jährig (n=23)"   )
  
  #NA in 0 für Plot
  df_mae_mnm_ver  %<>%  
    complete(variable, Verpflegungstyp, fill= list(per=0))
  
  # Mensagänger zuerst, gefolgt von Selbstverpfleger
  df_mae_mnm_ver$Verpflegungstyp <- ordered(df_mae_mnm_ver$Verpflegungstyp, levels = c("Mensagänger", "Selbstverpfleger", "Abwechsler"))
  
  #Plot
  
  ggplot(df_mae_mnm_ver,aes(x = Verpflegungstyp,y = value, width=0.7)) +
    geom_bar(aes(fill = variable),stat = "identity", position = position_dodge(width =0.8))+
    labs(x = '',y='Prozent')+
    scale_fill_manual("",values = c("17- bis 25-jährig (n=206)"="#fad60d", "26- bis 34-jährig (n=126)"="#505050", "35- bis 49-jährig (n=49)"="grey", "50- bis 64-jährig (n=23)"="#c5b87c"))+
    mytheme+ 
    theme(legend.position = "bottom")+ 
    scale_y_continuous(labels = scales::percent)+
    geom_text(aes(label=(ifelse(value<0.001, "",paste( format(round(value*100,1), nsmall = 1), "%", sep = " "))), group=variable),
              size = 3, position = position_dodge(width = 0.8), vjust = -0.5, size = 4)+  
    ggtitle("Verpflegungstyp\nMänner")
  
  ggsave(("04_plots/Frage_11_altersgruppen_maenner_191018_vori.pdf"),
         width = 17,
         height = 8,
         dpi=600,
         units="in",
         device="pdf")  
  
  