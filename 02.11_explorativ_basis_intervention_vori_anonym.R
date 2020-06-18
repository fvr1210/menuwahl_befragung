# Vergleich von Basis und Interventionswochen -----

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




# Daten einlesen und bearbeiten ---------

df_mensa <- read_delim("2019_ZHAW_vonRickenbach_cleaned_recorded_survey_dataset_NOVANIMAL_anonym.csv", delim=';', locale = locale(encoding = 'ISO-8859-2')) %>% 
  filter(mensa==1) %>%  # nur Mensamenues
  mutate(intervention = ifelse(intervention==1, "Intervention (n = 494)", "Basis (n = 381)")) # recode



# Fragebogen Basis (mensa)
df_basis <- df_mensa %>% 
  filter(intervention=="Basis")

df_basis <- df_basis %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_basis <- df_basis %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_basis$gender, df_basis$member)

# summary(df_basis$age)
# sd(df_basis$age, na.rm = T)


# Fragebogen Intervention (mensa)
df_inter <- df_mensa %>% 
  filter(intervention=="Intervention")

df_inter <- df_inter %>% mutate(gender = ifelse(is.na(gender), "NA", gender))
df_inter <- df_inter %>% mutate(member = ifelse(is.na(member), "NA", member))

CrossTable(df_inter$gender, df_inter$member)

# summary(df_inter$age)
# sd(df_inter$age, na.rm = T)


# Frage 3: Direkter Gruppenvergleich ----

# Datensätze erstellen

df_mensa_satis1 <- df_mensa%>% 
  select(c(satis_1, intervention)) %>% 
  mutate(tot=intervention) %>% 
  mutate(Wahl=satis_1) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("satis_1")) %>% 
  mutate(question = "Meine Erwartungen an das Menü haben sich erfüllt.")

df_mensa_satis2 <- df_mensa%>% 
  select(c(satis_2, intervention)) %>% 
  mutate(tot=intervention) %>% 
  mutate(Wahl=satis_2) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("satis_2")) %>% 
  mutate(question = "Ich fand das Menü gut.")

df_mensa_satis3 <- df_mensa%>% 
  select(c(satis_3, intervention)) %>% 
  mutate(tot=intervention) %>% 
  mutate(Wahl=satis_3) %>% 
  mutate(Zustimmung=sum(Wahl==4, na.rm = T)) %>% 
  select(-c("satis_3")) %>% 
  mutate(question = "Ich werde dieses Menü nicht mehr nehmen.")



# Datensätze zusammenführen
df_mensa_satis <- bind_rows(df_mensa_satis1, df_mensa_satis2, df_mensa_satis3) %>% # 
  mutate(Wahl = recode(.$Wahl, .missing = "NA", "1" = "trifft nicht zu", "2" = "trifft eher nicht zu","3" = "trifft eher zu", "4" = "trifft zu" )) %>% 
  group_by(question, Zustimmung, tot, Wahl) %>%
  summarise(tot_answ = n()) %>%  
  mutate(pct = (tot_answ/sum(tot_answ))) 

# add colors
ColsPerCat = c("NA" = "grey", "trifft nicht zu" = "#DC6413", "trifft eher nicht zu" = "#Fdd200", "trifft eher zu" = "#AFC410", "trifft zu"="#006885")

source("09_function_is_dark_190114_egel.R", chdir = T)


label_color <-  sapply(unlist(ColsPerCat), function(color) { if (isDark(color)) 'white' else 'black' }) # check if color is dark, than give back "white" else "black"

label_color <- as.data.frame(label_color)

df_color <- tibble::rownames_to_column(label_color, var = "Wahl")

df_mensa_satis <- merge(df_mensa_satis, df_color, by="Wahl")


# für Plot Sortieren Augrund der Gesamtzustimmung
df_mensa_satis$question <- reorder(df_mensa_satis$question, -df_mensa_satis$Zustimmung)


  ## plot ----
ggplot(data = df_mensa_satis, aes(x=tot, y=pct, fill=factor(Wahl, levels= c ("NA", "trifft nicht zu", "trifft eher nicht zu", "trifft eher zu", "trifft zu")), color = label_color)) +
  geom_bar(stat = "identity", position = position_stack(), color = NA, width = 0.6) +
  scale_color_manual(values = levels(df_mensa_satis$label_color)) +
  scale_fill_manual(breaks = attributes(ColsPerCat)$name,
                    values = ColsPerCat) +
  scale_x_discrete(limits=c("Basis (n = 381)", "Intervention (n = 494)"))+
  geom_text(aes(label=(ifelse(pct<0.0249, "",paste( round(pct*100), "%", sep = " ")))), size = 2, fontface = "bold", position = position_stack(vjust = 0.5))+
  xlab("")+
  ylab("")+
  guides(color = F, fill = guide_legend("", reverse = T,nrow=1)) +
  ggtitle("Wie zufrieden sind Sie mit dem gewählten Menü?\n(Frage 3, Experimentwoche, n = 875)")+
  coord_flip() +
  mytheme +
  facet_wrap(~question,  nrow = NULL, ncol = 1, scales = "free") +
  scale_y_continuous(labels = scales::percent)

ggsave("04_plots/Frage_3_basis_intervention_200108.pdf",
       width = 14.5,
       height = 6,
       dpi=600,
       units="cm",
       device=cairo_pdf)
