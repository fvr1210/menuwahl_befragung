library (ggforce) #Version 0.3.0
library (reshape2) #Version 1.4.3
library (magrittr) #Version 1.5
library (gmodels) #Version 2.18.1
library (tidyverse) #Version 1.2.1
library (extrafont) # Version 0.17
loadfonts(device = "win")

# Daten einlesen ---------

df_tot <- read_delim("2019_ZHAW_vonRickenbach_cleaned_recorded_survey_dataset_NOVANIMAL_anonym.csv", delim=';', locale = locale(encoding = 'ISO-8859-2'))

# Teilstichproben bilden

# Nur Personen die zum ersten mal den Fragebogen ausgefuellt haben. Teilstichprobe A
df_tot_fill <- df_tot %>% 
  filter(fill==0)

# Nur Personen die ein Mensamenue konsumiert haben. Teilstichprobe B
df_mensa <- df_tot %>% 
  filter(mensa==1)

# Nur Personen die ein Mensamenue konsumiert haben Basiswoche. Teilstichprobe Bb
df_mensa_b <- df_tot %>% 
  filter(mensa==1) %>% 
  filter(intervention==0)

# Nur Personen die ein Mensamenue konsumiert haben Interventionswoche. Teilstichprobe Bc
df_mensa_i <- df_tot %>% 
  filter(mensa==1) %>% 
  filter(intervention==1)

# Nur Personen die ein Mensamenue konsumiert haben und den Fragebogen zum ersten mal ausgefuellt haben. Teilstichprobe C
df_mensa_fill <- df_mensa %>% 
  filter(fill==0)

# Nur Personen bei denen die Menuelinie des konsumierten Mensamenue bekannt ist. Teilstichprobe D
df_linie <- df_mensa %>% 
  filter(!is.na(meal))

# Nur Personen bei denen die Menuelinie des konsumierten Mensamenue bekannt ist und die den Fragebogen zum ersten Mal ausgefuellt haben. Teilstichprobe E
df_linie_fill <- df_linie %>% 
  filter(fill==0)

# Nur Personen die kein Mensamen? konsumiert haben. Teilstichprobe F
df_nonmensa <- df_tot %>% 
  filter(mensa==0)

# Nur Personen die kein Mensamen? konsumiert haben und die den Fragebogen zum ersten Mal ausgefuellt haben. Teilstichprobe G
df_nonmensa_fill <- df_nonmensa %>% 
  filter(fill==0)

# Personen die mehrheitlich ihr Essen in der Mensa kaufen, ohne Mehrfachteilnahmen. Teilstichprobe H
df_MG_fill <- df_tot_fill %>% 
  filter(Verpflegungstyp=="Mensagaenger") 

# Personen die mehrheitlich ihr Essen selber mitzunehmen, ohne Mehrfachteilnahmen. Teilstichprobe I
df_SV_fill <- df_tot_fill %>% 
  filter(Verpflegungstyp=="Selbstverpfleger")

# Personen die keinen Hauptverpflegungstyp haben, ohne Mehrfachteilnahmen. Teilstichprobe J
df_AW_fill <- df_tot_fill %>% 
  filter(Verpflegungstyp=="Abwechsler") 


# Personen die mehrheitlich Sachen eingekauft haben (auch in dieser Gruppe wenn sie gleichviel von zuhause mitgenommen haben), ohne Mehrfachteilnahmen. Teilstichprobe Ja
df_EK_fill <- df_tot_fill %>% 
  filter(Verpflegungstyp=="Einkaeufer") 

# Personen die ein Fleisch Gericht konsumiert haben. Teilstichprobe K
df_meat <- df_linie %>% 
  filter(label_content=="Fleisch/Fisch")

# Personen die ein Fleisch Gericht konsumiert haben, ohne Mehrfachteilnahmen. Teilstichprobe L
df_meat_fill <- df_linie_fill %>% 
  filter(label_content=="Fleisch/Fisch")

# Personen die ein vegetarisches Gericht konsumiert haben. Teilstichprobe M
df_veget <- df_linie %>% 
  filter(label_content=="vegetarisch")

# Personen die ein pflanzliches Gericht konsumiert haben, ohne Mehrfachteilnahme. Teilstichprobe N
df_veg <- df_linie %>% 
  filter(label_content=="rein pflanzlich" | label_content=="rein pflanzlich*")

# Personen die ein Hot und Cold Gericht konsumiert haben. Teilstichprobe O
df_HC <- df_linie %>% 
  filter(label_content=="H&C")

# Personen die ein Favorite oder World Gericht konsumiert haben. Teilstichprobe P
df_favwor <- df_tot %>% 
  filter(meal=="Favorite" | meal=="World")

# Personen die ein Favorite oder World Gericht konsumiert haben. Teilstichprobe Pa
df_kitch<- df_tot %>% 
  filter(meal=="Kitchen")

# Männer Teilstichprobe Q
df_mann <- df_tot %>% 
  filter(gender=="Mann") 

# Männer, die ein Mensamenü konsumiert haben. Teilstichprobe R
df_mann_mensa <- df_mensa %>% 
  filter(gender=="Mann") 

# Männer ohne Mehrfachteilnahme. Teilstichprobe S
df_mann_fill <- df_tot_fill %>% 
  filter(gender=="Mann") 

# Männer ohne Mehrfachteilnahme bei denen das Alter bekannt ist. Teilstichprobe Sb
df_mann_age_fill <- df_tot_fill %>% 
  filter(gender=="Mann" & !is.na(age_groups)) 

# Frauen Teilstichprobe T
df_frau <- df_tot %>% 
  filter(gender=="Frau") 

# Frauen, die ein Mensamenü konsumiert haben. Teilstichprobe U
df_frau_mensa <- df_mensa %>% 
  filter(gender=="Frau") 

# Frauen ohne Mehrfachteilnahme. Teilstichprobe V (Stichprobe mit Alter ist gleich)
df_frau_fill <- df_tot_fill %>% 
  filter(gender=="Frau") 

# Männer bei denen die Menülinie des konsumierten Mensamenü bekannt ist. Teilstichprobe W
df_mann_linie <- df_mensa %>% 
  filter(!is.na(meal)) %>% 
  filter(gender=="Mann")

# Männer bei denen die Menülinie des konsumierten Mensamenü bekannt ist, ohne Mehrfachteilnahmen. Teilstichprobe X
df_mann_linie_fill <- df_mensa %>% 
  filter(!is.na(meal)) %>% 
  filter(gender=="Mann") %>% 
  filter(fill==0)
  
# Frauen bei denen die Menülinie des konsumierten Mensamenü bekannt ist. Teilstichprobe y
df_frau_linie <- df_mensa %>% 
  filter(!is.na(meal)) %>% 
  filter(gender=="Frau")
  
# Frauen bei denen die Menülinie des konsumierten Mensamenü bekannt ist, ohne Mehrfachteilnahmen. Teilstichprobe z
df_frau_linie_fill <- df_mensa %>% 
  filter(!is.na(meal)) %>% 
  filter(gender=="Frau") %>% 
  filter(fill==0)

# Fleischkonsum

df_tot_meatyp <- df_tot_fill %>%
filter(!is.na(meat)) %>% 
  mutate(meat_diet = case_when (meat==7 ~ "Fleischliebhaber",
                                meat==6 ~ "Fleischliebhaber",
                                meat==5 ~ "Fleischesser",
                                meat==4 ~ "Fleisch-Flexitarier",
                                meat==3 ~ "Vegi-Flexitarier",
                                meat==2 ~ "Fleischverzichter",
                                meat==1 ~ "Fleischverzichter", 
                                TRUE ~ as.character(NA)))
# Fleischliebhaber Teilstichprobe Aa
df_fl <- df_tot_meatyp %>% 
  filter(meat_diet=="Fleischliebhaber")

# Fleischesser Teilstichprobe Ab
df_fe <- df_tot_meatyp %>% 
  filter(meat_diet=="Fleischesser")  

# Flexitarier Teilstichprobe Ac
df_ffx <- df_tot_meatyp %>% 
  filter(meat_diet=="Fleisch-Flexitarier")  

# Flexitarier Teilstichprobe Ad
df_vfx <- df_tot_meatyp %>% 
  filter(meat_diet=="Vegi-Flexitarier")  

# Fleischverzichter Teilstichprobe Ae
df_fv <- df_tot_meatyp %>% 
  filter(meat_diet=="Fleischverzichter")  

# Stichrpbe nur nur für Personen die einen Fleischkonsum von 5-6x pro Woche und 1-2x pro Woche Fleisch Essen Ae
df_redu <- df_tot %>% 
  filter(meat<=5 & meat>=3)

# Stichrpbe nur nur für Personen die einen Fleischkonsum von 5-6x pro Woche und 1-2x pro Woche Fleisch Essen, ohne Mehrfachteilnahme Af
df_redu_fill<- df_tot_fill %>% 
  filter(meat<=5 & meat>=3)

# Maenner der verschiednen Altersgruppen Teilstichproben Ag

df_m1725 <- df_tot_fill %>% 
  filter(gender=="Mann" & age_groups=="17- bis 25-jaehrig")

df_m2634 <- df_tot_fill %>% 
  filter(gender=="Mann" & age_groups=="26- bis 34-jaehrig")

df_m3549 <- df_tot_fill %>% 
  filter(gender=="Mann" & age_groups=="35- bis 49-jaehrig")

df_m5064 <- df_tot_fill %>% 
  filter(gender=="Mann" & age_groups=="50- bis 64-jaehrig")

# Frauen der verschiednen Altersgruppen Teilstichproben Ah

df_f1725 <- df_tot_fill %>% 
  filter(gender=="Frau" & age_groups=="17- bis 25-jaehrig")

df_f2634 <- df_tot_fill %>% 
  filter(gender=="Frau" & age_groups=="26- bis 34-jaehrig")

df_f3549 <- df_tot_fill %>% 
  filter(gender=="Frau" & age_groups=="35- bis 49-jaehrig")

df_f5064 <- df_tot_fill %>% 
  filter(gender=="Frau" & age_groups=="50- bis 64-jaehrig")

# Nur Personen die MitarbeiterInnen, StudentInnen oder WeiterbildungsteilnehmerInnen sind und Mann/Frau, ohne mehrfachteilnahmen
# Teilstichprobe Ai
df_bsv <- df_tot_fill %>% 
  filter(member == "Student/in" | member == "Mitarbeiter/in" | member == "Weiterbildungsteilnehmer/in" ) %>% 
  filter(gender == "Mann" | gender == "Frau")


# Stichproben beschreibung mit Alter, Geschlecht und Hochschulzugehörigkeit  ----

# Gesamtstichprobe ----
# Alter
# summary(df_tot$age)
# sd(df_tot$age, na.rm = T)

# Geschlecht
table(df_tot$gender, exclude=NULL)
table(df_tot$gender, exclude=NULL)/nrow(df_tot)*100


# Hochschulzugeh?rigkeit
table(df_tot$member, exclude=NULL)
table(df_tot$member, exclude=NULL)/nrow(df_tot)*100

df_tot <- df_tot %>% mutate(gender = ifelse(is.na(gender), "Unbekannt", gender))
df_tot <- df_tot %>% mutate(member = ifelse(is.na(member), "Unbekannt", member))

CrossTable(df_tot$gender, df_tot$member, format = "SPSS")


# Teilstichprobe A ----
# Alter
# summary(df_tot_fill$age)
# sd(df_tot_fill$age, na.rm = T)

# Geschlecht
table(df_tot_fill$gender, exclude=NULL)
table(df_tot_fill$gender, exclude=NULL)/nrow(df_tot_fill)

# Hochschulzugeh?rigkeit
table(df_tot_fill$member, exclude=NULL)
table(df_tot_fill$member, exclude=NULL)/nrow(df_tot_fill)

df_tot_fill <- df_tot_fill %>% mutate(gender = ifelse(is.na(gender), "Unbekannt", gender))
df_tot_fill <- df_tot_fill %>% mutate(member = ifelse(is.na(member), "Unbekannt", member))

CrossTable(df_tot_fill$gender, df_tot_fill$member)


# Teilstichprobe B----
# Alter
# summary(df_mensa$age)
# sd(df_mensa$age, na.rm = T)

# Geschlecht
table(df_mensa$gender, exclude=NULL)
table(df_mensa$gender, exclude=NULL)/nrow(df_mensa)

# Hochschulzugeh?rigkeit
table(df_mensa$member, exclude=NULL)
table(df_mensa$member, exclude=NULL)/nrow(df_mensa)

df_mensa <- df_mensa %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))

  
  
CrossTable(df_mensa$member, df_mensa$gender, formate=c("SPSS"))

# Teilstichprobe Bb----
# Alter
# summary(df_mensa_b$age)
# sd(df_mensa_b$age, na.rm = T)

# Geschlecht
table(df_mensa_b$gender, exclude=NULL)
table(df_mensa_b$gender, exclude=NULL)/nrow(df_mensa_b)

# Hochschulzugeh?rigkeit
table(df_mensa_b$member, exclude=NULL)
table(df_mensa_b$member, exclude=NULL)/nrow(df_mensa_b)

df_mensa_b <- df_mensa_b %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_mensa_b$member, df_mensa_b$gender, formate=c("SPSS"))

# Teilstichprobe Bc----
# Alter
# summary(df_mensa_i$age)
# sd(df_mensa_i$age, na.rm = T)

# Geschlecht
table(df_mensa_i$gender, exclude=NULL)
table(df_mensa_i$gender, exclude=NULL)/nrow(df_mensa_i)

# Hochschulzugeh?rigkeit
table(df_mensa_i$member, exclude=NULL)
table(df_mensa_i$member, exclude=NULL)/nrow(df_mensa_i)

df_mensa_i <- df_mensa_i %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_mensa_i$member, df_mensa_i$gender, formate=c("SPSS"))



# Teilstichprobe C----
# Alter
# summary(df_mensa_fill$age)
# sd(df_mensa_fill$age, na.rm = T)

# Geschlecht
table(df_mensa_fill$gender, exclude=NULL)
table(df_mensa_fill$gender, exclude=NULL)/nrow(df_mensa_fill)

# Hochschulzugeh?rigkeit
table(df_mensa_fill$member, exclude=NULL)
table(df_mensa_fill$member, exclude=NULL)/nrow(df_mensa_fill)

# Teilstichprobe D----
# Alter
# summary(df_linie$age)
# sd(df_linie$age, na.rm = T)

# Geschlecht
table(df_linie$gender, exclude=NULL)
table(df_linie$gender, exclude=NULL)/nrow(df_linie)

# Hochschulzugeh?rigkeit
table(df_linie$member, exclude=NULL)
table(df_linie$member, exclude=NULL)/nrow(df_linie)

# Teilstichprobe E----
# Alter
# summary(df_linie_fill$age)
# sd(df_linie_fill$age, na.rm = T)

# Geschlecht
table(df_linie_fill$gender, exclude=NULL)
table(df_linie_fill$gender, exclude=NULL)/nrow(df_linie_fill)*100

# Hochschulzugeh?rigkeit
table(df_linie_fill$member, exclude=NULL)
table(df_linie_fill$member, exclude=NULL)/nrow(df_linie_fill)*100


# Teilstichprobe F----
# Alter
# summary(df_nonmensa$age)
# sd(df_nonmensa$age, na.rm = T)

# Geschlecht
table(df_nonmensa$gender, exclude=NULL)
table(df_nonmensa$gender, exclude=NULL)/nrow(df_nonmensa)

# Hochschulzugeh?rigkeit
table(df_nonmensa$member, exclude=NULL)
table(df_nonmensa$member, exclude=NULL)/nrow(df_nonmensa)

df_nonmensa <- df_nonmensa %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_nonmensa$member, df_nonmensa$gender, formate=c("SPSS"))


# Teilstichprobe G----
# Alter
# summary(df_nonmensa_fill$age)
# sd(df_nonmensa_fill$age, na.rm = T)

# Geschlecht
table(df_nonmensa_fill$gender, exclude=NULL)
table(df_nonmensa_fill$gender, exclude=NULL)/nrow(df_nonmensa_fill)*100

# Hochschulzugeh?rigkeit
table(df_nonmensa_fill$member, exclude=NULL)
table(df_nonmensa_fill$member, exclude=NULL)/nrow(df_nonmensa_fill)*100


# Teilstichprobe H----
# Alter
# summary(df_MG_fill$age)
# sd(df_MG_fill$age, na.rm = T)

# Geschlecht
table(df_MG_fill$gender, exclude=NULL)
table(df_MG_fill$gender, exclude=NULL)/nrow(df_MG_fill)*100

# Hochschulzugeh?rigkeit
table(df_MG_fill$member, exclude=NULL)
table(df_MG_fill$member, exclude=NULL)/nrow(df_MG_fill)*100

df_MG_fill <- df_MG_fill %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_MG_fill$member, df_MG_fill$gender, formate=c("SPSS"))

# Teilstichprobe I----
# Alter
# summary(df_SV_fill$age)
# sd(df_SV_fill$age, na.rm = T)

# Geschlecht
table(df_SV_fill$gender, exclude=NULL)
table(df_SV_fill$gender, exclude=NULL)/nrow(df_SV_fill)*100

# Hochschulzugeh?rigkeit
table(df_SV_fill$member, exclude=NULL)
table(df_SV_fill$member, exclude=NULL)/nrow(df_SV_fill)*100

df_SV_fill <- df_SV_fill %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_SV_fill$member, df_SV_fill$gender, formate=c("SPSS"))

# Teilstichprobe J----
# Alter
# summary(df_AW_fill$age)
# sd(df_AW_fill$age, na.rm = T)

# Geschlecht
table(df_AW_fill$gender, exclude=NULL)
table(df_AW_fill$gender, exclude=NULL)/nrow(df_AW_fill)*100

# Hochschulzugeh?rigkeit
table(df_AW_fill$member, exclude=NULL)
table(df_AW_fill$member, exclude=NULL)/nrow(df_AW_fill)*100


df_AW_fill <- df_AW_fill %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_AW_fill$member, df_AW_fill$gender, formate=c("SPSS"))


# Teilstichprobe Ja----
# Alter
# summary(df_EK_fill$age)
# sd(df_EK_fill$age, na.rm = T)

# Geschlecht
table(df_EK_fill$gender, exclude=NULL)
table(df_EK_fill$gender, exclude=NULL)/nrow(df_EK_fill)*100

# Hochschulzugeh?rigkeit
table(df_EK_fill$member, exclude=NULL)
table(df_EK_fill$member, exclude=NULL)/nrow(df_EK_fill)*100


df_EK_fill <- df_EK_fill %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_EK_fill$member, df_EK_fill$gender, formate=c("SPSS"))



# Teilstichprobe K----
# Alter
# summary(df_meat$age)
# sd(df_meat$age, na.rm = T)

# Geschlecht
table(df_meat$gender, exclude=NULL)
table(df_meat$gender, exclude=NULL)/nrow(df_meat)*100

# Hochschulzugeh?rigkeit
table(df_meat$member, exclude=NULL)
table(df_meat$member, exclude=NULL)/nrow(df_meat)*100


df_meat <- df_meat %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_meat$member, df_meat$gender, formate=c("SPSS"))


# Teilstichprobe L----
# Alter
# summary(df_meat_fill$age)
# sd(df_meat_fill$age, na.rm = T)

# Geschlecht
table(df_meat_fill$gender, exclude=NULL)
table(df_meat_fill$gender, exclude=NULL)/nrow(df_meat_fill)*100

# Hochschulzugeh?rigkeit
table(df_meat_fill$member, exclude=NULL)
table(df_meat_fill$member, exclude=NULL)/nrow(df_meat_fill)*100

# Teilstichprobe M----
# Alter
# summary(df_veget$age)
# sd(df_veget$age, na.rm = T)

# Geschlecht
table(df_veget$gender, exclude=NULL)
table(df_veget$gender, exclude=NULL)/nrow(df_veget)*100

# Hochschulzugeh?rigkeit
table(df_veget$member, exclude=NULL)
round(table(df_veget$member, exclude=NULL)/nrow(df_veget)*100, 1)

df_veget <- df_veget %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_veget$member, df_veget$gender, formate=c("SPSS"))



# Teilstichprobe N----
# Alter
# summary(df_veg$age)
# sd(df_veg$age, na.rm = T)

# Geschlecht
table(df_veg$gender, exclude=NULL)
round(table(df_veg$gender, exclude=NULL)/nrow(df_veg)*100,1)

# Hochschulzugeh?rigkeit
table(df_veg$member, exclude=NULL)
round(table(df_veg$member, exclude=NULL)/nrow(df_veg)*100,1)

df_veg <- df_veg %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_veg$member, df_veg$gender, formate=c("SPSS"))


# Teilstichprobe O----
# Alter
# summary(df_HC$age)
# sd(df_HC$age, na.rm = T)

# Geschlecht
table(df_HC$gender, exclude=NULL)
table(df_HC$gender, exclude=NULL)/nrow(df_HC)*100

# Hochschulzugeh?rigkeit
table(df_HC$member, exclude=NULL)
round(table(df_HC$member, exclude=NULL)/nrow(df_HC)*100, 1)


df_HC <- df_HC %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_HC$member, df_HC$gender, formate=c("SPSS"))


df_HC <- df_HC %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_HC$member, df_HC$gender, formate=c("SPSS"))

# Teilstichprobe P----
# Alter
# summary(df_favwor$age)
# sd(df_favwor$age, na.rm = T)

# Geschlecht
table(df_favwor$gender, exclude=NULL)
round(table(df_favwor$gender, exclude=NULL)/nrow(df_favwor)*100,1)

# Hochschulzugeh?rigkeit
table(df_favwor$member, exclude=NULL)
round(table(df_favwor$member, exclude=NULL)/nrow(df_favwor)*100,1)

df_favwor <- df_favwor %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_favwor$member, df_favwor$gender, formate=c("SPSS"))

# Teilstichprobe Pa----
# Alter
# summary(df_kitch$age)
# sd(df_kitch$age, na.rm = T)

# Geschlecht
table(df_kitch$gender, exclude=NULL)
round(table(df_kitch$gender, exclude=NULL)/nrow(df_kitch)*100,1)

# Hochschulzugeh?rigkeit
table(df_kitch$member, exclude=NULL)
round(table(df_kitch$member, exclude=NULL)/nrow(df_kitch)*100,1)

df_kitch <- df_kitch %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_kitch$member, df_kitch$gender, formate=c("SPSS"))



# Teilstichprobe Q----
# Alter
# summary(df_mann$age)
# sd(df_mann$age, na.rm = T)

# Hochschulzugeh?rigkeit
table(df_mann$member, exclude=NULL)
round(table(df_mann$member, exclude=NULL)/nrow(df_mann)*100,1)

# Teilstichprobe R----
# Alter
# summary(df_mann_mensa$age)
# sd(df_mann_mensa$age, na.rm = T)

# Hochschulzugeh?rigkeit
table(df_mann_mensa$member, exclude=NULL)
round(table(df_mann_mensa$member, exclude=NULL)/nrow(df_mann_mensa)*100,1)

# Teilstichprobe S----
# Alter
# summary(df_mann_fill$age)
# sd(df_mann_fill$age, na.rm = T)

# Hochschulzugeh?rigkeit
table(df_mann_fill$member, exclude=NULL)
round(table(df_mann_fill$member, exclude=NULL)/nrow(df_mann_fill)*100,1)

# Teilstichprobe Sb----
# Alter
# summary(df_mann_age_fill$age)
# sd(df_mann_age_fill$age, na.rm = T)

# Hochschulzugeh?rigkeit
# table(df_mann_age_fill$member, exclude=NULL)
# round(table(df_mann_age_fill$member, exclude=NULL)/nrow(df_mann_age_fill)*100,1)



# Teilstichprobe T----
# Alter
# summary(df_frau$age)
# sd(df_frau$age, na.rm = T)

# Hochschulzugeh?rigkeit
table(df_frau$member, exclude=NULL)
round(table(df_frau$member, exclude=NULL)/nrow(df_frau)*100,1)

# Teilstichprobe U----
# Alter
# summary(df_frau_mensa$age)
# sd(df_frau_mensa$age, na.rm = T)

# Hochschulzugeh?rigkeit
table(df_frau_mensa$member, exclude=NULL)
round(table(df_frau_mensa$member, exclude=NULL)/nrow(df_frau_mensa)*100,1)

# Teilstichprobe V----
# Alter
# summary(df_frau_fill$age)
# sd(df_frau_fill$age, na.rm = T)

# Hochschulzugeh?rigkeit
table(df_frau_fill$member, exclude=NULL)
round(table(df_frau_fill$member, exclude=NULL)/nrow(df_frau_fill)*100,1)

# Teilstichprobe W----
# Alter
# summary(df_mann_linie$age)
# sd(df_mann_linie$age, na.rm = T)

# Hochschulzugeh?rigkeit
table(df_fmann_linie$member, exclude=NULL)
round(table(df_mann_linie$member, exclude=NULL)/nrow(df_mann_linie)*100,1)

# Teilstichprobe X----
# Alter
# summary(df_mann_linie_fill$age)
# sd(df_mann_linie_fill$age, na.rm = T)

# Hochschulzugeh?rigkeit
table(df_mann_linie_fill$member, exclude=NULL)
round(table(df_mann_linie_fill$member, exclude=NULL)/nrow(df_mann_linie_fill)*100,1)

# Teilstichprobe Y----
# Alter
# summary(df_frau_linie$age)
# sd(df_frau_linie$age, na.rm = T)

# Hochschulzugeh?rigkeit
table(df_frau_linie$member, exclude=NULL)
round(table(df_frau_linie$member, exclude=NULL)/nrow(df_frau_linie)*100,1)

# Teilstichprobe Z----
# Alter
# summary(df_frau_linie_fill$age)
# sd(df_frau_linie_fill$age, na.rm = T)


# Hochschulzugeh?rigkeit
table(df_frau_linie_fill$member, exclude=NULL)
round(table(df_frau_linie_fill$member, exclude=NULL)/nrow(df_frau_linie_fill)*100,1)



# Teilstichprobe Aa -----
# Alter
# summary(df_fl$age)
# sd(df_fl$age, na.rm = T)

# Geschlecht
table(df_fl$gender, exclude=NULL)
round(table(df_fl$gender, exclude=NULL)/nrow(df_fl)*100,1)

# Hochschulzugeh?rigkeit
table(df_fl$member, exclude=NULL)
round(table(df_fl$member, exclude=NULL)/nrow(df_fl)*100,1)

df_fl <- df_fl %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_fl$member, df_fl$gender, formate=c("SPSS"))

# Teilstichprobe Ab -----
# Alter
# summary(df_fe$age)
# sd(df_fe$age, na.rm = T)

# Geschlecht
table(df_fe$gender, exclude=NULL)
round(table(df_fe$gender, exclude=NULL)/nrow(df_fe)*100,1)

# Hochschulzugeh?rigkeit
table(df_fe$member, exclude=NULL)
round(table(df_fe$member, exclude=NULL)/nrow(df_fe)*100,1)

df_fe <- df_fe %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))

# Teilstichprobe Ac -----
# Alter
# summary(df_ffx$age)
# sd(df_ffx$age, na.rm = T)

# Geschlecht
table(df_ffx$gender, exclude=NULL)
round(table(df_ffx$gender, exclude=NULL)/nrow(df_fx)*100,1)

# Hochschulzugeh?rigkeit
table(df_ffx$member, exclude=NULL)
round(table(df_ffx$member, exclude=NULL)/nrow(df_fx)*100,1)

df_ffx <- df_ffx %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_ffx$member, df_ffx$gender, formate=c("SPSS"))

# Teilstichprobe Ad -----
# Alter
# summary(df_vfx$age)
# sd(df_vfx$age, na.rm = T)

# Geschlecht
table(df_ffx$gender, exclude=NULL)
round(table(df_vfx$gender, exclude=NULL)/nrow(df_fx)*100,1)

# Hochschulzugeh?rigkeit
table(df_vfx$member, exclude=NULL)
round(table(df_vfx$member, exclude=NULL)/nrow(df_fx)*100,1)

df_vfx <- df_vfx %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_vfx$member, df_vfx$gender, formate=c("SPSS"))

# Teilstichprobe Ae -----
# Alter
# summary(df_fv$age)
# sd(df_fv$age, na.rm = T)

# Geschlecht
table(df_fv$gender, exclude=NULL)
round(table(df_fv$gender, exclude=NULL)/nrow(df_fv)*100,1)

# Hochschulzugeh?rigkeit
table(df_fv$member, exclude=NULL)
round(table(df_fv$member, exclude=NULL)/nrow(df_fv)*100,1)

df_fv <- df_fv %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_fv$member, df_fv$gender, formate=c("SPSS"))

# Teilstichprobe Ae -----
# Alter
# summary(df_redu$age)
# sd(df_redu$age, na.rm = T)

# Geschlecht
table(df_redu$gender, exclude=NULL)
round(table(df_redu$gender, exclude=NULL)/nrow(df_redu)*100,1)

# Hochschulzugeh?rigkeit
table(df_redu$member, exclude=NULL)
round(table(df_redu$member, exclude=NULL)/nrow(df_redu)*100,1)

df_redu <- df_redu %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_redu$member, df_redu$gender, formate=c("SPSS"))


# Teilstichprobe Af -----
# Alter
# summary(df_redu_fill$age)
# sd(df_redu_fill$age, na.rm = T)

# Geschlecht
table(df_redu_fill$gender, exclude=NULL)
round(table(df_redu_fill$gender, exclude=NULL)/nrow(df_redu_fill)*100,1)

# Hochschulzugeh?rigkeit
table(df_redu_fill$member, exclude=NULL)
round(table(df_redu_fill$member, exclude=NULL)/nrow(df_redu_fill)*100,1)

df_redu_fill <- df_redu_fill %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender )) %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member ))



CrossTable(df_redu_fill$member, df_redu_fill$gender, formate=c("SPSS"))


# Teilstichproben Ag -----


# Hochschulzugehoerigkeit
table(df_f1725$member, exclude=NULL)
round(table(df_f1725$member, exclude=NULL)/nrow(df_f1725)*100,1)

table(df_f2634$member, exclude=NULL)
round(table(df_f2634$member, exclude=NULL)/nrow(df_f2634)*100,1)

table(df_f3549$member, exclude=NULL)
round(table(df_f3549$member, exclude=NULL)/nrow(df_f3549)*100,1)

table(df_f5064$member, exclude=NULL)
round(table(df_f5064$member, exclude=NULL)/nrow(df_f5064)*100,1)

# Teilstichproben Ah -----


# Hochschulzugehoerigkeit
table(df_m1725$member, exclude=NULL)
round(table(df_m1725$member, exclude=NULL)/nrow(df_m1725)*100,1)

table(df_m2634$member, exclude=NULL)
round(table(df_m2634$member, exclude=NULL)/nrow(df_m2634)*100,1)

table(df_m3549$member, exclude=NULL)
round(table(df_m3549$member, exclude=NULL)/nrow(df_m3549)*100,1)

table(df_m5064$member, exclude=NULL)
round(table(df_m5064$member, exclude=NULL)/nrow(df_m5064)*100,1)


# Crosstable Mensamenü und Geschlecht inkl. missing ----
df_linie <- df_linie %>% 
  mutate(gender = recode(gender, .missing = "Unbekannt")) %>% 
  mutate(member = recode(member, .missing = "Unbekannt"))

CrossTable(df_linie$member, df_linie$gender, na.rm=F)

# Crosstable Mensamenü und Geschlecht inkl. missing, ohne Mehrfachteilnahmen----
df_linie_fill <- df_linie_fill %>% 
  mutate(gender = recode(gender, .missing = "Unbekannt")) %>% 
  mutate(member = recode(member, .missing = "Unbekannt"))


CrossTable(df_linie_fill$member, df_linie_fill$gender, na.rm=F)

# Teilstichproben Ai -----
CrossTable(df_bsv$member, df_bsv$gender, na.rm=F)

# Hochschulzugehoerigkeit




#vergleich Verkaufszahlen und Fragebogen -----

#Laden der Verkaufszahlen

df_verkauf <- read_delim("01_csv_Datenfiles/verkaufte_gerichte_befragungstage_190819_vori.csv", delim=';', locale = locale(encoding = 'ISO-8859-2'))



# with locals
df_daytot <- df_verkauf %>% group_by(date) %>% summarize(sum= sum(tot))
df_verkauf <- left_join(df_verkauf, df_daytot, by="date")
df_verkauf <- df_verkauf %>% 
  mutate(per = tot/sum) %>% 
  mutate(source = c("Anteil Menüs in Kassendatensatz\n(n = 3743)")) %>% 
  rename(meal = article_description) %>%  
  mutate(meal = recode_factor(meal, "Hot and Cold"="H&C")) %>%   
  mutate(meal = recode_factor(meal, "Local Kitchen"="Local\nKitchen", "Local Favorite"="Local\nFavorite", "Local World"="Local\nWorld"))


# Questionnair
df_tot_line <- df_tot %>% filter (mensa==1) %>% select(c (date, meal)) %>% mutate(meal = recode_factor(.$meal, .missing = "NA",  "Hot & Cold"="H&C"))
df_tot_line <- df_tot_line %>% group_by(date, meal) %>% summarise(tot=n())
df_tot_Sumline <- df_tot_line %>% group_by(date) %>% summarize(sum= sum(tot))
df_tot_line <- left_join(df_tot_line, df_tot_Sumline, by="date")
df_tot_line$per<- df_tot_line$tot/df_tot_line$sum
df_tot_line$source <- c("Anteil Menüs in Befragungsstichprobe\n(n = 875)")


df_sm<- bind_rows(df_tot_line, df_verkauf)

df_sm <- df_sm %>% 
  complete(meal, source, fill= list(per=0)) %>%
  ungroup ()

date_list <- list(
  '2017-10-17'="17.10.2017",
  '2017-10-19'="1.10.2017",
  '2017-11-06'="06.11.2017",
  '2017-11-08'="08.11.2017",
  '2017-11-14'="14.11.2017",
  '2017-11-16'="16.11.2017",
  '2017-11-21'="21.11.2017",
  '2017-11-30'="30.11.2017"
)

# Themes laden
mytheme <- theme_bw()+ # definve theme for plot
  theme(plot.title = element_text(size = 6.6, face = "bold",  margin = margin(0, 0, .05, 0., "cm")),
        axis.text.x = element_text(size = 5.5),
        axis.text.y = element_text(size = 6.2),
        legend.text = element_text(size = 6.2),
        legend.title = element_text(size = 6.2),
        strip.text.x = element_text(size = 6.6,  face = "bold", margin = margin(.05, 0, .05, 0, "cm")),
        axis.title.y = element_text(size = 6.2, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 6.2,  margin = margin(t = 0, r = 0, b = 0, l = 0)),
        plot.subtitle = element_text(margin=margin(b=15), size = 6.2),
        plot.caption = element_text(margin=margin(t=15), face="italic", size=  5),
        panel.spacing = unit(0.3, "lines"),
        legend.position = "bottom",
        legend.key.size = unit(0.4, "line"),
        legend.margin=margin(-0.5, 0, 0.05, 0, "cm"),
        text = element_text(family = "Akkurat Light"),
        plot.margin=unit(c(t = 0, r = 0.05, b = 0, l = 0),"cm"))

for (i in 1:2) {
  ggplot(df_sm, aes(x = meal,y = per, width=0.7)) +
    geom_bar(aes(fill = source),stat = "identity", position = position_dodge(width =0.8))+
    labs(x = '',y='')+
    geom_text(aes(label=(ifelse(per<0.001, "",paste( round(per*100), "%", sep = " "))), group=source), size = 1.5, position = position_dodge(width = 0.9), vjust = -0.5, size = 2)+  
    scale_fill_manual("",values = c("Anteil Menüs in Befragungsstichprobe\n(n = 875)"="#fad60d", "Anteil Menüs in Kassendatensatz\n(n = 3743)"="#505050"))+
    mytheme+ 
    theme(legend.position = "bottom")+ 
    facet_wrap_paginate(~date, labeller = labeller(date = c(
     '2017-10-17'="17.10.2017",
      '2017-10-19'="19.10.2017",
      '2017-11-06'="06.11.2017",
      '2017-11-08'="08.11.2017",
      '2017-11-14'="14.11.2017",
      '2017-11-16'="16.11.2017",
      '2017-11-21'="21.11.2017",
      '2017-11-30'="30.11.2017"
    )),
                        ncol = 2, nrow = 2, page = i,
                        strip.position="top", scales = "free")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0, 0.6))+
    scale_x_discrete(limits = c("Favorite", "World", "Kitchen", "H&C", "Local\nFavorite", "Local\nWorld", "Local\nKitchen", "NA"),
                     breaks = c("Favorite", "World", "Kitchen", "H&C", "Local\nFavorite", "Local\nWorld", "Local\nKitchen", "NA"),
                     labels = c("Favorite", "World", "Kitchen", "H&C", "Local\nFavorite", "Local\nWorld", "Local\nKitchen", "NA"))+
    ggtitle("Verteilte Fragebogen verglichen mit der Anzahl verkaufte Gericht pro Menü-Linie an den Befragungstagen")+ 
    ggsave(paste("04_plots/Befragung_vs_Kassendaten_190819_vori_", i, ".pdf", sep=""), 
           width = 14.5,
           height = 10,
           dpi=600,
           units="cm",
           device=cairo_pdf)
}


# Anteil der Menülinien total ohne Local ----
df_verkauf %<>% 
  group_by(article_description) %>% 
  mutate(liniesum = sum(tot)) %>% 
  ungroup() %>% 
  group_by(date) %>%  
  mutate(totsum=sum(liniesum)) %>% 
  mutate(linieper = liniesum/totsum)
