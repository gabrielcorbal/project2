library(dplyr)
library(ggplot2)
library(lubridate)
install.packages('xlsx')
library(xlsx)
install.packages("readxl")
library("readxl")
library(plyr)

df = read_excel('databaseglobo.xlsx')

head(df, 4)

#Taking off repeated values
df1 = df[!duplicated(df$Titulo), ]

#transforming to date format

df1$Dia = gsub(x = df1$Dia, replacement = '', pattern =  "\\s\\d*[h]\\d*")

df1$Dia = dmy(df1$Dia)

#Grouping by where the protest happened

df2 = df1 %>%  
  mutate(Onde = case_when(grepl("Chile", x = df1$Descri) ~ "Chile",
                          grepl("Santiago", x = df1$Descri) ~ "Chile",
                          grepl("Chile", x = df1$Titulo) ~ "Chile",
                          grepl("Santiago", x = df1$Titulo) ~ "Chile",
                          grepl("Equador", x = df1$Descri) ~"Equador",
                          grepl("Quito", x = df1$Descri) ~"Equador",
                          grepl("Equador", x = df1$Titulo) ~"Equador",
                          grepl("Quito", x = df1$Titulo) ~"Equador",
                          grepl("Venezuela", x = df1$Descri) ~"Venezuela",
                          grepl("Caracas", x = df1$Descri) ~"Venezuela",
                          grepl("Venezuela", x = df1$Titulo) ~"Venezuela",
                          grepl("Caracas", x = df1$Titulo) ~"Venezuela",
                          grepl("Londres", x = df1$Descri) ~"UK",
                          grepl("Inglaterra", x = df1$Descri) ~"UK",
                          grepl("Londres", x = df1$Titulo) ~"UK",
                          grepl("Inglaterra", x = df1$Titulo) ~"UK",
                          grepl("Barcelona", x = df1$Descri) ~"Espanha",
                          grepl("Catal", x = df1$Descri) ~"Espanha",
                          grepl("Espanha", x = df1$Descri) ~"Espanha",
                          grepl("Barcelona", x = df1$Titulo) ~"Espanha",
                          grepl("Catal", x = df1$Titulo) ~"Espanha",
                          grepl("Espanha", x = df1$Titulo) ~"Espanha",
                          grepl("Hong Kong", x = df1$Descri) ~"Hong Kong",
                          grepl("Hong Kong", x = df1$Titulo) ~"Hong Kong"))

#Column Month

df3 = df2 %>% 
  mutate(Mes = format(as.Date(df2$Dia,format="%Y-%m-%d"),"%Y-%m"))

df3$Mes[is.na(df3$Mes)] = "2019-10"

sum(is.na(df3$Mes))

#Plotting Data

theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

#News about Protest overtime
p = ggplot(df3, aes(Mes))
p + geom_bar(fill = "#87CEFA") +  
  theme(axis.text.x = element_text(angle = 90)) + 
  ylab(label = '# of news') +
  xlab(label = 'Year - Month')

#Protests in Chile

pchile = df3 %>% 
  filter(Onde == 'Chile')


ggplot(pchile, aes(Mes)) + 
  geom_bar(fill = "#FF6666") +  
  theme(axis.text.x = element_text(angle = 90)) + 
  ylab(label = '# of news') +
  xlab(label = 'Year - Month')



#Protests in Equador

pequador= df3 %>% 
  filter(Onde == 'Equador')


ggplot(pequador, aes(Mes)) + 
  geom_bar(fill = "#FF6666") +  
  theme(axis.text.x = element_text(angle = 90)) + 
  ylab(label = '# of news') +
  xlab(label = 'Year - Month')


#Protests in Venezuela

pvene= df3 %>% 
  filter(Onde == 'Venezuela')


ggplot(pvene, aes(Mes)) + 
  geom_bar(fill = "#FF6666") +  
  theme(axis.text.x = element_text(angle = 90)) + 
  ylab(label = '# of news') +
  xlab(label = 'Year - Month')

#Protests in UK

puk= df3 %>% 
  filter(Onde == 'UK')


ggplot(puk, aes(Mes)) + 
  geom_bar(fill = "#FF6666") +  
  theme(axis.text.x = element_text(angle = 90)) + 
  ylab(label = '# of news') +
  xlab(label = 'Year - Month')


#Protests in Espanha

pes= df3 %>% 
  filter(Onde == 'Espanha')


ggplot(pes, aes(Mes)) + 
  geom_bar(fill = "#FF6666") +  
  theme(axis.text.x = element_text(angle = 90)) + 
  ylab(label = '# of news') +
  xlab(label = 'Year - Month')

#Protests in Hong Kong

phk= df3 %>% 
  filter(Onde == 'Hong Kong')


ggplot(phk, aes(Mes)) + 
  geom_bar(fill = "#FF6666") +  
  theme(axis.text.x = element_text(angle = 90)) + 
  ylab(label = '# of news') +
  xlab(label = 'Year - Month')


#Protests categorized

dfall <- rbind(pchile,pequador,pes,phk,puk,pvene)

ggplot(dfall, aes(Mes)) + 
  geom_bar(aes(fill = dfall$Onde), position = "dodge") +  
  theme(axis.text.x = element_text(angle = 90)) + 
  ylab(label = '# of news') +
  xlab(label = 'Year - Month')


