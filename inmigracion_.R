
library(haven)
library(tidyverse)
library(nortest)
library(car)
library(hrbrthemes)
library(scales)
library(extrafont)
windowsFonts("Bahnschrift" = windowsFont("Bahnschrift"))
library(patchwork)



# REPRESENTACIÓN GRÁFICA ----------------------------



# percepcion * escideol -------------------

df <- read_sav("3326.sav")



df <- df %>% 
  select(PERCEPIONUMEROINMIGR, ESCIDEOL) %>% 
  filter(PERCEPIONUMEROINMIGR <5, ESCIDEOL <11) %>% 
  group_by(ESCIDEOL) %>% 
  count(PERCEPIONUMEROINMIGR) %>% 
  mutate(percent = (n/sum(n)*100))
df





p1 <- df %>% 
  ggplot(aes(fill=as.factor(PERCEPIONUMEROINMIGR), y=percent, x=as.factor(ESCIDEOL))) + 
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(name = "",  labels = scales::percent) +
  scale_x_discrete("Ideología") +
  coord_flip() +
  scale_fill_manual(values=c("darkgreen", "green", "firebrick", "darkred"),
                    name = "Percepción nº de \ninmigrantes en España",
                    labels = c("Insuficiente", "Aceptable", "Elevado", "Excesivo")) +
  theme(text = element_text(size=12, face = "bold", family = "Bahnschrift"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        strip.text.x = element_text(color = "white", face = "bold.italic"),
        panel.grid = element_line(color = "black"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white", size = 18, hjust = 0.5),
        plot.caption = element_text(color = "white", size = 8.5, hjust = 1),
        axis.title = element_text(color = "white"),
        legend.text = element_text(colour="white", size=10, 
                                   face="bold"),
        legend.title = element_text(color = "white", size = 10),
        legend.key = element_rect(fill = "black", color = NA),
        legend.background = element_rect(fill="black",
                                         size=0.5, linetype="solid", 
                                         colour ="black"),
        legend.position = "top")


# positividad * escideol ------------------------

df <- read_sav("3326.sav")
df$GRADOPOSITIVIDADINMI <- as.factor(df$GRADOPOSITIVIDADINMI)
df$GRADOPOSITIVIDADINMI <- fct_relevel(df$GRADOPOSITIVIDADINMI, "5", after = 2)
df$GRADOPOSITIVIDADINMI <- as.numeric(df$GRADOPOSITIVIDADINMI)


df <- df %>% 
  select(GRADOPOSITIVIDADINMI, ESCIDEOL) %>% 
  filter(GRADOPOSITIVIDADINMI <6, ESCIDEOL <11) %>% 
  group_by(ESCIDEOL) %>% 
  count(GRADOPOSITIVIDADINMI) %>% 
  mutate(percent = (n/sum(n)*100))
df


p2 <- df %>% 
  ggplot(aes(fill=as.factor(GRADOPOSITIVIDADINMI), y=percent, x=as.factor(ESCIDEOL))) + 
  geom_bar(position="fill", stat="identity") +
scale_y_continuous(name = "",  labels = scales::percent) +
  scale_x_discrete("Ideología") +
  coord_flip() +
  scale_fill_manual(values=c("darkgreen", "green", "yellow", "firebrick", "darkred"),
                    name = "La inmigración para \nEspaña es",
                    labels = c("Muy positiva", "Positiva", "Ni positiva \nni negativa",
                               "Negativa", "Muy negativa")) +
  theme(text = element_text(size=12, face = "bold", family = "Bahnschrift"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        strip.text.x = element_text(color = "white", face = "bold.italic"),
        panel.grid = element_line(color = "black"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white", size = 18, hjust = 0.5),
        plot.caption = element_text(color = "white", size = 8.5, hjust = 1),
        axis.title = element_text(color = "white"),
        legend.text = element_text(colour="white", size=10, 
                                   face="bold"),
        legend.title = element_text(color = "white", size = 10),
        legend.key = element_rect(fill = "black", color = NA),
        legend.background = element_rect(fill="black",
                                         size=0.5, linetype="solid", 
                                         colour ="black"),
        legend.position = "top")



# percepcion * edad -------------------

df <- read_sav("3326.sav")



df <- df %>% 
  mutate(EDAD_REC =
           case_when(EDAD <=24 ~ 1,
                     between(EDAD, 25,34) ~ 2,
                     between(EDAD, 35,44) ~ 3,
                     between(EDAD, 45,54) ~ 4,
                     between(EDAD, 55,64) ~ 5,
                     EDAD >=65 ~ 6)) %>% 
  select(PERCEPIONUMEROINMIGR, EDAD_REC) %>% 
  filter(PERCEPIONUMEROINMIGR <5) %>% 
  group_by(EDAD_REC) %>% 
  count(PERCEPIONUMEROINMIGR) %>% 
  mutate(percent = (n/sum(n)*100)) 
df


p3 <- df %>% 
  ggplot(aes(fill=as.factor(PERCEPIONUMEROINMIGR), y=percent, x=as.factor(EDAD_REC))) + 
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(name = "",  labels = scales::percent) +
  scale_x_discrete("Edad", 
                   labels = c("18-24", "25-34", "35-44", "45-54", "55-64", ">=65")) +
  coord_flip() +
  scale_fill_manual(values=c("darkgreen", "green", "firebrick", "darkred"),
                    name = "Percepción nº de \ninmigrantes en España",
                    labels = c("Insuficiente", "Aceptable", "Elevado", "Excesivo")) +
  theme(text = element_text(size=12, face = "bold", family = "Bahnschrift"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        strip.text.x = element_text(color = "white", face = "bold.italic"),
        panel.grid = element_line(color = "black"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white", size = 18, hjust = 0.5),
        plot.caption = element_text(color = "white", size = 8.5, hjust = 1),
        axis.title = element_text(color = "white"),
        legend.text = element_text(colour="white", size=10, 
                                   face="bold"),
        legend.title = element_text(color = "white", size = 10),
        legend.key = element_rect(fill = "black", color = NA),
        legend.background = element_rect(fill="black",
                                         size=0.5, linetype="solid", 
                                         colour ="black"),
        legend.position = "top")



# positividad * edad -----------------

df <- read_sav("3326.sav")
df$GRADOPOSITIVIDADINMI <- as.factor(df$GRADOPOSITIVIDADINMI)
df$GRADOPOSITIVIDADINMI <- fct_relevel(df$GRADOPOSITIVIDADINMI, "5", after = 2)
df$GRADOPOSITIVIDADINMI <- as.numeric(df$GRADOPOSITIVIDADINMI)


df <- df %>% 
  mutate(EDAD_REC =
           case_when(EDAD <=24 ~ 1,
                     between(EDAD, 25,34) ~ 2,
                     between(EDAD, 35,44) ~ 3,
                     between(EDAD, 45,54) ~ 4,
                     between(EDAD, 55,64) ~ 5,
                     EDAD >=65 ~ 6)) %>%
  filter(GRADOPOSITIVIDADINMI <6) %>% 
  group_by(EDAD_REC) %>% 
  count(GRADOPOSITIVIDADINMI) %>% 
  mutate(percent = (n/sum(n)*100))
df


p4 <- df %>% 
  ggplot(aes(fill=as.factor(GRADOPOSITIVIDADINMI), y=percent, x=as.factor(EDAD_REC))) + 
  geom_bar(position="fill", stat="identity")  +
  scale_y_continuous(name = "",  labels = scales::percent) +
  scale_x_discrete("Edad", 
                   labels = c("18-24", "25-34", "35-44", "45-54", "55-64", ">=65")) +
  coord_flip() +
  scale_fill_manual(values=c("darkgreen", "green", "yellow", "firebrick", "darkred"),
                    name = "La inmigración para \nEspaña es",
                    labels = c("Muy positiva", "Positiva", "Ni positiva \nni negativa",
                               "Negativa", "Muy negativa")) +
  theme(text = element_text(size=12, face = "bold", family = "Bahnschrift"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        strip.text.x = element_text(color = "white", face = "bold.italic"),
        panel.grid = element_line(color = "black"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white", size = 18, hjust = 0.5),
        plot.caption = element_text(color = "white", size = 8.5, hjust = 1),
        axis.title = element_text(color = "white"),
        legend.text = element_text(colour="white", size=10, 
                                   face="bold"),
        legend.title = element_text(color = "white", size = 10),
        legend.key = element_rect(fill = "black", color = NA),
        legend.background = element_rect(fill="black",
                                         size=0.5, linetype="solid", 
                                         colour ="black"),
        legend.position = "top")




# percepcion * ESTUDIOS ---------------------


df <- read_sav("3326.sav")

df$ESTUDIOS

df <- df %>% 
  select(PERCEPIONUMEROINMIGR, ESTUDIOS) %>% 
  filter(PERCEPIONUMEROINMIGR <5, ESTUDIOS <7) %>% 
  group_by(ESTUDIOS) %>% 
  count(PERCEPIONUMEROINMIGR) %>% 
  mutate(percent = (n/sum(n)*100)) 
df


p5 <- df %>% 
  ggplot(aes(fill=as.factor(PERCEPIONUMEROINMIGR), y=percent, x=as.factor(ESTUDIOS))) + 
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(name = "",  labels = scales::percent) +
  scale_x_discrete("Estudios",
                   labels = c("Sin estudios", "Primaria", "Secundaria \n1ª etapa", "Secundaria \n2ª etapa", "F.P.", "Superiores")) +
  coord_flip() +
  scale_fill_manual(values=c("darkgreen", "green", "firebrick", "darkred"),
                    name = "Percepción nº de \ninmigrantes en España",
                    labels = c("Insuficiente", "Aceptable", "Elevado", "Excesivo")) +
  theme(text = element_text(size=12, face = "bold", family = "Bahnschrift"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        strip.text.x = element_text(color = "white", face = "bold.italic"),
        panel.grid = element_line(color = "black"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white", size = 18, hjust = 0.5),
        plot.caption = element_text(color = "white", size = 8.5, hjust = 1),
        axis.title = element_text(color = "white"),
        legend.text = element_text(colour="white", size=10, 
                                   face="bold"),
        legend.title = element_text(color = "white", size = 10),
        legend.key = element_rect(fill = "black", color = NA),
        legend.background = element_rect(fill="black",
                                         size=0.5, linetype="solid", 
                                         colour ="black"),
        legend.position = "top")

# positividad * estudios ------------------


df <- read_sav("3326.sav")
df$GRADOPOSITIVIDADINMI <- as.factor(df$GRADOPOSITIVIDADINMI)
df$GRADOPOSITIVIDADINMI <- fct_relevel(df$GRADOPOSITIVIDADINMI, "5", after = 2)
df$GRADOPOSITIVIDADINMI <- as.numeric(df$GRADOPOSITIVIDADINMI)


df <- df %>% 
  select(GRADOPOSITIVIDADINMI, ESTUDIOS) %>% 
  filter(GRADOPOSITIVIDADINMI <6, ESTUDIOS <7) %>% 
  group_by(ESTUDIOS) %>% 
  count(GRADOPOSITIVIDADINMI) %>% 
  mutate(percent = (n/sum(n)*100))
df


p6 <- df %>% 
  ggplot(aes(fill=as.factor(GRADOPOSITIVIDADINMI), y=percent, x=as.factor(ESTUDIOS))) + 
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(name = "",  labels = scales::percent) +
  scale_x_discrete("Estudios",
                   labels = c("Sin estudios", "Primaria", "Secundaria \n1ª etapa", "Secundaria \n2ª etapa", "F.P.", "Superiores")) +
  coord_flip() +
  scale_fill_manual(values=c("darkgreen", "green", "yellow", "firebrick", "darkred"),
                    name = "La inmigración para \nEspaña es",
                    labels = c("Muy positiva", "Positiva", "Ni positiva \nni negativa",
                               "Negativa", "Muy negativa")) +
  theme(text = element_text(size=12, face = "bold", family = "Bahnschrift"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        strip.text.x = element_text(color = "white", face = "bold.italic"),
        panel.grid = element_line(color = "black"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white", size = 18, hjust = 0.5),
        plot.caption = element_text(color = "white", size = 8.5, hjust = 1),
        axis.title = element_text(color = "white"),
        legend.text = element_text(colour="white", size=10, 
                                   face="bold"),
        legend.title = element_text(color = "white", size = 10),
        legend.key = element_rect(fill = "black", color = NA),
        legend.background = element_rect(fill="black",
                                         size=0.5, linetype="solid", 
                                         colour ="black"),
        legend.position = "top")








# percepcion * CNO11 ----------------------------------------


df <- read_sav("3326.sav")

df$CNO11

df <- df %>% 
  select(PERCEPIONUMEROINMIGR, CNO11) %>% 
  filter(PERCEPIONUMEROINMIGR <5, CNO11 <12) %>% 
  group_by(CNO11) %>% 
  count(PERCEPIONUMEROINMIGR) %>% 
  mutate(percent = (n/sum(n)*100)) 
df


p7 <- df %>% 
  ggplot(aes(fill=as.factor(PERCEPIONUMEROINMIGR), y=percent, x=as.factor(CNO11))) + 
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(name = "",  labels = scales::percent) +
  scale_x_discrete("CNO11", labels = c("Dir y\n Gte ",
                                       "Cient e\nIntel",
                                       "Téc y prof \nnvl medio",
                                       "Pers apy \nadm", 
                                       "Trab ss y vend \ncom y mdo", 
                                       "Agrc y cfcd \n a, f y p",
                                       "Of, op,\nart...", 
                                       "Op inst \ny máq ens",
                                       "Oc elem",
                                       "FFAA y\nFCS",
                                       "Otro")) +
  coord_flip() +
  scale_fill_manual(values=c("darkgreen", "green", "firebrick", "darkred"),
                    name = "Percepción nº de \ninmigrantes en España",
                    labels = c("Insuficiente", "Aceptable", "Elevado", "Excesivo")) +
  theme(text = element_text(size=12, face = "bold", family = "Bahnschrift"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        strip.text.x = element_text(color = "white", face = "bold.italic"),
        panel.grid = element_line(color = "black"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white", size = 18, hjust = 0.5),
        plot.caption = element_text(color = "white", size = 8.5, hjust = 1),
        axis.title = element_text(color = "white"),
        legend.text = element_text(colour="white", size=10, 
                                   face="bold"),
        legend.title = element_text(color = "white", size = 10),
        legend.key = element_rect(fill = "black", color = NA),
        legend.background = element_rect(fill="black",
                                         size=0.5, linetype="solid", 
                                         colour ="black"),
        legend.position = "top")





# positividad * cno11 -----------------------------


df <- read_sav("3326.sav")
df$GRADOPOSITIVIDADINMI <- as.factor(df$GRADOPOSITIVIDADINMI)
df$GRADOPOSITIVIDADINMI <- fct_relevel(df$GRADOPOSITIVIDADINMI, "5", after = 2)
df$GRADOPOSITIVIDADINMI <- as.numeric(df$GRADOPOSITIVIDADINMI)


p8 <- df <- df %>% 
  select(GRADOPOSITIVIDADINMI, CNO11) %>% 
  filter(GRADOPOSITIVIDADINMI <6, CNO11 <12) %>% 
  group_by(CNO11) %>% 
  count(GRADOPOSITIVIDADINMI) %>% 
  mutate(percent = (n/sum(n)*100))
df


df %>% 
  ggplot(aes(fill=as.factor(GRADOPOSITIVIDADINMI), y=percent, x=as.factor(CNO11))) + 
  geom_bar(position="fill", stat="identity")  +
  scale_y_continuous(name = "",  labels = scales::percent) +
  scale_y_continuous(name = "",  labels = scales::percent) +
  scale_x_discrete("CNO11", labels = c("Dir y\n Gte ",
                                       "Cient e\nIntel",
                                       "Téc y prof \nnvl medio",
                                       "Pers apy \nadm", 
                                       "Trab ss y vend \ncom y mdo", 
                                       "Agrc y cfcd \n a, f y p",
                                       "Of, op,\nart...", 
                                       "Op inst \ny máq ens",
                                       "Oc elem",
                                       "FFAA y\nFCS",
                                       "Otro")) +
  coord_flip() +
  scale_fill_manual(values=c("darkgreen", "green", "yellow", "firebrick", "darkred"),
                    name = "La inmigración para \nEspaña es",
                    labels = c("Muy positiva", "Positiva", "Ni positiva \nni negativa",
                               "Negativa", "Muy negativa")) +
  theme(text = element_text(size=12, face = "bold", family = "Bahnschrift"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        strip.text.x = element_text(color = "white", face = "bold.italic"),
        panel.grid = element_line(color = "black"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white", size = 18, hjust = 0.5),
        plot.caption = element_text(color = "white", size = 8.5, hjust = 1),
        axis.title = element_text(color = "white"),
        legend.text = element_text(colour="white", size=10, 
                                   face="bold"),
        legend.title = element_text(color = "white", size = 10),
        legend.key = element_rect(fill = "black", color = NA),
        legend.background = element_rect(fill="black",
                                         size=0.5, linetype="solid", 
                                         colour ="black"),
        legend.position = "top")



# unir plots -----------------------

library(patchwork)




pizq <- (p1 / p3 / p5 + plot_layout(guides = "collect") & theme(legend.position = "top"))
pizq


pdch <- (p2 / p4 / p6 + plot_layout(guides = "collect") & theme(legend.position = "top"))
pdch

g <- (pizq | pdch)
g

g <- g + plot_annotation(
  title = 'PERCEPCIÓN DEL Nº DE INMIGRANTES EN ESPAÑA Y GRADO DE POSITIVIDAD DE LA INMIGRACIÓN',
  subtitle = 'Por ubicación ideológica, grupo de edad y nivel de estudios',
  caption = 'CIS Estudio nº 3326 | @dataR_amateur') +
  theme(plot.caption = element_text(size = 9.5, hjust = 1))

g <- g + plot_annotation(theme = theme(plot.background = element_rect(fill  = 'black', color = "black"),
                                       plot.title = element_text(color = "white", hjust = 0.5),
                                       plot.subtitle = element_text(color = "white", hjust = 0.5),
                                       #panel.background = element_rect(fill = "black", color = "black"),
                                       plot.caption = element_text(color = "white")))

g <- g + plot_annotation(theme = theme(text=element_text(family = "Bahnschrift", face = "bold")))

g <- g + plot_annotation(theme = theme(plot.title = element_text(size = 18, hjust = 0.5),
                                       plot.caption = element_text(size = 9.5, hjust = 1)))
g

g 


