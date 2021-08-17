library(haven)
library(tidyverse)
library(extrafont)
windowsFonts("Bahnschrift" = windowsFont("Bahnschrift"))
library(patchwork)


# DF BY RECUVOTOGR ----------------------

df <- read_sav("3326.sav")

glimpse(df)


df$RECUVOTOGR[df$RECUVOTOGR == 6] <- 3
df$RECUVOTOGR[df$RECUVOTOGR == 21 ] <- 3
df$RECUVOTOGR[df$RECUVOTOGR == 67 ] <- 3


df$RECUVOTOGR


df <- df %>% 
  select(RECUVOTOGR, GRADOFUNCIONADEMOCRA, FUNCIONABAHACE10) %>% 
  filter(GRADOFUNCIONADEMOCRA <11, FUNCIONABAHACE10 <11) 

df <- df %>%
group_by(RECUVOTOGR) %>%
  summarise_at(vars(GRADOFUNCIONADEMOCRA, FUNCIONABAHACE10), funs(mean(., na.rm=TRUE))) %>% 
  mutate(difference = (GRADOFUNCIONADEMOCRA - FUNCIONABAHACE10))
df

p1 <- df %>% 
  drop_na(RECUVOTOGR) %>% 
  ggplot(aes(x = as.factor(reorder(RECUVOTOGR, difference)), y = difference, 
             fill = as.factor(RECUVOTOGR), group = as.factor(RECUVOTOGR))) +
  geom_col() +
  scale_y_continuous(name = "Diferencia funcionamiento democracia actual - hace 10 años" ) +
  scale_x_discrete("Recuerdo de voto noviembre 2019", 
                   labels = c("VOX", 
                              "Na+", 
                              "PP", 
                              "N.C.", 
                              "JXCat", 
                              "Cs",
                              "N.R",
                              "NULO",
                              "Blanco",
                              "PRC",
                              "MP",
                              "ERC",
                              "CUP",
                              "PSOE",
                              "PACMA",
                              "BILDU",
                              "PNV",
                              "CCa-NC",
                              "BNG",
                              "TE",
                              "OTROS",
                              "MÉS C",
                              "UP")) +
  scale_fill_manual(values=c("blue", "red", "purple", 
                             "darkorange", "#E65F00", "yellow",
                             "turquoise", "forestgreen", "greenyellow",
                             "lightgoldenrod", "#f6090c", "#ADBE18",
                             "darkgreen", "gold", "skyblue",
                             "#C2CE0C", "seagreen", "springgreen",
                             "grey40", "lightpink", "white",
                             "grey40", "grey40")) +
  theme(text = element_text(size=10, face = "bold", family = "Bahnschrift"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        strip.text.x = element_text(color = "white", face = "bold.italic"),
        panel.grid = element_line(color = "black"),
        axis.text = element_text(color = "white"),
        axis.text.x = element_text(color = "white", size = 7),
        axis.title.y = element_text(color = "white", size = 8),
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
        legend.position = "none")


# DF BY PREFPTE------------------------

df <- read_sav("3326.sav")

glimpse(df)


df$PREFPTE


df <- df %>% 
  select(PREFPTE, GRADOFUNCIONADEMOCRA, FUNCIONABAHACE10) %>% 
  filter(GRADOFUNCIONADEMOCRA <11, FUNCIONABAHACE10 <11) 

df <- df %>%
  group_by(PREFPTE) %>%
  summarise_at(vars(GRADOFUNCIONADEMOCRA, FUNCIONABAHACE10), funs(mean(., na.rm=TRUE))) %>% 
  mutate(difference = (GRADOFUNCIONADEMOCRA - FUNCIONABAHACE10)) %>% 
arrange(difference)
df


p2 <- df %>% 
  drop_na(PREFPTE) %>% 
  ggplot(aes(x = as.factor(reorder(PREFPTE, difference)), y = difference, 
             fill = as.factor(PREFPTE), group = as.factor(PREFPTE))) +
  geom_col() +
  scale_y_continuous(name = "Diferencia funcionamiento democracia actual - hace 10 años" ) +
  scale_x_discrete("Preferencia presidente del Gobierno", 
                   labels = c("Ayuso",
                              "Abascal",
                              "Casado",
                              "Otro",
                              "Ninguno",
                              "N.S",
                              "Arrimadas",
                              "N.C",
                              "Garzón",
                              "Errejón",
                              "Y.Díaz",
                              "Sánchez")) +
  scale_fill_manual(values=c("red", "blue", "darkgreen", 
                             "purple", "green", "darkorange",
                             "seagreen", "skyblue", "lightpink",
                             "white", "gray40", "gray40")) +
  theme(text = element_text(size=10, face = "bold", family = "Bahnschrift"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        strip.text.x = element_text(color = "white", face = "bold.italic"),
        panel.grid = element_line(color = "black"),
        axis.text = element_text(color = "white"),
        axis.title.y = element_text(color = "white", size = 7.7),
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
        legend.position = "none")
  



# DF BY CASES * RECUVOTOGR ---------------------------------


df <- read_sav("3326.sav")

glimpse(df)

df$RECUVOTOGR[df$RECUVOTOGR == 6] <- 3
df$RECUVOTOGR[df$RECUVOTOGR == 21 ] <- 3
df$RECUVOTOGR[df$RECUVOTOGR == 67 ] <- 3




df$RECUVOTOGR <- as.factor(df$RECUVOTOGR)

df <- df %>% 
  select(RECUVOTOGR, GRADOFUNCIONADEMOCRA, FUNCIONABAHACE10) %>% 
  filter(GRADOFUNCIONADEMOCRA <11, FUNCIONABAHACE10 <11) 

df <- df %>% 
  select(RECUVOTOGR, GRADOFUNCIONADEMOCRA, FUNCIONABAHACE10) %>% 
mutate(difference = (GRADOFUNCIONADEMOCRA - FUNCIONABAHACE10)) 
df

dfcount <- df %>% 
  group_by(RECUVOTOGR) %>% 
 count(difference) %>% 
  mutate(percenta = n/sum(n)*100)
dfcount



p3 <- dfcount %>% 
  drop_na(RECUVOTOGR) %>% 
  #filter(RECUVOTOGR %in% c("1", "2", "3", "4", "18")) %>% 
  mutate(RECUVOTOGR = recode(RECUVOTOGR, "1" = "PP",
                             "2" = "PSOE",
                             "3" = "UP",
                             "4" = "Cs",
                             "7" = "Més Compromís",
                             "8" = "ERC",
                             "9" = "JXCat",
                             "11" = "PNV",
                             "12" = "EH-BILDU",
                             "13" = "CCa-NC",
                             "14" = "Na+",
                             "17" = "PACMA",
                             "18" = "VOX",
                             "19" = "CUP",
                             "24" = "BNG",
                             "43" = "PRC",
                             "50" = "Más País",
                             "68" = "Teruel Existe",
                             "77" = "Voto nulo",
                             "95" = "Otros",
                             "96" = "En blanco",
                             "98" = "No recuerda",
                             "99" = "N.C.")) %>% 
  ggplot(aes(x = difference, y = n, fill = as.factor(RECUVOTOGR))) +
  geom_col() +
  facet_wrap(~ as.factor(RECUVOTOGR)) +
  scale_fill_manual(values=c("blue", "red", "purple", 
                             "darkorange", "#E65F00", "yellow",
                             "turquoise", "forestgreen", "greenyellow",
                             "lightgoldenrod", "#f6090c", "#ADBE18",
                             "darkgreen", "gold", "skyblue",
                             "#C2CE0C", "seagreen", "springgreen",
                             "grey40", "lightpink", "white",
                             "grey40", "grey40")) +
  scale_y_log10(name = "Distribución por recuerdo de voto (escala log10)") +
  scale_x_continuous(name = "Diferencia funcionamiento democracia actual - hace 10 años", 
                     limits = c(-9,9),breaks = seq(-9, 9, by = 3)) +
  theme(text = element_text(size=10, face = "bold", family = "Bahnschrift"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "gray40"),
        strip.text.x = element_text(color = "white", face = "bold.italic"),
        panel.grid.minor = element_line(color = "gray40"),
        panel.grid.major = element_line(color = "black"),
        axis.text = element_text(color = "white"),
        axis.title.y = element_text(color = "white", size = 8),
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
        legend.position = "none")

p3

 
 

# DF BY CASES * PREFPTE------------------------------

df <- read_sav("3326.sav")

glimpse(df)

df$PREFPTE <- as.factor(df$PREFPTE) 

df$PREFPTE


df <- df %>% 
  select(PREFPTE, GRADOFUNCIONADEMOCRA, FUNCIONABAHACE10) %>% 
  filter(GRADOFUNCIONADEMOCRA <11, FUNCIONABAHACE10 <11) 

df <- df %>% 
  select(PREFPTE, GRADOFUNCIONADEMOCRA, FUNCIONABAHACE10) %>% 
  mutate(difference = (GRADOFUNCIONADEMOCRA - FUNCIONABAHACE10)) 
df

dfcount <- df %>% 
  group_by(PREFPTE) %>% 
  count(difference) %>% 
  mutate(percenta = n/sum(n)*100) 
dfcount


p4 <- dfcount %>% 
  drop_na(PREFPTE) %>% 
  mutate(PREFPTE = recode(PREFPTE, "1" = "Sánchez",
                             "2" = "Casado",
                             "3" = "Abascal",
                             "4" = "Y. Díaz",
                             "5" = "Garzón",
                             "6" = "Arrimadas",
                             "7" = "Errejón",
                             "8" = "Ayuso",
                             "96" = "Otro",
                             "97" = "Ninguno",
                             "98" = "N.S",
                             "99" = "N.C")) %>% 
  ggplot(aes(x = difference, y = n, fill = as.factor(PREFPTE))) +
  geom_col() +
  facet_wrap(~ as.factor(PREFPTE)) +
  scale_fill_manual(values=c("red", "blue", "darkgreen",
                             "purple", "green", "darkorange",
                             "seagreen", "skyblue", "lightpink",
                             "white", "gray40", "gray40")) +
  scale_y_log10(name = "Distribución por preferencia presidente del Gobierno (escala log10)") +
  scale_x_continuous(name = "Diferencia funcionamiento democracia actual - hace 10 años", 
                     limits = c(-9,9),breaks = seq(-9, 9, by = 3)) +
  theme(text = element_text(size=10, face = "bold", family = "Bahnschrift"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "gray40"),
        strip.text.x = element_text(color = "white", face = "bold.italic"),
        panel.grid.minor = element_line(color = "gray40"),
        panel.grid.major = element_line(color = "black"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white", size = 18, hjust = 0.5),
        plot.caption = element_text(color = "white", size = 8.5, hjust = 1),
        axis.title.y = element_text(color = "white", size = 8),
        axis.title = element_text(color = "white"),
        legend.text = element_text(colour="white", size=10, 
                                   face="bold"),
        legend.title = element_text(color = "white", size = 10),
        legend.key = element_rect(fill = "black", color = NA),
        legend.background = element_rect(fill="black",
                                         size=0.5, linetype="solid", 
                                         colour ="black"),
        legend.position = "none")
p4


# unir plots -------------------




pizq <- (p1 / p3)
pizq


pdch <- (p2 / p4)
pdch

g <- (pizq | pdch)
g

g <- g + plot_annotation(
  title = 'Diferencia funcionamiento democracia actual - hace diez años',
  subtitle = 'Por recuerdo de voto en noviembre 2019 y preferencia presidente del Gobierno',
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


