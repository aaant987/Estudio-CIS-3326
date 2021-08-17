
library(haven)
library(tidyverse)
library(extrafont)
windowsFonts("Bahnschrift" = windowsFont("Bahnschrift"))
library(patchwork)
library(waffle)


# diferencias en la confianza en casado como líder de la oposición entre aquellos que prefieren a casado vs prefieren a ayuso ---------------------

df <- read_sav("3326.sav")
df$CONFIANZAOPOSIC <- as.numeric(df$CONFIANZAOPOSIC)

df <- df %>% 
  select(CONFIANZAOPOSIC, PREFPTE) %>% 
  filter(PREFPTE %in% c("2", "8"), CONFIANZAOPOSIC <5)

p1 <- df %>% 
  ggplot(aes(x = as.factor(PREFPTE), y = CONFIANZAOPOSIC)) +
  geom_hline(aes(yintercept = mean(df$CONFIANZAOPOSIC)), color = "yellow", size = 2.6) +
  geom_boxplot(fill = "black", color = "white") +
  stat_summary(fun.y=mean, geom="point", color="red", size = 7) +
  geom_jitter(position = position_jitter(seed = 2019, width = 0.2), size = 2.3, alpha = 0.6,
              color = "pink") +
  scale_y_reverse(limits = c(4, 1), name = "Confianza en Casado como líder de la oposición",
                     labels = c("1" = "Mucha", 
                                "2" = "Bastante",
                                "3" = "Poca",
                                "4" = "Ninguna")) +
  scale_x_discrete(name = "", labels=c("2" = "Casado", "8" = "Ayuso")) +
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
        axis.title.y = element_text(color = "white", size = 9.5),
        legend.text = element_text(colour="white", size=10, 
                                   face="bold"),
        legend.title = element_text(color = "white", size = 10),
        legend.key = element_rect(fill = "black", color = NA),
        legend.background = element_rect(fill="black",
                                         size=0.5, linetype="solid", 
                                         colour ="black"),
        legend.position = "none")


# diferencias en la valoración entre aquellos que prefieren a casado como presidente vs prefieren a ayuso

df <- read_sav("3326.sav")

glimpse(df)
df$VALORALIDERES_2 <- as.numeric(df$VALORALIDERES_2)

df <- df %>% 
  select(VALORALIDERES_2, PREFPTE) %>% 
  filter(PREFPTE %in% c("2", "8"), VALORALIDERES_2 <11)

p2 <- df %>% 
  ggplot(aes(x = as.factor(PREFPTE), y = VALORALIDERES_2)) +
  geom_boxplot(fill = "black", color = "white") +
  stat_summary(fun.y=mean, geom="point", color="red", size = 7) +
  geom_jitter(position = position_jitter(seed = 2019, width = 0.2), size = 2.3, alpha = 0.6,
              color = "pink") +
  scale_y_continuous(limits=c(1, 10), breaks = seq(0, 10, by = 1), name = "Valoración a Casado (1-10)") +
  scale_x_discrete(name = "", labels=c("2" = "Casado", "8" = "Ayuso")) +
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
        legend.position = "none")


# fidelidad de voto a un partido según prefiere a casado o ayuso

library(waffle)

df <- read_sav("3326.sav")

df$FIDEVOTO <- as.factor(df$FIDEVOTO)
df$PREFPTE <- as.factor(df$PREFPTE)


df <- df %>% 
  select(FIDEVOTO, PREFPTE) %>% 
  filter(PREFPTE %in% c("2", "8"))

df <- df %>% 
  select(FIDEVOTO, PREFPTE) %>% 
  group_by(PREFPTE) %>% 
  count(FIDEVOTO) %>% 
  mutate(percenta = (n/sum(n)*100))
df


levels(df$PREFPTE) <- c("1", "Casado",
                        "3", "4",
                        "5", "6",
                        "7", "Ayuso",
                        "96", "97",
                        "98", "99")



p3 <- df %>%
  ggplot(aes(fill=FIDEVOTO, values=percenta)) +
  geom_waffle(color = "Black", size=2, n_rows = 6) +
  scale_fill_manual(
    name = NULL,
    values = c("blue", "skyblue", "yellow", "gray40", "gray40"),
    labels = c("Siempre", "Suele", "Según el \nmomento", "Blanco o nulo", "No vota", "1ª vez que vota",
               "NR/NS", "NC"))+
  facet_wrap(~PREFPTE, ncol=1) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  labs(
    title = "Fidelidad de voto a un partido"
  ) +
  theme(text = element_text(size=12, face = "bold", family = "Bahnschrift"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        strip.text.x = element_text(color = "white", face = "bold"),
        panel.grid = element_line(color = "black"),
        axis.text.y = element_text(color = "orange"),
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


# evolución preferencia presidente del gobierno por recuerdo de voto al PP, CS y VOX --------------

df1 <- read_sav("3322.sav")
df1$PREFPTE



df1 <- df1 %>% 
  select(RECUVOTOGR, PREFPTE, ESTUDIO) %>% 
  filter(RECUVOTOGR %in% c("1", "4", "18"), PREFPTE %in% c("2", "3", "6", "8"))

df2 <- read_sav("3326.sav")

df2 <- df2 %>% 
  select(RECUVOTOGR, PREFPTE, ESTU) %>% 
  filter(RECUVOTOGR %in% c("1", "4", "18"), PREFPTE %in% c("2", "3", "6", "8"))  %>% 
  rename(ESTUDIO = ESTU)

df <- rbind(df1, df2)

df



df <- df %>% 
  select(RECUVOTOGR, PREFPTE, ESTUDIO) %>% 
  group_by(RECUVOTOGR, ESTUDIO) %>% 
  count(PREFPTE) %>% 
  mutate(percenta = n/sum(n)*100)
df

df$RECUVOTOGR <- as.factor(df$RECUVOTOGR)

levels(df$RECUVOTOGR) <- c("PP", "Cs", "VOX")


p4 <- df %>% 
  ggplot(aes(x = as.factor(ESTUDIO), y = percenta, fill = as.factor(PREFPTE))) +
  geom_col(position = "dodge") +
  facet_wrap(~ RECUVOTOGR) +
  labs(title = "Preferencia de presidente del \nGobierno por recuerdo de voto") +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 10),
                     labels = scales::percent_format(scale = 1),
                     name = "") +
  scale_x_discrete(name = "", labels = c("Mayo", "Junio")) +
  scale_fill_manual(values = c("blue", "darkgreen", "darkorange", "skyblue"),
                    labels = c("Casado", "Abascal", "Arrimadas", "Ayuso"),
                    name = "") +
  theme(text = element_text(size=12, face = "bold", family = "Bahnschrift"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "gray40"),
        strip.text.x = element_text(color = "white", face = "bold.italic"),
        panel.grid.major = element_line(color = "black"),
        panel.grid.minor = element_line(color = "white"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white", size = 16, hjust = 0.5),
        plot.caption = element_text(color = "white", size = 8.5, hjust = 1),
        axis.title = element_text(color = "white"),
        legend.text = element_text(colour="white", size=10, 
                                   face="bold"),
        legend.title = element_text(color = "white", size = 12),
        legend.key = element_rect(fill = "black", color = NA),
        legend.background = element_rect(fill="black",
                                         size=0.5, linetype="solid", 
                                         colour ="black"),
        legend.position = "top")


# unir plots -------------

pizq <- (p1 / p3)
pizq


pdch <- (p2 / p4)
pdch

g <- (pizq | pdch)
g

g <- g + plot_annotation(
  title = 'Pulso Casado vs Ayuso',
  subtitle = '',
  caption = 'CIS Estudio nº 3322 y 3326 | @dataR_amateur') +
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


