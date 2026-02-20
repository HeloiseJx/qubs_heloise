library(here)
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)

#Add new .env variables (access to database)
readRenviron(".env")

source(here::here("functions", "function_import_from_mosaic.R"))
source(here::here("functions", "function_encoding_utf8.R"))

## data noctambules
query <- read_sql_query(here::here("sql", "export_a_plat_noctambules.sql"))
dt_noctambules <- import_from_mosaic(query,
                                     database_name = "qubs",
                                     force_UTF8 = TRUE)

## data escargots
query <- read_sql_query(here::here("sql", "export_a_plat_escargots.sql"))
dt_escargots <- import_from_mosaic(query,
                                     database_name = "qubs",
                                     force_UTF8 = TRUE)

## data aspifaune
query <- read_sql_query(here::here("sql", "export_a_plat_aspifaune.sql"))
dt_aspifaune <- import_from_mosaic(query,
                                     database_name = "qubs",
                                     force_UTF8 = TRUE) %>%
  filter(protocole == "Aspifaune")

champs_communs <- Reduce(intersect, list(colnames(dt_aspifaune),
                                         colnames(dt_noctambules),
                                         colnames(dt_escargots)))


dt_qubs <- rbind(dt_aspifaune %>% select(all_of(champs_communs)),
                 dt_noctambules %>% select(all_of(champs_communs)),
                 dt_escargots %>% select(all_of(champs_communs)))







#SUIVI ESCARGOTS

#hist nb participations par participant
hist_participation <- dt_escargots %>% group_by(participant_id) %>% 
  reframe(nb_participations = as.integer(n_distinct(participation_id))) %>%
  ggplot(aes(x = nb_participations)) + 
  geom_bar(fill = "#2e9945") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(x = "Nombre de participations", y = "Nombre de participants") +
  theme_bw() +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(size = 16))
plotly::ggplotly(hist_participation)
ggsave(here::here("reporting", "hist_participation_escargots.tiff"),hist_participation)

# participation à la semaine 
participation_semaine <- dt_escargots %>% mutate(semaine = lubridate::floor_date(as.Date(date), "week")) %>%
  group_by(semaine) %>% reframe(nb_participations = n_distinct(participation_id)) %>%
  ggplot(aes(x = semaine, y  = nb_participations)) +
  geom_col(fill = "#4d3494")+ 
  labs(x = "Date", y = "Nombre de participations") +
  scale_x_date(labels = scales::date_format("%m/%Y"), breaks = "1 month")+
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 16))

plotly::ggplotly(participation_semaine)
# ggsave(here::here("reporting", "participation_semaine_escargots.tiff"), participation_semaine)









