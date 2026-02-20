# acces a db qubs


## QUBS 
#Add new .env variables (access to database)
readRenviron(here::here(".env"))
#load functions to fetch data
source("C:/Git/qubs/functions/function_import_from_mosaic.R")
source("C:/Git/qubs/functions/function_encoding_utf8.R")

## data noctambules
query <- read_sql_query("C:/Git/qubs/sql/export_a_plat_noctambules.sql")
dt_noctambules <- import_from_mosaic(query,
                                     database_name = "qubs",
                                     force_UTF8 = TRUE) %>%
  rename( session_id = participation_id) 

dt_noctambules$year <- year(as.Date(dt_noctambules$date_debut))
dt_noctambules$user_id <- year(as.Date(dt_noctambules$participant_id))

## data escargots
query <- read_sql_query("C:/Git/qubs/sql/export_a_plat_escargots.sql")
dt_escargots <- import_from_mosaic(query,
                                   database_name = "qubs",
                                   force_UTF8 = TRUE) %>%
  rename( session_id = participation_id) %>%
  rename(date_debut = date)

dt_escargots$year <- year(as.Date(dt_escargots$date_debut))
dt_escargots$user_id <- year(as.Date(dt_escargots$participant_id))

## data aspifaune
query <- read_sql_query("C:/Git/qubs/sql/export_a_plat_aspifaune.sql")
dt_aspifaune <- import_from_mosaic(query,
                                   database_name = "qubs",
                                   force_UTF8 = TRUE) %>%
  rename( session_id = participation_id)
dt_aspifaune$year <- year(as.Date(dt_aspifaune$date_debut))
dt_aspifaune$user_id <- year(as.Date(dt_aspifaune$participant_id))

## data vers de terre
query <- read_sql_query("C:/Git/qubs/sql/export_a_plat_vers_de_terre.sql")
dt_vers <- import_from_mosaic(query,
                              database_name = "qubs",
                              force_UTF8 = TRUE)%>%
  rename( session_id = participation_id)
dt_vers$year <- year(as.Date(dt_vers$date_debut))
dt_vers$user_id <- year(as.Date(dt_vers$participant_id))



#identifier les champs communs aux differents dataframe
champs_communs <- Reduce(intersect, list(colnames(dt_aspifaune),
                                         colnames(dt_noctambules),
                                         colnames(dt_escargots),
                                         colnames(dt_vers)))
#rbind des donnees des 3 protocoles
dt_qubs <- rbind(dt_aspifaune %>% select(all_of(champs_communs)),
                 dt_noctambules %>% select(all_of(champs_communs)),
                 dt_escargots %>% select(all_of(champs_communs)),
                 dt_vers %>% select(all_of(champs_communs)))

# Ajouter la taxonomie et les regroupements d'especes
taxo <- data.table::fread("C:/Git/qubs/data/thesaurus_V1V2.csv") %>% 
  select(`Nom français V1`, `Regroupement2a`, `Regroupement2b`, `Regroupement3`)
colnames(taxo) <- c("taxon", "Regroupement2a", "Regroupement2b", "Regroupement3")

dt_qubs <- dt_qubs %>% left_join(taxo, by = "taxon") 






