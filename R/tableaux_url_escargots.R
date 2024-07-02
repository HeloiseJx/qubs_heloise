library(here)
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)

#Add new .env variables (access to database)
readRenviron(".env")

source(here::here("functions", "function_import_from_mosaic.R"))
source(here::here("functions", "function_encoding_utf8.R"))

## data escargots
query <- read_sql_query(here::here("sql", "export_a_plat_escargots.sql"))
dt_escargots <- import_from_mosaic(query,
                                   database_name = "qubs",
                                   force_UTF8 = TRUE)


#TABLEAU VERIFICATION PHOTOS ANGELIQUE
#modif a faire ensuite dans excel pour transformer en lien hypertexte
dt_photos_abris <- dt_escargots %>% 
  select(site_id, abri_id, abri_typ, url_photo_abri) %>%
  distinct()
data.table::fwrite(dt_photos_abris, here::here("tableaux", "photos_abris.csv"), sep = ";")


dt_photos_taxons <- dt_escargots %>%
  filter(presence_organisme == 1) %>% # ne conserver que les collections avec escargots
  select(participation_id, url_photo_collecte,
         taxon,
         taxon_valide, taxon_ne_sait_pas, taxon_pas_dans_liste,
         url_photo_taxon_dessus, url_photo_taxon_dessous, url_photo_taxon_cote,
         commentaire_releve)
readr::write_excel_csv2(dt_photos_taxons, here::here("tableaux", "photos_taxons.csv"))
