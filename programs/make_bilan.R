#' @description 
#' ce script consiste à appeler les différents scripts qubs qui permettront de
#' produire une restitution qubs 
#' 
#' @author Héloïse Jeux
#' 
#' @date 2025


source(here::here("programs/library.R"))
# source(here::here("programs/db_qubs.R")) # si connecté au serveur mnhn

# si pas connecté au serveur :
dt_qubs = readRDS("data/dt_qubs.rds") %>%
  rename(year = date) %>% 
  mutate(abondance = as.numeric(abondance)) %>%
  mutate(abondance = case_when(presence_organisme == 1 & is.na(abondance) ~ 1, 
                               TRUE ~ abondance)) 


palette_qubs <- c("#ff8800",
                  "#3c24a6",
                  "#50FFB1",
                  "#aa1256",
                  "#2baca6",
                  "#1a702d",
                  "#0f0608")


                  
palette_protocoles = colorFactor(palette = c("#ff8800",
                                             "#3c24a6",
                                             "#50FFB1",
                                             "#aa1256"),
                                 domain = dt_qubs$protocole)
  

palette_camemberts <- c("Les Insectes" = "#377eb8",
                        "Les Gastéropodes" = "#e41a1c",
                        "Les Collemboles et diploures" = "#4daf4a",
                        "Arachnides" = "#984ea3",
                        "Les Crustacés terrestres" = "#ff7f00",
                        "Les Vers sens large" = "#ffff33",
                        "Les Milles-pattes" = "#a25427",
                        "Les Escargots" = "#43185d",
                        "Les Limaces" = "#c99de5")


dir.create(here::here("reporting"))


source(here::here("programs/filtre_spatial.R"))
source(here::here("programs/script_indic.R"))
# source(here::here("programs/render_template_heloise.R"))


