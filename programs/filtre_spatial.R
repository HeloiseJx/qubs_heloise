# Appliquer un filtre spatial de notre choix aux données qubs 


#### FILTRE SPATIAL SUR LES DONNEES qubs ####################################
# A DECOMMENTER LORSQUE L'ON EST CONNECTE AU SERVEUR
#transformer les coordonnees des collections
coord_qubs_national <- dt_qubs %>%
  select(session_id,  protocole) %>%
  distinct() 
#%>%
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


# Représentation graphique pour vérifier
# leaflet(coord_qubs_national) %>%
#   addTiles() %>%
#   addCircleMarkers(color = ~ palette_protocoles(protocole)) %>%
#   addLegend(position = "topright", pal = palette_protocoles, values = coord_qubs_national$protocole)
# # choisir IDF et Metro Dijon en commune miroir ?



# filtre sur l'emprise spatiale qui nous intéresse --> à modifier 
## import des couches de communes
invisible(capture.output({epci <- st_read(here::here("maps/ADMIN-EXPRESS_3-2__SHP_WGS84G_FRA_2024-12-18/ADMIN-EXPRESS_3-2__SHP_WGS84G_FRA_2024-12-18/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2024-12-00243/ADE_3-2_SHP_WGS84G_FRA-ED2024-12-18/EPCI.shp"), quiet = TRUE) }))
epci <- epci[-c(26, 526),]
plaineco = epci %>% filter(NOM == "Plaine Commune") 
terreenv = epci %>% filter(NOM == "Paris Terres d'Envol") 
# metro_lyon = epci %>% filter(NOM == "Métropole de Lyon") 
metro_gdparis = epci %>% filter(NOM == "Métropole du Grand Paris") 
# metro_dijon = epci %>% filter(NOM == "Dijon Métropole") 
# insee <- sf::read_sf(here::here("maps", "communes-version-simplifiee.geojson"))  # communes insee
# lyon = insee %>% filter(nom == "Lyon")


# Jointure avec les données qubs --> table contenant les coorodonnées des collections 
# pour les emprises spatiales désirées
coord_qubs_plaineco = st_join(coord_qubs_national, plaineco, left = FALSE)
coord_qubs_terreenv = st_join(coord_qubs_national, terreenv, left = FALSE)
# coord_qubs_lyon = st_join(coord_qubs_national, metro_lyon, left = FALSE)
# coord_qubs_dijon = st_join(coord_qubs_national, metro_dijon, left = FALSE)
coord_qubs_gdparis = st_join(coord_qubs_national, metro_gdparis, left = FALSE)




# SELECTION ECHELLE SPATIALE ET FILTRE -----------------------------------------

# ECHELLE NATIONALE 
dt_qubs_total = dt_qubs # stockage du df global dans la variable dt_qubs_total, contient toutes les modalités de validation (0, 1) = (pas validé, validé)

dt_qubs_total_valide = dt_qubs_total %>% filter(taxon_valide == 1)

# sessions vides
dt_vide = dt_qubs_total %>% filter(is.na(observation_id)) 

# on considere que les sessions vides sont valides
# df final 
dt_qubs_total_valide = dt_qubs_total_valide %>% bind_rows(dt_vide)


# ECHELLE SITE 
coord_qubs_local = coord_qubs_plaineco

#df sans les taxons non validés
dt_qubs = as.data.frame(coord_qubs_local) %>% # df de base avec l'emprise qui nous intéresse 
  select(session_id) %>% #on récupère les numeros des sessions qui ont été faites au site considéré
  left_join(dt_qubs_total_valide) %>% # on filtre les observations validées uniquement 
  filter(!is.na(taxon_valide))


# df avec les taxons non validés
dt_qubs_nonvalid = as.data.frame(coord_qubs_local) %>% # df de base avec l'emprise qui nous intéresse 
  select(session_id) %>% #on récupère les numeros des sessions qui ont été faites au site considéré
  left_join(dt_qubs_total) 






# REFERENTIEL ------------------------------------------------------------------
# invisible(capture.output({regions = st_read("maps/regions-version-simplifiee.geojson")}))


# coord_qubs_bourgo = coord_qubs_national %>%
#   st_join(regions) %>%
#   filter(nom == "Bourgogne-Franche-Comté")

# recuperation polygone parc de la Tete d'Or Lyon a partir de la couche CLC ----
# CLC_site = CLC_12 %>% filter(ID == "FR-34337")
# 
# coord_qubs_site <- coord_qubs_national %>%
#   st_join(CLC_site) %>%
#   filter(!is.na(CODE_12)) %>%
#   select(session_id, geometry)

# dt_qubs_site =  as.data.frame(coord_qubs_site) %>% select(session_id) %>% left_join(dt_qubs_total)

# jointure couche métro lyon et habitats CLC = habitats lyon
# CLC_lyon = CLC_12 %>% st_join(metro_lyon, left = F) 

# jointure couche régions et habitats CLC = habitats regionaux ou nationaux
# CLC_regions = CLC_12 %>% st_join(regions, left = F)
# CLC_aura = CLC_regions %>% filter(nom == "Auvergne-Rhône-Alpes")


# definition données ref valides et non valides ----

# dt_qubs_bourgo = as.data.frame(coord_qubs_bourgo) %>% select(session_id) %>% left_join(dt_qubs_total)
dt_qubs_gdparis = as.data.frame(coord_qubs_gdparis) %>% select(session_id) %>% left_join(dt_qubs_total)


dt_ref = dt_qubs_gdparis
  
dt_ref_valide = dt_ref %>% filter(session_id %in% dt_qubs_total_valide) %>%   filter(!is.na(taxon_valide))

# dt_ref = dt_qubs_bourgo
# dt_ref_valide = dt_ref %>% filter(taxon_valide == 1)

emprise_locale = metro_gdparis



dt_protoc_oso = readRDS("data/dt_qubs_oso_calcul_1000.rds")


# on considère que l'on applique pas de filtre pour les escargots ; on récupère le paysage_local == "impermeable pour les autres protocoles
# dt_protoc_oso %>%
# filter(paysage_local == "dominance_impermeable") %>%
# #(paysage_local == "dominance_impermeable" & protocole != "Opération escargots") | protocole == "Opération escargots"
# as.data.frame() %>%
# select(session_id, protocole) %>%
# left_join(as.data.frame(dt_qubs_nonvalid)) %>%
# filter(!st_is_empty(geometry))

