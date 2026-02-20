# calcul des référentiels qubs : 


# dt_ref_session
# dt_ref_site
# dt_stat_participant_buffer_aspifaune 
# dt_stat_participant_buffer_vers
# dt_stat_participant_buffer_escargots
# dt_stat_participant_buffer_noctambules



# à l'échelle des sessions, des sites, des participants selon des critères écologiques et dans un buffer

# IMPORT  ET MANIP DES DONNEES ----

# source(here::here("programs","library.R"))

## import db qubs : utile ? ----
# dt_qubs = readRDS("data/dt_qubs_2025_05_19.rds") %>% st_join(france_metro, left = F) # retirer les points qui ne sont pas en france metro
# source("functions/acces_db/db_qubs_en_local_sur_ordi.R")

# readRenviron(here::here(".env"))
# source(here::here("functions","function_encoding_utf8.R"))
# source(here::here("functions","acces_db","db_qubs.R"))

## import des fonctions ----
source(here::here("programs","fonctions_stat.R"))

## calcul du paysage local ----
# pour un buffer de 1000m sur les données qubs
dt_protoc_oso = readRDS("data/dt_qubs_oso_calcul_1000.rds") %>%
  mutate(paysage_local = case_when(paysage_local %in% c("mixte_impermeable_seminaturel", "mixte_agricole_seminaturel", "mixte_agricole_impermeable") ~  "equilibre", 
                                   TRUE ~ paysage_local))






# SESSION ----

# données valides pour les protocoles
dt_aspifaune = dt_qubs_total_valide %>% 
  filter(protocole == "Aspifaune") 

dt_noctambules= dt_qubs_total_valide %>% 
  filter(protocole == "Noctambules") 

dt_vers = dt_qubs_total_valide %>% 
  filter(protocole == "En quête de vers") 

dt_escargots = dt_qubs_total_valide %>% 
  filter(protocole == "Opération escargots") 



## combinaison avec le critère couverture du sol ----

dt_aspifaune_oso_valid = dt_protoc_oso %>%
  filter(protocole == "Aspifaune") %>%
  filter(session_id %in% dt_aspifaune$session_id)

dt_noctambules_oso_valid = dt_protoc_oso %>%
  filter(protocole == "Noctambules") %>% 
  filter(session_id %in% dt_noctambules$session_id)

dt_escargots_oso_valid = dt_protoc_oso %>%
  filter(protocole == "Opération escargots")  %>% 
  filter(session_id %in% dt_escargots$session_id)

dt_vers_oso_valid = dt_protoc_oso %>%
  filter(protocole == "En quête de vers") %>% 
  filter(session_id %in% dt_vers$session_id)



dt_aspifaune_oso_nnvalid = dt_protoc_oso %>%
  filter(protocole == "Aspifaune")

dt_noctambules_oso_nnvalid = dt_protoc_oso %>%
  filter(protocole == "Noctambules") 

dt_escargots_oso_nnvalid = dt_protoc_oso %>%
  filter(protocole == "Opération escargots")  

dt_vers_oso_nnvalid = dt_protoc_oso %>%
  filter(protocole == "En quête de vers")






## critère impermabilisation sol SOLIDEO ----

# formation des catégories de couverture imperméable pour correspondre au
#    -  Village des médias im € [0.45; 0.75]
#    -  Village des athlètes im € [0.65; 0.95]

func_calcul_critere_solideo_im = function(dt_oso_valid) {
  dt_oso_valid = dt_oso_valid %>%
    mutate(range_VA = case_when(im >= 0.65 & im <= 0.95 ~ "urbainVA",
                                TRUE ~ "autre"),
           range_VM = case_when(im >= 0.45 & im <= 0.65 ~ "urbainVM",
                                TRUE ~ "autre")) 
}

  
dt_aspifaune_oso_valid = func_calcul_critere_solideo_im(dt_aspifaune_oso_valid)
dt_noctambules_oso_valid = func_calcul_critere_solideo_im(dt_noctambules_oso_valid)
dt_escargots_oso_valid = func_calcul_critere_solideo_im(dt_escargots_oso_valid)
dt_vers_oso_valid = func_calcul_critere_solideo_im(dt_vers_oso_valid)


dt_aspifaune_oso_nnvalid = func_calcul_critere_solideo_im(dt_aspifaune_oso_nnvalid)
dt_noctambules_oso_nnvalid = func_calcul_critere_solideo_im(dt_noctambules_oso_nnvalid)
dt_escargots_oso_nnvalid = func_calcul_critere_solideo_im(dt_escargots_oso_nnvalid)
dt_vers_oso_nnvalid = func_calcul_critere_solideo_im(dt_vers_oso_nnvalid)


### DF FINAL SOLIDEO ----
dt_qubs_VA_VM = dt_aspifaune_oso_valid %>%
  bind_rows(dt_noctambules_oso_valid) %>%
  bind_rows(dt_escargots_oso_valid) %>%
  bind_rows(dt_vers_oso_valid) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  select(protocole, session_id, im, range_VA, range_VM) %>%
  unique()



## critere couverture sol a l'emplacement de relevé qubs
# colonne couv_point dt_protoc_oso

a1 = data.frame(
  regroupement = "semi_nat_foret", couv_point = c("forets de feuillus", "forets de coniferes", "landes ligneuses"))

a2 = data.frame(
  regroupement = "semi_nat_pelouse", couv_point = "pelouses")

a3 = data.frame(
  regroupement = "semi_nat_nonvegetalise", couv_point = c("eau", "surfaces minérales", "plages et dunes", "glaciers ou neiges"))

a4 = data.frame(
  regroupement = "agricole_vignes_vergers", couv_point = c("vergers", "vignes"))

a5 = data.frame(
  regroupement = "agricole_prairies", couv_point = "prairies")

a6 = data.frame(
  regroupement = "agricole_cultures", couv_point = c("colza", "cereales à pailles", "protéagineux", "soja", "tournesol", "maïs", "riz", "tubercules/racines"))

a7 =  data.frame(
  regroupement = "artificialise", couv_point = c("batis denses", "batis diffus", "zones ind et com", "surfaces routes", "serres"))
    
df_corresp_regroupement = a1 %>% 
  bind_rows(a2) %>% 
  bind_rows(a3) %>%
  bind_rows(a4) %>% 
  bind_rows(a5) %>%
  bind_rows(a6) %>%
  bind_rows(a7)



dt_qubs_couv_point_OPVT = dt_aspifaune_oso_valid %>%
  bind_rows(dt_noctambules_oso_valid) %>%
  bind_rows(dt_escargots_oso_valid) %>%
  bind_rows(dt_vers_oso_valid) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  select(protocole, session_id, couv_point ) %>%
  unique() %>%
  left_join(df_corresp_regroupement)


rm(a1, a2, a3, a4, a5, a6, a7, df_corresp_regroupement)

## critere paysage_local ----

# fonction qui se repose sur un jointure par protocole
func_sel_ref_paysage_local = function(dt_oso_valid, dt_oso_nnvalid, protoc = NULL) {
  
  n = length(unique(dt_oso_valid$session_id))
  if (n >= 50) {
    df_ref_session = as.data.frame(table(dt_oso_valid$paysage_local)) %>%
      rename(paysage_local = Var1) %>%
      mutate(ref_choisi = case_when(Freq > 49 ~ paysage_local,
                                    TRUE ~ "all_data"
      ),
      n_ref_choisi = case_when(Freq > 49 ~ Freq,
                               TRUE ~ n
      )
      ) %>%
      mutate(protocole = protoc) %>%
      left_join(dt_oso_valid) %>%
      select(protocole, ref_choisi, n_ref_choisi, session_id) %>%
      unique() %>%
      mutate(protocole = protoc) %>%
      mutate(ref_lies = ref_choisi, n_ref_lies = n_ref_choisi) %>%
      select(- session_id) %>%
      unique()
    
    liste_ref_choisis = unique(df_ref_session$ref_choisi)
    
    # association aux sessions non valides du protocole les référentiels
    # précedemment on a construit les référentiels sans les prendre en compte. 
    # Là on leur associe des référentiels formés.
    dt_ref_toutes_sessions = dt_oso_nnvalid %>%
      select(session_id, paysage_local, protocole) %>%
      
      # les paysage locaux sont-ils des référentiels valides ?
      mutate(ref_choisi = case_when(paysage_local %in% liste_ref_choisis ~ paysage_local,
                                    TRUE ~ "all_data")) %>%
      as.data.frame()  %>%
      select(-paysage_local, -geometry) %>%
      left_join(df_ref_session) %>%
      select(session_id, protocole, ref_choisi, n_ref_choisi, ref_lies, n_ref_lies) %>%
      unique()
    
    df_ref_session = dt_ref_toutes_sessions
    
    
  } else {
    df_ref_session = data.frame(ref_choisi = "all_data", n_ref_choisi = n, ref_lies = "all_data", n_ref_lies = n, protocole = protoc) %>%
      left_join(dt_oso_nnvalid) %>%
      select(protocole, ref_choisi, ref_lies, n_ref_lies, n_ref_choisi, session_id) %>%
      unique()
  }
  
  return(df_ref_session)
  
}

# df donnant pour chaque session, même les non valides, le référentiels associé. 
# Le référentiel est calculé en ne considérant que les sessions valides. 
# Pour calculer une valeur d'indicateur à partir de ce dt, 
# il faut récupérer les sessions qui nous intéressent, et leur référentiel "ref_choisi", 
# puis il faut pour ces référentiels aller chercher les sessions qui les composent 
# en utilisant la colonne ref_lies et en allant filtrer les sessions des ref_lies. 
df_ref_noctambules_session = func_sel_ref_paysage_local(dt_oso_valid = dt_noctambules_oso_valid, dt_oso_nnvalid = dt_noctambules_oso_nnvalid, protoc = "Noctambules")
df_ref_aspifaune_session = func_sel_ref_paysage_local(dt_oso_valid = dt_aspifaune_oso_valid, dt_oso_nnvalid = dt_aspifaune_oso_nnvalid, protoc = "Aspifaune")
df_ref_vers_session = func_sel_ref_paysage_local(dt_oso_valid = dt_vers_oso_valid, dt_oso_nnvalid = dt_vers_oso_nnvalid, protoc = "En quête de vers")
df_ref_escargots_session = func_sel_ref_paysage_local(dt_oso_valid = dt_escargots_oso_valid, dt_oso_nnvalid = dt_escargots_oso_nnvalid, protoc = "Opération escargots")


### DF FINAL paysage local ----

# le df qui lie les session_id à leur referentiel
dt_ref_session_paysloc = df_ref_aspifaune_session %>% 
  bind_rows(df_ref_escargots_session) %>% 
  bind_rows(df_ref_noctambules_session) %>% 
  bind_rows(df_ref_vers_session)






## critere paysage_local * N/S ----

# fonction qui se repose sur un jointure par protocole

func_sel_ref_couvsol_x_NS = function(dt_oso_valid, dt_oso_nnvalid, protoc = NULL){
  # automatisation de la décision : y a t-il au moins 50 sessions valides dans le protocole ? Si oui, alors y a t-il au moins 50 sessions lorsque l'on applique le critère couverture du sol ?
  n = length(unique(dt_oso_valid$session_id))
  if (n >= 50) {
    
    
    # on voit si le paysage local peut servir de critère de filtre pour les référentiels en fonction des protocoles
    df_ref_session0 = as.data.frame(table(dt_oso_valid$paysage_local)) %>%
      rename(paysage_local = Var1) %>%
      mutate(ref_choisi = case_when(Freq > 49 ~ paysage_local,
                                    TRUE ~ "all_data"
      ),
      n_ref_choisi = case_when(Freq > 49 ~ Freq,
                               TRUE ~ n
      )
      ) %>%
      mutate(protocole = protoc) %>%
      left_join(dt_oso_valid) %>%
      select(protocole, ref_choisi, n_ref_choisi, session_id) %>%
      unique()
    
    ## troisième niveau de référentiel : paysage local * N/S
    
    temp = df_ref_session0 %>% 
      left_join(dt_oso_valid) %>%
      st_as_sf() %>%
      mutate(lat = st_coordinates(.)[, 2]) %>%
      mutate(localisation = case_when(lat >= 46 ~ "N", TRUE ~ "S")) %>%
      mutate(combi_ref = paste0(ref_choisi, "---", localisation)) %>%
      group_by(combi_ref) %>%
      mutate(nb_session_combi = n_distinct(session_id)) %>%
      ungroup() %>%
      select(session_id, ref_choisi, n_ref_choisi ,combi_ref, nb_session_combi) %>%
      unique()  %>%
      as.data.frame() %>%
      select(-geometry) %>%
      
      # choix du referentiel --> ref_choisi = referentiel conservé pour la sessions  
      # ref_lies = autres référentiels dont la session est constitutive
      
      # changer nom colonne ref_choisi pour pouvoir le réutiliser
      rename(ref_initial = ref_choisi) %>%
      rename(n_ref_initial = n_ref_choisi) %>%
      
      mutate(ref_choisi = case_when(nb_session_combi > 49 ~ combi_ref,
                                    TRUE ~ ref_initial),
             n_ref_choisi = case_when(nb_session_combi > 49 ~ nb_session_combi,
                                      TRUE ~ n_ref_initial))
    
    
    # calcul des 3 echelles de référentiels_lies et je les associe à chaque session : 
    #echelle 1 = all_data
    #echelle 2 = paysage_local
    #echelle 3 = paysage_local * N/S
    # je distingue différents df pour ensuite les empiler et construire la colonne ref_lies et n_ref_lies
    temp1 = temp %>%
      select(combi_ref, nb_session_combi, session_id) %>%
      rename(ref_lies = combi_ref, n_ref_lies = nb_session_combi)
    
    temp2 = temp %>%
      mutate(ref_lies = "all_data", n_ref_lies = n_distinct(temp$session_id)) %>%
      select(session_id, ref_lies, n_ref_lies)
    
    dt_combi = temp %>%
      select(ref_initial, n_ref_initial, session_id) %>%
      rename(ref_lies = ref_initial, n_ref_lies = n_ref_initial) %>%
      bind_rows(temp1) %>%
      bind_rows(temp2) %>%
      left_join(temp) %>%
      select(session_id, ref_choisi, n_ref_choisi, ref_lies, n_ref_lies)
    
    # liste des referentiels choisis = ceux qui sont retenus 
    liste_ref_choisis = unique(dt_combi$ref_choisi)
    
    df_ref_session = dt_combi %>%
      filter(ref_lies %in% liste_ref_choisis) %>%
      mutate(protocole = protoc) %>%
      select(- session_id) %>%
      unique()
    
    
    # association aux sessions non valides du protocole les référentiels
    # précedemment on a construit les référentiels sans les prendre en compte. 
    # Là on leur associe des référentiels formés.
    dt_ref_toutes_sessions = dt_oso_nnvalid %>%
      select(session_id, paysage_local, protocole, geometry) %>%
      
      # ajout des colonnes manquantes à la formation des référentiels : N/S
      st_as_sf() %>%
      mutate(lat = st_coordinates(.)[, 2]) %>%
      mutate(localisation = case_when(lat >= 46 ~ "N", TRUE ~ "S")) %>%
      mutate(combi_ref = paste0(paysage_local, "---", localisation)) %>%
      mutate(dernier_ref = "all_data") %>%
      
      # on passe les colonnes avec les référentiels possibles en lignes
      pivot_longer(cols = c(paysage_local, dernier_ref, combi_ref),
                   values_to = "ref_lies") %>%
      filter(ref_lies %in% liste_ref_choisis) %>%
      
      # recuperation du ref_choisi à partir de tous les ref lies possibles par session
      group_by(session_id) %>%
      mutate(ref_choisi = ref_lies[which.max(nchar(ref_lies))]) %>%
      ungroup() %>%
    
      as.data.frame() %>%
      select(-geometry) %>%
      
      # join pour avoir les taille des ref_choisi et des ref_lies
      left_join(df_ref_session) %>%
      select(session_id, ref_choisi, n_ref_choisi, ref_lies, n_ref_lies, protocole) %>%
      unique()
    
    df_ref_session = dt_ref_toutes_sessions
    
  } else {
    df_ref_session = data.frame(ref_choisi = "all_data", n_ref_choisi = n, ref_lies = "all_data", n_ref_lies = n, protocole = protoc) %>%
      left_join(dt_oso_nnvalid) %>%
      select(protocole, ref_choisi, n_ref_choisi, ref_lies, n_ref_lies, session_id) %>%
      unique()
  }
  
  
  
}



# df donnant pour chaque session, même les non valides, le référentiels associé. 
# Le référentiel est calculé en ne considérant que les sessions valides. 
# Pour calculer une valeur d'indicateur à partir de ce dt, 
# il faut récupérer les sessions qui nous intéressent, et leur référentiel "ref_choisi", 
# puis il faut pour ces référentiels aller chercher les sessions qui les composent 
# en utilisant la colonne ref_lies et en allant filtrer les sessions des ref_lies. 

df_ref_noctambules_session_pays_X_NS = func_sel_ref_couvsol_x_NS(dt_oso_valid = dt_noctambules_oso_valid, dt_oso_nnvalid = dt_noctambules_oso_nnvalid, protoc = "Noctambules") 
df_ref_aspifaune_session_pays_X_NS = func_sel_ref_couvsol_x_NS(dt_oso_valid = dt_aspifaune_oso_valid,  dt_oso_nnvalid = dt_aspifaune_oso_nnvalid, protoc = "Aspifaune")
df_ref_vers_session_pays_X_NS = func_sel_ref_couvsol_x_NS(dt_oso_valid = dt_vers_oso_valid,  dt_oso_nnvalid = dt_vers_oso_nnvalid, protoc = "En quête de vers")
df_ref_escargots_session_pays_X_NS = func_sel_ref_couvsol_x_NS(dt_oso_valid = dt_escargots_oso_valid,  dt_oso_nnvalid = dt_escargots_oso_nnvalid, protoc = "Opération escargots")



#### DF FINAL paysage local * NS ----

# le df qui lie les session_id à leur referentiel
dt_ref_session_paysloc_x_NS = df_ref_noctambules_session_pays_X_NS %>% 
  bind_rows(df_ref_aspifaune_session_pays_X_NS) %>% 
  bind_rows(df_ref_vers_session_pays_X_NS) %>% 
  bind_rows(df_ref_escargots_session_pays_X_NS)





## critere taux urbanisation * N/S  ----

#### Villages des Athlètes ----
# Avec les taux d'urbanisation correspondant au Village des médias et au Village des athlètes 


func_sel_ref_urbainVA_x_NS = function(dt_oso_valid, dt_oso_nnvalid, protoc = NULL){
  # automatisation de la décision : y a t-il au moins 50 sessions valides dans le protocole ? Si oui, alors y a t-il au moins 50 sessions lorsque l'on applique le critère couverture du sol ?
  n = length(unique(dt_oso_valid$session_id))
  # dt_oso_valid$range_VA = as.character(dt_oso_valid$range_VA)
  # dt_oso_nnvalid$range_VA = as.character(dt_oso_nnvalid$range_VA)
  
  if (n >= 50) {
    
    
    # on voit si le paysage local peut servir de critère de filtre pour les référentiels en fonction des protocoles
    df_ref_session0 = as.data.frame(table(dt_oso_valid$range_VA)) %>%
      rename(range_VA = Var1) %>%
      filter(range_VA == "urbainVA") %>%
      mutate(ref_choisi = case_when(Freq > 49 ~ range_VA,
                                    TRUE ~ "all_data"
      ),
      n_ref_choisi = case_when(Freq > 49 ~ Freq,
                               TRUE ~ n
      )
      ) %>%
      mutate(protocole = protoc) %>%
      left_join(dt_oso_valid) %>%
      select(protocole, ref_choisi, n_ref_choisi, session_id) %>%
      unique()
    
    ## troisième niveau de référentiel : paysage local * N/S
    
    temp = df_ref_session0 %>% 
      left_join(dt_oso_valid) %>%
      st_as_sf() %>%
      mutate(lat = st_coordinates(.)[, 2]) %>%
      mutate(localisation = case_when(lat >= 46 ~ "N", TRUE ~ "S")) %>%
      mutate(combi_ref = paste0(ref_choisi, "---", localisation)) %>%
      group_by(combi_ref) %>%
      mutate(nb_session_combi = n_distinct(session_id)) %>%
      ungroup() %>%
      select(session_id, ref_choisi, n_ref_choisi ,combi_ref, nb_session_combi) %>%
      unique()  %>%
      as.data.frame() %>%
      select(-geometry) %>%
      
      # choix du referentiel --> ref_choisi = referentiel conservé pour la sessions  
      # ref_lies = autres référentiels dont la session est constitutive
      
      # changer nom colonne ref_choisi pour pouvoir le réutiliser
      rename(ref_initial = ref_choisi) %>%
      rename(n_ref_initial = n_ref_choisi) %>%
      
      mutate(ref_choisi = case_when(nb_session_combi > 49 ~ combi_ref,
                                    TRUE ~ ref_initial),
             n_ref_choisi = case_when(nb_session_combi > 49 ~ nb_session_combi,
                                      TRUE ~ n_ref_initial))
    
    
    # calcul des 3 echelles de référentiels_lies et je les associe à chaque session : 
    #echelle 1 = all_data
    #echelle 2 = range_VA
    #echelle 3 = range_VA * N/S
    # je distingue différents df pour ensuite les empiler et construire la colonne ref_lies et n_ref_lies
    temp1 = temp %>%
      select(combi_ref, nb_session_combi, session_id) %>%
      rename(ref_lies = combi_ref, n_ref_lies = nb_session_combi)
    
    temp2 = temp %>%
      mutate(ref_lies = "all_data", n_ref_lies = n_distinct(temp$session_id)) %>%
      select(session_id, ref_lies, n_ref_lies)
    
    dt_combi = temp %>%
      select(ref_initial, n_ref_initial, session_id) %>%
      rename(ref_lies = ref_initial, n_ref_lies = n_ref_initial) %>%
      bind_rows(temp1) %>%
      bind_rows(temp2) %>%
      left_join(temp) %>%
      select(session_id, ref_choisi, n_ref_choisi, ref_lies, n_ref_lies)
    
    # liste des referentiels choisis = ceux qui sont retenus 
    liste_ref_choisis = unique(dt_combi$ref_choisi)
    
    df_ref_session = dt_combi %>%
      filter(ref_lies %in% liste_ref_choisis) %>%
      mutate(protocole = protoc) %>%
      select(- session_id) %>%
      unique()
    
    
    # association aux sessions non valides du protocole les référentiels
    # précedemment on a construit les référentiels sans les prendre en compte. 
    # Là on leur associe des référentiels formés.
    dt_ref_toutes_sessions = dt_oso_nnvalid %>%
      select(session_id, range_VA, protocole, geometry) %>%
      
      # ajout des colonnes manquantes à la formation des référentiels : N/S
      st_as_sf() %>%
      mutate(lat = st_coordinates(.)[, 2]) %>%
      mutate(localisation = case_when(lat >= 46 ~ "N", TRUE ~ "S")) %>%
      mutate(combi_ref = paste0(range_VA, "---", localisation)) %>%
      mutate(dernier_ref = "all_data") %>%
      
      # on passe les colonnes avec les référentiels possibles en lignes
      pivot_longer(cols = c(range_VA, dernier_ref, combi_ref),
                   values_to = "ref_lies") %>%
      filter(ref_lies %in% liste_ref_choisis) %>%
      
      # recuperation du ref_choisi à partir de tous les ref lies possibles par session
      group_by(session_id) %>%
      mutate(ref_choisi = ref_lies[which.max(nchar(ref_lies))]) %>%
      ungroup() %>%
      
      as.data.frame() %>%
      select(-geometry) %>%
      
      # join pour avoir les taille des ref_choisi et des ref_lies
      left_join(df_ref_session) %>%
      select(session_id, ref_choisi, n_ref_choisi, ref_lies, n_ref_lies, protocole) %>%
      unique()
    
    df_ref_session = dt_ref_toutes_sessions
    
  } else {
    df_ref_session = data.frame(ref_choisi = "all_data", n_ref_choisi = n, ref_lies = "all_data", n_ref_lies = n, protocole = protoc) %>%
      left_join(dt_oso_nnvalid) %>%
      select(protocole, ref_choisi, n_ref_choisi, ref_lies, n_ref_lies, session_id) %>%
      unique()
  }
  
  
  
}



# df donnant pour chaque session, même les non valides, le référentiels associé. 
# Le référentiel est calculé en ne considérant que les sessions valides. 
# Pour calculer une valeur d'indicateur à partir de ce dt, 
# il faut récupérer les sessions qui nous intéressent, et leur référentiel "ref_choisi", 
# puis il faut pour ces référentiels aller chercher les sessions qui les composent 
# en utilisant la colonne ref_lies et en allant filtrer les sessions des ref_lies. 

df_ref_noctambules_session_urbainVA_x_NS = func_sel_ref_urbainVA_x_NS(dt_oso_valid = dt_noctambules_oso_valid, dt_oso_nnvalid = dt_noctambules_oso_nnvalid, protoc = "Noctambules") 
df_ref_aspifaune_session_urbainVA_x_NS = func_sel_ref_urbainVA_x_NS(dt_oso_valid = dt_aspifaune_oso_valid,  dt_oso_nnvalid = dt_aspifaune_oso_nnvalid, protoc = "Aspifaune")
df_ref_vers_session_urbainVA_x_NS = func_sel_ref_urbainVA_x_NS(dt_oso_valid = dt_vers_oso_valid,  dt_oso_nnvalid = dt_vers_oso_nnvalid, protoc = "En quête de vers")
df_ref_escargots_session_urbainVA_x_NS = func_sel_ref_urbainVA_x_NS(dt_oso_valid = dt_escargots_oso_valid,  dt_oso_nnvalid = dt_escargots_oso_nnvalid, protoc = "Opération escargots")



#### DF FINAL urbainVA * NS ----

# le df qui lie les session_id à leur referentiel
dt_ref_session_urbainVA_x_NS = df_ref_noctambules_session_urbainVA_x_NS %>% 
  bind_rows(df_ref_aspifaune_session_urbainVA_x_NS) %>% 
  bind_rows(df_ref_vers_session_urbainVA_x_NS) %>% 
  bind_rows(df_ref_escargots_session_urbainVA_x_NS)


#### Village des Médias ----

# Avec les taux d'urbanisation correspondant au Village des médias et au Village des athlètes 


func_sel_ref_urbainVM_x_NS = function(dt_oso_valid, dt_oso_nnvalid, protoc = NULL){
  # automatisation de la décision : y a t-il au moins 50 sessions valides dans le protocole ? Si oui, alors y a t-il au moins 50 sessions lorsque l'on applique le critère couverture du sol ?
  n = length(unique(dt_oso_valid$session_id))
  # dt_oso_valid$range_VM = as.character(dt_oso_valid$range_VM)
  # dt_oso_nnvalid$range_VM = as.character(dt_oso_nnvalid$range_VM)
  
  if (n >= 50) {
    
    
    # on voit si le paysage local peut servir de critère de filtre pour les référentiels en fonction des protocoles
    df_ref_session0 = as.data.frame(table(dt_oso_valid$range_VM)) %>%
      rename(range_VM = Var1) %>%
      filter(range_VM == "urbainVM") %>%
      mutate(ref_choisi = case_when(Freq > 49 ~ range_VM,
                                    TRUE ~ "all_data"
      ),
      n_ref_choisi = case_when(Freq > 49 ~ Freq,
                               TRUE ~ n
      )
      ) %>%
      mutate(protocole = protoc) %>%
      left_join(dt_oso_valid) %>%
      select(protocole, ref_choisi, n_ref_choisi, session_id) %>%
      unique()
    
    ## troisième niveau de référentiel : paysage local * N/S
    
    temp = df_ref_session0 %>% 
      left_join(dt_oso_valid) %>%
      st_as_sf() %>%
      mutate(lat = st_coordinates(.)[, 2]) %>%
      mutate(localisation = case_when(lat >= 46 ~ "N", TRUE ~ "S")) %>%
      mutate(combi_ref = paste0(ref_choisi, "---", localisation)) %>%
      group_by(combi_ref) %>%
      mutate(nb_session_combi = n_distinct(session_id)) %>%
      ungroup() %>%
      select(session_id, ref_choisi, n_ref_choisi ,combi_ref, nb_session_combi) %>%
      unique()  %>%
      as.data.frame() %>%
      select(-geometry) %>%
      
      # choix du referentiel --> ref_choisi = referentiel conservé pour la sessions  
      # ref_lies = autres référentiels dont la session est constitutive
      
      # changer nom colonne ref_choisi pour pouvoir le réutiliser
      rename(ref_initial = ref_choisi) %>%
      rename(n_ref_initial = n_ref_choisi) %>%
      
      mutate(ref_choisi = case_when(nb_session_combi > 49 ~ combi_ref,
                                    TRUE ~ ref_initial),
             n_ref_choisi = case_when(nb_session_combi > 49 ~ nb_session_combi,
                                      TRUE ~ n_ref_initial))
    
    
    # calcul des 3 echelles de référentiels_lies et je les associe à chaque session : 
    #echelle 1 = all_data
    #echelle 2 = range_VM
    #echelle 3 = range_VM * N/S
    # je distingue différents df pour ensuite les empiler et construire la colonne ref_lies et n_ref_lies
    temp1 = temp %>%
      select(combi_ref, nb_session_combi, session_id) %>%
      rename(ref_lies = combi_ref, n_ref_lies = nb_session_combi)
    
    temp2 = temp %>%
      mutate(ref_lies = "all_data", n_ref_lies = n_distinct(temp$session_id)) %>%
      select(session_id, ref_lies, n_ref_lies)
    
    dt_combi = temp %>%
      select(ref_initial, n_ref_initial, session_id) %>%
      rename(ref_lies = ref_initial, n_ref_lies = n_ref_initial) %>%
      bind_rows(temp1) %>%
      bind_rows(temp2) %>%
      left_join(temp) %>%
      select(session_id, ref_choisi, n_ref_choisi, ref_lies, n_ref_lies)
    
    # liste des referentiels choisis = ceux qui sont retenus 
    liste_ref_choisis = unique(dt_combi$ref_choisi)
    
    df_ref_session = dt_combi %>%
      filter(ref_lies %in% liste_ref_choisis) %>%
      mutate(protocole = protoc) %>%
      select(- session_id) %>%
      unique()
    
    
    # association aux sessions non valides du protocole les référentiels
    # précedemment on a construit les référentiels sans les prendre en compte. 
    # Là on leur associe des référentiels formés.
    dt_ref_toutes_sessions = dt_oso_nnvalid %>%
      select(session_id, range_VM, protocole, geometry) %>%
      
      # ajout des colonnes manquantes à la formation des référentiels : N/S
      st_as_sf() %>%
      mutate(lat = st_coordinates(.)[, 2]) %>%
      mutate(localisation = case_when(lat >= 46 ~ "N", TRUE ~ "S")) %>%
      mutate(combi_ref = paste0(range_VM, "---", localisation)) %>%
      mutate(dernier_ref = "all_data") %>%
      
      # on passe les colonnes avec les référentiels possibles en lignes
      pivot_longer(cols = c(range_VM, dernier_ref, combi_ref),
                   values_to = "ref_lies") %>%
      filter(ref_lies %in% liste_ref_choisis) %>%
      
      # recuperation du ref_choisi à partir de tous les ref lies possibles par session
      group_by(session_id) %>%
      mutate(ref_choisi = ref_lies[which.max(nchar(ref_lies))]) %>%
      ungroup() %>%
      
      as.data.frame() %>%
      select(-geometry) %>%
      
      # join pour avoir les taille des ref_choisi et des ref_lies
      left_join(df_ref_session) %>%
      select(session_id, ref_choisi, n_ref_choisi, ref_lies, n_ref_lies, protocole) %>%
      unique()
    
    df_ref_session = dt_ref_toutes_sessions
    
  } else {
    df_ref_session = data.frame(ref_choisi = "all_data", n_ref_choisi = n, ref_lies = "all_data", n_ref_lies = n, protocole = protoc) %>%
      left_join(dt_oso_nnvalid) %>%
      select(protocole, ref_choisi, n_ref_choisi, ref_lies, n_ref_lies, session_id) %>%
      unique()
  }
  
  
  
}



# df donnant pour chaque session, même les non valides, le référentiels associé. 
# Le référentiel est calculé en ne considérant que les sessions valides. 
# Pour calculer une valeur d'indicateur à partir de ce dt, 
# il faut récupérer les sessions qui nous intéressent, et leur référentiel "ref_choisi", 
# puis il faut pour ces référentiels aller chercher les sessions qui les composent 
# en utilisant la colonne ref_lies et en allant filtrer les sessions des ref_lies. 

df_ref_noctambules_session_urbainVM_x_NS = func_sel_ref_urbainVM_x_NS(dt_oso_valid = dt_noctambules_oso_valid, dt_oso_nnvalid = dt_noctambules_oso_nnvalid, protoc = "Noctambules") 
df_ref_aspifaune_session_urbainVM_x_NS = func_sel_ref_urbainVM_x_NS(dt_oso_valid = dt_aspifaune_oso_valid,  dt_oso_nnvalid = dt_aspifaune_oso_nnvalid, protoc = "Aspifaune")
df_ref_vers_session_urbainVM_x_NS = func_sel_ref_urbainVM_x_NS(dt_oso_valid = dt_vers_oso_valid,  dt_oso_nnvalid = dt_vers_oso_nnvalid, protoc = "En quête de vers")
df_ref_escargots_session_urbainVM_x_NS = func_sel_ref_urbainVM_x_NS(dt_oso_valid = dt_escargots_oso_valid,  dt_oso_nnvalid = dt_escargots_oso_nnvalid, protoc = "Opération escargots")



#### DF FINAL urbainVM * NS ----

# le df qui lie les session_id à leur referentiel
dt_ref_session_urbainVM_x_NS = df_ref_noctambules_session_urbainVM_x_NS %>% 
  bind_rows(df_ref_aspifaune_session_urbainVM_x_NS) %>% 
  bind_rows(df_ref_vers_session_urbainVM_x_NS) %>% 
  bind_rows(df_ref_escargots_session_urbainVM_x_NS)




























# Pour pouvoir l'utiliser il faudrait refaire ce code
# en prenant en compte les sessions non valides et la distinction ref_choisi / ref_lies

## session : critères buffer ----

# SITE----

## site : critères écologiques ----

# Pour pouvoir l'utiliser il faudrait refaire ce code
# en prenant en compte les sessions non valides et la distinction ref_choisi / ref_lies

### identification des sites valides ---- 
# à partir des sessions valides

# # aspifaune
# dt_site_aspifaune = df_ref_aspifaune_session %>% 
#   left_join(dt_aspifaune) %>%
#   select(ref_choisi, site_id) %>%
#   unique() %>% 
#   rename(paysage_local = ref_choisi)
# 
# df_ref_aspifaune_site = as.data.frame(table(dt_site_aspifaune$paysage_local)) %>%
#   rename(paysage_local = Var1) %>%
#   mutate(ref_choisi = case_when(Freq > 49 ~ paysage_local,
#                                 TRUE ~ "all_data"
#   ),
#   n_ref_choisi = case_when(Freq > 49 ~ Freq,
#                            TRUE ~ sum(Freq))
#   ) %>%
#   left_join(dt_site_aspifaune) %>%
#   select(site_id, ref_choisi, n_ref_choisi) %>%
#   mutate(protocole = "Aspifaune")
# 
# # noctambules
# dt_site_noctambules = df_ref_noctambules_session %>% 
#   left_join(dt_noctambules) %>%
#   select(ref_choisi, site_id) %>%
#   unique() %>% 
#   rename(paysage_local = ref_choisi)
# 
# df_ref_noctambules_site = as.data.frame(table(dt_site_noctambules$paysage_local)) %>%
#   rename(paysage_local = Var1) %>%
#   mutate(ref_choisi = case_when(Freq > 49 ~ paysage_local,
#                                 TRUE ~ "all_data"
#   ),
#   n_ref_choisi = case_when(Freq > 49 ~ Freq,
#                            TRUE ~ sum(Freq)
#   )
#   ) %>%
#   left_join(dt_site_noctambules) %>%
#   select(site_id, ref_choisi, n_ref_choisi) %>%
#   mutate(protocole = "Noctambules")
# 
# #escargots
# dt_site_escargots = df_ref_escargots_session %>% 
#   left_join(dt_escargots) %>%
#   select(ref_choisi, site_id) %>%
#   unique() %>% 
#   rename(paysage_local = ref_choisi)
# 
# df_ref_escargots_site = as.data.frame(table(dt_site_escargots$paysage_local)) %>%
#   rename(paysage_local = Var1) %>%
#   mutate(ref_choisi = case_when(Freq > 49 ~ paysage_local,
#                                 TRUE ~ "all_data"
#   ),
#   n_ref_choisi = case_when(Freq > 49 ~ Freq,
#                            TRUE ~ sum(Freq)
#   )
#   ) %>%
#   left_join(dt_site_escargots) %>%
#   select(site_id, ref_choisi, n_ref_choisi) %>%
#   mutate(protocole = "Opération escargots")
# 
# # vers
# dt_site_vers= df_ref_vers_session %>% 
#   left_join(dt_vers) %>%
#   select(ref_choisi, site_id) %>%
#   unique() %>% 
#   rename(paysage_local = ref_choisi)
# 
# df_ref_vers_site = as.data.frame(table(dt_site_vers$paysage_local)) %>%
#   rename(paysage_local = Var1) %>%
#   mutate(ref_choisi = case_when(Freq > 49 ~ paysage_local,
#                                 TRUE ~ "all_data"
#   ),
#   n_ref_choisi = case_when(Freq > 49 ~ Freq,
#                            TRUE ~ sum(Freq)
#   )
#   ) %>%
#   left_join(dt_site_vers) %>%
#   select(site_id, ref_choisi, n_ref_choisi) %>%
#   mutate(protocole = "En Quête de Vers")
# 
# 
# 
# 
# 
# 
# 
# ## DF FINAL ----
# dt_ref_site = df_ref_aspifaune_site %>%
#   bind_rows(df_ref_escargots_site) %>%
#   bind_rows(df_ref_vers_site) %>%
#   bind_rows(df_ref_noctambules_site)
# 


## site : critères buffer ----


# # PARTICIPANT ----
# 
# ## participant : critère buffer ----
# 
# # On considère un buffer modulable 
# 
# func_stat_participants = function(dt_protocole, dt_protocole_tot, buffer_size){
#   
#   # récupération des sites associés aux participants pour pouvoir avoir l'info de 
#   # cb de sites sont réalisés par participant et combien d'années ils sont suivis
#   df_continu = dt_protocole_tot %>%
#     rename(an = year) %>%
#     select(participant_id, site_nom, session_id, an) %>%
#     unique() %>% 
#     group_by(participant_id, site_nom, an) %>%
#     reframe(nb_passage = n_distinct(session_id)) %>%
#     ungroup() %>%
#     # filter(nb_passage >= 3 & nb_passage < 15) %>% # (filtre grossier haut pour enlever les anomalies)
#     group_by(participant_id, site_nom) %>%
#     reframe(nb_an_suivi = n_distinct(an)) %>%
#     ungroup() %>%
#     rename(user_id = participant_id) %>%
#     unique()
#   
#   
#   
#   # calcul d'une coordonnée par participant
#   df_oso = dt_protoc_oso %>%
#     as.data.frame() %>%
#     select(session_id, user_id, paysage_local) %>%
#     unique()
#   
#   dt_centroide_participant = dt_protocole %>%
#     group_by(user_id) %>%
#     summarize(centroide = st_centroid(st_union(geometry))) %>%  # calcule le centroïde de cette géométrie)
#     ungroup() #%>%
#   # left_join(df_oso) 
#   
#   
#   
#   
#   
#   
#   # BOUCLE : calcul des participants dans un buffer de 200 km  #######################
#   dt_stat_struct_buffer = data.frame()
#   
#   for (participant in unique(dt_centroide_participant$user_id)) {
#     
#     zone_buffer <- dt_centroide_participant %>%
#       filter(user_id == participant) %>%
#       pull(centroide) %>%
#       st_buffer(dist = buffer_size) # fabrique un cercle de 200 km de rayon autour du point
#     
#     # je récupère une coordonnée par participant = centroide des points
#     struct_proches <- dt_centroide_participant %>%
#       filter(user_id != participant) %>%
#       st_intersection(zone_buffer) %>% # je récupère les points qui sont dans le buffer = participants dans le buffer
#       left_join(df_oso)
#     
#     
#     # savoir combien d'annees les participants alentours ont participé sur leurs sites
#     df_conti_buffer = df_continu %>%
#       filter(user_id %in% unique(struct_proches$user_id))
#     
#     
#     list_struct_proches = c(unique(struct_proches$user_id))
#     
#     # stats sur les structures proches
#     dt_stat = tibble(user_id = participant, # participant concernée
#                      liste_struct_proches = list(list_struct_proches), # liste des participants dans un buffer de 200km
#                      nb_struct = n_distinct(list_struct_proches), # nombre de participants dans le buffer 
#                      nb_sessions_buffer = n_distinct(struct_proches$session_id), # nombre de sessions réalisées dans le buffer (exclut les sessions de la participant concernée)
#                      nb_sess_sn_buffer = sum(struct_proches$paysage_local == "dominance_semi_naturel", na.rm = TRUE),# nombre de sessions dans un paysage Semi-Naturel pour les autres participants 
#                      nb_sess_ag_buffer = sum(struct_proches$paysage_local == "dominance_agricole", na.rm = TRUE),# nombre de sessions dans un paysage Agricole pour les autres participants 
#                      nb_sess_im_buffer = sum(struct_proches$paysage_local == "dominance_impermeable", na.rm = TRUE),# nombre de sessions dans un paysage Impermeable pour les autres participants 
#                      nb_sess_eq_buffer = sum(struct_proches$paysage_local == "equilibre", na.rm = TRUE),# nombre de sessions dans un paysage Equilibré pour les autres participants 
#                      
#                      # nombre de sites dans le buffer
#                      nb_site_buffer = n_distinct(df_conti_buffer$site_nom),
#                      # maximum de continuité dans le buffer
#                      max_conti_site_buffer = unique(max(df_conti_buffer$nb_an_suivi, na.rm = TRUE))
#     ) %>% 
#       
#       # on récupère les mêmes infos pour le participant concerné
#       left_join(df_oso) %>%
#       mutate(nb_sess_user_id = n_distinct(session_id), 
#              nb_sess_im_user_id = sum(paysage_local == "dominance_impermeable", na.rm = TRUE),
#              nb_sess_ag_user_id = sum(paysage_local == "dominance_agricole", na.rm = TRUE),
#              nb_sess_sn_user_id = sum(paysage_local == "dominance_semi_naturel", na.rm = TRUE),
#              nb_sess_eq_user_id = sum(paysage_local == "equilibre", na.rm = TRUE),
#              nb_site_struct = sum(df_continu$user_id == participant, na.rm = TRUE),
#              max_conti_site_struct = max(df_continu$nb_an_suivi[df_continu$user_id == participant], na.rm = TRUE)) %>%
#       select(-session_id, -paysage_local) %>%
#       unique()
#     
#     dt_stat_struct_buffer = dt_stat_struct_buffer %>%
#       bind_rows(dt_stat)
#     
#   }
#   
#   # FIN DE BOUCLE sur tous les participants ###########################
#   
#   
#   # ajout de quelques calculs en dehors de la boucle sur le df presque achevé
#   dt_stat_struct_buffer = dt_stat_struct_buffer %>% 
#     mutate(
#       # pourcentage de sessions dans impermeable dans le buffer
#       pourc_im_buffer = round(100 * nb_sess_im_buffer / nb_sessions_buffer, 1),
#       # pourcentage de sessions dans agricole dans le buffer
#       pourc_ag_buffer = round(100 * nb_sess_ag_buffer / nb_sessions_buffer, 1),
#       # pourcentage de sessions dans semi-naturel dans le buffer
#       pourc_sn_buffer = round(100 * nb_sess_sn_buffer / nb_sessions_buffer, 1),
#       # pourcentage de sessions dans equilibré dans le buffer
#       pourc_eq_buffer = round(100 * nb_sess_eq_buffer / nb_sessions_buffer, 1),
#       
#       # pourcentage de sessions dans impermeable en local
#       pourc_im_struct = round(100 * nb_sess_im_user_id / nb_sess_user_id, 1),
#       # pourcentage de sessions dans agricole en local
#       pourc_ag_struct = round(100 * nb_sess_ag_user_id / nb_sess_user_id, 1),
#       # pourcentage de sessions dans semi-naturel en local
#       pourc_sn_struct = round(100 * nb_sess_sn_user_id / nb_sess_user_id, 1),
#       # pourcentage de sessions dans equilibre en local
#       pourc_eq_struct = round(100 * nb_sess_eq_user_id / nb_sess_user_id, 1),
#       
#       # moyenne du nombre de sessions par participant 
#       moy_nb_sess_par_user_buffer = round(nb_sessions_buffer / nb_struct),
#       # poids de la participant dans le buffer
#       poids_struct_buffer = round(100 * nb_sess_user_id / (nb_sessions_buffer + nb_sess_user_id), 1),
#       # nombre de sites moyens par participant
#       nb_moy_site_buffer = round(100 * nb_site_buffer / nb_struct, 1)
#     ) %>%
#     # on remplace les -Inf qui apparaissent pour les participants qui n'ont aucun site avec des années suivant le protocole ( 3 sessions ) par des 0
#     mutate(max_conti_site_struct = ifelse(is.infinite(max_conti_site_struct), 0, max_conti_site_struct), 
#            max_conti_site_buffer = ifelse(is.infinite(max_conti_site_buffer), 0, max_conti_site_buffer))  %>%
#     # reordonnéer les colonnes
#     select(user_id, 
#            pourc_im_buffer, pourc_im_struct,  
#            pourc_ag_buffer, pourc_ag_struct,
#            pourc_sn_buffer, pourc_sn_struct,
#            pourc_eq_buffer, pourc_eq_struct,
#            poids_struct_buffer, nb_struct, moy_nb_sess_par_user_buffer, nb_sessions_buffer, nb_sess_user_id, 
#            nb_sess_im_buffer, nb_sess_im_user_id, 
#            nb_sess_ag_buffer, nb_sess_ag_user_id,
#            nb_sess_sn_buffer, nb_sess_sn_user_id,
#            nb_sess_eq_buffer, nb_sess_eq_user_id,
#            nb_moy_site_buffer, nb_site_struct, nb_site_buffer, max_conti_site_buffer,  max_conti_site_struct, liste_struct_proches)
#   
#   return(dt_stat_struct_buffer)
#   
# }
# 
# # buffer_size : taille du buffer en m
# dt_stat_participant_buffer_aspifaune = func_stat_participants(dt_aspifaune_oso, dt_aspifaune, buffer_size = 200000) 
# dt_stat_participant_buffer_vers = func_stat_participants(dt_vers_oso, dt_vers, buffer_size = 200000)
# dt_stat_participant_buffer_escargots = func_stat_participants(dt_escargots_oso, dt_escargots, buffer_size = 200000)
# dt_stat_participant_buffer_noctambules = func_stat_participants(dt_noctambules_oso, dt_noctambules, buffer_size = 200000)
# 


# 
# # automatisation de la décision : y a t-il au moins 50 sessions valides dans le protocole ? Si oui, alors y a t-il au moins 50 sessions lorsque l'on applique le critère couverture du sol ?
# nn = length(unique(dt_noctambules_oso_valid$session_id))
# if (nn >= 50) {
#   df_ref_noctambules_session = as.data.frame(table(dt_noctambules_oso_valid$paysage_local)) %>%
#     rename(paysage_local = Var1) %>%
#     mutate(ref_choisi = case_when(Freq > 49 ~ paysage_local,
#                                   TRUE ~ "all_data"
#     ),
#     n_ref_choisi = case_when(Freq > 49 ~ Freq,
#                              TRUE ~ nn
#     )
#     ) %>%
#     mutate(protocole = "Noctambules") %>%
#     left_join(dt_noctambules_oso_valid) %>%
#     select(protocole, ref_choisi, n_ref_choisi, session_id) %>%
#     unique()
#   
# } else {
#   df_ref_noctambules_session = data.frame(ref_choisi = "all_data", n_ref_choisi = nn, protocole = "Noctambules") %>%
#     left_join(dt_noctambules_oso_valid) %>%
#     select(protocole, ref_choisi, n_ref_choisi, session_id) %>%
#     unique()
# }
# 
# 
# 
# # automatisation de la décision : y a t-il au moins 50 sessions valides dans le protocole ? Si oui, alors y a t-il au moins 50 sessions lorsque l'on applique le critère couverture du sol ?
# nv = length(unique(dt_vers_oso_valid$session_id))
# if (nv >= 50) {
#   df_ref_vers_session = as.data.frame(table(dt_vers_oso_valid$paysage_local)) %>%
#     rename(paysage_local = Var1) %>%
#     mutate(ref_choisi = case_when(Freq > 49 ~ paysage_local,
#                                   TRUE ~ "all_data"
#     ),
#     n_ref_choisi = case_when(Freq > 49 ~ Freq,
#                              TRUE ~ nv
#     )
#     ) %>%
#     mutate(protocole = "En quête de vers") %>%
#     left_join(dt_vers_oso_valid) %>%
#     select(protocole, ref_choisi, n_ref_choisi, session_id) %>%
#     unique()
#   
# } else {
#   df_ref_vers_session = data.frame(ref_choisi = "all_data", n_ref_choisi = nv, protocole = "En quête de vers") %>%
#     left_join(dt_vers_oso_valid) %>%
#     select(protocole, ref_choisi, n_ref_choisi, session_id) %>%
#     unique()
# }
# 
# # automatisation de la décision : y a t-il au moins 50 sessions valides dans le protocole ? Si oui, alors y a t-il au moins 50 sessions lorsque l'on applique le critère couverture du sol ?
# ne = length(unique(dt_escargots_oso_valid$session_id))
# if (ne >= 50) {
#   df_ref_escargots_session = as.data.frame(table(dt_escargots_oso_valid$paysage_local)) %>%
#     rename(paysage_local = Var1) %>%
#     mutate(ref_choisi = case_when(Freq > 49 ~ paysage_local,
#                                   TRUE ~ "all_data"
#     ),
#     n_ref_choisi = case_when(Freq > 49 ~ Freq,
#                              TRUE ~ ne
#     )
#     ) %>%
#     mutate(protocole = "Opération escargots") %>%
#     left_join(dt_escargots_oso_valid) %>%
#     select(protocole, ref_choisi, n_ref_choisi, session_id) %>%
#     unique()
#   
# } else {
#   df_ref_escargots_session = data.frame(ref_choisi = "all_data", n_ref_choisi = ne, protocole = "Opération escargots") %>%
#     left_join(dt_escargots_oso_valid) %>%
#     select(protocole, ref_choisi, n_ref_choisi, session_id) %>%
#     unique()
# }












# CORBEILLE ----

# func_sel_ref_couvsol_x_NS = function(dt_oso_valid, protoc = NULL){
#   # automatisation de la décision : y a t-il au moins 50 sessions valides dans le protocole ? Si oui, alors y a t-il au moins 50 sessions lorsque l'on applique le critère couverture du sol ?
#   n = length(unique(dt_oso_valid$session_id))
#   if (n >= 50) {
#     
#     
#     # on voit si le paysage local peut servir de critère de filtre pour les référentiels en fonction des protocoles
#     df_ref_session0 = as.data.frame(table(dt_oso_valid$paysage_local)) %>%
#       rename(paysage_local = Var1) %>%
#       mutate(ref_choisi = case_when(Freq > 49 ~ paysage_local,
#                                     TRUE ~ "all_data"
#       ),
#       n_ref_choisi = case_when(Freq > 49 ~ Freq,
#                                TRUE ~ n
#       )
#       ) %>%
#       mutate(protocole = protoc) %>%
#       left_join(dt_oso_valid) %>%
#       select(protocole, ref_choisi, n_ref_choisi, session_id) %>%
#       unique()
#     
#     ## troisième niveau de référentiel : paysage local * N/S
#     
#     temp = df_ref_session0 %>% 
#       left_join(dt_oso_valid) %>%
#       st_as_sf() %>%
#       mutate(lat = st_coordinates(.)[, 2]) %>%
#       mutate(localisation = case_when(lat >= 46 ~ "N", TRUE ~ "S")) %>%
#       mutate(combi_ref = paste0(ref_choisi, "---", localisation)) %>%
#       group_by(combi_ref) %>%
#       mutate(nb_session_combi = n_distinct(session_id)) %>%
#       ungroup() %>%
#       select(session_id, ref_choisi, n_ref_choisi ,combi_ref, nb_session_combi) %>%
#       unique()  %>%
#       as.data.frame() %>%
#       select(-geometry) %>%
#       
#       # choix du referentiel --> ref_choisi = referentiel conservé pour la sessions  
#       # ref_lies = autres référentiels dont la session est constitutive
#       
#       # changer nom colonne ref_choisi pour pouvoir le réutiliser
#       rename(ref_initial = ref_choisi) %>%
#       rename(n_ref_initial = n_ref_choisi) %>%
#       
#       mutate(ref_choisi = case_when(nb_session_combi > 49 ~ combi_ref,
#                                     TRUE ~ ref_initial),
#              n_ref_choisi = case_when(nb_session_combi > 49 ~ nb_session_combi,
#                                       TRUE ~ n_ref_initial))
#     
#     
#     # calcul des 3 echelles de référentiels_lies et je les associe à chaque session : 
#     #echelle 1 = all_data
#     #echelle 2 = paysage_local
#     #echelle 3 = paysage_local * N/S
#     # je distingue différents df pour ensuite les empiler et construire la colonne ref_lies et n_ref_lies
#     temp1 = temp %>%
#       select(combi_ref, nb_session_combi, session_id) %>%
#       rename(ref_lies = combi_ref, n_ref_lies = nb_session_combi)
#     
#     temp2 = temp %>%
#       mutate(ref_lies = "all_data", n_ref_lies = n_distinct(temp$session_id)) %>%
#       select(session_id, ref_lies, n_ref_lies)
#     
#     dt_combi = temp %>%
#       select(ref_initial, n_ref_initial, session_id) %>%
#       rename(ref_lies = ref_initial, n_ref_lies = n_ref_initial) %>%
#       bind_rows(temp1) %>%
#       bind_rows(temp2) %>%
#       left_join(temp) %>%
#       select(session_id, ref_choisi, n_ref_choisi, ref_lies, n_ref_lies)
#     
#     # liste des referentiels choisis = ceux qui sont retenus 
#     liste_ref_choisis = unique(dt_combi$ref_choisi)
#     
#     df_ref_session = dt_combi %>%
#       filter(ref_lies %in% liste_ref_choisis) %>%
#       mutate(protocole = protoc) 
#     
#   } else {
#     df_ref_session = data.frame(ref_choisi = "all_data", n_ref_choisi = n, ref_lies = "all_data", n_ref_lies = n, protocole = protoc) %>%
#       left_join(dt_oso_valid) %>%
#       select(protocole, ref_choisi, n_ref_choisi, ref_lies, n_ref_lies, session_id) %>%
#       unique()
#   }
#   
#   
#   
# }







# func_sel_ref_paysage_localOG = function(dt_oso_valid, protoc = NULL) {
# 
#   n = length(unique(dt_oso_valid$session_id))
#   if (n >= 50) {
#     df_ref_session = as.data.frame(table(dt_oso_valid$paysage_local)) %>%
#       rename(paysage_local = Var1) %>%
#       mutate(ref_choisi = case_when(Freq > 49 ~ paysage_local,
#                                     TRUE ~ "all_data"
#       ),
#       n_ref_choisi = case_when(Freq > 49 ~ Freq,
#                                TRUE ~ n
#       )
#       ) %>%
#       mutate(protocole = protoc) %>%
#       left_join(dt_oso_valid) %>%
#       select(protocole, ref_choisi, n_ref_choisi, session_id) %>%
#       unique() %>%
#       mutate(protocole = protoc) %>%
#       mutate(ref_lies = ref_choisi, n_ref_lies = n_ref_choisi)
#     
#     # # ajout ref liés
#     # temp = df_ref_session %>%
#     # mutate(ref_lies = "all_data", n_ref_lies = n) %>%
#     #   select(session_id, ref_lies, n_ref_lies)
#     # 
#     # 
#     # df_ref_session = temp0 %>%
#     #   mutate(ref_lies = ref_choisi, n_ref_lies = n_ref_choisi) %>%
#     #   left_join(temp) %>%
#     #   unique()
#     # 
#     
#   } else {
#     df_ref_session = data.frame(ref_choisi = "all_data", n_ref_choisi = n, ref_lies = "all_data", n_ref_lies = n, protocole = protoc) %>%
#       left_join(dt_oso_valid) %>%
#       select(protocole, ref_choisi, ref_lies, n_ref_lies, n_ref_choisi, session_id) %>%
#       unique()
#   }
#   
#   return(df_ref_session)
#   
# }
