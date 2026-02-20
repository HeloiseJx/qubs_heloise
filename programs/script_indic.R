# INDICATEURS QUBS
# Sur ce diagramme de Sankey, le rectangle de gauche correspond au participant_id.
# 
# A faire :
#   
#   -   Donut régime alimentaire
# 
# Insérer les photos des espèces / des dernières observations ?




# INDICATEURS D'APPROPRIATION ----

## PARTICIPATION ANNUELLE 
# df complet avec les taxons non validés
synthese_participation_an <- dt_qubs_nonvalid %>%
  group_by(year) %>%
  reframe(nb_sites = n_distinct(site_id),
          nb_participants = n_distinct(participant_id),
          nb_sessions = n_distinct(session_id),
          nb_sessions_vides = sum(presence_organisme == 0), 
          nb_taxon = n_distinct(na.omit(taxon))) %>%
  mutate(richesse_cum = cumsum(nb_taxon)) 

nb_tot_participation = n_distinct(dt_qubs_nonvalid$session_id)
  


### Richesse cumulée en fonction du nombre de sessions----  



# TOUT PROTOCOLE CONFONDU
# on enlève les taxons non validés
func_riches_cum_tt_protoc = function(dt_qubs, titre = NULL) {
  # richesse spécifique en fonction du nombre de parties, avec la ligne de jauge
  #df qui nous donne pour chaque date le nombre de parties cumulées à ce jour localement
  df_nb_partie_cum = dt_qubs %>%
    select(date_debut, session_id) %>% # la date indique-t-elle bien les jours ?
    unique() %>%
    arrange(date_debut) %>%
    mutate(nb_session = row_number()) %>%
    select(-session_id) %>%
    group_by(date_debut) %>%
    summarize(nb_partie = max(nb_session)) %>% 
    ungroup() 
    
  # on enlève les taxons non validés  
  plot_richesse_cum_partie <- dt_qubs %>%
    # calcul de la première date d'observation de l'espèce
    select(session_id, taxon, date_debut) %>%
    distinct() %>%
    group_by(taxon) %>%
    summarize(first_obs = min(date_debut)) %>% 
    ungroup() %>%
    na.omit() %>%
    rename(date_debut = first_obs) %>%
    # jointure avec la table liant date et nombre cumulé de parties jouées 
    left_join(df_nb_partie_cum) %>% 
    # calcul de la somme cumulée du nombre d'espèces en fonction du nombre de parties 
    # jouées, en conaissant les first_obs des espèces
    group_by(nb_partie) %>%
    summarize(nb_sp = n_distinct(taxon)) %>%
    ungroup() %>%
    mutate(richesse_cum = cumsum(nb_sp)) %>%  
    # graphe
    ggplot(aes(x = nb_partie, y = richesse_cum, group = 1)) +
    geom_line(linewidth = 0.8) +
    labs(x = "nombre de parties", 
         y = "richesse en espèces cumulée", 
         title = titre) +
    # # ligne du nombre d'espèces de la liste
    # geom_hline(yintercept = 30, linetype = "dashed", color = "purple", linewidth = 0.8) +
    # geom_hline(yintercept = 15, linetype = "dashed", color = "lightblue", linewidth = 0.8) +
    # annotate("text", x = 2, y = 15, label = "50% des espèces de\nla liste observées", hjust = 0, size = 3, color = "lightblue") +
    theme_minimal()
  
  return(plot_richesse_cum_partie)

}



# UNE COURBE DE RICHESSE CUM PAR PROTOCOLE 
func_riches_cum_par_protoc = function(dt_q, titre = NULL) {
  # richesse spécifique en fonction du nombre de parties, avec la ligne de jauge
  
  #df qui nous donne pour chaque date le nombre de parties cumulées à ce jour localement, 
  # par protocole
  df_nb_partie_cum <- dt_q %>%
    select(date_debut, session_id, protocole) %>%
    distinct() %>%
    group_by(protocole) %>%
    arrange(date_debut, .by_group = TRUE) %>%
    mutate(nb_partie = row_number() ) %>%  
    ungroup() %>%
    select(-session_id) %>%
    unique()
  
  plot_richesse_cum_partie <- dt_q %>%
    select(session_id, taxon, date_debut, protocole) %>%
    distinct() %>%
    group_by(taxon, protocole) %>%
    summarise(first_obs = min(date_debut)) %>%
    rename(date_debut = first_obs) %>%
    left_join(df_nb_partie_cum) %>%
    ungroup() %>%
    group_by(taxon, protocole) %>%
    reframe(nb_partie = max(nb_partie)) %>%
    ungroup() %>%
    group_by(nb_partie, protocole) %>%
    summarise(nb_sp = n_distinct(taxon)) %>%
    ungroup() %>%
    arrange(protocole, nb_partie) %>%
    group_by(protocole) %>%
    mutate(richesse_cum = cumsum(nb_sp)) %>%
    ungroup() %>%
    ggplot(aes(x = nb_partie, y = richesse_cum, color = protocole)) +
    geom_line(linewidth = 0.8) +
    labs(
      x = "Nombre de parties", 
      y = "Richesse en espèces cumulée",
      color = "Protocole",
      title = titre
    ) +
    # geom_hline(yintercept = 30, linetype = "dashed", color = "purple", linewidth = 0.8) +
    # geom_hline(yintercept = 15, linetype = "dashed", color = "lightblue", linewidth = 0.8) +
    # annotate(
    #   "text", x = 2, y = 15,
    #   label = "50% des espèces de\nla liste observées",
    #   hjust = 0, size = 3, color = "lightblue"
    # ) +
    theme_minimal()
  
  return(plot_richesse_cum_partie)
  
}


# RICHESSE CUMULEE EN ESPECES A L'ECHELLE NATIONALE
func_riches_cum_national = function() {
  # richesse spécifique en fonction du nombre de parties, avec la ligne de jauge
  
  #df qui nous donne pour chaque date le nombre de parties cumulées à ce jour localement, 
  # par protocole
  df_nb_partie_cum <- dt_qubs_national %>%
    select(date_debut, session_id, protocole) %>%
    distinct() %>%
    group_by(protocole) %>%
    arrange(date_debut, .by_group = TRUE) %>%
    mutate(nb_session = row_number() ) %>%  
    ungroup() %>%
    group_by(date_debut) %>%
    summarise(nb_partie = max(nb_session), .groups = "drop")
  
  
  plot_richesse_cum_partie <- dt_qubs_national %>%
    select(session_id, espece, date_debut, protocole) %>%
    distinct() %>%
    group_by(espece, protocole) %>%
    summarise(first_obs = min(date_debut), .groups = "drop") %>%
    rename(date = first_obs) %>%
    left_join(df_nb_partie_cum, by = c("date" = "date_debut", "protocole")) %>%
    group_by(nb_partie, protocole) %>%
    summarise(nb_sp = n_distinct(espece), .groups = "drop") %>%
    arrange(protocole, nb_partie) %>%
    group_by(protocole) %>%
    mutate(richesse_cum = cumsum(nb_sp)) %>%
    ungroup() %>%
    ggplot(aes(x = nb_partie, y = richesse_cum, color = protocole)) +
    geom_line(linewidth = 0.8) +
    labs(
      x = "Nombre de parties", 
      y = "Richesse en espèces cumulée",
      color = "Protocole"
    ) +
    # geom_hline(yintercept = 30, linetype = "dashed", color = "purple", linewidth = 0.8) +
    # geom_hline(yintercept = 15, linetype = "dashed", color = "lightblue", linewidth = 0.8) +
    # annotate(
    #   "text", x = 2, y = 15,
    #   label = "50% des espèces de\nla liste observées",
    #   hjust = 0, size = 3, color = "lightblue"
    # ) +
    theme_minimal()  

}  



## RESUME DE LA PARTICIPATION
synthese_participation_qubs <- dt_qubs_nonvalid %>%
  group_by(protocole) %>%
  reframe(nb_sites = n_distinct(site_id),
          nb_participants = n_distinct(participant_id),
          nb_sessions = n_distinct(session_id),
          nb_sessions_vides = sum(presence_organisme == 0)) 






# nombre total de participants
nb_tot_participants = n_distinct(dt_qubs_nonvalid$participant_id)



# fonction produisant les stats de participation aux protocoles ANNUELLEMENT
func_df_participation = function() {
  stats_participation <- dt_qubs_nonvalid %>%
    group_by(year) %>%
    summarise(nb_participants = n_distinct(participant_id),
              nb_collections = n_distinct(session_id))
  # ajouter le nombre de nouveaux participants
  nouveaux_participants <- dt_qubs_nonvalid %>%
    group_by(participant_id) %>%
    summarise(first_participation = min(unique(year))) %>%
    ungroup() %>%
    group_by(first_participation) %>% 
    summarise(nouveaux_participants = n()) %>%
    rename(year = first_participation)
  # jointure
  stats_participation <- left_join(stats_participation, 
                                   nouveaux_participants, 
                                   by = "year") %>%
    mutate(nouveaux_participants = case_when(is.na(nouveaux_participants) ~ 0, 
                                             TRUE ~ nouveaux_participants))

  
  # ajouter infos sur nb de collections moyen/participant, ainsi que nb de participations uniques 
  # et "grosse participants" (gros participant = >40 collections dans l'année)
  moyenne_participation <- dt_qubs_nonvalid %>%
    group_by(year, participant_id) %>%
    summarise(nb_collections = n_distinct(session_id)) %>%
    group_by(year) %>%
    summarise(nb_collections_moyen = median(nb_collections),
              participations_uniques = sum(nb_collections == 1),
              gros_participants = sum(nb_collections > 20)
    )
  
  stats_participation <- left_join(stats_participation,
                                   moyenne_participation,
                                   by = "year")
  stats_participation[is.na(stats_participation)] = 0
  
  return(stats_participation)
  
}


stats_participation = func_df_participation()

plot_fidel_participant = ggplot(stats_participation, aes(x = factor(year), y = nb_participants, fill = "Anciens participants")) +
  geom_col() +
  geom_col(aes(y = nouveaux_participants, fill = "Nouveaux participants")) +
  theme_minimal() +
  xlab("Année") +
  ylab("Nombre de participants") +
  scale_fill_manual(name = "Catégorie", values = c("Anciens participants" = "#009ef8", 
                                                   "Nouveaux participants" = "orange")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# nombre de gros participants total
df_gros_participants <- dt_qubs %>%
  group_by(participant_id) %>%
  summarise(nb_collections = n_distinct(session_id)) %>%
  summarise(gros_participants = sum(nb_collections > 20))

nb_gros_participants = sum(df_gros_participants$gros_participants)

# activité des participants : nombre de participation par participant
# Calculer les participations et déterminer le max
dt_participation <- dt_qubs_nonvalid %>% 
  group_by(participant_id) %>%
  summarize(nb_participation = n_distinct(session_id), 
            nb_protocole = n_distinct(protocole)) %>%
  ungroup() %>%
  mutate(nb_participation = as.character(nb_participation)) %>%  # Convertir en caractère avant de modifier
  mutate(nb_participation = ifelse(as.numeric(nb_participation) > 8, "9 et plus", nb_participation)) %>% 
  mutate(nb_participation = as.factor(nb_participation))  # Reconversion en facteur





plot_activite_participants = ggplot(dt_participation, aes(x = factor(nb_participation))) +
  geom_bar(fill = "lightblue", width = 0.5) +  # `geom_bar()` pour les données catégoriques
  labs(title = "Distribution du nombre de participations par utilisateur",
       x = "Nombre de participations",
       y = "Nombre d'utilisateurs") +
  theme_minimal() 


plot_nb_protoc_user = ggplot(dt_participation, aes(x = factor(nb_protocole))) +
  geom_bar(fill = "orange", width = 0.5) +  # `geom_bar()` pour les données catégoriques
  labs(title = "Nombre de protocoles réalisés par utilisateur",
       x = "Nombre de protocoles",
       y = "Nombre de participants", 
       fill = "Nombre de protocoles\ndifférents réalisés" ) +
  theme_minimal() 



# m2 de terre retournée pour vers


## Richesse cumulée en parallèle de la participation ----

# func_graphe_double_axe <- function(df, annee, axe_gauche, label_gauche = NULL, couleur = NULL, axe_droit, label_droit = NULL) {
#   # Normalize axe_droit to fit in the same y-axis range as axe_gauche
#   scale_factor <- max(df[[axe_gauche]], na.rm = TRUE) / max(df[[axe_droit]], na.rm = TRUE)
#   
#   p <- ggplot(df, aes(x = .data[[annee]])) +
#     # Primary y-axis (richesse taxo)
#     geom_line(aes(y = .data[[axe_gauche]] ), size = 1, color = couleur) +
#     geom_point(aes(y = .data[[axe_gauche]] ), color = couleur) +
#     
#     # Secondary y-axis (axe_droit), rescaled
#     geom_line(aes(y = .data[[axe_droit]] * scale_factor), size = 1, color = "black") +
#     geom_point(aes(y = .data[[axe_droit]] * scale_factor), color = "black") +
#     
#     # Labels
#     xlab("Années") +
#     ylab(label_gauche) +
#     
#     # Secondary y-axis for sessions
#     scale_y_continuous(
#       limits = c(0, max(df[[axe_gauche]], na.rm = TRUE)),  # Primary y-axis limit
#       sec.axis = sec_axis(~ . / scale_factor, name = label_droit)  # Reverse transformation
#     ) +
#     
#     # Theme
#     theme_minimal() +
#     scale_x_continuous(breaks = seq(min(df[[annee]], na.rm = TRUE), 
#                                     max(df[[annee]], na.rm = TRUE), by = 1)) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   
#   return(p)
# }


# MODIFIER LA FONCTION CI-DESSUS POUR AVOIR DES TITRES D'AXES QUI SOIENT PLUS ADAPTABLES
# plot_participations_richesse = func_graphe_double_axe(synthese_participation_an, 
#                                                       "year", 
#                                                       "richesse_cum",  
#                                                       label_gauche = "richesse cumulée en taxons (en vert)", 
#                                                       couleur = "lightgreen", 
#                                                       "nb_sessions", 
#                                                       label_droit = "nb de collections")




# plot_richesse_cumul = synthese_participation_an

# représentativité des données dans le référentiel ----

func_poids_radar = function() {
  library(fmsb)
  dt_loc = dt_qubs_nonvalid %>% 
    group_by(protocole) %>%
    reframe(nb_sess_loc = n_distinct(session_id)) %>%
    ungroup()
  
  dt_combi = dt_ref %>%
    group_by(protocole) %>%
    reframe(nb_sess_ref = n_distinct(session_id)) %>%
    ungroup() %>%
    left_join(dt_loc) %>%
    mutate(nb_sess_ref =  replace_na(nb_sess_ref, 0),
           nb_sess_loc =  replace_na(nb_sess_loc, 0),
           poids = round(nb_sess_loc/nb_sess_ref *100)) %>%
    select(-nb_sess_ref, -nb_sess_loc) %>%
    t() %>% 
    as.data.frame()
  
  
  colnames(dt_combi) = dt_combi[1,]
  dt_combi = dt_combi[-1,]
  
  dt_combi = dt_combi %>%
    mutate(across(everything(), as.numeric)) # on passe tout en numeric
  
  dt_combi <- rbind(rep(100,4) , rep(0,4) , dt_combi)# ajout des max et min de valeurs pour le diag radar
  

  
  
  radarchart(dt_combi, 
                 pfcol = rgb(0.2,0.5,0.5,0.4),
                 pcol = rgb(0.2,0.5,0.5,0.9), 
                 caxislabels = seq(0,100,25), 
                 cglcol="darkgrey",
                 axistype = 1,
                 axislabcol="darkgrey", 
                 title = "poids des données locales dans les\n données du référentiel")
  

}




# INDICATEURS DE BIODIV ----

## tableau resumé des observations qubs ----
synthese_obs_qubs <- dt_qubs_nonvalid %>%
  group_by(protocole) %>%
  reframe(nb_taxons_qubs = n_distinct(na.omit(taxon)),
          nb_taxons_identifies = sum(!is.na(taxon)), # identifications de plusieurs fois le même taxon, 
          # ici mesure de activité de capacité de reconnaissance des participants presque
          nb_taxons_non_identifies = sum(is.na(taxon) & presence_organisme == 1),
          `%_valides` = round(sum(taxon_valide)/sum(presence_organisme == 1)*100, 2)) %>%
  mutate(nb_photos = NA) # initialiser la colonne nb de photo

nb_tot_tax = sum(synthese_obs_qubs$nb_taxons_qubs)
nb_tot_obs = sum(synthese_obs_qubs$nb_taxons_identifies) + sum(synthese_obs_qubs$nb_taxons_non_identifies)
pourc_tax_nonidentif = round(sum(synthese_obs_qubs$nb_taxons_non_identifies) / sum(synthese_obs_qubs$nb_taxons_identifies) *100)

## Top taxons validés -----

# Le tableau ci-dessous fournit, pour chaque taxon de la clé Qubs et pour chaque protocole, le nombre d'identifications validées. Petites précisions :
# 
# -   Si le chiffre est égal à zéro : le taxon a été identifié dans le cadre du protocole mais n'a pas encore été validé
# 
# -   Si la case est vide (NA) : le taxon n'a pas été identifié dans le cadre du protocole


## TOP TAXON
# on faire un classement des taxons les plus observés de manière vérifiée
top_taxons_valides <- dt_qubs %>% 
  group_by(taxon, protocole) %>%
  summarise(nb_valide = sum(taxon_valide)) %>% 
  na.omit() %>%
  filter(nb_valide != 0) 

top3 <- top_taxons_valides %>%
  group_by(protocole) %>%
  arrange(desc(nb_valide)) %>%
  mutate(rang = row_number()) %>%  # Create a rank index within each protocol
  slice_head(n = 3) %>%
  ungroup() %>%
  select(protocole, rang, taxon) %>%
  pivot_wider(names_from = protocole, values_from = taxon) %>%
  select(-rang)

top_taxons_valides_abond <- dt_qubs %>% 
  mutate(abondance = as.numeric(abondance)) %>%
  group_by(taxon, protocole) %>%
  summarise(nb_valide = sum(abondance)) %>% 
  na.omit() %>%
  filter(nb_valide != 0) 

top3_abond <- top_taxons_valides_abond %>%
  group_by(protocole) %>%
  arrange(desc(nb_valide)) %>%
  mutate(rang = row_number()) %>%  # Create a rank index within each protocol
  slice_head(n = 3) %>%
  ungroup() %>%
  select(protocole, rang, taxon) %>%
  pivot_wider(names_from = protocole, values_from = taxon) %>%
  select(-rang)



## FREQUENCES D'ESPECES ----

# fréquences par especes ; especes valides
func_freq_sp = function(dt_q) {
  ntot = n_distinct(dt_q$session_id)
  
  df_freq_sp <- dt_q %>% 
  group_by(taxon, protocole) %>%
  summarise(freq = 100 * n_distinct(session_id) / ntot) %>%
  ungroup() %>%
  na.omit()
  
  return(df_freq_sp)
} 

df_freq_local = func_freq_sp(dt_qubs) %>% 
  mutate(echelle = "local") %>%
  group_by(protocole) %>% 
  arrange(desc(freq)) %>%
  slice_head(n = 3) %>%
  ungroup()


df_freq_ref = func_freq_sp(dt_ref_valide) %>%
  mutate(echelle = "ref") %>%
  # on récupere dans les donénes du référentiel les couples protocole/espece correspondant aux données locales
  # forcément présent dans les données du référentiel comme les données locales en font partie.
  filter(paste0(protocole, taxon) %in% paste0(df_freq_local$protocole, df_freq_local$taxon)) 


dt_combi = rbind(df_freq_local, df_freq_ref)

# plot des frequences par especes, par protocoles et à différentes échelles
func_freq_combi = function() {
  plotte = NULL
  
  for (protoc in unique(dt_combi$protocole)) {
    # print(protoc)
    
    plot_freq = dt_combi  %>%
      filter(protocole == protoc) %>%
      ggplot(aes(x = reorder(taxon, freq), y = freq, fill = echelle)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Espèce", 
           y = "Fréquence (%)", 
           fill = "Echelle",
           title = protoc) +
      theme_minimal() +
      coord_flip() 
    
    plotte = plotte / plot_freq
    
  }
  
  return(plotte)
}

"red"


## POURCENTAGE D'ESPECES en abondance --> frequence d'abondance ----
# fréquences par especes ; especes valides
func_pourc_sp = function(dt_q) {
  ntot = sum(dt_q$abondance)
  
  df_pourc_sp <- dt_q %>% 
    group_by(taxon, protocole) %>%
    summarise(pourc = 100 * sum(abondance) / ntot) %>%
    ungroup() %>%
    na.omit()
  
  return(df_pourc_sp)
} 

df_pourc_local = func_pourc_sp(dt_qubs) %>% 
  mutate(echelle = "local") %>%
  group_by(protocole) %>% 
  arrange(desc(pourc)) %>%
  slice_head(n = 3) %>%
  ungroup()


df_pourc_ref = func_pourc_sp(dt_ref_valide) %>%
  mutate(echelle = "ref") %>%
  # on récupere dans les donénes du référentiel les couples protocole/espece correspondant aux données locales
  # forcément présent dans les données du référentiel comme les données locales en font partie.
  filter(paste0(protocole, taxon) %in% paste0(df_pourc_local$protocole, df_pourc_local$taxon)) 


dt_combi_pourc = rbind(df_pourc_local, df_pourc_ref)

# plot des frequences par especes, par protocoles et à différentes échelles
func_pourc_combi = function() {
  plotte = NULL
  
  for (protoc in unique(dt_combi$protocole)) {
    # print(protoc)
    
    plot_pourc = dt_combi_pourc  %>%
      filter(protocole == protoc) %>%
      ggplot(aes(x = reorder(taxon, pourc), y = pourc, fill = echelle)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Espèce", 
           y = "Pourcentage d'individus (%)", 
           fill = "Echelle",
           title = protoc) +
      theme_minimal() +
      coord_flip() 
    
    plotte = plotte / plot_pourc
    
  }
  
  return(plotte)
}






"red"


func_nb_tax_moy = function(dt_q) {
  # calcul du nombre moyen de taxon par session par protocole
  df_moy_tax_session = dt_q %>%
    # filtre sur la quatité de données des protocoles
    group_by(protocole) %>%
    reframe(nb_session = n_distinct(session_id)) %>%
    ungroup() %>%
    # filter(nb_session > 5) %>% # on ne conserve les protocoles s'il y a au moins 5 parties de ce protocole
    # calcul de moyennes
    left_join(dt_q) %>%
    select(session_id, protocole, observation_id, abondance) %>%
    unique() %>%
    group_by(session_id, protocole) %>%
    reframe(nb_tax_sess = case_when(is.na(observation_id) ~ 0,
                                      TRUE ~ n_distinct(observation_id)),
              nb_indiv_sess = case_when(is.na(observation_id) ~ 0,
                                        TRUE ~ sum(abondance))) %>%
    ungroup() %>%
    group_by(protocole) %>%
    reframe(moy_nb_tax = round(mean(nb_tax_sess), 1),
              abond_moy = round(mean(nb_indiv_sess), 1), 
              nb_session = n_distinct(session_id)) %>%
    ungroup()
  
  return(df_moy_tax_session)
}


# répartition des abondances ----
# dt_abond_sess = 
# a = dt_qubs_total_valide %>%
#   group_by(session_id, protocole) %>%
#   summarize(abond_sess = sum(abondance)) %>%
#   ungroup() 
# for (p in unique(a$protocole)) {
#   plot = a %>% 
#     filter(protocole == p) %>%
#     ggplot(aes(x = abond_sess)) +
#     geom_bar(position = "dodge") +
#     ggtitle(p)
#   print(plot)
# }






  
# valeurs de réference pour le nombre taxon moyen par collection : 
# pour l'instant on prend en référentiel l'échelle nathttp://127.0.0.1:10937/graphics/82bc507b-713e-4b89-b06b-8e7579103eef.pngionale
df_moy_tax_session_ref = dt_ref %>%# filtre sur la quatité de données des protocoles
  group_by(protocole) %>%
  reframe(nb_session = n_distinct(session_id)) %>%
  ungroup() %>%
  filter(nb_session > 5) %>% # on ne conserve les protocoles s'il y a au moins 5 parties de ce protocole
  # calcul de moyennes
  left_join(dt_ref) %>%
  select(session_id, protocole, observation_id) %>%
  unique() %>%
  group_by(session_id, protocole) %>%
  summarize(nb_tax_sess = n_distinct(observation_id)) %>%
  ungroup() %>%
  group_by(protocole) %>%
  summarize(moy_nb_tax = round(mean(nb_tax_sess), 1),
            nb_session = n_distinct(session_id)) %>%
  ungroup() 



   




## représentativité taxonomique ----

# Distribution des obs selon le regroupement 2a.
regroupement_2a <- dt_qubs %>% 
  filter(!is.na(Regroupement2a)) %>%
  group_by(Regroupement2a) %>%
  reframe(nb_indiv = sum(abondance)) %>%
  arrange(desc(nb_indiv)) %>%
  mutate(Regroupement2a = factor(Regroupement2a, levels = Regroupement2a)) %>%
  mutate(prop = nb_indiv/sum(nb_indiv), # Compute percentages
         ymax = cumsum(prop), # Compute the cumulative percentages (top of each rectangle)
         ymin = c(0, head(ymax, n=-1))) # Compute the bottom of each rectangle

# Donut plot pour regroupement 2a
donut_taxo <- ggplot(regroupement_2a, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Regroupement2a)) +
  geom_rect(color = "black") +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4))  + # Try to remove that to see how to make a pie chart
  scale_fill_manual(values = palette_camemberts, 
                    breaks = sort(as.character(regroupement_2a$Regroupement2a))) +
  labs(fill = "Groupes d'espèces") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
# donut_taxo
# ggsave(here::here("reporting", "donut_taxo_qubs.tiff"), donut_taxo)










## phenologie ----


# dt_pheno = 
  # dt_qubs_total %>%
  # mutate(date_pheno = month(as.Date(date_debut))) %>%
  # filter(taxon_valide == 1) %>%
  # group_by(protocole, session_id, date_pheno) %>%
  # reframe(nb_tax = n_distinct(taxon),
  #         abond = length(taxon)) %>%
  # ungroup() %>%
  # group_by(date_pheno, protocole) %>%
  # reframe(moy_riches = mean(nb_tax),
  #         moy_abond = mean(abond)) %>%
  # ungroup() %>%
  # ggplot(aes(x = date_pheno)) +
  # geom_line(aes(y = moy_abond, color = protocole), linewidth = 1) +
  # geom_line(aes(y = moy_riches, color = protocole), linetype = 2, linewidth = 1) +
  # theme_minimal()




# SENSIBILISATION / COMMUNAUTE ----
# 
# func_pourc_tax_valide = function(){
#   pourc_taxon_valide = round((sum(dt_qubs_nonvalid$taxon_valide) / nrow(dt_qubs_nonvalid)*100), 1)
#   return(pourc_taxon_valide)
# }
# 
# pourc_taxon_valide = func_pourc_tax_valide()
# 
# 
# # recuperer les donnees des interactions sociales via la table qubs.comments
# query <- read_sql_query(here::here("sql", "table_comments_qubs.sql"))
# dt_comments <- import_from_mosaic(query,
#                                   database_name = "qubs",
#                                   force_UTF8 = TRUE)
# 
# #ajouter le protocole aux donnees sociales
# ## melt le tableau dt_qubs sur observation_id et session_id 
# ## pour obtenir une colonne resource_id equivalente à celle de la table qubs.comments
# dt_comments <- dt_comments %>% left_join(reshape2::melt(dt_qubs %>% 
#                                                           select(observation_id, session_id, protocole) %>% 
#                                                           rename(observation = observation_id,
#                                                                  participation = session_id) %>%
#                                                           distinct(), id = c("protocole") %>% 
#                                                           na.omit()) %>%
#                                            #rename columns to match the structure of dt_comments
#                                            rename(resource_type = variable,
#                                                   resource_id = value),
#                                          #jointure sur resource_id et resource_type
#                                          by = c("resource_id", "resource_type"))
# 
# 
# # on récupère les données d'interaction entre participants pour des participants 
# # qui ont participé sur le site considéré
# dt_comments_site = dt_comments %>% 
#   filter(user_id %in% dt_qubs$participant_id) 
# 
# # nombre de valideurs sur le site
# nb_valideurs_tot = n_distinct(dt_comments_site$user_id[dt_comments_site$comment_type == "vote"])
# 
# 
# 
# # Ce tableau donne pour chaque protocole le **nombre de participant.es** ayant
# # fait des validations, des suggestions, des ré-identifications ou des commentaires 
# # sur participation
# 
# 
# ## tableau recapitulatif interactions sociales PAR PROTOCOLE : nombre de users
# # on ne considère là encore que les interactions produites sur le site
# # interaction_comm = dt_comments_site %>% 
# #   select(protocole, comment_type, user_id) %>% 
# #   na.omit() %>% 
# #   group_by(protocole) %>%
# #   reframe(nb_validations = n_distinct(user_id[comment_type == 'vote']),
# #           nb_suggestions = n_distinct(user_id[comment_type == "suggestion"]),
# #           nb_reidentifications = n_distinct(user_id[comment_type == "reidentification"]),
# #           nb_comm_participations = n_distinct(user_id[comment_type == "comment"]))
# 
# ## tableau recapitulatif interactions sociales POUR TOUS LES PROTOCOLES CONFONDUS
# interaction_comm_global = dt_comments_site %>% 
#   select(protocole, comment_type, user_id, comment_id) %>% 
#   na.omit() %>% 
#   reframe(nb_validations = n_distinct(comment_id[comment_type == 'vote']),
#           nb_suggestions = n_distinct(comment_id[comment_type == "suggestion"]),
#           nb_reidentifications = n_distinct(comment_id[comment_type == "reidentification"]),
#           nb_comm_participations = n_distinct(comment_id[comment_type == "comment"]))
# 
# 
# 
# 
# 
# 
# # graphe Circlemarkers imbriqués : communauté Qubs
# 
# library(igraph)
# library(ggraph)
# 
# func_cercle_commu = function(){
#   
#   hierarchy_df <- interaction_comm_global %>% 
#     pivot_longer(cols = c(nb_validations,
#                           nb_suggestions, 
#                           nb_reidentifications, 
#                           nb_comm_participations), 
#                  names_to = 'categorie', 
#                  values_to = 'valeur') %>%
#     mutate(qubseur = "valideurs") %>%
#     select(qubseur, everything())  %>%
#     filter(valeur != 0)
#   
#   # 2. Construire un tableau avec parent/enfant
#   # df renseignant sur les imbrications : qui est parent (from), qui est enfant (to)
#   edg = tibble(from = hierarchy_df$qubseur, to = hierarchy_df$categorie) 
#   
#   # df renseignant sur les vertices, soit les éléments qui composent ce réseau d'interaction, 
#   # indépendamment de leurs imbrications : name = nom, et size : valeur de l'élément
#   vertic = tibble(name = "valideurs", size = nb_valideurs_tot) %>%
#     bind_rows(tibble(name = hierarchy_df$categorie, size = hierarchy_df$valeur)) 
#   
#   
#   # 3. Créer le graphe
#   mygraph <- graph_from_data_frame(edg, vertices = vertic)
#   
#   
#   # 4. Visualisation
#   titre_graphe = paste0(vertic$name[1], " : ", vertic$size[1])
#   graph = ggraph(mygraph, layout = 'circlepack', weight=size ) + 
#     geom_node_circle(aes(fill = depth)) +
#     geom_node_label( aes(label = paste0(name, " : ", size), filter = leaf)) +
#     # geom_node_text( aes(label = paste0(name, " : ", size), filter =! leaf), position = "identity") + # info contenue dans le titre en dehors du cercle
#     theme_void() + 
#     labs(title = titre_graphe ) +
#     theme(legend.position="FALSE") + 
#     scale_fill_viridis()
#   
#   return(graph)
#   
# }
# 
# 
# 
# # Le même graphe que précédemment mais un cercle correspond à un participant
# func_cercle_par_participant <- function() {
#   
#   # 1. Transforme le tableau long : une ligne = une interaction d'un participant dans une catégorie
#   hierarchy_df <- interaction_comm_global |>
#     pivot_longer(
#       cols = c(nb_validations, nb_suggestions, nb_reidentifications, nb_comm_participations),
#       names_to = "categorie",
#       values_to = "valeur"
#     ) |>
#     filter(valeur > 0) |>
#     mutate(
#       qubseur = as.character(dt_qubs$participant_id),
#       categorie_label = recode(
#         categorie,
#         nb_validations         = "Nombre de validations",
#         nb_suggestions         = "Nombre de suggestions",
#         nb_reidentifications   = "Nombre de réidentifications",
#         nb_comm_participations = "Nombre de commentaires"
#       ),
#       node_id = paste(qubseur, categorie, sep = "_")  # identifiant unique pour le graphe
#     )
#   
#   
#   # 2. Relations entre parent (participant) et enfant (catégorie)
#   # relations hiérarchiques
#   edg <- tibble(from = hierarchy_df$qubseur, to = hierarchy_df$node_id)
#   
#   #3. données des nœuds
#   vertic <- tibble(
#     name = unique(hierarchy_df$qubseur),  # participants
#     size = NA,
#     label = unique(hierarchy_df$qubseur)  # pas affiché dans les labels des feuilles
#   ) |>
#     bind_rows(
#       hierarchy_df |>
#         select(name = node_id, size = valeur, label = categorie_label)
#     )
#   
#   mygraph <- graph_from_data_frame(edg, vertices = vertic)
#   
#   #4. graphe hierarchique
#   ggraph(mygraph, layout = 'circlepack', weight = size) +
#     geom_node_circle(aes(fill = depth)) +
#     geom_node_label(
#       aes(label = label, filter = leaf),
#       size = 3, repel = TRUE
#     ) +
#     theme_void() +
#     labs(title = "Interactions par participant") +
#     theme(legend.position = "none") +
#     scale_fill_viridis_c()
#   
#   return(graph)
# }  
# 
# 
# ## Sankey diag communauté ----
# func_plot_commu_sankey = function() {
#   library(networkD3)
#   
#   dt_commu_sankey = dt_comments_site %>%
#     mutate(data_concerne = case_when(
#       resource_id %in% dt_qubs_nonvalid$session_id ~ "donnees du site",
#       TRUE ~ "donnees à l'exterieur du site"
#     )) %>%
#     select(-protocole) %>%
#     rename(observation_id = resource_id) %>%
#     left_join(dt_qubs_total[, c("observation_id", "protocole") ])
#   
#   links = data.frame(
#     source = c(dt_commu_sankey$user_id, dt_commu_sankey$comment_type, dt_commu_sankey$data_concerne),
#     target = c(dt_commu_sankey$comment_type, dt_commu_sankey$data_concerne, dt_commu_sankey$protocole)
#   ) %>% 
#     group_by(source, target) %>%
#     reframe(value = n()) %>%
#     ungroup()
#   
#   
#   nodes <- data.frame(
#     name=c(as.character(links$source), 
#            as.character(links$target)) %>% unique()
#   )
#   
#   links$IDsource <- match(links$source, nodes$name)-1 
#   links$IDtarget <- match(links$target, nodes$name)-1
#   
#   p <- sankeyNetwork(Links = links, Nodes = nodes,
#                      Source = "IDsource", Target = "IDtarget",
#                      Value = "value", NodeID = "name", 
#                      sinksRight=FALSE, fontSize = 18)
#   
#   
#   return(p)
# }
# 
# 
# 
# 
# 
# 

# CARTES ----

# coloré par protocole
dt_carto_qubs = coord_qubs_local %>%
  left_join(dt_qubs_nonvalid)

pal = colorFactor(
  palette = c("#5495CFFF", "#F5AF4DFF", "#DB4743FF", "#7C873EFF"),
  domain = dt_carto_qubs$protocole
)

carte_collections <- leaflet(dt_carto_qubs) %>% 
  addTiles() %>% 
  addPolygons(data = emprise_locale, fillOpacity = 0) %>% # popup ne fonctionne pas
  addCircleMarkers(radius = 7,
                   fillOpacity = 1, 
                   stroke = F, 
                   color = ~ pal(protocole)) %>%
  addLegend(values = ~protocole, pal = pal, position = "topright")





  
