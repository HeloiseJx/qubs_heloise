select
    `p`.`id` AS `participation_id`,
    `o`.`id` AS `observation_id`,
    cast(replace(replace(replace(json_value(`p`.`data`, '$.nom'), '\\r', ' '), '\\n', ' '), '\\t', ' ') as char charset utf8mb3) AS `participation_nom`,
    `pr`.`name` AS `protocole`,
    `p`.`mission_id` AS `mission_id`,
    `m`.`name` AS `mission_nom`,
    `p`.`user_id` AS `participant_id`,
    `u`.`username` AS `participant_nom`,
    `p`.`observation_area_id` AS `site_id`,
    json_value(`p`.`data`,
    '$.site.nom') AS `site_nom`,
    st_y(`s`.`geodata`) AS latitude,
    st_x(`s`.`geodata`) AS longitude,
    `s`.`postcode` AS `site_code_postale`,
    `t1`.`title` AS `site_type`,
    cast(replace(replace(replace(json_value(`p`.`data`, '$.site.typeAutrePrecision'), '\\r', ' '), '\\n', ' '), '\\t', ' ') as char charset utf8mb3) AS `site_type_autre`,
    json_value(`p`.`data`,
    '$.site.taille') AS `site_taille`,
    replace(replace(replace(replace(replace(replace(json_extract(`p`.`data`,
    '$.site.compositions'),
    '[',
    ''),
    '"',
    ''),
    '","',
    ';'),
    'qubs.site.compositions.',
    ''),
    ']',
    ''),
    '-',
    ' ') AS `site_compositions`,
    cast(replace(replace(replace(json_value(`p`.`data`, '$.site.compositionsAutrePrecision'), '\\r', ' '), '\\n', ' '), '\\t', ' ') as char charset utf8mb3) AS `site_compositions_autre`,
    replace(replace(replace(replace(replace(replace(json_extract(`p`.`data`,
    '$.site.arrosages'),
    '[',
    ''),
    '"',
    ''),
    '","',
    ';'),
    'qubs.site.arrosages.',
    ''),
    ']',
    ''),
    '-',
    ' ') AS `site_arrosages`,
    replace(replace(replace(replace(replace(replace(json_extract(`p`.`data`,
    '$.site.produitsPhytosanitaires'),
    '[',
    ''),
    '"',
    ''),
    '","',
    ';'),
    'qubs.site.produits-phytosanitaires.',
    ''),
    ']',
    ''),
    '-',
    ' ') AS `site_produits_phyto`,
    replace(replace(replace(replace(replace(replace(json_extract(`p`.`data`,
    '$.site.techniquesEntretien'),
    '[',
    ''),
    '"',
    ''),
    '","',
    ';'),
    'qubs.site.techniques-entretien.',
    ''),
    ']',
    ''),
    '-',
    ' ') AS `site_techniques_entretien`,
    cast(replace(replace(replace(json_value(`p`.`data`, '$.site.techniquesEntretienAutrePrecision'), '\\r', ' '), '\\n', ' '), '\\t', ' ') as char charset utf8mb3) AS `site_techniques_entretien_autre`,
    json_value(`p`.`data`,
    '$.site.anneeDernierChangementTerre') AS `site_anne_changement_terre`,
    `a`.`id` AS `abri_id`,
    cast(replace(replace(replace(json_value(`a`.`data`, '$.nom'), '\\r', ' '), '\\n', ' '), '\\t', ' ') as char charset utf8mb3) AS `abri_nom`,
    `t2`.`title` AS `abri_typ`,
    json_value(`a`.`data`,
    '$.datePose') AS `abri_date_pose`,
    json_value(`a`.`data`,
    '$.dateRetrait') AS `abri_date_retrait`,
    replace(replace(replace(replace(replace(replace(json_extract(`a`.`data`,
    '$.milieux'),
    '[',
    ''),
    '"',
    ''),
    '","',
    ';'),
    'qubs.abri.milieux.',
    ''),
    ']',
    ''),
    '-',
    ' ') AS `abri_milieux`,
    `t3`.`title` AS `abri_exposition`,
    json_value(`p`.`data`,
    '$.date') AS `date`,
    json_value(`p`.`data`,
    '$.heureReleve') AS `heure_releve`,
    json_value(`p`.`data`,
    '$.dateDernierePluie') AS `date_derniere_pluie`,
    json_value(`p`.`data`,
    '$.dateDerniereTonte') AS `date_derniere_tonte`,
    `t4`.`title` AS `temperature`,
    json_value(`p`.`data`,
    '$.presenceOrganisme') AS `presence_organisme`,
    replace(replace(replace(replace(replace(replace(json_extract(`p`.`data`,
    '$.autresEspeces'),
    '[',
    ''),
    '"',
    ''),
    '","',
    ';'),
    'qubs.autres-especes.',
    ''),
    ']',
    ''),
    '-',
    ' ') AS `autres_especes`,
    cast(replace(replace(replace(json_value(`p`.`data`, '$.commentaire'), '\\r', ' '), '\\n', ' '), '\\t', ' ') as char charset utf8mb3) AS `commentaire_releve`,
    concat('https://www.qubs.fr/upload/medias/', `me2`.`name`) AS `url_photo_collecte`,
    concat('https://www.qubs.fr/upload/medias/', `me1`.`name`) AS `url_photo_abri`,
    `t5`.`title` AS `taxon`,
    `t6`.`title` AS `categorie_taxon`,
    if(`stat`.`nb_validation` >= 3,
    1,
    0) AS `taxon_valide`,
    json_value(`o`.`data`,
    '$.abondance') AS `abondance`,
    json_value(`o`.`data`,
    '$.taxonJeNeSaisPas') AS `taxon_ne_sait_pas`,
    json_value(`o`.`data`,
    '$.taxonPasDansLaListe') AS `taxon_pas_dans_liste`,
    cast(replace(replace(replace(json_value(`o`.`data`, '$.denominationPlusPreciseTruePrecision'), '\\r', ' '), '\\n', ' '), '\\t', ' ') as char charset utf8mb3) AS `taxon_denomination_precision`,
    concat('https://www.qubs.fr/upload/medias/', `me3`.`name`) AS `url_photo_taxon_dessus`,
    concat('https://www.qubs.fr/upload/medias/', `me4`.`name`) AS `url_photo_taxon_dessous`,
    concat('https://www.qubs.fr/upload/medias/', `me5`.`name`) AS `url_photo_taxon_cote`
from
    ((((((((((((((((((((((((`qubs`.`participations` `p`
join `qubs`.`protocols` `pr` on
    (`pr`.`id` = `p`.`protocol_id`))
join `qubs`.`users` `u` on
    (`u`.`id` = `p`.`user_id`))
join `qubs`.`observation_areas` `s` on
    (`s`.`id` = `p`.`observation_area_id`))
join `qubs`.`shelters` `a` on
    (`a`.`observation_area_id` = `s`.`id`))
left join `qubs`.`missions` `m` on
    (`m`.`id` = `p`.`mission_id`))
left join `qubs`.`thesaurus_values` `t1` on
    (`t1`.`value` = json_value(`p`.`data`,
    '$.site.type')))
left join `qubs`.`thesaurus_values` `t2` on
    (`t2`.`value` = json_value(`a`.`data`,
    '$.type')))
left join `qubs`.`thesaurus_values` `t3` on
    (`t3`.`value` = json_value(`a`.`data`,
    '$.exposition')))
left join `qubs`.`thesaurus_values` `t4` on
    (`t4`.`value` = json_value(`p`.`data`,
    '$.temperature')))
join `qubs`.`shelters_medias` `am1` on
    (`am1`.`shelter_id` = `a`.`id`
        and `am1`.`relation` = 'photoInstallation'))
left join `qubs`.`medias` `me1` on
    (`me1`.`id` = `am1`.`media_id`))
left join `qubs`.`participations_medias` `pm2` on
    (`pm2`.`participation_id` = `p`.`id`
        and `pm2`.`relation` = 'photoEnsemble'))
left join `qubs`.`medias` `me2` on
    (`me2`.`id` = `pm2`.`media_id`))
left join `qubs`.`observations` `o` on
    (`o`.`participation_id` = `p`.`id`))
left join `qubs`.`thesaurus_values` `t5` on
    (`t5`.`value` = json_value(`o`.`data`,
    '$.taxon')))
left join `qubs`.`thesaurus_values` `t6` on
    (`t6`.`value` = json_value(`o`.`data`,
    '$.taxonCategorie')))
left join `qubs`.`observations_statistics` `stat` on
    (`stat`.`observation_id` = `o`.`id`))
left join `qubs`.`observations_medias` `om1` on
    (`om1`.`observation_id` = `o`.`id`
        and `om1`.`relation` = 'photoDessus'))
left join `qubs`.`medias` `me3` on
    (`me3`.`id` = `om1`.`media_id`))
left join `qubs`.`observations_medias` `om2` on
    (`om2`.`observation_id` = `o`.`id`
        and `om2`.`relation` = 'photoDessous'))
left join `qubs`.`medias` `me4` on
    (`me4`.`id` = `om2`.`media_id`))
left join `qubs`.`observations_medias` `om3` on
    (`om3`.`observation_id` = `o`.`id`
        and `om3`.`relation` = 'photoCote'))
left join `qubs`.`medias` `me5` on
    (`me5`.`id` = `om3`.`media_id`))
left join `qubs`.`comments` `c` on
    (`c`.`resource_id` = `o`.`id`
        and `c`.`resource_type` = 'observation'
        and json_value(`c`.`data`,
        '$.taxon') = json_value(`o`.`data`,
        '$.taxon')))
where
    `p`.`deleted_at` is null
    and `o`.`deleted_at` is null
    and `p`.`protocol_id` = 4
group by
    `p`.`id`,
    `o`.`id`
order by
    `p`.`id`;