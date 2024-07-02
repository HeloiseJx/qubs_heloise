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
    json_value(`p`.`data`,
    '$.dateDebut') AS `date_debut`,
    json_value(`p`.`data`,
    '$.heureDebutCollecte') AS `heure_debut`,
    `t2`.`title` AS `meteo`,
    `t3`.`title` AS `exposition`,
    replace(replace(replace(replace(replace(replace(json_extract(`p`.`data`,
    '$.typesVegetation'),
    '[',
    ''),
    '"',
    ''),
    '","',
    ';'),
    'qubs.types-vegetation.',
    ''),
    ']',
    ''),
    '-',
    ' ') AS `vegetations`,
    cast(replace(replace(replace(json_value(`p`.`data`, '$.typesVegetationAutrePrecision'), '\\r', ' '), '\\n', ' '), '\\t', ' ') as char charset utf8mb3) AS `vegetations_autre`,
    json_value(`p`.`data`,
    '$.zoneCultive') AS `zone_cultivee`,
    replace(replace(replace(replace(replace(replace(json_extract(`p`.`data`,
    '$.couverturesSol'),
    '[',
    ''),
    '"',
    ''),
    '","',
    ';'),
    'qubs.couvertures-sol.',
    ''),
    ']',
    ''),
    '-',
    ' ') AS `couvertures_sol`,
    cast(replace(replace(replace(json_value(`p`.`data`, '$.couverturesSolAutrePrecision'), '\\r', ' '), '\\n', ' '), '\\t', ' ') as char charset utf8mb3) AS `couverture_sol_autre`,
    replace(replace(replace(replace(replace(replace(json_extract(`p`.`data`,
    '$.typesResidusVegetaux'),
    '[',
    ''),
    '"',
    ''),
    '","',
    ';'),
    'qubs.types-residus-vegetaux.',
    ''),
    ']',
    ''),
    '-',
    ' ') AS `residus_vegetaux`,
    json_value(`p`.`data`,
    '$.presenceOrganisme') AS `presence_organisme`,
    cast(replace(replace(replace(json_value(`p`.`data`, '$.presenceOrganismeFalsePrecision'), '\\r', ' '), '\\n', ' '), '\\t', ' ') as char charset utf8mb3) AS `commentaires_presence_false`,
    cast(replace(replace(replace(json_value(`p`.`data`, '$.commentaire'), '\\r', ' '), '\\n', ' '), '\\t', ' ') as char charset utf8mb3) AS `commentaires_collecte`,
    concat('https://www.qubs.fr/upload/medias/', `me1`.`name`) AS `url_photo_2m`,
    concat('https://www.qubs.fr/upload/medias/', `me2`.`name`) AS `url_photo_installation`,
    concat('https://www.qubs.fr/upload/medias/', `me3`.`name`) AS `url_photo_collecte`,
    `t4`.`title` AS `taxon`,
    if(count(if(`c`.`model` = 'vote' and `c`.`deleted_at` is null, 1, NULL)) >= 3,
    1,
    0) AS `taxon_valide`,
    json_value(`o`.`data`,
    '$.abondance') AS `abondance`,
    json_value(`o`.`data`,
    '$.taxonJeNeSaisPas') AS `taxon_ne_sait_pas`,
    json_value(`o`.`data`,
    '$.taxonPasDansLaListe') AS `taxon_pas_dans_liste`,
    cast(replace(replace(replace(json_value(`o`.`data`, '$.denominationPlusPreciseTruePrecision'), '\\r', ' '), '\\n', ' '), '\\t', ' ') as char charset utf8mb3) AS `taxon_denomination_precision`,
    concat('https://www.qubs.fr/upload/medias/', `me4`.`name`) AS `url_photo_taxon`
from
    ((((((((((((((((((`qubs`.`participations` `p`
join `qubs`.`protocols` `pr` on
    (`pr`.`id` = `p`.`protocol_id`))
join `qubs`.`users` `u` on
    (`u`.`id` = `p`.`user_id`))
join `qubs`.`observation_areas` `s` on
    (`s`.`id` = `p`.`observation_area_id`))
left join `qubs`.`missions` `m` on
    (`m`.`id` = `p`.`mission_id`))
left join `qubs`.`thesaurus_values` `t1` on
    (`t1`.`value` = json_value(`p`.`data`,
    '$.site.type')))
left join `qubs`.`thesaurus_values` `t2` on
    (`t2`.`value` = json_value(`p`.`data`,
    '$.meteo')))
left join `qubs`.`thesaurus_values` `t3` on
    (`t3`.`value` = json_value(`p`.`data`,
    '$.exposition')))
join `qubs`.`participations_medias` `pm1` on
    (`pm1`.`participation_id` = `p`.`id`
        and `pm1`.`relation` = 'photo2Metres'))
left join `qubs`.`medias` `me1` on
    (`me1`.`id` = `pm1`.`media_id`))
join `qubs`.`participations_medias` `pm2` on
    (`pm2`.`participation_id` = `p`.`id`
        and `pm2`.`relation` = 'photoUp'))
left join `qubs`.`medias` `me2` on
    (`me2`.`id` = `pm2`.`media_id`))
left join `qubs`.`participations_medias` `pm3` on
    (`pm3`.`participation_id` = `p`.`id`
        and `pm3`.`relation` = 'photoEnsemble'))
left join `qubs`.`medias` `me3` on
    (`me3`.`id` = `pm3`.`media_id`))
left join `qubs`.`observations` `o` on
    (`o`.`participation_id` = `p`.`id`))
left join `qubs`.`thesaurus_values` `t4` on
    (`t4`.`value` = json_value(`o`.`data`,
    '$.taxon')))
left join `qubs`.`observations_medias` `om1` on
    (`om1`.`observation_id` = `o`.`id`))
left join `qubs`.`medias` `me4` on
    (`me4`.`id` = `om1`.`media_id`))
left join `qubs`.`comments` `c` on
    (`c`.`resource_id` = `o`.`id`
        and `c`.`resource_type` = 'Observation'
        and json_value(`c`.`data`,
        '$.taxon') = json_value(`o`.`data`,
        '$.taxon')))
where
    `p`.`deleted_at` is null
    and `o`.`deleted_at` is null
    and `p`.`protocol_id` = 3
group by
    `p`.`id`,
    `o`.`id`
order by
    `p`.`id`;