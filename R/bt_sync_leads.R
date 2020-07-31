# Roxygen documentation

#' Bastiat, please extract all URLs you find in the following string.


# Function infos and parameters  --------------------------------------------

bt_sync_leads = function(){

  library(gtasql)
  library(gtalibrary)
  library(pool)
  library(RMariaDB)
  library(data.table)
  library(gtabastiat)
  library(xlsx)

  gta_sql_kill_connections()

  database <<- "ricardomain"

  gta_sql_pool_open(db.title=database,
                    db.host = gta_pwd(database)$host,
                    db.name = gta_pwd(database)$name,
                    db.user = gta_pwd(database)$user,
                    db.password = gta_pwd(database)$password,
                    table.prefix = "bt_")



  database <<- "gtamain"

  gta_sql_pool_open(pool.name = "main" ,
                    db.title=database,
                    db.host = gta_pwd(database)$host,
                    db.name = gta_pwd(database)$name,
                    db.user = gta_pwd(database)$user,
                    db.password = gta_pwd(database)$password,
                    table.prefix = "gta_")


  ## (1) sending entries on lift-off states to leads section

  # (1a) select those that are on state 3: 5 and, for those that are in collection, only use starred item.
  new.leads=gta_sql_get_value("SELECT nh.hint_id, acting_agency, registration_date, bid, gjl.jurisdiction_id, jurisdiction_name, un_code, assessment_name,
                                      hint_title, hint_description, url, url_type_name, date_announced, date_implemented, date_removed
                               FROM ( SELECT bhl.hint_id, acting_agency, registration_date
                                      FROM bt_hint_log bhl
                                      WHERE NOT EXISTS (SELECT NULL FROM b221_hint_collection WHERE  b221_hint_collection.hint_id = bhl.hint_id)
                                      AND bhl.hint_state_id IN (3,4,5)
                                      AND bhl.gta_id IS NULL
                                      UNION
                                      SELECT bhl.hint_id, acting_agency, registration_date
                                      FROM bt_hint_log bhl
                                      JOIN b221_collection_star b2cs
                                      ON bhl.hint_id = b2cs.hint_id
                                      WHERE bhl.hint_state_id IN (3,4,5)
                                      AND bhl.gta_id IS NULL
                                      AND b2cs.collection_id NOT IN ( SELECT DISTINCT(collection_id)
                                                                      FROM b221_hint_collection b2hc
                                                                      JOIN bt_hint_log bhl
                                                                      ON b2hc.hint_id=bhl.hint_id
                                                                      WHERE bhl.gta_id IS NOT NULL)
                                      ) nh
                              LEFT JOIN bt_hint_bid bhb
                              on nh.hint_id = bhb.hint_id
                              LEFT JOIN bt_hint_jurisdiction bhj
                              ON nh.hint_id = bhj.hint_id
                              LEFT JOIN gta_jurisdiction_list gjl
                              ON bhj.jurisdiction_id = gjl.jurisdiction_id
                              LEFT JOIN b221_hint_assessment b2ha
                              ON nh.hint_id = b2ha.hint_id
                              LEFT JOIN b221_assessment_list b2al
                              ON b2ha.assessment_id=b2al.assessment_id
                              LEFT JOIN bt_hint_text bht
                              ON nh.hint_id = bht.hint_id
                              LEFT JOIN bt_hint_url bhu
                              ON nh.hint_id = bhu.hint_id
                              LEFT JOIN bt_url_log bul
                              ON bhu.url_id = bul.url_id
                              LEFT JOIN bt_url_type_list butl
                              ON bhu.url_type_id = butl.url_type_id
                              LEFT JOIN (SELECT hint_id, date AS date_announced
                                    FROM bt_hint_date
                                    WHERE date_accepted=1
                                    AND date_Type_id=1) bhda
                              ON nh.hint_id = bhda.hint_id
                              LEFT JOIN (SELECT hint_id, date AS date_implemented
                                    FROM bt_hint_date
                                    WHERE date_accepted=1
                                    AND date_type_id=2) bhdi
                              ON nh.hint_id = bhdi.hint_id
                              LEFT JOIN (SELECT hint_id, date AS date_removed
                                    FROM bt_hint_date
                                    WHERE date_accepted=1
                                    AND date_type_id=3) bhdr
                              ON nh.hint_id = bhdr.hint_id
                              WHERE bhj.jurisdiction_accepted = 1
                              AND b2ha.assessment_accepted = 1
                              AND bhu.url_accepted=1
                              AND bht.language_id=1")


  ## (1b) upload into gtamain leads section


  # confirming jurisdicition IDs:

  ## ensuring jurisdiciton ID correspondence
  gta.jur=gtalibrary::country.names[,c("jurisdiction.id","name","un_code")]
  setnames(gta.jur, "jurisdiction.id","gta.jur.id")
  setnames(gta.jur, "un_code","un.code")
  new.leads=merge(new.leads, gta.jur[,c("un.code","gta.jur.id")],by="un.code", all.x=T)

  ## assigning one lead jurisdiction for customs unions
  new.leads$gta.jur.id[new.leads$jurisdiction.id==234]=230 # no jur -> Western Sahara
  new.leads$gta.jur.id[new.leads$jurisdiction.id==235]=173 # EEU -> RUS
  new.leads$gta.jur.id[new.leads$jurisdiction.id==236]=118 # EU -> LUX
  new.leads$gta.jur.id[new.leads$jurisdiction.id==237]=195 # SACU -> ZAR



  new.leads$hint.description[new.leads$jurisdiction.id==235]=paste0("Eurasian Economic Union: ",new.leads$hint.description[new.leads$jurisdiction.id==235])
  new.leads$hint.description[new.leads$jurisdiction.id==236]=paste0("European Union: ",new.leads$hint.description[new.leads$jurisdiction.id==236])
  new.leads$hint.description[new.leads$jurisdiction.id==237]=paste0("South African Customs Union: ",new.leads$hint.description[new.leads$jurisdiction.id==237])


  ## forming text out of title + description
  new.leads$hint.text=paste(paste0(new.leads$acting.agency,":   "),
                            new.leads$hint.title,
                            new.leads$hint.description,
                            sep="\n")
  new.leads$hint.text[!is.na(new.leads$date.announced)]=paste(new.leads$hint.text[!is.na(new.leads$date.announced)],
                                                              paste0("Announcement date: " ,new.leads$date.announced[!is.na(new.leads$date.announced)]),

                                                    sep="\n")
  new.leads$hint.text[!is.na(new.leads$date.implemented)]=paste(new.leads$hint.text[!is.na(new.leads$date.implemented)],
                                                                paste0("Implementation date: " ,new.leads$date.implemented[!is.na(new.leads$date.implemented)]),
                                                    sep="\n")
  new.leads$hint.text[!is.na(new.leads$date.removal)]=paste(new.leads$hint.text[!is.na(new.leads$date.removal)],
                                                            paste0("Removal date: " ,new.leads$date.removal[!is.na(new.leads$date.removal)]),
                                                            sep="\n")
  new.leads$hint.text=gsub("'","",new.leads$hint.text)
  new.leads$acting.agency=gsub("'","",new.leads$acting.agency)

  ## gtamain source types
  new.leads$source.type=1
  new.leads$source.type[new.leads$url.type.name!="official"]=4

  ## lead.date (R misbehving badly, hence the for loop :/ )
  new.leads$lead.date=sys.Date()

  for(i in 1:nrow(new.leads)){

    new.leads$lead.date[i]=as.Date(as.numeric(min(c(as.Date(new.leads$date.announced[i]), as.Date(new.leads$date.implemented[i]), Sys.Date()),na.rm = T)), origin="1970-01-01")


  }

  new.leads$lead.date=as.Date(as.numeric(new.leads$lead.date), origin="1970-01-01")
  new.leads$lead.date[is.na(new.leads$date.announced) & is.na(new.leads$date.implemented)]=new.leads$registration.date[is.na(new.leads$date.announced) & is.na(new.leads$date.implemented)]

  new.leads=unique(new.leads)

  ## upload in chunks

  for(chunk in seq(1, nrow(new.leads), 50)){

    upload.chunk=new.leads[c(chunk:min((chunk+49), nrow(new.leads))),]

    gta_sql_update_table(paste0("INSERT INTO gta_leads (lead_text, lead_comment, bastiat_id, source_type_id, announcement_year, creation_time, acting_agency)
                              VALUES ",paste(paste0("('",upload.chunk$url ,"','",upload.chunk$hint.text,"','",upload.chunk$bid,"',",upload.chunk$source.type,",'",as.Date(upload.chunk$lead.date),"', CURRENT_TIMESTAMP,'",upload.chunk$acting.agency,"')"), collapse=","),";"),
                         "main")

    print("leads")


    upload.bids=gta_sql_get_value(paste0("SELECT id as lead_id, bastiat_id as bid FROM gta_leads WHERE bastiat_id IN (",paste(paste0("'",upload.chunk$bid,"'"), collapse=","),");"), "main")

    upload.chunk=merge(upload.chunk, upload.bids, by="bid", all.x=T)

    upload.chunk=aggregate(gta.jur.id ~ lead.id, upload.chunk, min)

    upload.chunk=subset(upload.chunk, lead.id>=(max(upload.chunk$lead.id)-49))


    gta_sql_update_table(paste0("INSERT INTO gta_lead_jurisdiction (lead_id, jurisdiction_id)
                              VALUES ",paste(paste0("(",upload.chunk$lead.id,",",upload.chunk$gta.jur.id ,")"), collapse=","),";"),
                         "main")

    print("leads jurisdictions")

    print(chunk)

  }

  ### (1c) update states of all uploaded hints to 6 (sent out)
  gta_sql_update_table(paste0("UPDATE bt_hint_log SET hint_state_id = 6 WHERE hint_id IN (",paste(new.leads$hint.id, collapse=","),");"))

  # ... and for collections
  gta_sql_update_table(paste0("UPDATE bt_hint_log SET hint_state_id = 6
                              WHERE hint_id IN (SELECT hint_id
                                                FROM b221_hint_collection
                                                WHERE collection_id IN (SELECT DISTINCT(collection_id)
                                                                        FROM b221_collection_star
                                                                        WHERE hint_id IN (",paste(new.leads$hint.id, collapse=","),")));"))



  # TO DO

  # (2) add site-submitted leads to b221
  site.submit=gta_sql_get_value("SELECT id, hint_id, bastiat_id AS bid, is_remove, removal_reason, jurisdiction_id
                               FROM gta_leads gl
                               LEFT JOIN gta_lead_jurisdiction glj
                               ON gl.id = glj.lead_id
                               WHERE gl.fully_processed=0
                               AND gl.bastiat_id IS NULL;", "main")



  # (3) check processing status for hints in state 6 and move processed leads into state 7

  # ... and for collections


gta_sql_update_table("UPDATE bt_hint_log SET hint_type_id=2;")
  ## (4) set gtamain lead priorities
  priority.time=90
  gta_sql_multiple_queries(paste0("UPDATE gta_leads SET is_priority_processing=0;
                                   UPDATE gta_leads SET is_priority_processing=1 WHERE announcement_year>='",as.Date(Sys.Date()-priority.time),"';"),1,"main")







  ## Syncing databases:
  ## (1) Update processed leads in RIC


  ## (2) Update processed hints in GTA
  #### (a) declared irrelevant
  ##### if in collection:
  #### (b) added intervention
  #### (c) substituted by new starred item

  ### LEAD-HINT INTERCHANGE
  ## (1)
  ## fetch lead.ids that are not in bt_hint_lead from from gta main


  ## (2)
  ## Add processed hints to leads section (incl conflict resolution)


  ## Record evaluation
  ## (3)
  ## fetch new evaluations and record them on bt_hint_evaluation
  useful=gta_sql_get_value("SELECT id FROM gta_leads WHERE removal_reason IS NOT NULL AND removal_reason != 'IRREVELANT';", "main")
  useless=gta_sql_get_value("SELECT id FROM gta_leads WHERE removal_reason = 'IRREVELANT';", "main")


  gta_sql_update_table(paste0("UPDATE bt_hint_evaluation
                              SET evaluation_id=2
                              WHERE hint_id IN (SELECT hint_id
                                                FROM bt_hint_lead
                                                WHERE lead_id IN(",paste(useful, collapse=","),"));"))

  gta_sql_update_table(paste0("UPDATE bt_hint_evaluation
                              SET evaluation_id=3
                              WHERE hint_id IN (SELECT hint_id
                                                FROM bt_hint_lead
                                                WHERE lead_id IN(",paste(useless, collapse=","),"));"))


  gta_sql_pool_close("main")
  gta_sql_pool_close("ricdb")


}
