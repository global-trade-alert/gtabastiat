# Roxygen documentation

#' Bastiat, please extract all URLs you find in the following string.


# Function infos and parameters  --------------------------------------------

bt_sync_221_main = function(){


  #change this to turn the function on or off (in case it breaks)
  if(T){

  library(gtasql)
  library(gtalibrary)
  library(pool)
  library(RMariaDB)
  library(data.table)
  library(gtabastiat)
  library(xlsx)
  library(stringr)

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
  new.leads=gta_sql_get_value("SELECT DISTINCT nh.hint_id, acting_agency, registration_date, bid, hint_type_id, gjl.jurisdiction_id, jurisdiction_name, un_code, assessment_name,
                                      hint_title, hint_description, url, url_type_name,
                                      MAX(IF(bhda2.date_type_id = 1, bhda2.date, NULL )) AS date_announced,
                    									MAX(IF(bhda2.date_type_id = 2, bhda2.date, NULL )) AS date_implemented,
                    									MAX(IF(bhda2.date_type_id = 3, bhda2.date, NULL )) AS date_removed
                               FROM ( SELECT bhl.hint_id, acting_agency, registration_date, hint_type_id
                                      FROM bt_hint_log bhl
                                      WHERE NOT EXISTS (SELECT NULL FROM b221_hint_collection WHERE  b221_hint_collection.hint_id = bhl.hint_id)
                                      AND bhl.hint_state_id IN (3,4,5)
                                      AND bhl.gta_id IS NULL
                                      UNION
                                      SELECT bhl.hint_id, acting_agency, registration_date, hint_type_id
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
                              INNER JOIN (
                              SELECT bhj2.hint_id AS hint_id, bhj2.jurisdiction_id AS jurisdiction_id, bhj2.jurisdiction_accepted AS jurisdiction_accepted
                								FROM bt_hint_jurisdiction bhj2
                								INNER JOIN (
                								    SELECT hint_id, jurisdiction_id, MAX(classification_id) AS classification_id
                								    FROM bt_hint_jurisdiction
                								    WHERE jurisdiction_accepted = 1
                								    GROUP BY hint_id
                								) bhj3 ON bhj2.classification_id = bhj3.classification_id
                						    	) bhj
                              ON nh.hint_id = bhj.hint_id
                              AND bhj.jurisdiction_accepted = 1
                              LEFT JOIN gta_jurisdiction_list gjl
                              ON bhj.jurisdiction_id = gjl.jurisdiction_id
                              LEFT JOIN (
                                SELECT bal.hint_id AS hint_id, bal.assessment_id AS assessment_id, bal.assessment_accepted
                    								FROM b221_hint_assessment bal
                    								INNER JOIN (
                    								    SELECT hint_id, assessment_id, MAX(classification_id) AS classification_id
                    								    FROM b221_hint_assessment
                    								    WHERE assessment_accepted = 1 OR (hint_id IN (SELECT hint_id
                    								    	FROM b221_hint_assessment bhint
                    								    	WHERE bhint.assessment_accepted IS NULL
                    								    	GROUP BY bhint.hint_id HAVING COUNT(DISTINCT bhint.hint_id) = 1))
                    								    GROUP BY hint_id
                    								) bal2 ON bal.classification_id = bal2.classification_id
                    								AND (bal.assessment_accepted = 1 OR bal.assessment_accepted IS NULL)
                              ) b2ha
                              ON nh.hint_id = b2ha.hint_id
                              AND (b2ha.assessment_accepted = 1 OR b2ha.assessment_accepted IS NULL)
                              LEFT JOIN b221_assessment_list b2al
                              ON b2ha.assessment_id=b2al.assessment_id
                              INNER JOIN bt_hint_text bht
                              ON nh.hint_id = bht.hint_id
                              AND bht.language_id=1
                              LEFT JOIN bt_hint_url bhu
                              ON nh.hint_id = bhu.hint_id
                              AND (bhu.url_accepted=1 OR (bhu.hint_id IN (SELECT bhu2.hint_id
          											FROM bt_hint_url bhu2
          											WHERE bhu2.url_accepted IS NULL
          											GROUP BY bhu2.hint_id HAVING COUNT(DISTINCT bhu2.hint_id) = 1)))
                              LEFT JOIN bt_url_log bul
                              ON bhu.url_id = bul.url_id
                              LEFT JOIN bt_url_type_list butl
                              ON bhu.url_type_id = butl.url_type_id
              							  LEFT JOIN (
                							  SELECT a.hint_id AS hint_id, a.date AS date, a.date_type_id AS date_type_id, a.date_accepted AS date_accepted
                  								FROM bt_hint_date a
                  								INNER JOIN (
                  								    SELECT hint_id, date_type_id, MAX(classification_id) AS classification_id, date
                  								    FROM bt_hint_date
                  								    WHERE date_accepted = 1
                  								    GROUP BY hint_id, date_type_id
                  								) b ON a.classification_id = b.classification_id AND a.date_type_id = b.date_type_id) bhda2
                              ON nh.hint_id = bhda2.hint_id
                              AND bhda2.date_accepted = 1
                              GROUP BY nh.hint_id")





  #I think it's much quicker doing this in a separate query due to size of bt_hint_relevance
  hint.relevance = gta_sql_get_value(query = paste0("SELECT distinct bhr.hint_id, bhr.relevance_probability
                                     FROM bt_hint_relevance bhr
                                     WHERE bhr.hint_id IN (", paste0(new.leads$hint.id, collapse=", "), ")"))

  #remove NAs
  hint.relevance = subset(hint.relevance, !(is.na(relevance.probability)))
  hint.relevance = subset(hint.relevance, !duplicated(hint.id))


  #merge back in, not all have a relevance score for some reason
  new.leads = merge(new.leads, hint.relevance, all.x = T)
  new.leads$relevance.probability[is.na(new.leads$relevance.probability)] = "NULL"

  egi.hints=gta_sql_get_value(paste0("SELECT hint_id
                                     FROM b221_hint_product_group
                                     WHERE product_group_id IN (2,3,4,5,7);"))
  egi.hints=egi.hints[egi.hints %in% new.leads$hint.id] %>% unique()

  ## correct for hints from collections that are already on the site
  main.bid=gta_sql_get_value("SELECT DISTINCT(bastiat_id) FROM gta_leads WHERE creation_time>='2020-08-01';","main")
  new.leads=unique(subset(new.leads, ! bid %in% main.bid))

  #fix problem where acting.agency > 100 chars (nb sometimes diacritics corrupted to several chars hence limit of 85 for safety)


# truncation --------------------------------------------------------------


  new.leads$acting.agency = str_trunc(new.leads$acting.agency, width = 85)

  new.leads$hint.description = str_trunc(new.leads$hint.description, 3000, ellipsis = " [truncated by Bastiat]")







  #failsafe in case of duplicates
  dup.bid.check = gta_sql_get_value(paste0("SELECT DISTINCT gl.bastiat_id
                                    FROM gta_leads gl
                                    WHERE gl.bastiat_id IN (", paste("'", new.leads$bid, "'", sep = "", collapse = ", "), ");"), "main")

  if(!is.na(dup.bid.check)){
    warning("Some BIDs are already in the gtamain database - please check these! Saving recorded duplicates to `logs/duplicated_bids.Rdata`")
    load(file = "logs/duplicated_bids.Rdata")

    duplicated.bids = c(duplicated.bids, dup.bid.check)

    save(duplicated.bids, file = "logs/duplicated_bids.Rdata")

  }


  if(nrow(new.leads)==0){
    warning("No new hints.")
  } else {

    ## (1b) upload into gtamain leads section


    # confirming jurisdicition IDs:
#nl2 = new.leads
    ## ensuring jurisdiciton ID correspondence

    #this took me ages to find
    new.leads$un.code = as.integer(new.leads$un.code)

    gta.jur=gtalibrary::country.names[,c("jurisdiction.id","name","un_code")]
    setnames(gta.jur, "jurisdiction.id","gta.jur.id")
    setnames(gta.jur, "un_code","un.code")
    new.leads=merge(new.leads, gta.jur[,c("un.code","gta.jur.id")],by="un.code", all.x=T)

    ## assigning one lead jurisdiction for customs unions
    new.leads$gta.jur.id[new.leads$jurisdiction.id==234]=230 # no jur -> Western Sahara
    new.leads$gta.jur.id[new.leads$jurisdiction.id==235]=173 # EEU -> RUS
    new.leads$gta.jur.id[new.leads$jurisdiction.id==236]=118 # EU -> LUX
    new.leads$gta.jur.id[new.leads$jurisdiction.id==237]=195 # SACU -> ZAR


    if(235 %in% unique(new.leads$jurisdiction.id)){
      new.leads$hint.description[new.leads$jurisdiction.id==235]=paste0("Eurasian Economic Union: ",new.leads$hint.description[new.leads$jurisdiction.id==235])
    }


    if(236 %in% unique(new.leads$jurisdiction.id)){
      new.leads$hint.description[new.leads$jurisdiction.id==236]=paste0("European Union: ",new.leads$hint.description[new.leads$jurisdiction.id==236])
    }


    if(237 %in% unique(new.leads$jurisdiction.id)){
      new.leads$hint.description[new.leads$jurisdiction.id==237]=paste0("South African Customs Union: ",new.leads$hint.description[new.leads$jurisdiction.id==237])
    }



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
    new.leads$source.type[!grepl(pattern = "GNEWS", x=new.leads$bid)]=3

    ## lead.date (R misbehving badly, hence the for loop :/ )
    # FTFY in here also as an attempt at fixing encoding
    new.leads$lead.date=Sys.Date()

    ftfy=reticulate::import("ftfy")
    for(i in 1:nrow(new.leads)){

      new.leads$lead.date[i]=as.Date(as.numeric(min(c(as.Date(new.leads$date.announced[i]), as.Date(new.leads$date.implemented[i]), Sys.Date()),na.rm = T)), origin="1970-01-01")
      new.leads$hint.text[i] = ftfy$fix_text(new.leads$hint.text[i])

      #APOSTROPHES
      new.leads$hint.text[i] = gsub(pattern = "'",
                                    replacement = "\\\\'",
                                    x = new.leads$hint.text[i])

      #EMOJI


    # for some weird reason, the below doesn't work on linux so had to dl a package
    #   if(str_detect(new.leads$hint.text[i],pattern = "[:emoji:]")){
    #     emoji = str_extract_all(new.leads$hint.text[i], pattern = "[:emoji:]")[[1]] %>%
    #       str_extract(pattern = "[\\D]")
    #
    #     emoji = emoji[!is.na(emoji)]
    #     if(length(emoji)>0){
    #       for(j in 1:length(emoji)){
    #         new.leads$hint.text[i] = str_remove_all(string = new.leads$hint.text[i], pattern = emoji[j])
    #       }
    #     }
    #   }

      new.leads$hint.text[i] = textclean::replace_emoji(new.leads$hint.text[i])

     }

      new.leads$hint.text[i] = textclean::replace_emoji(new.leads$hint.text[i])

    new.leads$lead.date=as.Date(as.numeric(new.leads$lead.date), origin="1970-01-01")
    new.leads$lead.date[is.na(new.leads$date.announced) & is.na(new.leads$date.implemented)]=new.leads$registration.date[is.na(new.leads$date.announced) & is.na(new.leads$date.implemented)]

    new.leads=unique(new.leads)
    Encoding(new.leads$hint.title)="UTF-8"
    Encoding(new.leads$hint.description)="UTF-8"
    new.leads=unique(new.leads)


    # even with the tryCatch() there is some nasty error causing this to fail. disabled until required.
    # nl.xlsx=new.leads
    # nl.xlsx$priority="yes"
    # nl.xlsx$priority[nl.xlsx$lead.date<Sys.Date()-90 ]="no"
    # nl.xlsx=nl.xlsx[,c("hint.id","bid","jurisdiction.name","acting.agency","priority", "lead.date","date.announced","date.implemented","date.removed","assessment.name","hint.title","hint.description","url")]
    #
    # tryCatch(expr={
    #   xlsx::write.xlsx(nl.xlsx, file=paste0("0 projects/BT leads sync/BT leads - ",Sys.time(),".xlsx"), row.names = F, showNA = F)
    # },error = function(e){
    #   warning(paste("problem writing new leads xlsx:", e))
    # }
    # )
    # rm(nl.xlsx)

    ## upload in chunks

    #dbg
    #chunk = seq(1, nrow(new.leads), 50)[6:length(seq(1, nrow(new.leads), 50))][1]

      for(chunk in seq(1, nrow(new.leads), 50)){

      upload.chunk=new.leads[c(chunk:min((chunk+49), nrow(new.leads))),]

      gta_sql_update_table(paste0("INSERT INTO gta_leads (lead_text, lead_comment, bastiat_id, source_type_id, announcement_year, creation_time, display_id, acting_agency, relevance_probability)
                              VALUES ",paste(paste0("('",upload.chunk$url ,"','",
                                                    upload.chunk$hint.text,"','",
                                                    upload.chunk$bid,"',",
                                                    upload.chunk$source.type,",'",
                                                    as.Date(upload.chunk$lead.date),
                                                    "', CURRENT_TIMESTAMP,",
                                                    upload.chunk$hint.type.id,",'",
                                                    upload.chunk$acting.agency,"', ",
                                                    upload.chunk$relevance.probability, ")"), collapse=","),";"),
                           "main")


      print("leads")


      upload.bids=gta_sql_get_value(paste0("SELECT id as lead_id, bastiat_id as bid
                                           FROM gta_leads
                                           WHERE bastiat_id IN (",paste(paste0("'",upload.chunk$bid,"'"), collapse=","),");"), "main")

      upload.chunk=merge(upload.chunk, upload.bids, by="bid", all.x=T)


      ## adding EGI theme
      if(any(upload.chunk$hint.id %in% egi.hints)){

        egi.leads=unique(upload.chunk$lead.id[upload.chunk$hint.id %in% egi.hints])


        gta_sql_update_table(paste0("INSERT INTO gta_lead_theme (lead_id, theme_id)
                              VALUES ",paste(paste0("(",egi.leads,", 3)"), collapse=","),";"),
                             "main")


      }

      print("leads theme")


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



    # (2) add site-submitted leads to b221
    # site.submit=gta_sql_get_value("SELECT id, bastiat_id AS bid, is_remove, removal_reason, jurisdiction_id
    #                              FROM gta_leads gl
    #                              LEFT JOIN gta_lead_jurisdiction glj
    #                              ON gl.id = glj.lead_id
    #                              WHERE gl.bastiat_id IS NULL;", "main")
    #
    #
    #
    # # (3) check processing status for hints in state 6 and move processed leads into state 7
    #
    # # ... and for collections
    #
    #
    # gta_sql_update_table("UPDATE bt_hint_log SET hint_type_id=2;")

  }

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
    leads.checked=gta_sql_get_value("SELECT bastiat_id, removal_reason
                                 FROM gta_leads
                                 WHERE creation_time>='2020-08-01'
                                 AND is_remove=1","main")

    # hints.relevant=gta_sql_get_value(paste0("SELECT hint_id FROM bt_hint_bid
    #                                  WHERE bid IN (",paste(paste0("'",subset(leads.checked, removal.reason!="IRREVELANT")$bastiat.id,"'"), collapse=","),")"))


    #reminder of reasons
    # * = relevant
    # discard_reason_id - discard_reason_name
    # 1 - no policy mentioned
    # 2 - is technical standard (TBT)*
    # 3 - is health standard (SPS)*
    # 4 - not a commercial policy
    # 5 - no meaningful change / below de minimis*
    # 6 - fails RTT*
    # 7 - no change
    # 8 - other, see comment
    # 9 - no credible action
    # 10 - update to existing intervention*
    # 11 - duplicate of other hint or GTA entry*
    # 12 - no unilateral act (but bi- or multilateral)*
    # 13 - useful*
    # 14 - keep for EGI*
    # 15 - beyond DPA scope
    # 16 - No thanks*

    hints.relevant=gta_sql_get_value(paste0("SELECT hint_id FROM bt_hint_bid
                                     WHERE bid IN (",paste(paste0("'",subset(leads.checked,  removal.reason %in% c(2,3,5,6,10,11,12,13,14,16))$bastiat.id,"'"), collapse=","),")"))

    #very rarely, duplicate BIDs get into this table and are assigned hint_id as
    #NULL. should be OK, as they are duplicates anyway, so remove the NAs.

    hints.relevant = hints.relevant[!is.na(hints.relevant)]

    if(length(hints.relevant)>0){

      gta_sql_update_table(paste("UPDATE bt_hint_log
                                SET hint_state_id=7
                                WHERE hint_id IN (",paste(hints.relevant, collapse=","),")"))

      print(paste(length(hints.relevant), "leads sent to state 7"))

      # ... and for collections
      gta_sql_update_table(paste("UPDATE bt_hint_log
                                SET hint_state_id=7
                                WHERE hint_id IN (SELECT hint_id
                                                  FROM b221_hint_collection
                                                  WHERE collection_id IN (SELECT collection_id FROM b221_hint_collection
                                                                          WHERE hint_id IN (",paste(hints.relevant, collapse=","),")))"))


    }


    #old version. now discard reasons are integers with FK reference to gta_lead_discard_reason_list
    # hints.irrelevant=gta_sql_get_value(paste0("SELECT hint_id FROM bt_hint_bid
    #                                  WHERE bid IN (",paste(paste0("'",subset(leads.checked, removal.reason=="IRREVELANT")$bastiat.id,"'"), collapse=","),")"))

    hints.irrelevant=gta_sql_get_value(paste0("SELECT hint_id FROM bt_hint_bid
                                     WHERE bid IN (",paste(paste0("'",subset(leads.checked, !removal.reason %in% c(2,3,5,6,10,12,13,14,16))$bastiat.id,"'"), collapse=","),")"))

    #see above for whence the NAs here can arise
    hints.irrelevant = hints.irrelevant[!is.na(hints.irrelevant)]

    if(length(hints.irrelevant)>0){

      gta_sql_update_table(paste("UPDATE bt_hint_log
                                SET hint_state_id=9
                                WHERE hint_id IN (",paste(hints.irrelevant, collapse=","),")"))
      print(paste(length(hints.irrelevant), "leads sent to state 9"))

      gta_sql_get_value(paste("INSERT INTO bt_hint_relevance (hint_id, relevance,classification_id, relevance_accepted, validation_classification, confirm_status)
                              SELECT hint_id, 0 as relevance, validation_classification as classification_id, 1 as relevance_accepted, validation_classification, 0 as confirm_status
                              FROM bt_hint_relevance
                              WHERE relevance=1
                              AND hint_id IN (",paste(hints.irrelevant, collapse=","),")"))

      gta_sql_get_value(paste("UPDATE bt_hint_relevance
                             SET relevance_accepted=0
                             WHERE relevance=1
                             AND hint_id IN (",paste(hints.irrelevant, collapse=","),")"))

      # ... and for collections
      gta_sql_update_table(paste("UPDATE bt_hint_log
                                SET hint_state_id=9
                                WHERE hint_id IN (SELECT hint_id
                                                  FROM b221_hint_collection
                                                  WHERE collection_id IN (SELECT collection_id FROM b221_hint_collection
                                                                          WHERE hint_id IN (",paste(hints.irrelevant, collapse=","),")))"))

      gta_sql_get_value(paste("INSERT INTO bt_hint_relevance (hint_id, relevance,classification_id, relevance_accepted, validation_classification, confirm_status)
                              SELECT hint_id, 0 as relevance, validation_classification as classification_id, 1 as relevance_accepted, validation_classification, 0 as confirm_status
                              FROM bt_hint_relevance
                              WHERE relevance=1
                              AND hint_id IN (SELECT hint_id
                                              FROM b221_hint_collection
                                              WHERE collection_id IN (SELECT collection_id FROM b221_hint_collection
                                                                      WHERE hint_id IN (",paste(hints.irrelevant, collapse=","),")))"))

      gta_sql_get_value(paste("UPDATE bt_hint_relevance
                             SET relevance_accepted=0
                             WHERE relevance=1
                             AND hint_id IN (SELECT hint_id
                                             FROM b221_hint_collection
                                             WHERE collection_id IN (SELECT collection_id FROM b221_hint_collection
                                                                     WHERE hint_id IN (",paste(hints.irrelevant, collapse=","),")))"))


    }


    ## Record evaluation
    ## (3)
    ## fetch new evaluations and record them on bt_hint_evaluation
    # useful=gta_sql_get_value("SELECT id FROM gta_leads WHERE removal_reason IS NOT NULL AND removal_reason != 'IRREVELANT';", "main")
    # useless=gta_sql_get_value("SELECT id FROM gta_leads WHERE removal_reason = 'IRREVELANT';", "main")
    #
    #
    # gta_sql_update_table(paste0("UPDATE bt_hint_evaluation
    #                             SET evaluation_id=2
    #                             WHERE hint_id IN (SELECT hint_id
    #                                               FROM bt_hint_lead
    #                                               WHERE lead_id IN(",paste(useful, collapse=","),"));"))
    #
    # gta_sql_update_table(paste0("UPDATE bt_hint_evaluation
    #                             SET evaluation_id=3
    #                             WHERE hint_id IN (SELECT hint_id
    #                                               FROM bt_hint_lead
    #                                               WHERE lead_id IN(",paste(useless, collapse=","),"));"))








  gta_sql_pool_close("main")
  gta_sql_pool_close()


}
  }
