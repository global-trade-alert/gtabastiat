# Roxygen documentation

#' Get the latest version from GitHub.
#'
#' Syncs your local library with our latest GTA GitHub release.
#'
#' @return Be up to date with our latest functions.
#' @references www.globaltradealert.org
#' @author Global Trade Alert

bt_leads_core_update = function(update.df=NULL,
                                exclude.by="act.url",
                                all.covid=F,
                                force.create=F){



  eval(parse(text=paste("lc.update<<-",update.df,sep="")))


  if(nrow(lc.update)==0){
    print("All leads recorded already.")

  } else{

    ######### CLEANING & PREPARATION

    ### trade defence country correction
    td.phrases="antidumping|anti-?dumping|countervailing|anti-?subsidy"

    lc.update$country.lead[grepl(td.phrases, lc.update$act.title.en, ignore.case = T)]="Vatican"
    lc.update$country.lead[grepl(td.phrases, lc.update$act.description.en, ignore.case = T)]="Vatican"
    lc.update$country.lead[grepl(td.phrases, lc.update$act.title.ll, ignore.case = T)]="Vatican"
    lc.update$country.lead[grepl(td.phrases, lc.update$act.description.ll, ignore.case = T)]="Vatican"


    ## adding leads-checker columns
    lc.update$relevance.probability=NA
    lc.update$relevance.probability[lc.update$classify==0 & lc.update$relvant==1]=1
    lc.update$relevance.probability[lc.update$classify==0 & lc.update$relvant==0]=0

    ## act_url_official
    non.official=c("AGNET","CHN-GLOBAL", "CHN-KPMG", "HKTDC-PICK", "MENA-ZAWYA")
    lc.update$act.url.official=as.numeric(grepl(paste(non.official, collapse="|"), lc.update$bid, ignore.case = T))

    ## force_create
    if(! "force.create" %in% names(lc.update)){
      lc.update$force.create=as.numeric(force.create)
    }



    ## is_covid

    if(all.covid){

      lc.update$is.covid=1

    } else {
      lc.update$is.covid=0
      lc.update$is.covid[grepl("( covid)|( corona)", lc.update$act.title.en, ignore.case = T)]=1
      lc.update$is.covid[grepl("( covid)|( corona)", lc.update$act.title.ll, ignore.case = T)]=1
      lc.update$is.covid[grepl("( covid)|( corona)", lc.update$act.description.en, ignore.case = T)]=1
      lc.update$is.covid[grepl("( covid)|( corona)", lc.update$act.description.ll, ignore.case = T)]=1
      lc.update$is.covid[lc.update$act.date<="2019-12-01"]=0


    }

    ### COVID correction for trade defence
    lc.update$is.covid[grepl(td.phrases, lc.update$act.title.en, ignore.case = T)]=0
    lc.update$is.covid[grepl(td.phrases, lc.update$act.description.en, ignore.case = T)]=0
    lc.update$is.covid[grepl(td.phrases, lc.update$act.title.ll, ignore.case = T)]=0
    lc.update$is.covid[grepl(td.phrases, lc.update$act.description.ll, ignore.case = T)]=0


    ### Redundancy check
    ### is it already in bt_url_log? If so, what hints?

    bt.url=gta_sql_get_value("SELECT *
                              FROM bt_url_log;")
    bt.url$url=as.character(bt.url$url)


    database <<- "gtamain"
    gta_sql_pool_open(pool.name="main.pool",
                      db.title=database,
                      db.host = gta_pwd(database)$host,
                      db.name = gta_pwd(database)$name,
                      db.user = gta_pwd(database)$user,
                      db.password = gta_pwd(database)$password,
                      table.prefix = "bt_")


    gta.sa=gta_sql_get_value("SELECT id, source
                              FROM gta_measure;",
                             db.connection = "main.pool")
    gta_sql_pool_close("main.pool")
    gta.sa$source=as.character(gta.sa$source)
    gta.sa$source=stringi::stri_trans_general(gta.sa$source, "latin-ascii")


    lc.update$background.url=as.character(lc.update$background.url)
    lc.update$act.url=as.character(lc.update$act.url)


    for(i in 1:nrow(lc.update)){
      print(i)

      if(grepl("EU-SA",lc.update$bid[i])==F){

        check.url=gsub("(https?://www.)|(/$)","",lc.update$act.url[i])

        if(is.na(check.url)==F){

          ## checking state acts
          sa.ids=unique(subset(gta.sa, grepl(check.url, source))$id)

          if(length(sa.ids)>0){

            lc.update$act.description.en[i]=paste(lc.update$act.description.en[i], "\n ---- The following state acts have this lead URL among their sources:\n",
                                                  paste(paste("https://www.globaltradealert.org/state-act/",sa.ids, sep=""), collapse="\n"), sep="")

            if(! is.na(lc.update$act.description.ll[i])){

              lc.update$act.description.ll[i]=paste(lc.update$act.description.ll[i], "\n ---- The following state acts have this lead URL among their sources:\n",
                                                    paste(paste("https://www.globaltradealert.org/state-act/",sa.ids, sep=""), collapse="\n"), sep="")
            }

          }

          ## checking hints
          bt.url.ids=subset(bt.url, grepl(check.url, url))$url_id
          if(length(bt.url.ids)>0){

            hints=gta_sql_get_value(paste0("SELECT hint_id
                                            FROM bt_hint_url
                                            WHERE url_id IN (",paste(bt.url.ids, collapse=","),");"))
            hints=unique(hints[!is.na(hints)])

            if(length(hints)>0){

              lc.update$act.description.en[i]=paste(lc.update$act.description.en[i], "\n ---- The following hint IDs have this URL among their sources:\n",
                                                    paste(hints, collapse=","), sep="")

              if(! is.na(lc.update$act.description.ll[i])){

                lc.update$act.description.ll[i]=paste(lc.update$act.description.ll[i], "\n ---- The following hint IDs have this URL among their sources:\n",
                                                      paste(hints, collapse=","), sep="")
              }

            }
            rm(hints)
          }
        }


      }

    }





    ## classifying results
    classify=subset(lc.update, classify==1 & relevant==1 & country.lead!="Vatican")

    if(nrow(classify)>0){


      classify$text=classify$act.title.en
      classify$text[is.na(classify$act.description.en)==F]=paste(classify$text[is.na(classify$act.description.en)==F], classify$act.description.en[is.na(classify$act.description.en)==F], sep=" ")
      classify$text[is.na(classify$act.values)==F]=paste(classify$text[is.na(classify$act.values)==F], classify$act.values[is.na(classify$act.values)==F], sep=" ")

      # removing non-ASCII
      classify$text=stringi::stri_trans_general(classify$text, "latin-ascii")
      classify$text=gsub("[^\001-\177]","",classify$text)

      classification.result=bt_squad_prediction(prediction.data.id=classify$bid,
                                                prediction.data.text=classify$text,
                                                prediction.acting.agency=classify$acting.agency)
      classify$relevant=NULL
      classify$relevance.probability=NULL
      classify$text=NULL

      classify=merge(classify, classification.result, by.x="bid",by.y="text.id")

      classified.bids=classify$bid
      classified.lids=classify$lead.id
      classified.relevance=classify$relevant
      classified.rel.prob=round(classify$relevance.probability,4)

      lc.update=rbind(subset(lc.update, ! bid %in% classified.bids),
                      classify)



    }




    ## checking for keywords
    print("checking for negative keywords")
    contains.negative.key=bt_classify_by_keyword(text.to.check=lc.update$act.title.en,
                                                 text.id=lc.update$bid,
                                                 flavour="negative")


    if(any(contains.negative.key)){
      lc.update$relevant[contains.negative.key]=0

    }



    ## UPLOAD TO RICARDO
    leads.core=gta_sql_get_value("SELECT * FROM bt_leads_core LIMIT 1;")

    if(length(setdiff(names(leads.core)[! names(leads.core) %in% names(lc.update)],c("upload.id", "hint.id")))>0){
      stop("Error: not all columns of leads.core present in input.")
    }


    lc.update=lc.update[,names(lc.update)[names(lc.update) %in% names(leads.core)]]
    lc.update=unique(lc.update)
    names(lc.update)=gsub("\\.","_",names(lc.update))

    gta_sql_multiple_queries("DROP TABLE IF EXISTS bt_leads_core_temp;
                              DELETE FROM bt_leads_core WHERE 1 = 1;",1)
    dbWriteTable(conn = pool, name = "bt_leads_core_temp", value = lc.update, row.names=F, append=F, overwrite=T)

    gta_sql_multiple_queries(paste0("INSERT INTO bt_leads_core(",paste(names(lc.update), collapse=","),")
                                 SELECT ",paste(names(lc.update), collapse=","),"
                                 FROM bt_leads_core_temp;
                                 DROP TABLE IF EXISTS bt_leads_core_temp;"),1)


    parsing.query="
               /* Avoiding duplicate hints from BT */
               /* ASSUMES NO FORCE_CREATE! */
                DELETE FROM bt_leads_core
                WHERE act_url IN (SELECT url FROM bt_url_log) AND force_create=0;

                /* Writing into hint_log */
                INSERT INTO bt_hint_log(hint_type_id, hint_state_id, user_id, registration_date, acting_agency, hint_date, hint_values, upload_id)
                SELECT 1 AS hint_type_id, 5 AS hint_state_id, 70 AS user_id, collection_date, acting_agency, act_date, act_values, upload_id
                FROM bt_leads_core;


                /* store hint_id & bid pairs*/
                UPDATE bt_leads_core blc
                JOIN bt_hint_log bhl
                ON bhl.upload_id = blc.upload_id
                SET blc.hint_id = bhl.hint_id;



                UPDATE bt_hint_log
                SET hint_type_id=2
                WHERE hint_id IN (SELECT hint_id
                				  FROM bt_leads_core blc
                				  WHERE is_covid=1);


                /* some odd NULL rows in first iteration*/
                DELETE FROM bt_leads_core
                WHERE collection_date IS NULL;



                /* Creating hint_jurisdiction to see if any are missing */
                DROP TABLE IF EXISTS bt_temp_hj;

                CREATE TABLE bt_temp_hj (
                	  hint_id INT,
                    jurisdiction_name TEXT,
                    jurisdiction_id INT
                );

                ALTER TABLE bt_temp_hj
                ADD FOREIGN KEY (hint_id) REFERENCES bt_hint_log(hint_id);

                INSERT INTO bt_temp_hj (hint_id, jurisdiction_name, jurisdiction_id)
                SELECT hint_id, jl.jurisdiction_name, jurisdiction_id
                FROM bt_leads_core lc
                JOIN gta_jurisdiction_list jl
                ON lc.country_lead = jl.jurisdiction_name;



                /* moving hints to check by freelancer (state 1) */
                UPDATE bt_hint_log
                SET hint_state_id=1
                WHERE hint_id IN (SELECT hint_id FROM bt_leads_core WHERE hint_id NOT IN (SELECT hint_id FROM bt_temp_hj));

               /* also all covid stuff goes into B221 */
                UPDATE bt_hint_log
                SET hint_state_id=1
                WHERE hint_id IN (SELECT hint_id FROM bt_leads_core WHERE is_covid=1);

                /* moving hints to relevance validation to editor (state 2): no use case right now*/

                /* moving hints to OSC for processing (state  3)*/
                UPDATE bt_hint_log
                SET hint_state_id=3
                WHERE hint_id IN (SELECT hint_id FROM bt_leads_core WHERE relevant=1 AND act_url_official=0);

                /* moving hints to OSC for validation (state 4): no use case right now*/

                /* moving hints to hints through OSC into leads (state 5): the default*/

                /* moving to hints into trash (state 8) */
                UPDATE bt_hint_log
                SET hint_state_id=8
                WHERE hint_id IN (SELECT hint_id FROM bt_leads_core WHERE relevant=0 and is_covid=0);

                /* removing old hints into parking position */
                UPDATE bt_hint_log
                SET hint_state_id=10
                WHERE hint_date<='2019-01-01'
                AND hint_state_id != 8;

                /* Writing into classificatin log*/
                INSERT INTO bt_classification_log(hint_id, user_id, hint_state_id)
                SELECT hint_id, 70 AS user_id, 11 AS hint_state_id
                FROM bt_leads_core;



                /* adding act_urls */
                /** update bt_url_log **/

                INSERT INTO bt_url_log(url)
                SELECT DISTINCT(act_url)
                FROM bt_leads_core blc
                WHERE act_url IS NOT NULL
                AND act_url NOT IN (SELECT url FROM bt_url_log);

                INSERT INTO bt_url_log(url)
                SELECT DISTINCT(background_url)
                FROM bt_leads_core blc
                WHERE background_url IS NOT NULL
                AND background_url NOT IN (SELECT url FROM bt_url_log);


                /** update bt_hint_url **/
                INSERT INTO bt_hint_url(hint_id, url_id, url_type_id, classification_id, url_accepted, validation_user)
                SELECT blc.hint_id, url_id, 1 AS url_type_id, classification_id, 1 AS url_accepted, 70 AS validation_user
                FROM bt_leads_core blc
                JOIN bt_url_log bul
                ON blc.act_url=bul.url
                JOIN bt_classification_log bcl
                ON blc.hint_id=bcl.hint_id
                WHERE blc.act_url IS NOT NULL;

                UPDATE bt_hint_url
                SET url_accepted = NULL
                WHERE hint_id IN (SELECT hint_id FROM bt_leads_core WHERE relevant=1 AND act_url_official=0)
                AND url_type_id != 5;

                UPDATE bt_hint_url
                SET validation_user = NULL
                WHERE hint_id IN (SELECT hint_id FROM bt_leads_core WHERE relevant=1 AND act_url_official=0)
                AND url_type_id != 5;

                UPDATE bt_hint_url
                SET url_type_id = 2
                WHERE hint_id IN (SELECT hint_id FROM bt_leads_core WHERE act_url_official=0)
                AND url_type_id != 5;

                /** update bt_hint_background_url **/
                INSERT INTO bt_hint_background_url(hint_id, url_id)
                SELECT hint_id, url_id
                FROM bt_leads_core blc
                JOIN bt_url_log bul
                ON blc.background_url=bul.url;



                /* Writing into bt_hint_jurisdiction */
                INSERT INTO bt_hint_jurisdiction(hint_id, classification_id, jurisdiction_id, jurisdiction_accepted, validation_user)
                SELECT bhj.hint_id, classification_id, jurisdiction_id, 1 AS jurisdiction_accepted, 70 AS validation_user
                FROM bt_temp_hj bhj
                JOIN bt_classification_log bcl
                ON bhj.hint_id = bcl.hint_id;

                UPDATE bt_hint_jurisdiction
                SET jurisdiction_accepted = NULL
                WHERE hint_id IN (SELECT hint_id
                				  FROM bt_leads_core
                				  WHERE hint_id IN (SELECT hint_id
                				 					FROM bt_hint_log
                				 					WHERE hint_state_id =1));

                UPDATE bt_hint_jurisdiction
                SET validation_user= NULL
                WHERE hint_id IN (SELECT hint_id
                				  FROM bt_leads_core
                				  WHERE hint_id IN (SELECT hint_id
                				 					FROM bt_hint_log
                				 					WHERE hint_state_id =1));


                DROP TABLE IF EXISTS bt_temp_hj;



                /* bt_hint_bid */
                INSERT INTO bt_hint_bid (hint_id, bid)
                SELECT hint_id, bid
                FROM bt_leads_core
                WHERE bid IS NOT NULL;



                /* bt_hint_relevance */
                INSERT INTO bt_hint_relevance (hint_id, classification_id, relevance, relevance_probability, relevance_accepted, validation_user)
                SELECT blc.hint_id, classification_id, relevant, relevance_probability, 1 AS relevance_accepted, 70 AS validation_user
                FROM bt_leads_core blc
                JOIN bt_classification_log bcl
                ON blc.hint_id=bcl.hint_id;

                UPDATE bt_hint_relevance
                SET relevance_accepted= NULL
                WHERE hint_id IN (SELECT hint_id
                				  FROM bt_leads_core
                				  WHERE hint_id IN (SELECT hint_id
                				 					FROM bt_hint_log
                				 					WHERE hint_state_id =1));

                UPDATE bt_hint_relevance
                SET validation_user= NULL
                WHERE hint_id IN (SELECT hint_id
                				  FROM bt_leads_core
                				  WHERE hint_id IN (SELECT hint_id
                				 					FROM bt_hint_log
                				 					WHERE hint_state_id =1));



                /* Writing into bt_hint_text*/
                /** English **/
                INSERT INTO bt_hint_text(hint_id, hint_title, hint_description, language_id)
                SELECT hint_id, act_title_en, act_description_en, 1 AS language_id
                FROM bt_leads_core blc
                WHERE act_title_en IS NOT NULL
                OR act_description_en IS NOT NULL;

                UPDATE bt_hint_text SET hint_title = '[hint without title]' where hint_title IS NULL;


                /** local language **/
                INSERT INTO bt_hint_text(hint_id, hint_title, hint_description, language_id)
                SELECT hint_id, act_title_ll, act_description_ll, 2 AS language_id
                FROM bt_leads_core blc
                WHERE act_title_ll IS NOT NULL
                OR act_description_ll IS NOT NULL;

                /* cleaning up */
                DELETE FROM bt_leads_core WHERE 1 = 1;"


    reset.query="SET FOREIGN_KEY_CHECKS = 0;
                TRUNCATE b221_collection_assessment;
                TRUNCATE b221_collection_intervention;
                TRUNCATE b221_collection_jurisdiction;
                TRUNCATE b221_collection_log;
                TRUNCATE b221_collection_product_group;
                TRUNCATE b221_collection_relevance;
                TRUNCATE b221_hint_assessment;
                TRUNCATE b221_hint_collection;
                TRUNCATE b221_hint_comment_log;
                TRUNCATE b221_hint_intervention;
                TRUNCATE b221_hint_product_group;
                TRUNCATE bt_classification_log;
                TRUNCATE bt_hint_background_url;
                TRUNCATE bt_hint_bid;
                TRUNCATE bt_hint_evaluation;
                TRUNCATE bt_hint_jurisdiction;
                TRUNCATE bt_hint_lead;
                TRUNCATE bt_hint_log;
                TRUNCATE bt_hint_processing;
                TRUNCATE bt_hint_relevance;
                TRUNCATE bt_hint_text;
                TRUNCATE bt_hint_url;
                TRUNCATE bt_url_log;
                SET FOREIGN_KEY_CHECKS = 1;"




    gta_sql_multiple_queries(parsing.query,1)

    rm(lc.update)


  }

}
