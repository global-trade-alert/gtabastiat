
#' Send a prepared leads.core dataframe to the ricardo database
#'
#' see parameter explanations for some help.
#'
#' @param update.df the data frame containing leads to update. min 17 specifically named cols
#' @param exclude.by unique identifier for each row; 'primary key'
#' @param all.covid are these all to do with you know what
#' @param force.create
#' @param set.official is the officiality of the source designated in the df?
#' @param destination "b221" -> ricardo; "leads" -> main site; "parking" -> state 10; "5" -> state 5
#' @param incl.kanji does the source include CJK chars? (kanji, hanzi, kana, hangeul)
#' @param invoke.mrs.hudson use to disable mrs. h invocation
#' @param mrs.hudson.keep.results.ratio the ratio of results for Mrs. H to keep. eg. 0.9 keeps 90% of them.
#'
#' @return
#' @export
#'
#' @examples bt_leads_core_update("update.core", "bid", destination = "b221")
bt_leads_core_update = function(update.df=NULL,
                                exclude.by="act.url",
                                all.covid=F,
                                force.create=F,
                                set.official=T,
                                destination="b221",
                                incl.kanji=F,
                                invoke.mrs.hudson=T,
                                mrs.hudson.keep.results.ratio=0.95){

  #### for testing
  # load(file = "uc.Rdata")
  # lc.update = update.core
  # exclude.by="bid"
  # all.covid=F
  # force.create=F
  # set.official=T
  # destination="b221"
  # incl.kanji=F
  # invoke.mrs.hudson=T
  # mrs.hudson.keep.results.ratio=0.95


  if(! destination %in% c("parking","b221","leads", "5")){
    stop("Please choose destination value as either 'b221', 'parking', 'leads', or '5' for hint_state_5.")
  }

  library(lubridate)
  library(stringr)

  eval(parse(text=paste("lc.update<<-",update.df,sep="")))

  ## date check
  if(any(!is.Date(lc.update$act.date))){

    lc.update$act.date=as.Date(as.numeric(as.character(lc.update$act.date)), origin="1970-01-01")

  }

  if(nrow(lc.update)==0){
    print("All leads recorded already.")

  } else{

    ######### CLEANING & PREPARATION

    ## ad-hoc irrelevance decisions based on noisy sources
    noise.agencies=c("Radio Petrer 107,2 fm", "Business Post",
                     "Radio hong kong","Tek'n'Life", "Stand news",
                     "El Pas", "StrandGazetteDe", "TV Serien News",
                     "Corriente Alterna", "Munchen Zeitung", "hkcnews.com")

    lc.update$relevant[lc.update$acting.agency %in% noise.agencies]=0


    ### trade defence country correction

    #USA/Canada TD cases handled by USA/Canada editor: do not send these to Vatican.
    #change filter.usa.can.td = T to apply filter


    td.phrases = "anti[- ]?dumping|countervailing|anti[- ]?subsidy"

    #NB this filters any leads with USA/Can in the titles etc.
    #So lead like 'Indonesia implements AD duties on blah from the USA' will be send to Marshall Is.
    # With the new US FR RSS scraper we should be OK
    filter.usa.can.td = F

    if(filter.usa.can.td){

      north.american.countries = "(United States of America)|(Canada)"
      # Title (en)
      lc.update$country.lead[(grepl(td.phrases, lc.update$act.title.en, ignore.case = T)) &
                               (grepl(north.american.countries, lc.update$country.lead, ignore.case = T))] = "Marshall Islands"
      # Description (en)
      lc.update$country.lead[(grepl(td.phrases, lc.update$act.description.en, ignore.case = T)) &
                               (grepl(north.american.countries, lc.update$country.lead, ignore.case = T))] = "Marshall Islands"
      # Title (local lang)
      lc.update$country.lead[(grepl(td.phrases, lc.update$act.title.ll, ignore.case = T)) &
                               (grepl(north.american.countries, lc.update$country.lead, ignore.case = T))] = "Marshall Islands"
      # Description (local lang)
      lc.update$country.lead[(grepl(td.phrases, lc.update$act.description.ll, ignore.case = T)) &
                               (grepl(north.american.countries, lc.update$country.lead, ignore.case = T))] = "Marshall Islands"

    } else {

    # Title (en)
    lc.update$country.lead[(grepl(td.phrases, lc.update$act.title.en, ignore.case = T))] = "Vatican"
    # Description (en)
    lc.update$country.lead[(grepl(td.phrases, lc.update$act.description.en, ignore.case = T))] = "Vatican"
    # Title (local lang)
    lc.update$country.lead[(grepl(td.phrases, lc.update$act.title.ll, ignore.case = T))] = "Vatican"
    # Description (local lang)
    lc.update$country.lead[(grepl(td.phrases, lc.update$act.description.ll, ignore.case = T))] = "Vatican"

    }

    #Bastiat does a good job of tracking TD leads.
    # when official.td.leads.only == T, the leads from Ricardo (i.e. Google News) will not be sent to the editor

    official.td.leads.only = T

    if(official.td.leads.only){


      lc.update$relevant[(grepl(td.phrases, lc.update$act.title.en, ignore.case = T)) &
                             (grepl("GNEWS", lc.update$bid, ignore.case = T)) ] = 0

      lc.update$classify[(grepl(td.phrases, lc.update$act.title.en, ignore.case = T)) &
                           (grepl("GNEWS", lc.update$bid, ignore.case = T)) ] = 0

    }



    ## adding leads-checker columns
    lc.update$relevance.probability=NA
    lc.update$relevance.probability[lc.update$classify==0 & lc.update$relevant==1]=1
    lc.update$relevance.probability[lc.update$classify==0 & lc.update$relevant==0]=0

    ## act_url_official
    if(set.official){
      non.official=c("AGNET","CHN-GLOBAL", "CHN-KPMG", "HKTDC-PICK", "MENA-ZAWYA")
      lc.update$act.url.official=as.numeric(grepl(paste(non.official, collapse="|"), lc.update$bid, ignore.case = T))

    }


    ## force_create
    if(! "force.create" %in% names(lc.update)){
      lc.update$force.create=as.numeric(force.create)
    }



    ## is_covid

    if(all.covid){

      lc.update$is.covid=1

    } else {
      lc.update$is.covid=0
      lc.update$is.covid[grepl("( covid)|( corona)|(pandemic)", lc.update$act.title.en, ignore.case = T)]=1
      lc.update$is.covid[grepl("( covid)|( corona)|(pandemic)", lc.update$act.title.ll, ignore.case = T)]=1
      lc.update$is.covid[grepl("( covid)|( corona)|(pandemic)", lc.update$act.description.en, ignore.case = T)]=1
      lc.update$is.covid[grepl("( covid)|( corona)|(pandemic)", lc.update$act.description.ll, ignore.case = T)]=1
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


    ## URL cleanout

    #get standard regex for identifying urls
    regex_url = gtabastiat::regex_url

    lc.update$background.url=as.character(lc.update$background.url)
    lc.update$act.url=as.character(lc.update$act.url)

    lc.urls=data.frame()
    for(i in 1:nrow(lc.update)){

      ## checking act.url
      this.source=lc.update$act.url[i]

      if(is.na(nchar(this.source))|is.na(this.source)){
        ## source field is empty

        url.description=NA
        url.value="No direct source provided. Please check background URL in description or discard."

      } else {

        this.act.url=unique(bt_extract_url(this.source))

        url.description= str_replace_all(this.source, regex_url, "<br />\\[source URL\\])")

        url.description=gsub("\\s+"," ", gsub("(Viewed at:?)","", url.description))

        if(nchar(gsub("(<br />\\[source URL\\])|\\s+|","",url.description))<5){
          url.description=NA

        } else {

          lc.update$act.description.en[i]=paste(lc.update$act.description.en[i], "<br />The source was described as: '", url.description," '.", sep="")

          if(! is.na(lc.update$act.description.ll[i])){

            lc.update$act.description.ll[i]=paste(lc.update$act.description.ll[i], "<br />The source was described as: '", url.description," '.", sep="")
          }

        }


        if(length(this.act.url)==1 & any(is.na(this.act.url))){

          url.value="No specific URL was provided for this hint. All information is stated in the description."

        } else {

          url.value=this.act.url

        }



      }


      lc.urls=rbind(lc.urls,
                    data.frame(bid=lc.update$bid[i],
                               url=url.value,
                               url.description=url.description,
                               stringsAsFactors = F))

      ## checking background.
      this.source=lc.update$background.url[i]

      if(is.na(this.source)==F | is.na(nchar(this.source))==F ){


        this.act.url=unique(bt_extract_url(this.source))

        lc.update$act.description.en[i]=paste(lc.update$act.description.en[i], "<br /><br>More background information beyond the source can be found here: ", paste(this.act.url, collapse=" ;  "), sep="")

        if(! is.na(lc.update$act.description.ll[i])){

          lc.update$act.description.ll[i]=paste(lc.update$act.description.ll[i], "<br /><br>More background information beyond the source can be found here: ", paste(this.act.url, collapse=" ;  "), sep="")
        }


      }

    }


    lc.urls$url=gsub("^\\s+|\\s+$","",lc.urls$url)

    lc.update=merge(lc.update, unique(lc.urls[,c("bid","url")]), by="bid", all.x=T)
    lc.update$act.url=lc.update$url

    print("Checking against existing GTA state acts...")
    for(i in 1:nrow(lc.update)){
      print(i)

      if(grepl("EU-SA",lc.update$bid[i])==F){

        check.url=str_extract(lc.update$act.url[i], regex_url)

        if(is.na(check.url)==F){

          ## checking state acts
          #sa.ids=unique(subset(gta.sa, grepl(check.url, this.source, fixed = T))$id)
          sa.ids=unique(subset(gta.sa, grepl(check.url, gta.sa$source, fixed = T))$id)

          if(length(sa.ids)>0){

            lc.update$act.description.en[i]=paste(lc.update$act.description.en[i], "<br /><br />The following state acts have this lead URL among their sources:<br />",
                                                  paste(paste("https://www.globaltradealert.org/state-act/",sa.ids, sep=""), collapse="<br />"), sep="")

            if(! is.na(lc.update$act.description.ll[i])){

              lc.update$act.description.ll[i]=paste(lc.update$act.description.ll[i], "<br /><br /> The following state acts have this lead URL among their sources:<br />",
                                                    paste(paste("https://www.globaltradealert.org/state-act/",sa.ids, sep=""), collapse="<br />"), sep="")
            }

          }

          ## checking hints
          bt.url.ids=subset(bt.url, grepl(check.url, url, fixed = T))$url_id
          if(length(bt.url.ids)>0){

            hints=gta_sql_get_value(paste0("SELECT hint_id
                                            FROM bt_hint_url
                                            WHERE url_id IN (",paste(bt.url.ids, collapse=","),");"))
            hints=unique(hints[!is.na(hints)])

            if(length(hints)>0){

              lc.update$act.description.en[i]=paste(lc.update$act.description.en[i], "<br /><br /> The following hint IDs have this URL among their sources:<br />",
                                                    paste(hints, collapse=", "),".", sep="")

              if(! is.na(lc.update$act.description.ll[i])){

                lc.update$act.description.ll[i]=paste(lc.update$act.description.ll[i], "<br /><br /> The following hint IDs have this URL among their sources:<br />",
                                                      paste(hints, collapse=", "),".", sep="")
              }

            }
            rm(hints)
          }
        }


      }

    }


    #Mrs Hudson
    #Use on leads from google news search and reuters.
    if(invoke.mrs.hudson & all(grepl("(GNEWS)|(RTNEWS)", lc.update$bid))){

      #add col with the rating
      mrs.hudson.result = bt_estimate_news_leads(lc.update,
                                                 keep.results.ratio = mrs.hudson.keep.results.ratio,
                                                 binary.prediction = F,
                                                 return.both = T)
      #old
      #lc.update$mrs.hudson.rating = bt_estimate_news_leads(lc.update,
      #                                                     keep.results.ratio = mrs.hudson.keep.results.ratio)

      lc.update$mrs.hudson.rating = mrs.hudson.result$binary.prediction.result

      #in the SQL later on, leads that are is.covid = 0 and relevant = 0 are
      #sent directly to state 8
      lc.update$relevant[lc.update$mrs.hudson.rating==0] = 0
      lc.update$is.covid[lc.update$mrs.hudson.rating==0] = 0

      #ensure classify = 0 for good measure
      lc.update$classify[lc.update$mrs.hudson.rating==0] = 0

      #remove column
      lc.update$mrs.hudson.rating = NULL

      #add relevance probability
      lc.update$relevance.probability = mrs.hudson.result$raw.score

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
      stop(paste0("Error: not all columns of leads.core present in input. Missing: ",
                  paste(setdiff(names(leads.core)[! names(leads.core) %in% names(lc.update)],c("upload.id", "hint.id")), collapse=";")))
    }


    lc.update=lc.update[,names(lc.update)[names(lc.update) %in% names(leads.core)]]
    lc.update=lc.update[,names(leads.core)[names(leads.core) %in% names(lc.update)]]
    lc.update=unique(lc.update)

    ## splitting away mutli-links
    # multi.links=unique(names(table(lc.update$bid))[table(lc.update$bid)>1])
    # lc.update.multi=subset(lc.update, bid %in% multi.links)
    # save(lc.update.multi, file=paste0(gsub("\\D+", "", Sys.time()) ," RIC-COVID-MULTI only JF touches this.Rdata"))
    #
    # lc.update=subset(lc.update,! bid %in% multi.links)

    #save lc.update in its current R-friendly state for use later when processing the multi-links

    lc.update.copy = lc.update

    names(lc.update)=gsub("\\.","_",names(lc.update))


    gta_sql_multiple_queries("DELETE FROM bt_leads_core_temp WHERE 1 = 1;
                              DELETE FROM bt_leads_core WHERE 1 = 1;",1)

    ## cleaning apostrophes, etc.
    lc.update$act_description_en=gsub("\\'","",lc.update$act_description_en)
    lc.update$act_title_en=gsub("\\'","",lc.update$act_title_en)
    lc.update$act_description_ll=gsub("\\'","",lc.update$act_description_ll)
    lc.update$act_title_ll=gsub("\\'","",lc.update$act_title_ll)
    lc.update$acting_agency=gsub("\\'","",lc.update$acting_agency)

    lc.update$act_description_en=gsub("&lt;","<", lc.update$act_description_en)
    lc.update$act_description_en=gsub("&gt;",">", lc.update$act_description_en)
    lc.update$act_description_en=gsub("&amp;","&",lc.update$act_description_en)
    lc.update$act_description_ll=gsub("&lt;","<",lc.update$act_description_ll)
    lc.update$act_description_ll=gsub("&gt;",">",lc.update$act_description_ll)
    lc.update$act_description_ll=gsub("&amp;","&",lc.update$act_description_ll)



    ## converting variables to SQL values
    char.vars=c("bid", "country","country_lead", "act_title_en","act_description_en",
                "act_url", "email_language", "background_url","acting_agency")
    date.vars=c("collection_date","act_date")

    for(var in c(char.vars, date.vars)){

      eval(parse(text=paste0("lc.update$",var,"=paste0(\"'\",lc.update$",var,",\"'\")")))

    }

    kanji.candidates=c("act_description_ll","act_title_ll")

    for(var in kanji.candidates){

      if(incl.kanji){
        eval(parse(text=paste0("lc.update$",var,"=paste0(\"N'\",lc.update$",var,",\"'\")")))

      } else {

        eval(parse(text=paste0("lc.update$",var,"=paste0(\"'\",lc.update$",var,",\"'\")")))

      }


    }


    num.vars=c("act_url_official","relevant","act_values", "classify","relevance_probability", "is_covid","force_create")

    for(var in  num.vars ){

      eval(parse(text=paste0("lc.update$",var,"=as.numeric(lc.update$",var,")")))

    }

    row.values=c()

    for(i in 1:nrow(lc.update)){
      row.values=c(row.values,
                   paste0("(",paste(lc.update[i,], collapse=","),")"))
    }
    row.values=paste(row.values, collapse=",")


    row.values=gsub(",NA,",",NULL,",row.values)
    row.values=gsub("'NULL'","NULL",row.values)

    gta_sql_update_table(paste0("INSERT INTO bt_leads_core_temp(",paste(names(lc.update), collapse=","),")
                                 VALUES ",row.values,""))


    gta_sql_multiple_queries(paste0("INSERT INTO bt_leads_core(",paste(names(lc.update), collapse=","),")
                                 SELECT ",paste(names(lc.update), collapse=","),"
                                 FROM bt_leads_core_temp;
                                 DELETE FROM bt_leads_core_temp WHERE 1 = 1;"),1)


    # /* ASSUMES NO FORCE_CREATE! */
    #   DELETE bt_leads_core FROM bt_leads_core
    # JOIN bt_url_log ON bt_leads_core.act_url = bt_url_log.url WHERE bt_leads_core.force_create=0;
    #

    if(destination=="b221"){
      parsing.query="
               /* Avoiding duplicate hints from BT */
               /* REMOVES DUPLICATE BIDs*/
              DELETE bt_leads_core FROM bt_leads_core
              JOIN bt_hint_bid ON bt_leads_core.bid = bt_hint_bid.bid WHERE bt_leads_core.force_create=0;


              /* Writing into hint_log */
              INSERT INTO bt_hint_log(hint_type_id, hint_state_id, user_id, registration_date, acting_agency, hint_values, upload_id)
              SELECT 2 AS hint_type_id, 5 AS hint_state_id, 70 AS user_id, collection_date, acting_agency, act_values, upload_id
              FROM bt_leads_core;

              /* store hint_id & bid pairs*/
              UPDATE bt_leads_core blc
              JOIN bt_hint_log bhl
              ON bhl.upload_id = blc.upload_id
              SET blc.hint_id = bhl.hint_id;

              /* some odd NULL rows in first iteration*/
              DELETE FROM bt_leads_core
              WHERE collection_date IS NULL;

              /* in descending priority order */
              /* state 8 if  relevant = 0*/
              /* state 10 if old <=2019 */
              /* state 1 if no jur or covid = 1 */
              /* state 3 if relevant = 1 & not official & covid = 0 */
              /* state 5 otherwise (relevant = 1 official and covid = 0 */
              UPDATE bt_hint_log
              JOIN bt_leads_core ON bt_hint_log.hint_id = bt_leads_core.hint_id
              LEFT JOIN gta_jurisdiction_list ON bt_leads_core.country_lead = gta_jurisdiction_list.jurisdiction_name
              SET bt_hint_log.hint_state_id= (CASE WHEN bt_leads_core.relevant = 0 AND bt_leads_core.is_covid = 0 THEN 8
              				     WHEN bt_leads_core.act_date <= '2019-01-01' THEN 10
              				     WHEN gta_jurisdiction_list.jurisdiction_id IS NULL OR bt_leads_core.is_covid = 1 THEN 1
              				     WHEN gta_jurisdiction_list.jurisdiction_id IS NOT NULL AND bt_leads_core.relevant = 1 AND bt_leads_core.act_url_official = 0 AND bt_leads_core.is_covid = 0 THEN 3
              				     ELSE 5 END);

              /* Writing into classification log*/
              INSERT INTO bt_classification_log(hint_id, user_id, hint_state_id)
              SELECT hint_id, 70 AS user_id, 11 AS hint_state_id
              FROM bt_leads_core;

              /* adding act_urls */
              /** update bt_url_log **/
              INSERT INTO bt_url_log(url)
              SELECT DISTINCT url FROM
              (SELECT act_url AS url
              FROM bt_leads_core blc
              WHERE act_url IS NOT NULL
              AND NOT EXISTS (SELECT NULL FROM bt_url_log WHERE blc.act_url = bt_url_log.url)
              UNION
              SELECT background_url AS url
              FROM bt_leads_core blc
              WHERE background_url IS NOT NULL
              AND NOT EXISTS (SELECT NULL FROM bt_url_log WHERE blc.background_url = bt_url_log.url)) new_urls;

              /** update bt_hint_url **/
              INSERT INTO bt_hint_url(hint_id, url_id, url_type_id, classification_id, url_accepted, validation_classification)
              SELECT DISTINCT blc.hint_id, bul.url_id,
              (CASE WHEN blc.act_url_official = 1 THEN 1 ELSE 2 END) AS url_type_id, bcl.classification_id,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE 1 END) AS url_accepted,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE bcl.classification_id END) AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_url_log bul ON blc.act_url=bul.url
              JOIN bt_classification_log bcl ON blc.hint_id=bcl.hint_id
              JOIN bt_hint_log ON blc.hint_id = bt_hint_log.hint_id
              WHERE blc.act_url IS NOT NULL;

              /** update bt_hint_background_url **/
              INSERT INTO bt_hint_background_url(hint_id, url_id)
              SELECT DISTINCT hint_id, url_id
              FROM bt_leads_core blc
              JOIN bt_url_log bul
              ON blc.background_url=bul.url;

              /* Writing into bt_hint_jurisdiction */
              INSERT INTO bt_hint_jurisdiction(hint_id, classification_id, jurisdiction_id, jurisdiction_accepted, validation_classification)
              SELECT DISTINCT bcl.hint_id, bcl.classification_id, gta_jurisdiction_list.jurisdiction_id,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE 1 END) AS jurisdiction_accepted,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE bcl.classification_id END) AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              JOIN gta_jurisdiction_list ON gta_jurisdiction_list.jurisdiction_name = blc.country_lead
              JOIN bt_hint_log ON blc.hint_id = bt_hint_log.hint_id;

              /* Writing into bt_hint_date */
              INSERT INTO bt_hint_date(hint_id, date, date_type_id, classification_id, date_accepted, validation_classification)
              SELECT DISTINCT bcl.hint_id, act_date, 1 AS date_type_id, bcl.classification_id,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE 1 END) AS date_accepted,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE bcl.classification_id END) AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              JOIN bt_hint_log ON blc.hint_id = bt_hint_log.hint_id;


              /* bt_hint_bid */
              INSERT INTO bt_hint_bid (hint_id, bid)
              SELECT DISTINCT hint_id, bid
              FROM bt_leads_core
              WHERE bid IS NOT NULL;

              /* bt_hint_relevance */
              INSERT INTO bt_hint_relevance (hint_id, classification_id, relevance, relevance_probability, relevance_accepted, validation_classification)
              SELECT DISTINCT bcl.hint_id, bcl.classification_id, blc.relevant AS relevance, blc.relevance_probability,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE 1 END) AS relevance_accepted,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE bcl.classification_id END) AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              JOIN bt_hint_log ON blc.hint_id = bt_hint_log.hint_id
              WHERE blc.relevant IS NOT NULL;

              /* Writing into bt_hint_text*/
              /** English **/
              INSERT INTO bt_hint_text(hint_id, hint_title, hint_description, language_id, classification_id, description_accepted, validation_classification)
              SELECT DISTINCT * FROM
              (SELECT blc.hint_id, (CASE WHEN act_title_en IS NULL THEN '[hint without title]' ELSE act_title_en END) AS hint_title, act_description_en AS hint_description, 1 AS language_id, bcl.classification_id, 1 AS description_accepted, bcl.classification_id AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              WHERE act_title_en != 'NA'
              OR act_description_en != 'NA'
              UNION
              SELECT blc.hint_id, act_title_ll AS hint_title, act_description_ll AS hint_description, 2 AS language_id, bcl.classification_id, 1 AS description_accepted, bcl.classification_id AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              WHERE act_title_ll != 'NA'
              OR act_description_ll != 'NA') hint_text_entries;

              /* cleaning up */
              DELETE FROM bt_leads_core WHERE 1 = 1;"

    }

    if(destination=="parking"){

      parsing.query="
               /* Avoiding duplicate hints from BT */
               /* REMOVES DUPLICATE BIDs*/
              DELETE bt_leads_core FROM bt_leads_core
              JOIN bt_hint_bid ON bt_leads_core.bid = bt_hint_bid.bid WHERE bt_leads_core.force_create=0;


              /* Writing into hint_log */
              INSERT INTO bt_hint_log(hint_type_id, hint_state_id, user_id, registration_date, acting_agency, hint_values, upload_id)
              SELECT 2 AS hint_type_id, 10 AS hint_state_id, 70 AS user_id, collection_date, acting_agency, act_values, upload_id
              FROM bt_leads_core;

              /* store hint_id & bid pairs*/
              UPDATE bt_leads_core blc
              JOIN bt_hint_log bhl
              ON bhl.upload_id = blc.upload_id
              SET blc.hint_id = bhl.hint_id;

              UPDATE bt_hint_log
              JOIN bt_leads_core ON bt_leads_core.hint_id = bt_hint_log.hint_id AND is_covid = 1
              SET hint_type_id=2;

              /* some odd NULL rows in first iteration*/
              DELETE FROM bt_leads_core
              WHERE collection_date IS NULL;

              /* Writing into classification log*/
              INSERT INTO bt_classification_log(hint_id, user_id, hint_state_id)
              SELECT hint_id, 70 AS user_id, 10 AS hint_state_id
              FROM bt_leads_core;

              /* adding act_urls */
              /** update bt_url_log **/
              INSERT INTO bt_url_log(url)
              SELECT DISTINCT url FROM
              (SELECT act_url AS url
              FROM bt_leads_core blc
              WHERE act_url IS NOT NULL
              AND NOT EXISTS (SELECT NULL FROM bt_url_log WHERE blc.act_url = bt_url_log.url)
              UNION
              SELECT background_url AS url
              FROM bt_leads_core blc
              WHERE background_url IS NOT NULL
              AND NOT EXISTS (SELECT NULL FROM bt_url_log WHERE blc.background_url = bt_url_log.url)) new_urls;

              /** update bt_hint_url **/
              INSERT INTO bt_hint_url(hint_id, url_id, url_type_id, classification_id, url_accepted, validation_classification)
              SELECT DISTINCT blc.hint_id, bul.url_id,
              (CASE WHEN blc.act_url_official = 1 THEN 1 ELSE 2 END) AS url_type_id, bcl.classification_id,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE 1 END) AS url_accepted,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE bcl.classification_id END) AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_url_log bul ON blc.act_url=bul.url
              JOIN bt_classification_log bcl ON blc.hint_id=bcl.hint_id
              JOIN bt_hint_log ON blc.hint_id = bt_hint_log.hint_id
              WHERE blc.act_url IS NOT NULL;

              /** update bt_hint_background_url **/
              INSERT INTO bt_hint_background_url(hint_id, url_id)
              SELECT DISTINCT hint_id, url_id
              FROM bt_leads_core blc
              JOIN bt_url_log bul
              ON blc.background_url=bul.url;

              /* Writing into bt_hint_jurisdiction */
              INSERT INTO bt_hint_jurisdiction(hint_id, classification_id, jurisdiction_id, jurisdiction_accepted, validation_classification)
              SELECT DISTINCT bcl.hint_id, bcl.classification_id, gta_jurisdiction_list.jurisdiction_id,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE 1 END) AS jurisdiction_accepted,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE bcl.classification_id END) AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              JOIN gta_jurisdiction_list ON gta_jurisdiction_list.jurisdiction_name = blc.country_lead
              JOIN bt_hint_log ON blc.hint_id = bt_hint_log.hint_id;

              /* Writing into bt_hint_date */
              INSERT INTO bt_hint_date(hint_id, date, date_type_id, classification_id, date_accepted, validation_classification)
              SELECT DISTINCT bcl.hint_id, act_date, 1 AS date_type_id, bcl.classification_id,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE 1 END) AS date_accepted,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE bcl.classification_id END) AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              JOIN bt_hint_log ON blc.hint_id = bt_hint_log.hint_id;

              /* bt_hint_bid */
              INSERT INTO bt_hint_bid (hint_id, bid)
              SELECT DISTINCT hint_id, bid
              FROM bt_leads_core
              WHERE bid IS NOT NULL;

              /* bt_hint_relevance */
              INSERT INTO bt_hint_relevance (hint_id, classification_id, relevance, relevance_probability, relevance_accepted, validation_classification)
              SELECT DISTINCT bcl.hint_id, bcl.classification_id, blc.relevant AS relevance, blc.relevance_probability,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE 1 END) AS relevance_accepted,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE bcl.classification_id END) AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              JOIN bt_hint_log ON blc.hint_id = bt_hint_log.hint_id
              WHERE blc.relevant IS NOT NULL;

              /* Writing into bt_hint_text*/
              /** English **/
              INSERT INTO bt_hint_text(hint_id, hint_title, hint_description, language_id, classification_id, description_accepted, validation_classification)
              SELECT DISTINCT * FROM
              (SELECT blc.hint_id, (CASE WHEN act_title_en IS NULL THEN '[hint without title]' ELSE act_title_en END) AS hint_title, act_description_en AS hint_description, 1 AS language_id, bcl.classification_id, 1 AS description_accepted, bcl.classification_id AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              WHERE act_title_en != 'NA'
              OR act_description_en != 'NA'
              UNION
              SELECT blc.hint_id, act_title_ll AS hint_title, act_description_ll AS hint_description, 2 AS language_id, bcl.classification_id, 1 AS description_accepted, bcl.classification_id AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              WHERE act_title_ll != 'NA'
              OR act_description_ll != 'NA') hint_text_entries;

              /* cleaning up */
              DELETE FROM bt_leads_core WHERE 1 = 1;"

    }

    if(destination=="5"){

      parsing.query="
               /* Avoiding duplicate hints from BT */
               /* REMOVES DUPLICATE BIDs*/
              DELETE bt_leads_core FROM bt_leads_core
              JOIN bt_hint_bid ON bt_leads_core.bid = bt_hint_bid.bid WHERE bt_leads_core.force_create=0;


              /* Writing into hint_log */
              INSERT INTO bt_hint_log(hint_type_id, hint_state_id, user_id, registration_date, acting_agency, hint_values, upload_id)
              SELECT 2 AS hint_type_id, 5 AS hint_state_id, 70 AS user_id, collection_date, acting_agency, act_values, upload_id
              FROM bt_leads_core;

              /* store hint_id & bid pairs*/
              UPDATE bt_leads_core blc
              JOIN bt_hint_log bhl
              ON bhl.upload_id = blc.upload_id
              SET blc.hint_id = bhl.hint_id;

              UPDATE bt_hint_log
              JOIN bt_leads_core ON bt_leads_core.hint_id = bt_hint_log.hint_id AND is_covid = 1
              SET hint_type_id=2;

              /* some odd NULL rows in first iteration*/
              DELETE FROM bt_leads_core
              WHERE collection_date IS NULL;

              /* Writing into classification log*/
              INSERT INTO bt_classification_log(hint_id, user_id, hint_state_id)
              SELECT hint_id, 70 AS user_id, 10 AS hint_state_id
              FROM bt_leads_core;

              /* adding act_urls */
              /** update bt_url_log **/
              INSERT INTO bt_url_log(url)
              SELECT DISTINCT url FROM
              (SELECT act_url AS url
              FROM bt_leads_core blc
              WHERE act_url IS NOT NULL
              AND NOT EXISTS (SELECT NULL FROM bt_url_log WHERE blc.act_url = bt_url_log.url)
              UNION
              SELECT background_url AS url
              FROM bt_leads_core blc
              WHERE background_url IS NOT NULL
              AND NOT EXISTS (SELECT NULL FROM bt_url_log WHERE blc.background_url = bt_url_log.url)) new_urls;

              /** update bt_hint_url **/
              INSERT INTO bt_hint_url(hint_id, url_id, url_type_id, classification_id, url_accepted, validation_classification)
              SELECT DISTINCT blc.hint_id, bul.url_id,
              (CASE WHEN blc.act_url_official = 1 THEN 1 ELSE 2 END) AS url_type_id, bcl.classification_id,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE 1 END) AS url_accepted,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE bcl.classification_id END) AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_url_log bul ON blc.act_url=bul.url
              JOIN bt_classification_log bcl ON blc.hint_id=bcl.hint_id
              JOIN bt_hint_log ON blc.hint_id = bt_hint_log.hint_id
              WHERE blc.act_url IS NOT NULL;

              /** update bt_hint_background_url **/
              INSERT INTO bt_hint_background_url(hint_id, url_id)
              SELECT DISTINCT hint_id, url_id
              FROM bt_leads_core blc
              JOIN bt_url_log bul
              ON blc.background_url=bul.url;

              /* Writing into bt_hint_jurisdiction */
              INSERT INTO bt_hint_jurisdiction(hint_id, classification_id, jurisdiction_id, jurisdiction_accepted, validation_classification)
              SELECT DISTINCT bcl.hint_id, bcl.classification_id, gta_jurisdiction_list.jurisdiction_id,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE 1 END) AS jurisdiction_accepted,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE bcl.classification_id END) AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              JOIN gta_jurisdiction_list ON gta_jurisdiction_list.jurisdiction_name = blc.country_lead
              JOIN bt_hint_log ON blc.hint_id = bt_hint_log.hint_id;

              /* Writing into bt_hint_date */
              INSERT INTO bt_hint_date(hint_id, date, date_type_id, classification_id, date_accepted, validation_classification)
              SELECT DISTINCT bcl.hint_id, act_date, 1 AS date_type_id, bcl.classification_id,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE 1 END) AS date_accepted,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE bcl.classification_id END) AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              JOIN bt_hint_log ON blc.hint_id = bt_hint_log.hint_id;

              /* bt_hint_bid */
              INSERT INTO bt_hint_bid (hint_id, bid)
              SELECT DISTINCT hint_id, bid
              FROM bt_leads_core
              WHERE bid IS NOT NULL;

              /* bt_hint_relevance */
              INSERT INTO bt_hint_relevance (hint_id, classification_id, relevance, relevance_probability, relevance_accepted, validation_classification)
              SELECT DISTINCT bcl.hint_id, bcl.classification_id, blc.relevant AS relevance, blc.relevance_probability,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE 1 END) AS relevance_accepted,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE bcl.classification_id END) AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              JOIN bt_hint_log ON blc.hint_id = bt_hint_log.hint_id
              WHERE blc.relevant IS NOT NULL;

              /* Writing into bt_hint_text*/
              /** English **/
              INSERT INTO bt_hint_text(hint_id, hint_title, hint_description, language_id, classification_id, description_accepted, validation_classification)
              SELECT DISTINCT * FROM
              (SELECT blc.hint_id, (CASE WHEN act_title_en IS NULL THEN '[hint without title]' ELSE act_title_en END) AS hint_title, act_description_en AS hint_description, 1 AS language_id, bcl.classification_id, 1 AS description_accepted, bcl.classification_id AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              WHERE act_title_en != 'NA'
              OR act_description_en != 'NA'
              UNION
              SELECT blc.hint_id, act_title_ll AS hint_title, act_description_ll AS hint_description, 2 AS language_id, bcl.classification_id, 1 AS description_accepted, bcl.classification_id AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              WHERE act_title_ll != 'NA'
              OR act_description_ll != 'NA') hint_text_entries;

              /* cleaning up */
              DELETE FROM bt_leads_core WHERE 1 = 1;"

    }


    if(destination=="leads"){

      parsing.query="
               /* Avoiding duplicate hints from BT */
               /* REMOVES DUPLICATE BIDs*/
              DELETE bt_leads_core FROM bt_leads_core
              JOIN bt_hint_bid ON bt_leads_core.bid = bt_hint_bid.bid WHERE bt_leads_core.force_create=0;


              /* Writing into hint_log */
              INSERT INTO bt_hint_log(hint_type_id, hint_state_id, user_id, registration_date, acting_agency, hint_values, upload_id)
              SELECT 2 AS hint_type_id, 5 AS hint_state_id, 70 AS user_id, collection_date, acting_agency, act_values, upload_id
              FROM bt_leads_core;

              /* store hint_id & bid pairs*/
              UPDATE bt_leads_core blc
              JOIN bt_hint_log bhl
              ON bhl.upload_id = blc.upload_id
              SET blc.hint_id = bhl.hint_id;

              UPDATE bt_hint_log
              JOIN bt_leads_core ON bt_leads_core.hint_id = bt_hint_log.hint_id AND is_covid = 1
              SET hint_type_id=2;

              /* some odd NULL rows in first iteration*/
              DELETE FROM bt_leads_core
              WHERE collection_date IS NULL;

              /* in descending priority order */
              /* state 8 if  relevant = 0*/
              /* state 10 if old <=2019 */
              /* state 1 if no jur */
              /* else state 5 */
              UPDATE bt_hint_log
              JOIN bt_leads_core ON bt_hint_log.hint_id = bt_leads_core.hint_id
              LEFT JOIN gta_jurisdiction_list ON bt_leads_core.country_lead = gta_jurisdiction_list.jurisdiction_name
              SET bt_hint_log.hint_state_id= (CASE WHEN bt_leads_core.relevant = 0 AND bt_leads_core.is_covid = 0 THEN 8
              				     WHEN bt_leads_core.act_date <= '2019-01-01' THEN 10
              				     WHEN gta_jurisdiction_list.jurisdiction_id IS NULL THEN 1
              				     ELSE 5 END);

              /* Writing into classification log*/
              INSERT INTO bt_classification_log(hint_id, user_id, hint_state_id)
              SELECT hint_id, 70 AS user_id, 11 AS hint_state_id
              FROM bt_leads_core;

              /* adding act_urls */
              /** update bt_url_log **/
              INSERT INTO bt_url_log(url)
              SELECT DISTINCT url FROM
              (SELECT act_url AS url
              FROM bt_leads_core blc
              WHERE act_url IS NOT NULL
              AND NOT EXISTS (SELECT NULL FROM bt_url_log WHERE blc.act_url = bt_url_log.url)
              UNION
              SELECT background_url AS url
              FROM bt_leads_core blc
              WHERE background_url IS NOT NULL
              AND NOT EXISTS (SELECT NULL FROM bt_url_log WHERE blc.background_url = bt_url_log.url)) new_urls;

              /** update bt_hint_url **/
              INSERT INTO bt_hint_url(hint_id, url_id, url_type_id, classification_id, url_accepted, validation_classification)
              SELECT DISTINCT blc.hint_id, bul.url_id,
              (CASE WHEN blc.act_url_official = 1 THEN 1 ELSE 2 END) AS url_type_id, bcl.classification_id,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE 1 END) AS url_accepted,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE bcl.classification_id END) AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_url_log bul ON blc.act_url=bul.url
              JOIN bt_classification_log bcl ON blc.hint_id=bcl.hint_id
              JOIN bt_hint_log ON blc.hint_id = bt_hint_log.hint_id
              WHERE blc.act_url IS NOT NULL;

              /** update bt_hint_background_url **/
              INSERT INTO bt_hint_background_url(hint_id, url_id)
              SELECT DISTINCT hint_id, url_id
              FROM bt_leads_core blc
              JOIN bt_url_log bul
              ON blc.background_url=bul.url;

              /* Writing into bt_hint_jurisdiction */
              INSERT INTO bt_hint_jurisdiction(hint_id, classification_id, jurisdiction_id, jurisdiction_accepted, validation_classification)
              SELECT DISTINCT bcl.hint_id, bcl.classification_id, gta_jurisdiction_list.jurisdiction_id,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE 1 END) AS jurisdiction_accepted,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE bcl.classification_id END) AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              JOIN gta_jurisdiction_list ON gta_jurisdiction_list.jurisdiction_name = blc.country_lead
              JOIN bt_hint_log ON blc.hint_id = bt_hint_log.hint_id;

              /* Writing into bt_hint_date */
              INSERT INTO bt_hint_date(hint_id, date, date_type_id, classification_id, date_accepted, validation_classification)
              SELECT DISTINCT bcl.hint_id, act_date, 1 AS date_type_id, bcl.classification_id,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE 1 END) AS date_accepted,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE bcl.classification_id END) AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              JOIN bt_hint_log ON blc.hint_id = bt_hint_log.hint_id;

              /* bt_hint_bid */
              INSERT INTO bt_hint_bid (hint_id, bid)
              SELECT DISTINCT hint_id, bid
              FROM bt_leads_core
              WHERE bid IS NOT NULL;

              /* bt_hint_relevance */
              INSERT INTO bt_hint_relevance (hint_id, classification_id, relevance, relevance_probability, relevance_accepted, validation_classification)
              SELECT DISTINCT bcl.hint_id, bcl.classification_id, blc.relevant AS relevance, blc.relevance_probability,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE 1 END) AS relevance_accepted,
              (CASE WHEN bt_hint_log.hint_state_id = 1 THEN NULL ELSE bcl.classification_id END) AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              JOIN bt_hint_log ON blc.hint_id = bt_hint_log.hint_id
              WHERE blc.relevant IS NOT NULL;

              /* Writing into bt_hint_text*/
              /** English **/
              INSERT INTO bt_hint_text(hint_id, hint_title, hint_description, language_id, classification_id, description_accepted, validation_classification)
              SELECT DISTINCT * FROM
              (SELECT blc.hint_id, (CASE WHEN act_title_en IS NULL THEN '[hint without title]' ELSE act_title_en END) AS hint_title, act_description_en AS hint_description, 1 AS language_id, bcl.classification_id, 1 AS description_accepted, bcl.classification_id AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              WHERE act_title_en != 'NA'
              OR act_description_en != 'NA'
              UNION
              SELECT blc.hint_id, act_title_ll AS hint_title, act_description_ll AS hint_description, 2 AS language_id, bcl.classification_id, 1 AS description_accepted, bcl.classification_id AS validation_classification
              FROM bt_leads_core blc
              JOIN bt_classification_log bcl ON blc.hint_id = bcl.hint_id
              WHERE act_title_ll != 'NA'
              OR act_description_ll != 'NA') hint_text_entries;

              /* cleaning up */
              DELETE FROM bt_leads_core WHERE 1 = 1;"

    }



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

    print("SINGLE-LINK UPLOAD complete")

    #get lc.update back in its normal formatting (w/o vars surrounded by apostrophes etc.)
    lc.update = lc.update.copy
    rm(lc.update.copy)

    ## assign collections to multi-link hints
    multi.links=unique(names(table(lc.update$bid))[table(lc.update$bid)>1])

    if(length(multi.links)>1){


      source("https://raw.githubusercontent.com/global-trade-alert/ricardo/master/apps/b221/functions/b221_process_collections.R")


      lc.update=subset(lc.update, bid %in% multi.links)

      for(new.col in unique(lc.update$bid)){

        col.country=unique(lc.update$country[lc.update$bid==new.col])

        col.name=paste0(col.country, ": ")

        col.title=unique(lc.update$act.title.en[lc.update$bid==new.col])

        if(! is.na(col.title)){

          if(nchar(col.title)>50){

            col.name=paste0(col.name, substr(col.title, 1,50),"... ")

          } else {

            col.name=paste0(col.name, col.title)
          }

        } else {

          col.title=unique(lc.update$act.title.ll[lc.update$bid==new.col])

          if(! is.na(col.title)){

            if(nchar(col.title)>50){

              col.name=paste0(col.name, substr(col.title, 1,50),"... ")

            } else {

              col.name=paste0(col.name, col.title)
            }

          } else {

            col.title=unique(lc.update$act.description.en[lc.update$bid==new.col])


            if(! is.na(col.title)){

              if(nchar(col.title)>50){

                col.name=paste0(col.name, substr(col.title, 1,50),"... ")

              } else {

                col.name=paste0(col.name, col.title)
              }

            } else {

              col.title=unique(lc.update$act.description.ll[lc.update$bid==new.col])

              if(! is.na(col.title)){

                if(nchar(col.title)>50){

                  col.name=paste0(col.name, substr(col.title, 1,50),"... ")

                } else {

                  col.name=paste0(col.name, col.title)
                }

              } else {

                col.name=paste0(col.name, "AUTO-GENERATED COLLECTION, PLEASE CORRECT")



              }



            }


          }



        }

        Sys.setlocale("LC_ALL","English")




        col.name=paste0(col.country,": ", col.title," (",format(unique(lc.update$act.date[lc.update$bid==new.col]),"%B %Y") ,")")

        col.hints=gta_sql_get_value(paste0("SELECT hint_id
                                 FROM bt_hint_bid
                                 WHERE bid IN (",paste(paste0("'",new.col,"'"), collapse=", "),");"))

        if(all(is.na(col.hints))){
          stop(paste0("No hints for this BID: ", new.col))
        }

        b221_process_collections_hints(is.freelancer = FALSE,
                                       user.id = 70,
                                       new.collection.name = col.name,
                                       collection.id = NULL,
                                       hints.id = col.hints,
                                       country = col.country,
                                       product = NULL,
                                       intervention = NULL,
                                       assessment = NULL,
                                       relevance = NULL,
                                       collection.unchanged = F,
                                       starred.hint.id = NA,
                                       empty.attributes = T)


      }



    }






    rm(lc.update)

    print("FULL UPLOAD complete")


  }

}
