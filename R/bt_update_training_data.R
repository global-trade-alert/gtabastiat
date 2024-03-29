bt_update_training_data=function(update.gta.words=T,
                                 db.connection=NULL){

  library(gtalibrary)
  library(gtasql)
  library(pool)
  library(RMariaDB)
  library(data.table)
  library(tidytext)

  if(is.null(db.connection)){
    database="ricardomain"
    gta_sql_pool_open(pool.name="pool",
                      db.title=database,
                      db.host = gta_pwd(database)$host,
                      db.name = gta_pwd(database)$name,
                      db.user = gta_pwd(database)$user,
                      db.password = gta_pwd(database)$password,
                      table.prefix = "bt_")

    db.connection="pool"

  }


  ## Source 1: import from ricardomain
  ### B221-based hints
  ## variables needed: bid, acting.agency, act.title.en, act.description.en, act.values, evaluation

  leads.core.columns = c("bid",
                         "acting.agency",
                         "act.title.en",
                         "act.description.en",
                         "act.values",
                         "collection.date",
                         "country.lead",
                         "evaluation")

  #the below sql generates a df that is the same as the old 'leads.core' using the NF tables we now use.
  print("Fetching data for leads.core2...")
  leads.core2=gta_sql_get_value(
    "SELECT DISTINCT btbid.bid, bthl.acting_agency, btht.hint_title, btht.hint_description, bthl.hint_values, bthl.registration_date, btjl.jurisdiction_name, bthl.hint_state_id
FROM bt_hint_log bthl,
	bt_hint_bid btbid,
	bt_hint_text btht,
	bt_hint_jurisdiction bthj,
	bt_jurisdiction_list btjl

WHERE bthl.hint_id = btbid.hint_id
AND bthl.hint_id = btht.hint_id
AND bthl.hint_id = bthj.hint_id

AND btbid.hint_id = bthj.hint_id
AND btbid.hint_id = btht.hint_id

AND bthj.hint_id = btht.hint_id

AND bthj.jurisdiction_id = btjl.jurisdiction_id

AND btht.language_id = 1
AND bthl.hint_state_id IN (7, 9);") #state 8 = too many from Mrs H, i.e. overfitting

  #reminder:
  #state 7 = processed by editors
  #state 8 = entered trash bin i.e. not relevant

  leads.core2$evaluation = leads.core2$hint.state.id == 7
  leads.core2$hint.state.id = NULL

  #rename the cols to match leads.core etc
  colnames(leads.core2) = leads.core.columns


  ### legacy leads.core (temporary patch)
  leads.core=gta_sql_get_value("SELECT bid, acting_agency, act_title_en, act_description_en, act_values, collection_date, country_lead, relevant FROM bt_leads_core_200421;")






  ## Source 2: Import from XLSX sent out to team members
  team.leads=read.csv("data/training/bid training.csv", sep=";")
  team.training=merge(leads.core, team.leads[,c("bid","evaluation")], by="bid")
  rm(team.leads)


  ## Source 3: import from gtamain
  ### leads
  database="gtamain"
  gta_sql_pool_open(pool.name="main.pool",
                    db.title=database,
                    db.host = gta_pwd(database)$host,
                    db.name = gta_pwd(database)$name,
                    db.user = gta_pwd(database)$user,
                    db.password = gta_pwd(database)$password,
                    table.prefix = "gta_")

  print("Fetching data from GTA leads...")

  gta.leads=gta_sql_get_value("SELECT *
                          FROM gta_leads;",
                          db.connection = "main.pool")

  setnames(gta.leads, "bastiat.id", "bid")


  #for reference: gta_leads removal reason ids
  # 1 - no policy
  # 2 - is TBT
  # 3 - is SPS
  # 4 - no commercial policy
  # 5 - not meaningful
  # 6 - fails RTT
  # 7 - no change
  # 8 - other, see comment
  # 9 - no credible action
  # 10 - update to existing intervention
  # 11 - duplicate of lead or GTA entry
  # 12 - not unilateral
  # 13 - useful
  # 14 - keep for EGI


  gta.training=merge(leads.core, subset(gta.leads, is.remove==1)[,c("bid","removal.reason")], by="bid")
  gta.training$evaluation=1
  gta.training$evaluation[gta.training$removal.reason %in% c(1,4,7,8,9)]=0
  gta.training$evaluation[gta.training$relevant==0]=0 #mark the state 9 (trash) leads as evaluated irrelevant
  gta.training$removal.reason=NULL
  gta.training=subset(gta.training, !bid %in% team.training$bid)
  rm(gta.leads)

  ## gta text, if called for
  if(update.gta.words){
    print("Fetching gta.words...")
    gta.sa=gta_sql_get_value("SELECT *
                          FROM gta_measure;",
                             db.connection = "main.pool")
    gta.int=gta_sql_get_value("SELECT *
                          FROM gta_intervention;",
                              db.connection = "main.pool")

  }

  gta_sql_pool_close("main.pool")




  ## Source 4: those manually removed via Bastiat (i.e. assigned relevance = 0 in processing)
  bt.training=subset(leads.core, relevant==0 &
                   grepl("(WTO)|(EU-SA)|(EIB)",bid, ignore.case = T)==F &
                   country.lead!="Vatican" &
                   grepl("dumpin",act.description.en, ignore.case = T)==F &
                   grepl("dumpin",act.title.en, ignore.case = T)==F &
                   collection.date>="2018-11-01" &
                   ! bid %in% c(gta.training$bid, team.training$bid))
  bt.training$evaluation=0



  ## PROCESSING
  ### joining the sources
  training=unique(rbind(leads.core2[,c("bid","evaluation","act.title.en","act.description.en", "act.values","acting.agency")],
                        team.training[,c("bid","evaluation","act.title.en","act.description.en", "act.values","acting.agency")],
                        gta.training[,c("bid","evaluation","act.title.en","act.description.en", "act.values","acting.agency")],
                        bt.training[,c("bid","evaluation","act.title.en","act.description.en", "act.values","acting.agency")]))

  rm(leads.core2, team.training, gta.training, bt.training)


  ### generating the text
  training$text=training$act.title.en
  training$text[is.na(training$act.description.en)==F]=paste(training$text[is.na(training$act.description.en)==F], training$act.description.en[is.na(training$act.description.en)==F], sep=" ")
  training$text[is.na(training$act.values)==F]=paste(training$text[is.na(training$act.values)==F], training$act.values[is.na(training$act.values)==F], sep=" ")
  training=training[,c("bid","evaluation","text","acting.agency")]

  #TODO update this. Probably using the master scraper list as most of them are official.
  ##for now I turn this off as there are so many acting agencies now, it feels very harsh to only have a few categories plus other.

  ### processing the acting agencies
  #source("code/daily/infrastructure/Bastiat base.R")
  #training$acting.agency[!training$acting.agency %in% agency.dummies]="Other"

  print("Removing boilerplate GTA text...")

  #scraped sources MUST NOT HAVE <BR> tags in them
  training$text = gsub("</? ?br>[\\s\\S]+", "", training$text)

  n.d.bid = sum(duplicated(training$bid))

  training = subset(training, !duplicated(bid))

  print(paste("removed", n.d.bid, "duplicated training leads based on bid"))


  print("new training data generated. Saving...")

  ### store new data.
  save(training, file="data/classifier/training data.Rdata")


  ## Creating file for GTA word frequencies
  if(update.gta.words){

    library(stopwords)

    title=unique(gta.sa[,c("id","title")])
    # title$title=iconv(title$title, from="ANSI_X3.4-1986", to="UTF-8")
    names(title)=c("sa.id","text")
    title$type="title"

    sources=unique(gta.sa[,c("id","source")])
    # sources$source=iconv(sources$source, from="ANSI_X3.4-1986", to="UTF-8")
    names(sources)=c("sa.id","text")
    sources$type="source"

    descriptions=unique(gta.int[,c("measure.id","description")])
    # descriptions$description=iconv(descriptions$description, from="ANSI_X3.4-1986", to="UTF-8")
    names(descriptions)=c("sa.id","text")
    descriptions$type="description"

    gta.text=rbind(title, sources, descriptions)

    #for testing
    #gta.text1 = gta.text


    #### cleaning
    # country names
    gta.text$text[gta.text$type=="title"]=gsub("^.*:","",gta.text$text[gta.text$type=="title"])

    # tags
    gta.text$text=gsub("<.*?>","", gta.text$text)

    # URLs
    gta.text$text=gsub("((http)|(www)).*?([ \\\"]|$)","", gta.text$text)

    #ad hoc full stop fix
    gta.text$text = gsub(pattern = "\\.", replacement = " ", gta.text$text)
    gta.text$text = gsub(pattern = "(\\W)|(_)", replacement = " ", gta.text$text)


    #UNNEST TOKENS - should be ready for tokenisation now
    gta.text=unnest_tokens(gta.text, word, text, drop=T)
    gta.text$word=as.character(gta.text$word)

    # short or long words
    gta.text=subset(gta.text, nchar(word)>=4 & nchar(word)<=30)

    # stopwords
    stop.en=get_stopwords()$word
    gta.text=subset(gta.text, ! tolower(word) %in% stop.en)

    # numbers
    gta.text=subset(gta.text, grepl("[0-9]+", word)==F)

    # nonsense words
    nonsense=c("nbsp", "quot", "january", "february","march","april","may","june","july","august","september","october","november","december", "ndquo", "rsquo", "ndash", "rdquo", "quot", "ldquo")
    gta.text=subset(gta.text, ! word %in% nonsense)



    # stats for all types
    ## simple frequencies
    gta.words=as.data.frame(table(gta.text$word))
    names(gta.words)=c("word","gta.freq.word")
    gta.words$gta.share.word=gta.words$gta.freq.word/max(gta.words$gta.freq.word)

    ## doc.share
    gta.words=merge(gta.words,aggregate(sa.id ~word, gta.text, function(x) length(unique(x))), by="word", all.x=T)
    setnames(gta.words, "sa.id","gta.freq.doc")
    gta.words$gta.share.doc=gta.words$gta.freq.doc/nrow(gta.sa)
    gta.words$text="all"

    ## looping over rest
    for(t in unique(gta.text$type)){
      ## simple frequencies
      gw=as.data.frame(table(subset(gta.text, type==t)$word))
      names(gw)=c("word","gta.freq.word")
      gw$gta.share.word=gw$gta.freq.word/max(gw$gta.freq.word)

      ## doc.share
      gw=merge(gw,aggregate(sa.id ~word, subset(gta.text, type==t), function(x) length(unique(x))), by="word", all.x=T)
      setnames(gw, "sa.id","gta.freq.doc")
      gw$gta.share.doc=gw$gta.freq.doc/nrow(gta.sa)
      gw$text=t
      gta.words=rbind(gta.words, gw)

      print(t)
    }

    print(paste("vocab construction completed. total words:", nrow(gta.words)))
    save(gta.words, file="data/classifier/gta words.Rdata")


  }



  if(is.null(db.connection)){

    gta_pool_close("pool")

  }


}
