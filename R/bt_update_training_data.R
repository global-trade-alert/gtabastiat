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
                         "relevant")

  ### legacy leads.core (temporary patch)
  leads.core=gta_sql_get_value("SELECT bid, acting_agency, act_title_en, act_description_en, act_values, collection_date, country_lead, relevant FROM bt_leads_core_200421;")

  #the below sql generates a df that is the same as the old 'leads.core' using the NF tables we now use.
  print("Fetching data for leads.core...")
  leads.core2=gta_sql_get_value(
  "SELECT btbid.bid, bthl.acting_agency, btht.hint_title, btht.hint_description, bthl.hint_values, bthl.registration_date, btjl.jurisdiction_name, bthr.relevance
FROM bt_hint_log bthl,
	bt_hint_bid btbid,
	bt_hint_text btht,
	bt_hint_date bthd,
	bt_hint_jurisdiction bthj,
	bt_jurisdiction_list btjl,
	bt_hint_relevance bthr

WHERE bthl.hint_id = btbid.hint_id
AND bthl.hint_id = bthd.hint_id
AND bthl.hint_id = bthj.hint_id
AND bthl.hint_id = btht.hint_id
AND bthl.hint_id = bthr.hint_id

AND btbid.hint_id = bthd.hint_id
AND btbid.hint_id = bthj.hint_id
AND btbid.hint_id = btht.hint_id
AND btbid.hint_id = bthr.hint_id

AND bthd.hint_id = bthj.hint_id
AND bthd.hint_id = btht.hint_id
AND bthd.hint_id = bthr.hint_id

AND bthj.hint_id = btht.hint_id
AND bthj.hint_id = bthr.hint_id

AND btht.hint_id = bthr.hint_id

AND bthj.jurisdiction_id = btjl.jurisdiction_id

AND (bthl.hint_state_id = 7 OR bthl.hint_state_id = 9)
AND btht.language_id = 1;")

  #reminder:
  #state 7 = processed by editors
  #state 8 = entered trash bin i.e. not relevant

  #rename the cols to match leads.core
  colnames(leads.core2) = leads.core.columns


  leads.core = rbind(leads.core, leads.core2)

  rm(leads.core2)

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

  gta.training=merge(leads.core, subset(gta.leads, is.remove==1)[,c("bid","removal.reason")], by="bid")
  gta.training$evaluation=1
  gta.training$evaluation[gta.training$removal.reason=="IRREVELANT"]=0
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
  training=unique(rbind(team.training[,c("bid","evaluation","act.title.en","act.description.en", "act.values","acting.agency")],
                        gta.training[,c("bid","evaluation","act.title.en","act.description.en", "act.values","acting.agency")],
                        bt.training[,c("bid","evaluation","act.title.en","act.description.en", "act.values","acting.agency")]))
  rm(team.training, gta.training, bt.training)


  ### generating the text
  training$text=training$act.title.en
  training$text[is.na(training$act.description.en)==F]=paste(training$text[is.na(training$act.description.en)==F], training$act.description.en[is.na(training$act.description.en)==F], sep=" ")
  training$text[is.na(training$act.values)==F]=paste(training$text[is.na(training$act.values)==F], training$act.values[is.na(training$act.values)==F], sep=" ")
  training=training[,c("bid","evaluation","text","acting.agency")]

  ### processing the acting agencies
  source("code/daily/infrastructure/Bastiat base.R")
  training$acting.agency[!training$acting.agency %in% agency.dummies]="Other"

  ### store new data.
  save(training, file="data/classifier/training data.Rdata")


  ## Creating file for GTA word frequencies
  if(update.gta.words){

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


    #### cleaning
    # country names
    gta.text$text[gta.text$type=="title"]=gsub("^.*:","",gta.text$text[gta.text$type=="title"])

    # tags
    gta.text$text=gsub("<.*?>","", gta.text$text)
    # URLs
    gta.text$text=gsub("((http)|(www)).*?([ \\\"]|$)","", gta.text$text)
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
    nonsense=c("nbsp", "quot", "january", "february","march","april","may","june","july","august","september","october","november","december")
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


    save(gta.words, file="data/classifier/gta words.Rdata")


  }



  if(is.null(db.connection)){

    gta_pool_close("pool")

  }


}
