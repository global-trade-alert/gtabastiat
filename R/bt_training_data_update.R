bt_training_data_update=function(update.gta.words=T,
                                 db.connection=NULL){

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


  ## lead core &  evaluations
  leads.core=gta_sql_get_value("SELECT *
                             FROM bt_leads_core;",
                               db.connection = db.connection)
  leads.core$evaluation=NULL


  database="gtamain"
  gta_sql_pool_open(pool.name="main.pool",
                    db.title=database,
                    db.host = gta_pwd(database)$host,
                    db.name = gta_pwd(database)$name,
                    db.user = gta_pwd(database)$user,
                    db.password = gta_pwd(database)$password,
                    table.prefix = "bt_")

  leads=gta_sql_get_value("SELECT *
                          FROM gta_leads;",
                          db.connection = "main.pool")

  if(update.gta.words){
    gta.sa=gta_sql_get_value("SELECT *
                          FROM gta_measure;",
                             db.connection = "main.pool")
    gta.int=gta_sql_get_value("SELECT *
                          FROM gta_intervention;",
                              db.connection = "main.pool")

  }

  setnames(leads, "bastiat.id", "bid")
  gta_sql_pool_close("main.pool")


  trained=read.csv("data/training/bid training.csv", sep=";")
  trained=merge(leads.core, trained[,c("bid","evaluation")], by="bid")

  training=merge(leads.core, subset(leads, is.remove==1)[,c("bid","removal.reason")], by="bid")
  training$evaluation=1
  training$evaluation[training$removal_reason=="IRREVELANT"]=0
  training$removal.reason=NULL

  training=rbind(subset(training, !bid %in% trained$bid), trained)
  rm(trained, leads)

  ## adding those removed by Bastiat
  removed=subset(leads.core, relevant==0 &
                   grepl("(WTO)|(EU-SA)|(EIB)",bid, ignore.case = T)==F &
                   country.lead!="Vatican" &
                   grepl("dumpin",act.description.en, ignore.case = T)==F &
                   grepl("dumpin",act.title.en, ignore.case = T)==F &
                   collection.date>="2018-11-01")
  removed$evaluation=0
  training=rbind(training, removed)
  rm(removed,leads.core)

  ## generating the text
  training$text=training$act.title.en
  training$text[is.na(training$act.description.en)==F]=paste(training$text[is.na(training$act.description.en)==F], training$act.description.en[is.na(training$act.description.en)==F], sep=" ")
  training$text[is.na(training$act.values)==F]=paste(training$text[is.na(training$act.values)==F], training$act.values[is.na(training$act.values)==F], sep=" ")

  ## processing the text
  training=unique(training[,c("bid","evaluation","text","acting.agency")])
  source("code/daily/infrastructure/Bastiat base.R")
  training$acting.agency[!training$acting.agency %in% agency.dummies]="Other"

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




}
