# Roxygen documentation

#' Bastiat, please process my text and create the model variables.
#'
#' @return A list including data frames for the estimation data, the word scores and a vector of variables.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

bt_create_estimation_data <- function(bid=NULL,
                                      evaluation=NULL,
                                      word.score=NULL,
                                      text=NULL,
                                      acting.agency=NULL,
                                      train.share=.82,
                                      detective.name=NULL,
                                      detective.number=NULL,
                                      for.training=T
                                     ) {
  #not mounting these packages here causes failure when bt is used elsewhere
  library(tidytext)
  library(dplyr)
  library(stats)
  library(data.table)
  library(caret)


  #!!!VERY IMPORTANT!!!
  #in R integer variables can only go up to ~+-2*10^9 which is exceeded later
  #storing our vars as doubles solves this problem
  #I was originally doing this ad hoc but it happens again and again, so here is a janky function.

  cc_df_col_int2dbl = function(tgt.df){
    #rooting out naughty integers
    converted = 0
    for(col in colnames(tgt.df)){

      if(typeof(tgt.df[,col]) == "integer"){

        tgt.df[,col] = as.numeric(tgt.df[,col])
        converted = converted + 1
      }

    }

    message(paste(converted, "cols converted to double"))

    return(tgt.df)
  }

  #wrapper function to preserve original structure
  cc_count_freq = function(tgt.df){

    return(
      tgt.df %>%
        count(word) %>%
        cc_df_col_int2dbl()
    )

  }


  detective.characteristics=bt_get_detective_characteristics(d.name=detective.name,
                                                             d.number=detective.number)


  ### word-level variables
  if(is.null(evaluation)){

    tf=data.frame(bid=bid,
                  text=text,
                  stringsAsFactors = F)

  }else{
    tf=data.frame(bid=bid,
                  evaluation=evaluation,
                  text=text,
                  stringsAsFactors = F)
  }

  #testing
  #tf1 = tf


  #VERY IMPORTANT:
  #REMOVE non alphanumerics from the tf. this is to alleviate problems caused by
  #errant bits of punctuation, html, mojibake, etc
  tf$text = gsub(pattern = "[^A-zÀ-ÿ]|_", #the regex engine in R treats _ as an alphanumeric, must include it explicitly
                 replacement = " ",
                 x = tf$text)

  train.split=sample(unique(tf$bid), ceiling(nrow(tf)*train.share))

  variables=detective.characteristics$variables
  estimation.variables=variables

  ## ensuring I have all acting agencies, if called for
  if(detective.characteristics$vars.incl.acting.agency){

    #commented out because there are so many AAs now it's impossible to train on all of them.
    # if(for.training){
    #   while(length(setdiff(agency.dummies, unique(acting.agency[which(tf$bid %in% train.split)])))>0){
    #     rm(train.split)
    #     train.split=sample(unique(tf$bid), ceiling(nrow(tf)*train.share))
    #     print("resplitting to ensure presence of all agencies")
    #   }
    #}
    estimation.variables=c(estimation.variables[!variables %in% "acting.agency"],agency.dummies.col.names)
  }

  #warning: this creates a very large df (1 billion rows+)
  tf=unnest_tokens(tf, word, text, drop=F)

  #### FEATURE CLEANING
  ### removing numbers: do you ? or just the tags.
  # tf$jibberish=apply(tf, 1, function(x) as.numeric(nchar(paste(unlist(str_extract_all(x[which(names(tf)=="word")], "\\w+")), collapse="")))/nchar(x[which(names(tf)=="word")]))
  tf$word=as.character(tf$word)
  tf=subset(tf, nchar(word)>=4 & nchar(word)<=30)

  #test
  #tf2 = tf

  ### EXPLORE UDPIPE

  #I worry as a significant part of our corpus is not in proper english, or not in english at all.

  # rake=keywords_rake(tf, "word", "bid", n_min=1, ngram_max = 4)
  # coll=keywords_collocation(tf, "word", "bid", n_min=2, ngram_max = 4)
  #
  # x <- udpipe_download_model(language = "english")
  # x$file_model
  # ud_english <- udpipe_load_model(x$file_model)
  #
  # e=as.data.frame(udpipe_annotate(ud_english, x = training$text[1], tagger = "default", parser = "default"))
  #

  ## CREATING THE METRICS
  if(!is.null(evaluation)){

    ## simple frequencies
    #word.freq=as.data.frame(table(tf$word))

    word.freq = cc_count_freq(tf)

    names(word.freq)=c("word","frequency.corpus")

    tf = merge(tf, word.freq, by="word", all.x=T)

    #word.freq=as.data.frame(table(tf$word[tf$evaluation==1]))

    word.freq = cc_count_freq(tf[tf$evaluation==1,])

    names(word.freq)=c("word","frequency.relevant")
    tf=merge(tf, word.freq, by="word", all.x=T)

    #word.freq=as.data.frame(table(tf$word[tf$evaluation==0]))

    word.freq = cc_count_freq(tf[tf$evaluation==1,])

    names(word.freq)=c("word","frequency.irrelevant")
    tf=merge(tf, word.freq, by="word", all.x=T)
    tf[is.na(tf)]=0




    #test
    #tf3=tf

    ### GINI ###

    ### WARNING: 23-04-2021
    ### THIS IS BROKEN due to integer overflow caused by all the massive dfs.
    ### this is because our datasets have grown!

    ### need to change this process to be less memory intensive. I think it needs a total redo.
    ### completely supersede with w2v maybe? will think about how to do this.

    # I could exclude features by GINI value. Does not seem to do much, so I skip it here.
    # If reconsidered, need to add those BIDs removed due to lack of high-GINI words into the estimation results by assigning 0/1 randomnly.

    # if("gini.normalised" %in% unique(c(variables, detective.characteristics$dtmatrix.metric))){

    #gini=as.data.frame(table(subset(tf, bid %in% train.split)$word)))

    gini = subset(tf, bid %in% train.split) %>%
      cc_count_freq()

    names(gini)=c("word","freq.total")


    #gini$freq.total = as.numeric(gini$freq.total)

    #gini.ir=as.data.frame(table(subset(tf, bid %in% train.split & evaluation==0)$word))

    gini.ir = subset(tf, bid %in% train.split & evaluation==0) %>%
      cc_count_freq()

    names(gini.ir)=c("word","freq.irrelevant")
    gini=merge(gini, gini.ir, by="word", all.x=T)

    #gini.ir=as.data.frame(table(subset(tf, bid %in% train.split & evaluation==1)$word))
    gini.ir = subset(tf, bid %in% train.split & evaluation==1) %>%
      cc_count_freq()

    names(gini.ir)=c("word","freq.relevant")

    gini=merge(gini, gini.ir, by="word", all.x=T)

    rm(gini.ir)
    gini[is.na(gini)]=0

    gini$gini.simple=(gini$freq.relevant/gini$freq.total)^2+(gini$freq.irrelevant/gini$freq.total)^2

    gini$gini.normalised.num=gini$freq.relevant/(gini$freq.total*length(unique(subset(tf, bid %in% train.split & evaluation==1)$bid)))
    gini$gini.normalised.denom=gini$freq.irrelevant/(gini$freq.total*length(unique(subset(tf, bid %in% train.split & evaluation==0)$bid)))
    gini$gini.normalised=(gini$gini.normalised.num/(gini$gini.normalised.num+ gini$gini.normalised.denom))^2 +
      (gini$gini.normalised.denom/(gini$gini.normalised.denom+ gini$gini.normalised.num))^2

    #the below was used to root out problems with overflow. I leave it here in case it happens again.
    # tst = c()
    # err = c()
    # wrn = c()
    # const = length(unique(subset(tf, bid %in% train.split & evaluation==0)$bid))
    # for(i in 1:nrow(tf)){
    #   tryCatch(expr={
    #     tst=c(tst,
    #           (as.numeric(gini$freq.total)[i]*as.numeric(const) )
    #     )
    #
    #
    #   },error = function(e){
    #     print(paste("error with:", i))
    #     err = c(err, i)
    #   },
    #   warning = function(w){
    #     print(paste("warn with:", i))
    #     err = c(wrn, i)
    #   }
    #   )
    # }


    # }

    ### delta
    # if(sum(as.numeric(grepl("gta.gini", unique(c(variables, detective.characteristics$dtmatrix.metric)))))>0){

    gta.gini.threshold=100
    nonsense=c("nbsp", "quot", "january", "february","march","april","may","june","july","august","september","october","november","december")

    gta.words=gtabastiat::gta.corpus %>%
      cc_df_col_int2dbl()
    gta.words$word = tolower(gta.words$word)

    stop.en=get_stopwords()$word

    gini.result=data.frame(word=character(),
                           gta.text=character(),
                           gta.gini.ir=numeric(),
                           gta.gini.re=numeric(),
                           gta.gini.delta=numeric(),
                           stringsAsFactors = F)

    ## loop over GTA text types (all, description, title, source)
    for(txt in unique(gta.words$text)){

      ## normalising gta frequency by vastly different total word count
      gta.words$gta.freq.word=round(gta.words$gta.freq.word/sum(gta.words$gta.freq.word)*nrow(subset(tf, ! word %in% stop.en)),0)

      ## irrelevant cases
      #gta.gini=as.data.frame(table(tolower(subset(tf, bid %in% train.split & evaluation==0)$word)))

      gta.gini = subset(tf, bid %in% train.split & evaluation==0) %>%
        cc_count_freq()

      names(gta.gini)=c("word","gta.freq.word")

      gta.gini=rbind(gta.gini, subset(gta.words, text==txt)[,c("word","gta.freq.word")])

      # removing words also removed from GTA corpus
      gta.gini=subset(gta.gini, ! tolower(word) %in% stop.en)
      gta.gini=subset(gta.gini, grepl("[0-9]+", word)==F)
      gta.gini=subset(gta.gini, ! word %in% nonsense)


      gta.gini=aggregate(gta.freq.word ~word, gta.gini, sum)
      setnames(gta.gini, "gta.freq.word","freq.total")

      #dbg
      #gta.gini1 = gta.gini

      #gta.gini.ir=as.data.frame(table(subset(tf, bid %in% train.split & evaluation==0)$word))

      gta.gini.ir = subset(tf, bid %in% train.split & evaluation==0) %>%
        cc_count_freq()

      names(gta.gini.ir)=c("word","freq.irrelevant")

      gta.gini=merge(gta.gini, gta.gini.ir, by="word", all.x=T)
      rm(gta.gini.ir)

      gta.gini=merge(gta.gini, subset(gta.words, text==txt)[,c("word", "gta.freq.word")],by="word",all.x=T)
      setnames(gta.gini, "gta.freq.word","freq.gta")
      gta.gini[is.na(gta.gini)]=0
      gta.gini=subset(gta.gini, freq.total>gta.gini.threshold)


      gta.gini$gta.gini.ir=(gta.gini$freq.gta/gta.gini$freq.total)^2+(gta.gini$freq.irrelevant/gta.gini$freq.total)^2

      gta.gini.delta=gta.gini[,c("word","gta.gini.ir")]

      ## relevant cases
      gta.gini=as.data.frame(table(tolower(subset(tf, bid %in% train.split & evaluation==1)$word)))
      names(gta.gini)=c("word","gta.freq.word")

      gta.gini=rbind(gta.gini, subset(gta.words, text==txt)[,c("word","gta.freq.word")])

      # removing words also removed from GTA corpus
      gta.gini=subset(gta.gini, ! tolower(word) %in% stop.en)
      gta.gini=subset(gta.gini, grepl("[0-9]+", word)==F)
      gta.gini=subset(gta.gini, ! word %in% nonsense)

      gta.gini=aggregate(gta.freq.word ~word, gta.gini, sum)
      setnames(gta.gini, "gta.freq.word","freq.total")

      #gta.gini.ir=as.data.frame(table(subset(tf, bid %in% train.split & evaluation==1)$word))

      gta.gini.ir = subset(tf, bid %in% train.split & evaluation==1) %>%
        cc_count_freq()

      names(gta.gini.ir)=c("word","freq.relevant")
      gta.gini=merge(gta.gini, gta.gini.ir, by="word", all.x=T)
      rm(gta.gini.ir)

      gta.gini=merge(gta.gini, subset(gta.words, text==txt)[,c("word", "gta.freq.word")],by="word",all.x=T)
      setnames(gta.gini, "gta.freq.word","freq.gta")
      gta.gini[is.na(gta.gini)]=0
      gta.gini=subset(gta.gini, freq.total>gta.gini.threshold)

      gta.gini$gta.gini.re=(gta.gini$freq.gta/gta.gini$freq.total)^2+(gta.gini$freq.relevant/gta.gini$freq.total)^2

      gta.gini.delta=merge(gta.gini.delta, gta.gini[,c("word","gta.gini.re")], by="word", all.x=T)
      gta.gini.delta$gta.gini.delta=gta.gini.delta$gta.gini.ir-gta.gini.delta$gta.gini.re
      gta.gini.delta$gta.text=txt

      gini.result=rbind(gini.result, gta.gini.delta[,names(gini.result)])
      print(txt)
    }

    gta.gini=gini.result
    rm(gini.result, gta.gini.delta)


    # }

    ### odds ratio
    # if(sum(as.numeric((c("odds.relevant","odds.irrelevant", "odds.ratio") %in% unique(c(variables, detective.characteristics$dtmatrix.metric)))))>0){

    #odds=as.data.frame(table(subset(tf, bid %in% train.split)$word))

    odds = subset(tf, bid %in% train.split) %>%
      cc_count_freq()

    names(odds)=c("word","freq.total")

    #odds.ir=as.data.frame(table(subset(tf, bid %in% train.split & evaluation==0)$word))

    odds.ir = subset(tf, bid %in% train.split & evaluation==0) %>%
      cc_count_freq()

    names(odds.ir)=c("word","freq.irrelevant")
    odds=merge(odds, odds.ir, by="word", all.x=T)

    #same variable name used for relevant results for memory saving - don't get confused
    #odds.ir=as.data.frame(table(subset(tf, bid %in% train.split & evaluation==1)$word))

    odds.ir = subset(tf, bid %in% train.split & evaluation==1) %>%
      cc_count_freq()

    names(odds.ir)=c("word","freq.relevant")
    odds=merge(odds, odds.ir, by="word", all.x=T)
    odds[is.na(odds)]=0
    rm(odds.ir)


    odds$odds.relevant=((odds$freq.relevant/sum(odds$freq.relevant))/(1-odds$freq.relevant/sum(odds$freq.relevant)))
    odds$odds.irrelevant=((odds$freq.irrelevant/sum(odds$freq.irrelevant))/(1-odds$freq.irrelevant/sum(odds$freq.irrelevant)))
    odds$odds.ratio=log(odds$odds.relevant/odds$odds.irrelevant)
    odds$odds.ratio[odds$odds.irrelevant==0]=max(odds$odds.ratio[odds$odds.irrelevant!=0])*1.1
    odds$odds.ratio[odds$odds.relevant==0]=min(odds$odds.ratio[odds$odds.relevant!=0])*1.1


    # }



    ### information gain
    # observations=length(unique(tf$bid))
    # prob.relevant=length(unique(tf$bid[tf$evaluation==1]))/length(unique(tf$bid))
    # prob.irrelevant=length(unique(tf$bid[tf$evaluation==0]))/length(unique(tf$bid))
    #
    # gain=data.frame(word=gini$word,
    #                 info.gain.relevant=NA,
    #                 info.gain.irrelevant=NA)
    #
    # for(i in 1:nrow(gain)){
    #   gain$info.gain.relevant[i]=prob.relevant-length(unique(tf$bid[tf$evaluation==1 & tf$word==gain$word[i]]))/length(unique(tf$bid[tf$word==gain$word[i]]))
    #   gain$info.gain.irrelevant[i]=prob.irrelevant-length(unique(tf$bid[tf$evaluation==0 & tf$word==gain$word[i]]))/length(unique(tf$bid[tf$word==gain$word[i]]))
    #   print(i)
    # }


    showcols = function(tgt.df){

      for(col in colnames(tgt.df)){
       print(typeof(tgt.df[,col]))
      }

    }

    #### FEATURE TRANSFORMATION
    ### word2vec, LSI, PLSA, NMF here, if you want.

    ### NMF
    # https://sites.google.com/site/mlshortcourse/home/data-sets/text-classification-in-r

    ### word frequencies
    #word.share.relevant=as.data.frame(table(subset(tf, evaluation==1 & bid %in% train.split)$word))
    word.share.relevant = subset(tf, evaluation==1 & bid %in% train.split) %>%
      cc_count_freq()

    #word.share.relevant$share=word.share.relevant$Freq/sum(word.share.relevant$Freq)
    word.share.relevant$share=word.share.relevant$n/sum(word.share.relevant$n)

    #word.share.irrelevant=as.data.frame(table(subset(tf, evaluation==0 & bid %in% train.split)$word))
    word.share.irrelevant = subset(tf, evaluation==0 & bid %in% train.split) %>%
      cc_count_freq()

    #word.share.irrelevant$share=word.share.irrelevant$Freq/sum(word.share.irrelevant$Freq)
    word.share.irrelevant$share=word.share.irrelevant$n/sum(word.share.irrelevant$n)

    ### document frequencies
    doc.share=aggregate(bid~word + evaluation, subset(tf, bid %in% train.split), function(x) length(unique(x)))

    doc.share$d.share=doc.share$bid/nrow(subset(tf, evaluation==1 & bid %in% train.split))
    doc.share$d.share[doc.share$evaluation==0] =doc.share$bid[doc.share$evaluation==0]/nrow(subset(tf, evaluation==0 & bid %in% train.split))

    #setnames(word.share.irrelevant, "Var1", "word")
    #setnames(word.share.relevant, "Var1", "word")


    ### word scores (tf-idf, tf-oidf)
    word.share.relevant=merge(word.share.relevant, subset(doc.share, evaluation==1)[,c("word","d.share")], by="word", all.x=T)
    word.share.relevant$score.r.idf=word.share.relevant$share/(word.share.relevant$d.share + min(word.share.relevant$d.share, na.rm = T))
    word.share.relevant$d.share=NULL
    word.share.relevant=merge(word.share.relevant, subset(doc.share, evaluation==0)[,c("word","d.share")], by="word", all.x=T)
    word.share.relevant$d.share[is.na(word.share.relevant$d.share)]=min(subset(doc.share, evaluation==0)$d.share, na.rm=T)
    word.share.relevant$score.r.oidf=word.share.relevant$share/(word.share.relevant$d.share + min(word.share.relevant$d.share, na.rm = T))

    word.share.irrelevant=merge(word.share.irrelevant, subset(doc.share, evaluation==0)[,c("word","d.share")], by="word", all.x=T)
    word.share.irrelevant$score.ir.idf=word.share.irrelevant$share/(word.share.irrelevant$d.share + min(word.share.irrelevant$d.share, na.rm = T))
    word.share.irrelevant$d.share=NULL
    word.share.irrelevant=merge(word.share.irrelevant, subset(doc.share, evaluation==1)[,c("word","d.share")], by="word", all.x=T)
    word.share.irrelevant$d.share[is.na(word.share.irrelevant$d.share)]=min(subset(doc.share, evaluation==1)$d.share, na.rm=T)
    word.share.irrelevant$score.ir.oidf=word.share.irrelevant$share/(word.share.irrelevant$d.share + min(word.share.irrelevant$d.share, na.rm = T))

    word.score=merge(word.share.relevant[,c("word", "score.r.idf","score.r.oidf")], word.share.irrelevant[,c("word", "score.ir.idf","score.ir.oidf")], by="word", all=T)
    word.score[is.na(word.score)]=0

    word.score$score.delta=word.score$score.r.idf-word.score$score.ir.idf
    word.score$score.delta.abs=abs(word.score$score.delta)
    word.score$exclusive.r=as.numeric(word.score$word %in% subset(word.score, score.ir.idf==0)$word)
    word.score$exclusive.ir=as.numeric(word.score$word %in% subset(word.score, score.r.idf==0)$word)


    ### ToDo: Adjust exclusiveness for probability of appearance (e.g. inverse tf-idf in own corpus (=it's really rare here, too))

    #### Dampening features
    ## transform by natural log or square-root to smooth outliers

    ## packing word score DF

    # if("gini.normalised" %in% unique(c(variables, detective.characteristics$dtmatrix.metric))){
      word.score=merge(word.score, gini[,c("word","gini.normalised")], by="word", all=T)
    # }

    for(gg in c("gta.gini.all","gta.gini.source","gta.gini.title","gta.gini.description")){
      # if(gg %in% unique(c(variables, detective.characteristics$dtmatrix.metric))){

      g.var=gsub("gta.gini.","",gg)
      word.score=merge(word.score, subset(gta.gini, gta.text==g.var)[,c("word","gta.gini.delta")], by="word", all=T)
      setnames(word.score, "gta.gini.delta",gg)
      print(gg)

      # }
    }

    # if(sum(as.numeric((c("odds.relevant","odds.irrelevant", "odds.ratio") %in% unique(c(variables, detective.characteristics$dtmatrix.metric)))))>0){
      word.score=merge(word.score, odds[,c("word","odds.relevant","odds.irrelevant", "odds.ratio" )], by="word", all=T)
    # }


    # if(length(intersect(c("gta.share.all","gta.share.source","gta.share.title", "gta.share.description"), unique(c(variables, detective.characteristics$dtmatrix.metric))))>0){

      gta.words=gtabastiat::gta.corpus %>%
        cc_df_col_int2dbl()

      #ws1 = word.score

      # for(gs in intersect(c("gta.share.all","gta.share.source","gta.share.title", "gta.share.description"), unique(c(variables, detective.characteristics$dtmatrix.metric)))){
      for(gs in c("gta.share.all","gta.share.source","gta.share.title", "gta.share.description")){
        g.var=gsub("gta.share.","",gs)
        word.score=merge(word.score, subset(gta.words, text==g.var)[,c("word","gta.share.word")], by="word", all=T)
        setnames(word.score, "gta.share.word",gs)
      }


    # }


  }


  #tf4 = tf

  #### GENERATING THE VARIABLES
  #### AGGRECGATE VARIABLES TO DOC LEVEL
  tf=merge(tf, word.score, by="word", all.x = T)

  tf.agg=aggregate(score.delta ~ bid, tf, sum)

  if(is.null(evaluation)==F){
    tf.agg=merge(tf.agg, unique(data.frame(bid=bid, evaluation=evaluation, stringsAsFactors = F)), by="bid", all.x=T)
  }

  #tfa1 = tf.agg

  setnames(tf.agg, "score.delta","delta.sum")
  tf.agg=merge(tf.agg, aggregate(score.delta ~ bid , tf, min), by="bid", all.x=T)
  setnames(tf.agg, "score.delta","delta.min")
  tf.agg=merge(tf.agg, aggregate(score.delta ~ bid , tf, max), by="bid", all.x=T)
  setnames(tf.agg, "score.delta","delta.max")
  tf.agg=merge(tf.agg, aggregate(score.r.idf ~ bid , tf, sum), by="bid", all.x=T)
  tf.agg=merge(tf.agg, aggregate(score.r.oidf ~ bid , tf, sum), by="bid", all.x=T)
  tf.agg=merge(tf.agg, aggregate(score.ir.idf ~ bid , tf, sum), by="bid", all.x=T)
  tf.agg=merge(tf.agg, aggregate(score.ir.oidf ~ bid , tf, sum), by="bid", all.x=T)
  tf.agg=merge(tf.agg, aggregate(exclusive.r ~ bid , tf, sum), by="bid", all.x=T)
  setnames(tf.agg, "exclusive.r","exclusive.r.sum")
  tf.agg=merge(tf.agg, aggregate(exclusive.r ~ bid , tf, max), by="bid", all.x=T)
  setnames(tf.agg, "exclusive.r","exclusive.r.max")
  tf.agg=merge(tf.agg, aggregate(exclusive.ir ~ bid , tf, sum), by="bid", all.x=T)
  setnames(tf.agg, "exclusive.ir","exclusive.ir.sum")
  tf.agg=merge(tf.agg, aggregate(exclusive.ir ~ bid , tf, max), by="bid", all.x=T)
  setnames(tf.agg, "exclusive.ir","exclusive.ir.max")


  for(var in intersect(estimation.variables, c("gta.share.all","gta.share.source","gta.share.title","gta.share.description","gta.gini.all","gta.gini.source","gta.gini.title","gta.gini.description","gini.normalised","odds.relevant","odds.irrelevant", "odds.ratio"))){

    eval(parse(text=paste("tf.agg=merge(tf.agg, aggregate(",var," ~ bid , tf, function(x) mean(x, na.rm=T)), by='bid', all.x=T)",sep="")))

  }


  ## adding aggregate variables, if called for
  ## text.level variables
  aggregate.variables=data.frame(bid=bid,
                                 text=text,
                                 stringsAsFactors = F)

  #av1 = aggregate.variables

  ##TODO I think this would work nicely as a w2v model.
  ## acting.agency
  if(detective.characteristics$vars.incl.acting.agency){
    aggregate.variables$acting.agency=acting.agency
    aggregate.variables$acting.agency=factor(aggregate.variables$acting.agency, levels=agency.dummies)
    aggregate.variables$acting.agency[is.na(aggregate.variables$acting.agency)]="Other"
    aa.dummies = as.data.frame(predict(caret::dummyVars(~ acting.agency, data = aggregate.variables), newdata = aggregate.variables))
    names(aa.dummies)=tolower(gsub(" ","",gsub("acting.agency\\.?","",names(aa.dummies))))
    aa.dummies$bid=bid

    if(any(! agency.dummies.col.names %in% names(aa.dummies))){
      for(miss.aa in setdiff(agency.dummies.col.names, names(aa.dummies))){
        eval(parse(text=paste("aa.dummies$",miss.aa,"=0",sep="")))      }

    }


    aggregate.variables$acting.agency=NULL
    aggregate.variables=merge(aggregate.variables, aa.dummies[,c("bid", agency.dummies.col.names)], by="bid", all.x=T)
    rm(aa.dummies)
  }

  ## is.td
  if(detective.characteristics$vars.incl.td){
    aggregate.variables$is.td=as.numeric(grepl("-[(TD)|(SG)|(AD)|(CVD)]+-",aggregate.variables$bid))
  }

  ## keywords
 if(detective.characteristics$vars.incl.keywords){
   print("Generating keyword-related variables ...")

   #this causes an error if you don't set the locale when run on the server due to some encoding problem.

   Sys.setlocale(category = "LC_ALL", locale = "C")

   keyword.variables=b_process_keywords(bid=aggregate.variables$bid,
                                        text=aggregate.variables$text)

   aggregate.variables=merge(aggregate.variables,
                             keyword.variables[,c("bid","pos.word","pos.word.char", "neg.word", "neg.word.char")],
                             by="bid", all.x=T)

   print("Generating keyword-related variables ... complete.")
 }


  if(ncol(aggregate.variables)>2){
    aggregate.variables=unique(aggregate.variables)
    aggregate.variables=subset(aggregate.variables, bid %in% tf.agg$bid)
    tf.agg=merge(tf.agg, aggregate.variables, by="bid", all.x=T)
  }


  ## cleaning out tf.agg
  if(is.null(evaluation)){
    tf.agg=tf.agg[,c(estimation.variables, "bid")]
  } else {
    tf.agg=tf.agg[,c(estimation.variables, "bid","evaluation")]
  }

  ## DTM, if called for
  if(detective.characteristics$dtmatrix.included){

    nr.terms=detective.characteristics$dtmatrix.term.count


    eval(parse(text=paste("tf=tf[order(tf$",detective.characteristics$dtmatrix.metric,", decreasing = T),]", sep="")))
    model.words=unique(tf$word)[1:nr.terms]

    tf.dtm=count(subset(tf, word %in% model.words), vars=c('bid', 'word'))

    tf.dtm=as.data.frame(as.matrix(cast_dtm(tf.dtm, bid, word, freq)))
    tf.dtm$bid=rownames(tf.dtm)

    missing.bids=unique(tf$bid)[! unique(tf$bid) %in% tf.dtm$bid]
    tf.dt2=as.data.frame(matrix(0,nrow=length(missing.bids), ncol=ncol(tf.dtm)))
    names(tf.dt2)=names(tf.dtm)
    tf.dt2$bid=missing.bids
    tf.dtm=rbind(tf.dtm, tf.dt2)

    tf.agg=merge(tf.agg, tf.dtm[,c("bid", model.words)], by="bid", all.x=T)


    estimation.variables=c(estimation.variables, names(tf.agg)[!names(tf.agg) %in% c(estimation.variables, "bid","evaluation")])
  }



  output.list<- list("estimation.variables"=estimation.variables,
                     "word.score"=word.score,
                     "estimation.data"=tf.agg,
                     "detective.characteristics"=detective.characteristics)
  return(output.list)
}
