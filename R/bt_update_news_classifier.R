#' Pulls news leads from the Ricardomain database and trains a new Mrs Hudson
#' model based on these leads, then saves it.
#'
#' @param db.connection Database connection, should default to the correct one.
#' @param show.text.summary Do you want to see a summary of the input training
#'   data wordcounts?
#' @param create.training.testing.split Do you want to test the model on part of
#'   the training data and see its precision and recall?
#' @param training.testing.split Proportion to split for training and testing.
#'
#' @return Void. Returns a summary of the precision and recall of the model if
#'   create.training.testing.split=T
#'
#' @references www.globaltradealert.org
#' @Author Callum Campbell for Global Trade Alert.
#'
#'
bt_update_news_classifier = function(db.connection=NULL,
                                     show.text.summary=T,
                                     create.training.testing.split=F,
                                     training.testing.split = 0.9,
                                     mrs.h.method = "d2v"){


  accepted.methods= c("tokenise", "d2v")
  if(! mrs.h.method %in% accepted.methods){
   stop(paste("method must be one of", accepted.methods))
  }


  #for testing
  # db.connection=NULL
  # show.text.summary=T
  # create.training.testing.split=T
  # training.testing.split = 0.9

  #ML packages
  library(keras)
  library(randomForest)
  library(caret)
  library(word2vec)
  library(udpipe)

  #gta packages
  library(gtasql)
  library(gtabastiat)
  library(gtalibrary)

  #sql packages
  library(xml2)
  library(pool)
  library(RMariaDB)

  #text packages
  library(tidytext)
  library(tm)
  library(stringr)

  library(dplyr)
  #library(purrr)


  ## opening connection

  print(is.null(db.connection))
  if(is.null(db.connection)){
    print("retrieving training data from DB...")
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

  #list of google, reuters news leads in 4 states - this is broader than what we
  #use for BT usually in order to increase the amount of training data

  #RELEVANT NEWS LEADS
  leads.core.b221.rlv.news = gta_sql_get_value(
    "SELECT DISTINCT btbid.bid, bthl.acting_agency, btht.hint_title, btht.hint_description, bthl.hint_values, bthl.registration_date, gtajl.jurisdiction_name, bthl.hint_state_id
FROM bt_hint_log bthl,
	bt_hint_bid btbid,
	bt_hint_text btht,
	bt_hint_jurisdiction bthj,
	gta_jurisdiction_list gtajl

WHERE bthl.hint_id = btbid.hint_id
AND bthl.hint_id = btht.hint_id
AND bthl.hint_id = bthj.hint_id

AND btbid.hint_id = bthj.hint_id
AND btbid.hint_id = btht.hint_id

AND bthj.hint_id = btht.hint_id

AND bthj.jurisdiction_id = gtajl.jurisdiction_id

AND btht.language_id = 1
AND (bthl.hint_state_id IN (5, 6, 7))
AND (btbid.bid LIKE 'GNEWS-%' OR btbid.bid LIKE 'RTNEWS-%')
LIMIT 25000;")

  paste(nrow(leads.core.b221.rlv.news), "relevant NEWS rows retrieved") %>% print()


  #too much of these are bad, plus we have a lot more news training data now.
  #NON NEWS, NON-TD to increase training set
#   leads.core.b221.rlv.notnews = gta_sql_get_value(
#     "SELECT DISTINCT btbid.bid, bthl.acting_agency, btht.hint_title, btht.hint_description, bthl.hint_values, bthl.registration_date, gtajl.jurisdiction_name, bthl.hint_state_id
# FROM bt_hint_log bthl,
# 	bt_hint_bid btbid,
# 	bt_hint_text btht,
# 	bt_hint_jurisdiction bthj,
# 	gta_jurisdiction_list gtajl
#
# WHERE bthl.hint_id = btbid.hint_id
# AND bthl.hint_id = btht.hint_id
# AND bthl.hint_id = bthj.hint_id
#
# AND btbid.hint_id = bthj.hint_id
# AND btbid.hint_id = btht.hint_id
#
# AND bthj.hint_id = btht.hint_id
#
# AND bthj.jurisdiction_id = gtajl.jurisdiction_id
#
# AND btht.language_id = 1
# AND (bthl.hint_state_id IN (5, 6, 7))
# AND NOT (btbid.bid LIKE 'GNEWS-%' OR btbid.bid LIKE 'RTNEWS-%')
#
# AND bthj.jurisdiction_id NOT IN (89)
#
# LIMIT 25000;")
#
#   paste(nrow(leads.core.b221.rlv.notnews), "relevant NON-NEWS rows retrieved") %>% print()



  #IRRELEVANT NEWS LEADS
  leads.core.b221.irv = gta_sql_get_value(
    "SELECT DISTINCT btbid.bid, bthl.acting_agency, btht.hint_title, btht.hint_description, bthl.hint_values, bthl.registration_date, gtajl.jurisdiction_name, bthl.hint_state_id
FROM bt_hint_log bthl,
	bt_hint_bid btbid,
	bt_hint_text btht,
	bt_hint_jurisdiction bthj,
	gta_jurisdiction_list gtajl

WHERE bthl.hint_id = btbid.hint_id
AND bthl.hint_id = btht.hint_id
AND bthl.hint_id = bthj.hint_id

AND btbid.hint_id = bthj.hint_id
AND btbid.hint_id = btht.hint_id

AND bthj.hint_id = btht.hint_id

AND bthj.jurisdiction_id = gtajl.jurisdiction_id

AND btht.language_id = 1
AND (bthl.hint_state_id IN (8, 9))
AND (btbid.bid LIKE 'GNEWS-%' OR btbid.bid LIKE 'RTNEWS-%')
LIMIT 25000;")

  paste(nrow(leads.core.b221.irv), "IRrelevant rows retrieved") %>% print()

  leads.core.b221 = rbind(leads.core.b221.rlv.news,
                          #leads.core.b221.rlv.notnews, #removed for reasons stated above
                          leads.core.b221.irv)

  rm(leads.core.b221.irv,
     #leads.core.b221.rlv.notnews,
     leads.core.b221.rlv.news)

  paste(nrow(leads.core.b221), "total rows retrieved") %>% print()

  gta_sql_pool_close()

  #reminder
  # "1"	"B221 - freelancer desk"
  # "2"	"B221 - editor desk"
  # "3"	"OSC - freelancer desk"
  # "4"	"OSC - editor desk"
  # "5"	"BT - ready for dispatch"
  # "6"	"lead - sent out" #ie has been accepted in b221
  # "7"	"lead - fully processed"
  # "8"	"trash bin - entered"
  # "9"	"trash bin - fully processed"



  #construct training data from the retrieved table
  #having AA + title first means these will always be included as the initial values when the T-D sparse matrix is created
  #this ensures they will be taken into account when the learning process occurs

  training.b221.full = data.frame(bid = leads.core.b221$bid,
                                  text = paste(leads.core.b221$acting.agency, leads.core.b221$hint.title, leads.core.b221$hint.description),
                                  acting.agency = leads.core.b221$acting.agency,
                                  evaluation = leads.core.b221$hint.state.id %in% c(5, 6, 7)
  )

  #in case there are duplicates
  training.b221.full = subset(training.b221.full, !duplicated(training.b221.full$bid))



  #clean up the text (I was doing this per model but in all cases I think we
  #don't want punctuation and uppercase)



  training.b221.full$text = bt_text_preprocess(training.b221.full$text)

  #show summary of the text
  if(show.text.summary){
    print("Summary of training corpus documents word counts:")
    training.b221.full$text %>%
      strsplit(" ") %>%
      sapply(length) %>%
      summary() %>%
      print()
  }


  #random ids to separate into training/testing sets (if desired)
  #ratio can be changed if necessary
  #currently 90/10. ideally would be 80/20 - need more data.

  if(create.training.testing.split){

    training.id = sample.int(nrow(training.b221.full), size = nrow(training.b221.full)*training.testing.split)
    training.b221 = training.b221.full[training.id,]
    testing.b221 = training.b221.full[-training.id,]
    rm(training.id)

  } else {
    training.b221 = training.b221.full
  }




  ##### UDPipe! #####

  #UDPipe is very cool, but not sure if it is that useful in this case. I leave the code here for future reference.

  want.udpipe = F

  if(want.udpipe){

    #download model if for the first time, filepath here needs editing if we want to use this
    udmodel_english <- udpipe_load_model(file = 'english-ewt-ud-2.5-191206.udpipe')
    message("creating udpipe annotated model, may take a while...")

    #remove rubbish and prepare annotations DF
    udp.input = removePunctuation(training.b221$text[1]) %>% tolower()

    s2 <- udpipe_annotate(object = udmodel_english,
                          x=udp.input,
                          doc_id = seq_along(x))
    x.train.udp <- data.frame(s2)

    x.train.udp$evaluation = str_extract(string=x.train.udp$doc_id, pattern = "\\d+") %in% which(training.b221$evaluation)


    #RAKE analysis
    rake.stats <- keywords_rake(x = x.train.udp,
                                term = "lemma",
                                group = "doc_id",
                                relevant = x.train.udp$upos %in% c("NOUN", "ADJ"))

    rake.stats.relevant <- keywords_rake(x = x.train.udp[x.train.udp$evaluation,],
                                         term = "lemma",
                                         group = "doc_id",
                                         relevant = x.train.udp$upos[x.train.udp$evaluation] %in% c("NOUN", "ADJ"))

    rake.stats.irrelevant <- keywords_rake(x = x.train.udp[!x.train.udp$evaluation,],
                                           term = "lemma",
                                           group = "doc_id",
                                           relevant = x.train.udp$upos[!x.train.udp$evaluation] %in% c("NOUN", "ADJ"))

    rake.stats.relevant$key <- factor(rake.stats.relevant$keyword, levels = rev(rake.stats.relevant$keyword))
    rake.stats.irrelevant$key <- factor(rake.stats.irrelevant$keyword, levels = rev(rake.stats.irrelevant$keyword))



    barchart(key ~ rake, data = head(subset(rake.stats, freq > 3), 20), col = "purple",
             main = "Relevant Keywords by RAKE",
             xlab = "RAKE score")

    barchart(key ~ rake, data = head(subset(rake.stats.relevant, freq > 3), 20), col = "blue",
             main = "Relevant Keywords by RAKE",
             xlab = "RAKE score")

    barchart(key ~ rake, data = head(subset(rake.stats.irrelevant, freq > 3), 20), col = "red",
             main = "Irrelevant Keywords by RAKE",
             xlab = "RAKE score")

    count_r_words = function(rake.word, count_string){
      return(str_count(string = count_string,
                       pattern = rake.word))

    }

    rake.words = subset(rake.stats, freq > 3, rake >= 1)$keyword
    sapply(rake.words, FUN = count_r_words(rake.word = rake.words, count_string = count_string))


    # the idea I had here was to implement a rake score, a bit like tf-idf. Will set some time aside later to focus on this.


  }


  if(mrs.h.method == "d2v"){
    ##### word2vec #####

  train.w2v = training.b221$text

  set.seed(221)

  print("preparing word vector embeddings, may take a while...")

  model.w2v = word2vec(x = train.w2v,
                       type = "cbow",
                       dim = 100,
                       iter = 20)

  mrs.hudson.w2v.emb.fname = paste0("content/0 core/Mrs Hudson/", format(Sys.Date(), "%Y-%m-%d"), " - Mrs Hudson w2v.bin")

  print(paste("w2v embeddings created! saving to",mrs.hudson.w2v.emb.fname))
  write.word2vec(model.w2v, file = mrs.hudson.w2v.emb.fname)


  x.train = bt_d2v_preprocess(doc_id = training.b221$bid, text=training.b221$text)

  if(create.training.testing.split){
    x.test = bt_d2v_preprocess(doc_id = testing.b221$bid, text = testing.b221$text)
  }

  }

if(mrs.h.method == "tokenise"){
  #The tokeniser is now in a separate function.


  #tokenising the words. this overcomes the encoding problems because I preprocess
  #the tokens into padded int arrays myself

  #tokenising the text. throws error if you don't have nvidia gpu, can be ignrored.
  #keras is very good at this. for reference:
  #https://rdrr.io/cran/keras/man/text_tokenizer.html



  #parameters
  num_words = 15000 #vocab size (def = 10k)
  max_length = 150 #length of each item. longer will be chopped. shorter will be zero-padded

  #x is our sparse vector TD matrix this ensures the same tokeniser is used each
  #time when evaluating new leads - i.e. that the same words will have the same
  #values assigned to them
  print("Creating tokeniser...")
  mrs.hudson.tokeniser = text_tokenizer(num_words = num_words) %>%
    fit_text_tokenizer(training.b221$text)

  #save the tokeniser if it was created properly
  if(length(mrs.hudson.tokeniser)){

    mrs.hudson.tokeniser.file.name = paste0("content/0 core/Mrs Hudson/", format(Sys.Date(), "%Y-%m-%d"), " - Mrs Hudson tokeniser")
    print(paste("Tokeniser created! Saving to", mrs.hudson.tokeniser.file.name))
    save_text_tokenizer(mrs.hudson.tokeniser, file = mrs.hudson.tokeniser.file.name)
    print("Saved!")
  }




  x.train = bt_td_matrix_preprocess(num_words = num_words,
                                    max_length = max_length,
                                    text = training.b221$text)

  if(create.training.testing.split){
    x.test = bt_td_matrix_preprocess(num_words = num_words,
                                     max_length = max_length,
                                     text = testing.b221$text)
  }
}
  #the seed can be changed - but if it is changed the results will not be as exactly reproducible.
  #i used the number of mrs hudson's house here
  set.seed(221)





  x.train$evaluation = as.factor(training.b221$evaluation)

  print("Creating new model... (may take a while)")

  if(!mrs.h.grid.search){
    #without grid search - much faster
    mrs.hudson.model = randomForest(evaluation ~ .,
                                    data=x.train)

  }else{

    #with grid search - prepare your RAM

    # K-fold cross-validation from caret
    # Define the control
    trControl <- trainControl(method = "cv",
                              number = 5,
                              search = "grid")

    mrs.hudson.model = train(evaluation ~ .,
                             data=x.train,
                             method='rf',
                             metric="Accuracy",
                             #mtry=33,
                             trControl=trControl)
  }

  # x.test = bt_td_matrix_preprocess(num_words = num_words,
  #                                  max_length = max_length,
  #                                  text = testing.b221$text)
  # x.test$evaluation = as.factor(testing.b221$evaluation)


  mrs.hudson.model.file.name = paste0("content/0 core/Mrs Hudson/", format(Sys.Date(), "%Y-%m-%d"), " - Mrs Hudson model.Rdata")

  mrs.h.gen.method = mrs.h.method
  print(paste("New model created using", mrs.h.gen.method, "! Saving to", mrs.hudson.model.file.name))


  save(mrs.h.gen.method, mrs.hudson.model, file = mrs.hudson.model.file.name)


  #testing
  if(create.training.testing.split){

    print(paste("The retain quantile used for testing is", 1-training.testing.split))


    #x.test$evaluation = as.factor(testing.b221$evaluation)

    predictRF = as.data.frame(predict(mrs.hudson.model, newdata=x.test, type = "prob"))
    #table(x.test$evaluation, predictRF)

    testing.b221 = cbind(testing.b221, predictRF)
    testing.b221$predictRF = testing.b221$`TRUE` > quantile(testing.b221$`TRUE`, 0.2)
    testing.b221$correct.rf = testing.b221$evaluation == testing.b221$predictRF

    pr.metrics=bt_generate_pr_metrics(model.prediction = testing.b221$predictRF,
                                      real.label = testing.b221$evaluation,
                                      model.name = "RF")

    # used with JF's DS model comparison script
    # result2 = data.frame(bid = testing.b221$bid,
    #                      contender = "Mrs H 0.1",
    #                      pred = testing.b221$`TRUE`,
    #                      evaluation = testing.b221$evaluation)

    return(pr.metrics)
  }



}
