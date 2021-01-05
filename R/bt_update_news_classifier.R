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
                                     training.testing.split = 0.9){


  #ML packages
  library(keras)
  library(randomForest)

  #gta packages
  library(gtasql)
  library(gtabastiat)
  library(gtalibrary)

  #sql packages
  library(xml2)
  library(pool)
  library(RMariaDB)

  #library(tidytext)
  #library(caret)
  #library(dplyr)
  #library(purrr)


  ## opening connection

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

  #list of google, reuters news leads in 4 states - this is broader than what we
  #use for BT usually in order to increase the amount of training data

  leads.core.b221 = gta_sql_get_value(
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
AND (bthl.hint_state_id IN (5, 6, 7, 8))
AND (btbid.bid LIKE 'GNEWS-%' OR btbid.bid LIKE 'RTNEWS-%')
AND (bthl.hint_id < 250000);")

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
                                  evaluation = leads.core.b221$hint.state.id %in% c(2, 5, 6, 7)
  )

  #in case there are duplicates
  training.b221.full = subset(training.b221.full, !duplicated(training.b221.full$bid))


  #show summary of the text
  if(show.text.summary){
    print("Summary of training corpus documents word counts:")
    training.b221.full$text %>%
      strsplit(" ") %>%
      sapply(length) %>%
      summary()
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


  #parameters
  num_words = 15000 #vocab size (def = 10k)
  max_length = 100 #length of each item. longer will be chopped. shorter will be zero-padded



  #The tokeniser is now in a separate function.


  #tokenising the words. this overcomes the encoding problems because I preprocess
  #the tokens into padded int arrays myself

  #tokenising the text. throws error if you don't have nvidia gpu, can be ignrored.
  #keras is very good at this. for reference:
  #https://rdrr.io/cran/keras/man/text_tokenizer.html

  # bt_td_matrix_preprocess = function(num_words, max_length, text){
  #
  #   tokeniser = text_tokenizer(num_words = num_words) %>% fit_text_tokenizer(text)
  #
  #   text_seqs = texts_to_sequences(tokeniser, text)
  #   padded = text_seqs %>% pad_sequences(maxlen = max_length)
  #
  #   return(padded)
  # }


  #x is our sparse vector TD matrix this ensures the same tokeniser is used each
  #time when evaluating new leads - i.e. that the same words will have the same
  #values assigned to them
  mrs.hudson.tokeniser = text_tokenizer(num_words = num_words) %>% fit_text_tokenizer(training.b221$text)

  x.train = bt_td_matrix_preprocess(num_words = num_words,
                                    max_length = max_length,
                                    text = training.b221$text,
                                    tokeniser = mrs.hudson.tokeniser)

  #the seed can be changed - but if it is changed the results will not be as exactly reproducible.
  #i used the number of mrs hudson's house here ;)
  set.seed(221)
  x.train$evaluation = as.factor(training.b221$evaluation)

  print("Creating new model...")
  mrs.hudson.model = randomForest(evaluation ~ ., data=x.train)

  #tokeniser must be saved with the specific function.
  mrs.hudson.model.file.name = paste0("content/0 core/Mrs Hudson/", format(Sys.Date(), "%Y-%m-%d"), " - Mrs Hudson model.Rdata")
  mrs.hudson.tokeniser.file.name = paste0("content/0 core/Mrs Hudson/", format(Sys.Date(), "%Y-%m-%d"), " - Mrs Hudson tokeniser")

  print(paste("New model created! Saving to", mrs.hudson.model.file.name))

  save(mrs.hudson.model, file = mrs.hudson.model.file.name)
  save_text_tokenizer(mrs.hudson.tokeniser, file = mrs.hudson.tokeniser.file.name)

  #testing
  if(create.training.testing.split){
    x.test = bt_td_matrix_preprocess(num_words = num_words,
                                     max_length = max_length,
                                     text = testing.b221$text,
                                     tokeniser = mrs.hudson.tokeniser)

    x.test$evaluation = as.factor(testing.b221$evaluation)
    predictRF = as.data.frame(predict(mrs.hudson.model, newdata=x.test, type = "prob"))
    #table(x.test$evaluation, predictRF)

    testing.b221 = cbind(testing.b221, predictRF)
    testing.b221$predictRF = testing.b221$`TRUE` > quantile(testing.b221$`TRUE`, 0.1)
    testing.b221$correct.rf = testing.b221$evaluation == testing.b221$predictRF

    pr.metrics=bt_generate_pr_metrics(model.prediction = testing.b221$predictRF,
                                      real.label = testing.b221$evaluation,
                                      model.name = "RF")

    return(pr.metrics)
  }



}
