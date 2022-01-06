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
                                     mrs.h.method = "d2v",
                                     mrs.h.gen.alg = "XGB",
                                     use.tf.idf = TRUE){


  accepted.methods= c("tokenise", "d2v")
  if(! mrs.h.method %in% accepted.methods){
   stop(paste("method must be one of", accepted.methods))
  }
  if(mrs.h.method == "tokenise"){
    warning("Spare-matrix tokenisation is deprecated - it is only kept for compatibility and may not work.")
  }

  ##### DEPENDENCIES AND INIT #####

  #for testing
  # db.connection=NULL
  # show.text.summary=T
  # create.training.testing.split=T
  # training.testing.split = 0.9
  # mrs.h.method = "d2v"

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

  #print(is.null(db.connection))
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


# GET TRAINING DATA -------------------------------------------------------


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
      #AND (bthl.hint_state_id IN (5,6,7))
      AND (bthl.hint_state_id IN (7))
      AND (btbid.bid LIKE 'GNEWS-%' OR btbid.bid LIKE 'RTNEWS-%')
      LIMIT 25000;")

  paste(nrow(leads.core.b221.rlv.news), "relevant NEWS rows retrieved") %>% print()



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
AND (bthl.hint_state_id IN (9))
AND (btbid.bid LIKE 'GNEWS-%' OR btbid.bid LIKE 'RTNEWS-%')
LIMIT 25000;")

  paste(nrow(leads.core.b221.irv), "IRrelevant rows retrieved") %>% print()

  leads.core.b221 = rbind(leads.core.b221.rlv.news,
                          leads.core.b221.irv)

  rm(leads.core.b221.irv,
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
  # "7"	"lead - fully processed" <- the 'best' leads
  # "8"	"trash bin - entered"
  # "9"	"trash bin - fully processed" <- the 'worst' leads


# PREPARE THE TEXT --------------------------------------------------------
  leads.core.b221$text = paste(bt_text_preprocess(leads.core.b221$acting.agency, stop.rm = F),
                               bt_text_preprocess(leads.core.b221$hint.title),
                               bt_text_preprocess(leads.core.b221$hint.description))



  # TF-IDF ------------------------------------------------------------------

  if(use.tf.idf){

    #generate master vocab
    tf.idf.master = bt_unnest_tokens(doc.id = leads.core.b221$bid,
                                     text = leads.core.b221$text) %>%
      bt_generate_2cat_tf_idf(rlv.doc.ids = leads.core.b221$bid[leads.core.b221$hint.state.id == 7])



    #save
    tf.idf.file.name = paste0("content/0 core/Mrs Hudson/", format(Sys.Date(), "%Y-%m-%d"), " - Mrs Hudson tf-idf.Rdata")

    print(paste("tf-idf values calculated for training data! Saving to", tf.idf.file.name))
    save(tf.idf.master, file = tf.idf.file.name)



    #add the scores to the df for training and testing purposes later using master vocab we just generated

    tf.idf.agg = bt_generate_tf_idf_agg_score(tf.idf.file.name,
                                              doc.id = leads.core.b221$bid,
                                              text = leads.core.b221$text)

    leads.core.b221 = merge(leads.core.b221, tf.idf.agg, all.x = T, by.x = "bid", by.y = "doc.id")

  }







  #construct training data from the retrieved table
  #having AA + title first means these will always be included as the initial values when the T-D sparse matrix is created
  #this ensures they will be taken into account when the learning process occurs

  training.b221.full = data.frame(bid = leads.core.b221$bid,
                                  text = leads.core.b221$text,
                                  hint.title = leads.core.b221$hint.title,
                                  hint.description = leads.core.b221$hint.description,
                                  acting.agency = leads.core.b221$acting.agency,
                                  evaluation = leads.core.b221$hint.state.id %in% c(5,6,7)
  )

  #in case there are duplicates
  training.b221.full = subset(training.b221.full, !duplicated(training.b221.full$bid))



# TRAINING TESTING SPLIT --------------------------------------------------

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





# UDPipe! -----------------------------------------------------------------



  #UDPipe is very cool, but not sure if it is that useful in this case. I leave the code here for future reference.
  #useful for metrics, better to split out into its own thing but no time now

  want.udpipe = F

  if(want.udpipe){

    library(udpipe)

    #download model if for the first time, filepath here needs editing if we want to use this
    model <- udpipe_download_model(language = "english")
    udmodel_english <- udpipe_load_model(file = 'english-ewt-ud-2.5-191206.udpipe')




    #remove rubbish and prepare annotations DF
    #udp.input = training.b221$text
    udp.input = leads.core.b221$hint.title
    names(udp.input) = leads.core.b221$bid

    #UDPipe MUST have ONLY UTF-8 input or it will not work
    if (any(!stri_enc_isutf8(udp.input))) {
      message(paste0(sum(stri_enc_isutf8(udp.input)), "non-UTF-8 texts detected. Rejanking..."))

      #the regex is supposed to find words containing bad characters
      #usually these are coming from encoding problems

      udp.input[!stri_enc_isutf8(udp.input)] = udp.input[!stri_enc_isutf8(udp.input)] %>%
        gsub(pattern = "[a-z]*[^ a-z][a-z]*", replacement = "")

      #fwrite(data.frame(x=udp.input), file="udpi.csv")
      #udp.input = fread(file = "udpi.csv", encoding = "UTF-8")

    }

    message("creating udpipe annotated model, may take a while...")
    s2 <- udpipe_annotate(udmodel_english,
                          x=udp.input, doc_id = names(udp.input))

    message("annotated model generated! converting to dataframe...")
    x.train.udp <- as.data.frame(s2)


    #x.train.udp$evaluation = str_extract(string=x.train.udp$doc_id, pattern = "\\d+") %in% which(training.b221$evaluation)
    x.train.udp$evaluation = x.train.udp$doc_id %in% training.b221$bid[training.b221$evaluation]


    udp.file.path = str_extract(string = getwd(), pattern = ".+Bastiat") %>%
      paste0(., "/0 projects/021 udpipe/udp_annotated.Rdata")

    message("annotations dataframe generated, saving to ", udp.file.path)

    save(x.train.udp, file = udp.file.path)



    #RAKE analysis

    #hyperparams
    min.occurrence = 5
    ngram.max = 2
    desired.pos = c("NOUN", "VERB", "PROPN", "ADJ")

    rake.stats <- keywords_rake(x = x.train.udp,
                                term = "lemma",
                                group = "doc_id",
                                relevant = x.train.udp$upos %in% desired.pos,
                                n_min = min.occurrence,
                                ngram_max = ngram.max)

    rake.stats.relevant <- keywords_rake(x = x.train.udp[x.train.udp$evaluation,],
                                         term = "lemma",
                                         group = "doc_id",
                                         relevant = x.train.udp$upos[x.train.udp$evaluation] %in% desired.pos,
                                         n_min = min.occurrence,
                                         ngram_max = ngram.max)

    rake.stats.irrelevant <- keywords_rake(x = x.train.udp[!x.train.udp$evaluation,],
                                           term = "lemma",
                                           group = "doc_id",
                                           relevant = x.train.udp$upos[!x.train.udp$evaluation] %in% desired.pos,
                                           n_min = min.occurrence,
                                           ngram_max = ngram.max)

    rake.stats$key <- factor(rake.stats$keyword, levels = rev(rake.stats$keyword))
    rake.stats.relevant$key <- factor(rake.stats.relevant$keyword, levels = rev(rake.stats.relevant$keyword))
    rake.stats.irrelevant$key <- factor(rake.stats.irrelevant$keyword, levels = rev(rake.stats.irrelevant$keyword))



    barchart(key ~ rake, data = head(subset(rake.stats, freq > 3), 20), col = "cornflowerblue",
             main = paste0("Overall Keywords by RAKE (", ngram.max, "-grams)"),
             xlab = "RAKE score")

    barchart(key ~ rake, data = head(subset(rake.stats.relevant, freq > 3), 20), col = "chartreuse4",
             main = paste0("Relevant Keywords by RAKE (", ngram.max, "-grams)"),
             xlab = "RAKE score")

    barchart(key ~ rake, data = head(subset(rake.stats.irrelevant, freq > 3), 20), col = "coral",
             main = paste0("Irrelevant Keywords by RAKE (", ngram.max, "-grams)"),
             xlab = "RAKE score")

    count_r_words = function(rake.word, count_string){
      return(str_count(string = count_string,
                       pattern = rake.word))

    }

    #rake.words = subset(rake.stats, freq > 3, rake >= 1)$keyword
    #sapply(rake.words, FUN = count_r_words(rake.word = rake.words, count_string = count_string))


    # the idea I had here was to implement a rake score, a bit like tf-idf. Will set some time aside later to focus on this.


  }







# d2v Advanced implementation -------------------------------------------

  if(mrs.h.method == "d2v"){


# check the text before training ------------------------------------------


    orig.rows = nrow(training.b221)
    orig.bids = training.b221$bid

    print("checking preprocessed text...")

    problem.leads = subset(training.b221, !grepl(pattern = "[a-z]", training.b221$text))

    #rem.bids = orig.bids[which(! orig.bids %in% training.b221.full$bid)] %>% paste(collapse = ", ")

    rem.bids = problem.leads$bid %>% paste(collapse = ", ")

    training.b221 = subset(training.b221, !bid %in% rem.bids)

    #show summary of the text
    if(show.text.summary){
      print("Summary of training corpus documents word counts:")
      training.b221$text %>%
        strsplit(" ") %>%
        sapply(length) %>%
        summary() %>%
        print()
    }

    n.rm.rows = orig.rows-nrow(training.b221)

    if(n.rm.rows>0){
      warning(paste(n.rm.rows, "rows removed during preprocessing! probably because they consisted of only bad chars ([^A-z]) which cannot be classified! The BIDs for the removed rows were:", rem.bids))
    }


# prepare w2v word vector model -------------------------------------------


    train.w2v = training.b221$text

    set.seed(221)

    print("preparing word vector embeddings, may take a while...")

    model.w2v = word2vec(x = train.w2v,
                         type = "skip-gram",
                         dim = 150,
                         iter = 20,
                         min_count = 2,
                         window = 10)

    mrs.hudson.w2v.emb.fname = paste0("content/0 core/Mrs Hudson/", format(Sys.Date(), "%Y-%m-%d"), " - Mrs Hudson w2v.bin")

    print(paste("w2v embeddings created! saving to",mrs.hudson.w2v.emb.fname))
    write.word2vec(model.w2v, file = mrs.hudson.w2v.emb.fname)


    x.train = bt_d2v_preprocess(model.w2v, doc_id = training.b221$bid, text=training.b221$text)


    #preprocessing testing data

    if(create.training.testing.split){
      x.test = bt_d2v_col_preprocess(model.w2v, doc_id = testing.b221$bid, text = testing.b221$text)
    }






# Generic w2v/d2v method (SUPERSEDED) -------------------------------------


  # if(mrs.h.method == "d2v"){
  #   ##### word2vec #####
  #
  # train.w2v = training.b221$text
  #
  # set.seed(221)
  #
  # print("preparing word vector embeddings, may take a while...")
  #
  # model.w2v = word2vec(x = train.w2v,
  #                      type = "cbow",
  #                      dim = 100,
  #                      iter = 20)
  #
  # mrs.hudson.w2v.emb.fname = paste0("content/0 core/Mrs Hudson/", format(Sys.Date(), "%Y-%m-%d"), " - Mrs Hudson w2v.bin")
  #
  # print(paste("w2v embeddings created! saving to",mrs.hudson.w2v.emb.fname))
  # write.word2vec(model.w2v, file = mrs.hudson.w2v.emb.fname)
  #
  #
  # x.train = bt_d2v_preprocess(model.w2v, doc_id = training.b221$bid, text=training.b221$text)
  #
  # # if(create.training.testing.split){
  # #   x.test = bt_d2v_preprocess(model.w2v, doc_id = testing.b221$bid, text = testing.b221$text)
  # # }
  #
  # }



# Basic tokenisation method (deprecated) ----------------------------------



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

    # if(create.training.testing.split){
    #   x.test = bt_td_matrix_preprocess(num_words = num_words,
    #                                    max_length = max_length,
    #                                    text = testing.b221$text)
    # }
  }



  ##### TRAIN RF MODEL #####
  #other models were tested, but RF seems to perform well. Definitely future optimisation is possible.


    #TODO remove this
  #the seed can be changed - but if it is changed the results will not be as exactly reproducible.
  #i used the number of mrs hudson's house here
  # if(F){
  #
  #   set.seed(221)
  #
  # x.train$evaluation = as.factor(training.b221$evaluation)
  #
  # print("Creating new model... (may take a while)")
  #
  # if(!mrs.h.grid.search){
  #   #without grid search - much faster
  #   mrs.hudson.model = randomForest(evaluation ~ .,
  #                                   data=x.train)
  #
  # }else{
  #
  #   #with grid search - prepare your RAM
  #
  #   # K-fold cross-validation from caret
  #   # Define the control
  #   trControl <- trainControl(method = "cv",
  #                             number = 5,
  #                             search = "grid")
  #
  #   mrs.hudson.model = train(evaluation ~ .,
  #                            data=x.train,
  #                            method='rf',
  #                            metric="Accuracy",
  #                            #mtry=33,
  #                            trControl=trControl)
  # }
#
#   # x.test = bt_td_matrix_preprocess(num_words = num_words,
#   #                                  max_length = max_length,
#   #                                  text = testing.b221$text)
#   # x.test$evaluation = as.factor(testing.b221$evaluation)
#
#
#   mrs.hudson.model.file.name = paste0("content/0 core/Mrs Hudson/", format(Sys.Date(), "%Y-%m-%d"), " - Mrs Hudson model.Rdata")
#
#   mrs.h.gen.method = mrs.h.method
#   print(paste("New model created using", mrs.h.gen.method, "! Saving to", mrs.hudson.model.file.name))
#
#
#   save(mrs.h.gen.method, mrs.hudson.model, file = mrs.hudson.model.file.name)
#
#
#   }


# Generate model using specified method -----------------------------------


  set.seed(221)


    x.train = merge(leads.core.b221[,c("bid", "tf.idf.rlv", "tf.idf.irv", "bm25.rlv", "bm25.irv")], x.train, by.x = "bid", by.y = "row.names")
  x.train$evaluation = as.factor(training.b221$evaluation)


  rownames(x.train) = training.b221$bid

  #use this to tune the sample size... it can't be larger than this number of course
  class.freq.min = min(table(x.train$evaluation))


  # 1. Simple RandomForest with no grid search etc - this is the fastest to train (and not bad performance!)
  if(mrs.h.gen.alg == "RFstandard"){

    library(randomForest)
    print(paste("Creating new Mrs H RF model... (non-grid search)"))

    hypermodel = randomForest(evaluation ~ .,
                              data=x.train)


    #2. RandomForest with grid search and other tuning params - slower (prelim acc = 58%)
  }else if(mrs.h.gen.alg == "RFgrid"){
    library(caret)
    library(randomForest)

    print(paste("Creating new Mrs H model... (RF w/ grid search, may take a while)"))

    #with grid search - prepare your RAM

    # K-fold cross-validation from caret
    # Define the control
    # took about 40-45 mins to train on my machine
    # rf.trControl <- trainControl(method = "repeatedcv",
    #                              number = 5,
    #                              repeats = 1,
    #                              search = "grid",
    #                              sampling = "up",
    #                              verboseIter = TRUE)

    # normal cv, much faster
    rf.trControl <- trainControl(
      method = "cv",
      number = 5,
      search = "random",
      verboseIter = TRUE
    )


    dynamic.mtry <- sqrt(ncol(x.train %>% select(-evaluation)))

    rf.tunegrid <- expand.grid(.mtry=dynamic.mtry)

    hypermodel = train(evaluation ~ .,
                       data=x.train,
                       method='rf',
                       metric="Accuracy",
                       #mtry=dynamic.mtry,
                       tunegrid = rf.tunegrid,
                       trControl = rf.trControl)






    # 3. XGBoost

  }else if(mrs.h.gen.alg == "XGB"){

    library(caret)
    print(paste("Creating new hypermodel... (XGB w/ grid search, may take a while)"))
    library(xgboost)
    library(ROSE)

    #xgb likes to use dmatrices - IIRC this is required in python but not in R,
    #either way I do it here
    x.train.xgb = xgb.DMatrix(as.matrix(x.train %>% select(-evaluation)))
    y.train = as.factor(x.train$evaluation)

    # cross-validation method and number of folds + enable parallel computation
    xgb.trControl = trainControl(
      method = "cv",
      number = 10,
      allowParallel = TRUE,
      verboseIter = TRUE,
      returnData = FALSE
    )

    #grid space to search for the best hyperparameters
    xgbGrid <- expand.grid(nrounds = c(50,100),#nrounds = c(100,200),
                           max_depth = c(10, 15, 20, 25),
                           colsample_bytree = seq(0.5, 0.9, length.out = 5),
                           # The values below are default values in the sklearn-api.
                           eta = 0.1,
                           gamma=0,
                           min_child_weight = 1,
                           subsample = 1
    )

    hypermodel = train(
      x.train.xgb, y.train,
      trControl = xgb.trControl,
      tuneGrid = xgbGrid,
      method = "xgbTree"
    )

    # Support Vector Machines - Linear
    # prelim.acc ~50%
    # unfortunately errors out when used on unseen data, which is apparently common to this implementation of SVM
    # nothing I can do to fix except trying random seed values, which sounds problematic
  }else if(mrs.h.gen.alg == "svmLinear"){

    library(caret)
    library(kernlab)
    library(ROSE)#for sampling

    #these params are the best result I got after many tests
    #including a 1000 fold cv
    svm.trControl <- trainControl(method = "repeatedcv",
                                  number = 10,
                                  repeats = 3,
                                  sampling = "up")
    #classProbs = T)

    hypermodel <-
      train(
        evaluation ~ .,
        data = x.train,
        method = "svmLinear",
        trControl = svm.trControl,
        preProcess = c("center", "scale"),
        tuneGrid = expand.grid(C = seq(0.5, 2, length = 20))
      )




    #Relevance Vector Machines (RVM)
    #currently not supported for classification in R :(
    #I leave it here in case it is one day
  }else if(mrs.h.gen.alg == "rvm"){

    rvm.trControl <- trainControl(method = "repeatedcv",
                                  number = 10,
                                  repeats = 3)

    hypermodel <- train(evaluation ~.,
                        data = col.predictions,
                        method = "rvmLinear",
                        trControl=rvm.trControl,
                        preProcess = c("center", "scale"),
                        tuneLength = 10)

    #standard Naive Bayes
  }else if(mrs.h.gen.alg == "NBstandard"){

    library(caret)
    library(klaR)
    library(e1071) #this may not be required here

    nb.trControl <- trainControl(method = "repeatedcv",
                                 number = 10,
                                 repeats = 3,
                                 search = "grid")

    #fL = laplace correction - during testing this appeared to make zero difference,
    #may be due to the specific nature of the training data used

    # accuracy seems lower than RF or XGB
    nb.tuneGrid <- data.frame(fL = seq(1, length = 4),
                              usekernel = T,
                              adjust = seq(0.25, 1.5, length = 4))


    hypermodel <- train(evaluation ~.,
                        data = x.train,
                        method = "nb",
                        trControl=nb.trControl,
                        preProcess = c("center", "scale"),
                        tuneGrid = nb.tuneGrid)




  }

  # mrs.h.model = list(mrs.h.model = hypermodel,
  #                    mrs.h.model.method = hypermodel.method)

  mrs.hudson.model = hypermodel



  mrs.hudson.model.file.name = paste0("content/0 core/Mrs Hudson/", format(Sys.Date(), "%Y-%m-%d"), " - Mrs Hudson model.Rdata")


  mrs.h.gen.method = mrs.h.method
  mrs.h.gen.alg = mrs.h.gen.alg

  print(paste("New model created using", mrs.h.gen.method, "with", mrs.h.gen.alg,"! Saving to", mrs.hudson.model.file.name))


  save(mrs.h.gen.method, mrs.h.gen.alg, mrs.hudson.model, file = mrs.hudson.model.file.name)





  ##### TESTING NEW CLASSIFIER #####

  # if called for

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

    # to generate the df in full use this:
    # tb2 = leads.core.b221
    #
    # setnames(leads.core.b221, "hint.title", "act.title.en")
    # setnames(leads.core.b221, "hint.description", "act.description.en")
    #
    # res = bt_estimate_news_leads(leads.core.news = tb2,
    #                              binary.prediction = F,
    #                              return.both = T,
    #                              conf.cutoff = 0.3)
    #
    # mrs.h.w2v = data.frame(bid = tb2$bid,
    #                        contender = "mrs h d2v",
    #                        pred = res$raw.score,
    #                        evaluation = tb2$hint.state.id %in% c(5,6,7))
    #

    return(pr.metrics)
  }


}
}
