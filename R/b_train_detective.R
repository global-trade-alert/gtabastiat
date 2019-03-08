# Roxygen documentation

#' Bastiat, please train this detective.
#'
#' @return Several data frames about how the detective did.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

b_train_detective = function(detective.number=NULL,
                             variables=NULL,
                             estimation.method=NULL,
                             estimation.rounds=NULL,
                             dtm.incl=FALSE,
                             dtm.metric=NULL,
                             dtm.terms=NULL,
                             train.share=NULL,
                             practice=T){

  ## initialising
  result= data.frame(round=numeric(),
                     obs=numeric(),
                     pred=numeric())

  stats=data.frame(round=numeric(),
                   algorithm=character(),
                   max.score=numeric(),
                   cutoff.train=numeric(),
                   cutoff.test=numeric(),
                   cutoff.ratio=numeric(),
                   capture=numeric(),
                   reduction=numeric(),
                   capture.adjusted=numeric(),
                   reduction.adjusted=numeric())

  ## parameters
  d.no=detective.number
  my.vars=variables
  mv=variables
  turns=estimation.rounds

  ## training data
  load("data/classifier/training data.Rdata")

  ## data prep
  model.variables=b_create_model_variables(bid=training$bid,
                                           evaluation=training$evaluation,
                                           text=training$text,
                                           train.share = train.share,
                                           detective.number=detective.number,
                                           variables=variables,
                                           dtm.incl=dtm.incl,
                                           dtm.metric=dtm.metric,
                                           dtm.terms=dtm.terms)


  ####### SL Round

  #### ESTIMATION
  ## estimates with SuperLearner

  ## SuperLearner data prep
  train=model.variables[,c("bid","evaluation",my.vars)]

  train.x = subset(train, bid %in% train.split)
  train.x$acting.agency=as.numeric(train.x$acting.agency)
  test.x = subset(train, ! bid %in% train.split)
  test.x$acting.agency=as.numeric(test.x$acting.agency)
  test.y=test.x$evaluation
  test.x$bid=NULL
  test.x$evaluation=NULL

  train.y=train.x$evaluation
  train.x$bid=NULL
  train.x$evaluation=NULL


  ## opening cluster
  cluster = parallel::makeCluster(2)
  parallel::clusterEvalQ(cluster, library(SuperLearner))
  parallel::clusterSetRNGStream(cluster, 1)

  algorithm.dictionary=data.frame(short=c("XGB","KNN","RNF", "SVM"),
                                  full=c("SL.xgboost", "SL.kernelKnn", "SL.randomForest", "SL.ksvm"),
                                  stringsAsFactors = F)

  if(practice){

    sl.performance= data.frame(algorithm=character(),
                               max.score=numeric(),
                               cutoff.train=numeric(),
                               cutoff.test=numeric(),
                               cutoff.ratio=numeric(),
                               capture=numeric(),
                               reduction=numeric(),
                               capture.adjusted=numeric(),
                               reduction.adjusted=numeric())

    print("Looking for the algorithm")


    for(mdl in c(algorithm.dictionary$short, "SL")){
      print(paste("Started ",mdl,sep=""))

      if(mdl=="SL"){
        sl.model=algorithm.dictionary$full
      } else{
        sl.model=algorithm.dictionary$full[algorithm.dictionary$short==mdl]
      }

      # estimation & prediction
      bastiat.classifier = snowSuperLearner(train.y,train.x, family = binomial(),
                                            cluster=cluster,
                                            SL.library = sl.model)

      pred.train=data.frame(obs=train.y, pred=predict(bastiat.classifier, train.x)$pred[,1])
      pred.test= data.frame(obs=test.y, pred=predict(bastiat.classifier, test.x)$pred[,1])

      ## stats
      c.train=b_cutoff_probability(observations = pred.train$obs, predictions = pred.train$pred)
      c.test=b_cutoff_probability(observations = pred.test$obs, predictions = pred.test$pred)

      capture=nrow(subset(subset(pred.test, obs==1), pred>=c.train))/nrow(subset(pred.test, obs==1))
      reduction=1-nrow(subset(pred.test, obs==0& pred>=c.train))/nrow(subset(pred.test, obs==0))
      capture.adjusted=nrow(subset(subset(pred.test, obs==1), pred>=c.test))/nrow(subset(pred.test, obs==1))
      reduction.adjusted=1-nrow(subset(pred.test, obs==0 & pred>=c.test))/nrow(subset(pred.test, obs==0))

      sl.performance=rbind(sl.performance,
                           data.frame(algorithm=mdl,
                                      max.score=max(score_me(capture, reduction), score_me(capture.adjusted, reduction.adjusted)),
                                      cutoff.train=c.train,
                                      cutoff.test=c.test,
                                      cutoff.ratio=c.test/c.train,
                                      capture=nrow(subset(subset(pred.test, obs==1), pred>=c.train))/nrow(subset(pred.test, obs==1)),
                                      reduction=1-nrow(subset(pred.test, obs==0& pred>=c.train))/nrow(subset(pred.test, obs==0)),
                                      capture.adjusted=nrow(subset(subset(pred.test, obs==1), pred>=c.test))/nrow(subset(pred.test, obs==1)),
                                      reduction.adjusted=1-nrow(subset(pred.test, obs==0 & pred>=c.test))/nrow(subset(pred.test, obs==0)))
      )

      rm(capture, reduction, capture.adjusted, reduction.adjusted,c.train, c.test,bastiat.classifier)

      print(paste("Finished ",mdl,sep=""))

    }


    sl.winner=sl.performance$algorithm[sl.performance$max.score==max(sl.performance$max.score)]
    the.model<<-sl.winner

  } else {
    sl.winner=estimation.method
  }


  if(sl.winner=="SL"){
    sl.model=algorithm.dictionary$full
  } else{
    sl.model=algorithm.dictionary$full[algorithm.dictionary$short==sl.winner]
  }



  ###### SL Robustness
  print("Starting robustness")
  for(rnd in 1:turns){


    # another sampling for the data
    model.variables=b_create_model_variables(bid=training$bid,
                                             evaluation=training$evaluation,
                                             text=training$text,
                                             train.share = train.share,
                                             detective.number=detective.number,
                                             variables=variables,
                                             dtm.incl=dtm.incl,
                                             dtm.metric=dtm.metric,
                                             dtm.terms=dtm.terms)


    #### ESTIMATION
    ## estimates with SuperLearner

    ## SuperLearner data prep
    train=model.variables[,c("bid","evaluation",my.vars)]

    train.x = subset(train, bid %in% train.split)
    # train.x$acting.agency=as.numeric(train.x$acting.agency)
    test.x = subset(train, ! bid %in% train.split)
    # test.x$acting.agency=as.numeric(test.x$acting.agency)
    test.y=test.x$evaluation
    test.x$bid=NULL
    test.x$evaluation=NULL

    train.y=train.x$evaluation
    train.x$bid=NULL
    train.x$evaluation=NULL


    bastiat.classifier = snowSuperLearner(train.y,train.x, family = binomial(),
                                          cluster=cluster,
                                          SL.library = sl.model)

    pred.train=data.frame(obs=train.y, pred=predict(bastiat.classifier, train.x)$pred[,1])
    pred.test= data.frame(round=rnd, obs=test.y, pred=predict(bastiat.classifier, test.x)$pred[,1])

    # storing results
    result=rbind(result, pred.test)

    ## stats
    c.train=b_cutoff_probability(observations = pred.train$obs, predictions = pred.train$pred)
    c.test=b_cutoff_probability(observations = pred.test$obs, predictions = pred.test$pred)

    capture=nrow(subset(subset(pred.test, obs==1), pred>=c.train))/nrow(subset(pred.test, obs==1))
    reduction=1-nrow(subset(pred.test, obs==0& pred>=c.train))/nrow(subset(pred.test, obs==0))
    capture.adjusted=nrow(subset(subset(pred.test, obs==1), pred>=c.test))/nrow(subset(pred.test, obs==1))
    reduction.adjusted=1-nrow(subset(pred.test, obs==0 & pred>=c.test))/nrow(subset(pred.test, obs==0))

    stats=rbind(stats,
                data.frame(round=rnd,
                           algorithm=sl.model,
                           max.score=max(score_me(capture, reduction), score_me(capture.adjusted, reduction.adjusted)),
                           cutoff.train=c.train,
                           cutoff.test=c.test,
                           cutoff.ratio=c.test/c.train,
                           capture=nrow(subset(subset(pred.test, obs==1), pred>=c.train))/nrow(subset(pred.test, obs==1)),
                           reduction=1-nrow(subset(pred.test, obs==0& pred>=c.train))/nrow(subset(pred.test, obs==0)),
                           capture.adjusted=nrow(subset(subset(pred.test, obs==1), pred>=c.test))/nrow(subset(pred.test, obs==1)),
                           reduction.adjusted=1-nrow(subset(pred.test, obs==0 & pred>=c.test))/nrow(subset(pred.test, obs==0)))
    )

    rm(capture, reduction, capture.adjusted, reduction.adjusted,c.train, c.test)

    my.vars=mv

    print(rnd)
  }

  ## closing cluster
  parallel::stopCluster(cluster)
  ## end of estimation


  cutoff.adjustment=mean(stats$cutoff.ratio,na.rm = T)
  assumed.cutoff=cutoff.adjustment*min(stats$cutoff.train, na.rm=T)

  evaluation=as.data.frame.table(table(result$round))
  evaluation=merge(evaluation, as.data.frame.table(table(result$round[result$obs==1])), by="Var1")
  evaluation=merge(evaluation, as.data.frame.table(table(result$round[result$obs==1 & result$pred>=(assumed.cutoff)])), by="Var1")
  names(evaluation)=c("Var1", "leads.total", "leads.relevant", "leads.r.capture")

  evaluation=merge(evaluation, as.data.frame.table(table(result$round[result$pred>=(assumed.cutoff)])), by="Var1")
  names(evaluation)=c("Var1", "leads.total", "leads.relevant", "leads.r.capture", "leads.sent.adjusted")

  evaluation=merge(evaluation, as.data.frame.table(table(result$round[result$pred>=(min(stats$cutoff.train, na.rm=T))])), by="Var1")
  names(evaluation)=c("Var1", "leads.total", "leads.relevant", "leads.r.capture", "leads.sent.adjusted", "leads.sent")

  evaluation$capture.adjusted=evaluation$leads.r.capture/evaluation$leads.relevant

  setnames(evaluation, "Var1", "round")
  evaluation=merge(evaluation, stats[,c("round", "capture")], by="round", all.x=T)

  evaluation$reduction=1-(evaluation$leads.sent-round(evaluation$leads.relevant*evaluation$capture,0))/(evaluation$leads.total-evaluation$leads.relevant)
  evaluation$reduction.adjusted=1-(evaluation$leads.sent.adjusted-evaluation$leads.r.capture)/(evaluation$leads.total-evaluation$leads.relevant)
  # evaluation$reduction=1-evaluation$leads.sent/(evaluation$leads.total)
  # evaluation$reduction.adjusted=1-(evaluation$leads.sent.adjusted)/(evaluation$leads.total)

  stats.new=merge(stats[,c("round","cutoff.train","cutoff.test","capture","cutoff.ratio")], evaluation[,c("round", "reduction","reduction.adjusted", "capture.adjusted")], by="round")

  detective.stats=data.frame(detective.no=d.no,
                             date=Sys.Date(),
                             cutoff.ratio=mean(stats.new$cutoff.ratio),
                             cutoff.ratio.sd=sd(stats.new$cutoff.ratio),
                             capture=max(mean(stats.new$capture),0),
                             capture.sd=sd(stats.new$capture),
                             reduction=max(0,mean(stats.new$reduction)),
                             reduction.sd=sd(stats.new$reduction),
                             capture.adjusted=max(0,mean(stats.new$capture.adjusted)),
                             capture.adjusted.sd=sd(stats.new$capture.adjusted),
                             reduction.adjusted=max(0,mean(stats.new$reduction.adjusted)),
                             reduction.adjusted.sd=sd(stats.new$reduction.adjusted),
                             observed.cutoff=min(stats$cutoff.train, na.rm=T),
                             assumed.cutoff=assumed.cutoff)

  word.score.now=word.score

  output.list<- list("performance"=detective.stats, "word.scores"=word.score.now,
                     "classifier"=bastiat.classifier, "sl.outcome"=sl.performance)

  return(output.list)

}

