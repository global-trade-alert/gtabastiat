# Roxygen documentation

#' Bastiat, please re-estimate this detective.
#'
#' @return Several data frames about how the detective did.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

b_estimate_classifier = function(training.data=training,
                                 training.id="bid",
                                 training.y="evaluation",
                                 estimation.model=NULL,
                                 robustness.turns=0,
                                 train.share=.82
                                  ){

  train.split=sample(unique(training.data[,c(training.id)]), ceiling(nrow(training.data)*train.share))

  eval(parse(text = paste("train.x =subset(squad.result, ",training.id," %in% train.split)",sep="")))
  eval(parse(text = paste("test.x =subset(squad.result, !",training.id," %in% train.split)",sep="")))

  test.y=test.x[,c(training.y)]
  test.x=test.x[,setdiff(names(test.x),c(training.id, training.y))]

  train.y=train.x[,c(training.y)]
  train.x=train.x[,setdiff(names(train.x),c(training.id, training.y))]


  ## estimating an XGB
  algorithm.dictionary=data.frame(short=c("XGB","KNN","RNF", "SVM"),
                                  full=c("SL.xgboost", "SL.kernelKnn", "SL.randomForest", "SL.ksvm"),
                                  stringsAsFactors = F)

  if(is.null(estimation.model)){
    stop("No estimation model specified.")
  } else {
    sl.model=algorithm.dictionary$full[algorithm.dictionary$short==estimation.model]
  }

  new.classifier = SuperLearner(train.y,train.x, family = binomial(),
                                    SL.library = sl.model)

  pred.train=data.frame(obs=train.y, pred=predict(new.classifier, train.x)$pred[,1])
  pred.test= data.frame(obs=test.y, pred=predict(new.classifier, test.x)$pred[,1])

  ## stats
  c.train=b_cutoff_probability(observations = pred.train$obs, predictions = pred.train$pred)
  c.test=b_cutoff_probability(observations = pred.test$obs, predictions = pred.test$pred)

  capture=nrow(subset(subset(pred.test, obs==1), pred>=c.train))/nrow(subset(pred.test, obs==1))
  reduction=1-nrow(subset(pred.test, obs==0& pred>=c.train))/nrow(subset(pred.test, obs==0))
  capture.adjusted=nrow(subset(subset(pred.test, obs==1), pred>=c.test))/nrow(subset(pred.test, obs==1))
  reduction.adjusted=1-nrow(subset(pred.test, obs==0 & pred>=c.test))/nrow(subset(pred.test, obs==0))

  classifier.performance=data.frame(algorithm=sl.model,
                               score=b_score_me(capture, reduction),
                               score.adjusted=b_score_me(capture.adjusted, reduction.adjusted),
                               observed.cutoff=c.train,
                               assumed.cutoff=c.test,
                               cutoff.ratio=c.test/c.train,
                               cutoff.ratio.sd=NA,
                               capture=nrow(subset(subset(pred.test, obs==1), pred>=c.train))/nrow(subset(pred.test, obs==1)),
                               capture.sd=NA,
                               reduction=1-nrow(subset(pred.test, obs==0& pred>=c.train))/nrow(subset(pred.test, obs==0)),
                               reduction.sd=NA,
                               capture.adjusted=nrow(subset(subset(pred.test, obs==1), pred>=c.test))/nrow(subset(pred.test, obs==1)),
                               capture.adjusted.sd=NA,
                               reduction.adjusted=1-nrow(subset(pred.test, obs==0 & pred>=c.test))/nrow(subset(pred.test, obs==0)),
                               reduction.adjusted.sd=NA,
                               stringsAsFactors = F)



  if(robustness.turns>0){
    print("ADD THE ROBUSTNESS ROUNDS, JOHANNES")
  }


  output.list<- list("performance"=classifier.performance,
                     "classifier"=new.classifier)
  return(output.list)

}

