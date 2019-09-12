# Roxygen documentation

#' Bastiat, please estimate this classifier.
#'
#' @return A list including the classifier object plus a performance data frame.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA.


# Function infos and parameters  --------------------------------------------

bt_estimate_classifier = function(training.data=training,
                                  training.id="bid",
                                  training.y="evaluation",
                                  estimation.model="XGB",
                                  robustness.turns=0,
                                  train.share=.82
                                  ){


  classifier.performance=data.frame()

    algorithm.dictionary=data.frame(short=c("XGB","KNN","RNF", "SVM"),
                                  full=c("SL.xgboost", "SL.kernelKnn", "SL.randomForest", "SL.ksvm"),
                                  stringsAsFactors = F)

  if(is.null(estimation.model)| (! estimation.model %in% c("XGB","KNN","RNF", "SVM"))){
    stop("Unknown estimation model specified.")
  }

  for(model in estimation.model){

    sl.model=algorithm.dictionary$full[algorithm.dictionary$short==model]
    this.performance=data.frame()

    for(round in 1:(robustness.turns+1)){
      print(paste("Start estimation round",round,"for model", model))

      train.split=sample(unique(training.data[,c(training.id)]), ceiling(nrow(training.data)*train.share))

      eval(parse(text = paste("train.x =subset(squad.result, ",training.id," %in% train.split)",sep="")))
      eval(parse(text = paste("test.x =subset(squad.result, !",training.id," %in% train.split)",sep="")))

      test.y=test.x[,c(training.y)]
      test.x=test.x[,setdiff(names(test.x),c(training.id, training.y))]

      train.y=train.x[,c(training.y)]
      train.x=train.x[,setdiff(names(train.x),c(training.id, training.y))]


      ## estimation
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

      this.performance=rbind(this.performance,
                                   data.frame(algorithm=sl.model,
                                              score=b_score_me(capture, reduction),
                                              score.adjusted=b_score_me(capture.adjusted, reduction.adjusted),
                                              observed.cutoff=c.train,
                                              assumed.cutoff=c.test,
                                              cutoff.ratio=c.test/c.train,
                                              capture=nrow(subset(subset(pred.test, obs==1), pred>=c.train))/nrow(subset(pred.test, obs==1)),
                                              reduction=1-nrow(subset(pred.test, obs==0& pred>=c.train))/nrow(subset(pred.test, obs==0)),
                                              capture.adjusted=nrow(subset(subset(pred.test, obs==1), pred>=c.test))/nrow(subset(pred.test, obs==1)),
                                              reduction.adjusted=1-nrow(subset(pred.test, obs==0 & pred>=c.test))/nrow(subset(pred.test, obs==0)),
                                              stringsAsFactors = F))


      print(paste("Concluded estimation round",round,"for model", model))
    }


    classifier.performance=rbind(classifier.performance,
                                 data.frame(algorithm=sl.model,
                                            score=mean(this.performance$score),
                                            score.adjusted=mean(this.performance$score.adjusted),
                                            observed.cutoff=mean(this.performance$observed.cutoff),
                                            assumed.cutoff=mean(this.performance$assumed.cutoff),
                                            cutoff.ratio=mean(this.performance$cutoff.ratio),
                                            cutoff.ratio.sd=sd(this.performance$cutoff.ratio),
                                            capture=mean(this.performance$capture),
                                            capture.sd=sd(this.performance$capture),
                                            reduction=mean(this.performance$reduction),
                                            reduction.sd=sd(this.performance$reduction),
                                            capture.adjusted=mean(this.performance$capture.adjusted),
                                            capture.adjusted.sd=sd(this.performance$capture.adjusted),
                                            reduction.adjusted=mean(this.performance$reduction.adjusted),
                                            reduction.adjusted.sd=sd(this.performance$reduction.adjusted),
                                            stringsAsFactors = F))




  }

  output.list<- list("performance"=classifier.performance,
                     "classifier"=new.classifier)
  return(output.list)

}

