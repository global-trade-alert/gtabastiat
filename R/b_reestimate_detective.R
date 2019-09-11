# Roxygen documentation

#' Bastiat, please re-estimate this detective.
#'
#' @return Several data frames about how the detective did.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

b_reestimate_detective = function(detective=NULL,
                                  estimation.turns=10,
                                  train.share=.82,
                                  save.result=T,
                                  save.classifier=T
                                  ){
  load("content/0 core/Classifier statistics & history.Rdata")

  print(paste("Starting",detective))

  ## parameters
  d.no=max(subset(model, name==detective)$detective.no)
  my.vars=unique(unlist(strsplit(as.character(subset(model, name==detective & detective.no==d.no)$my.vars), ";")))
  the.model=subset(model, name==detective & detective.no==d.no)$estimation.method
  dt.incl=subset(model, name==detective & detective.no==d.no)$dtm.incl
  dt.mtrc=subset(model, name==detective & detective.no==d.no)$dtm.metric
  nr.of.terms=subset(model, name==detective & detective.no==d.no)$dtm.terms

  ## old stats
  old.scores=subset(stats, detective.no==d.no)$score
  old.scores.adjusted=subset(stats, detective.no==d.no)$score.adjusted
  old.scores=old.scores[length(old.scores)]
  old.scores.adjusted=old.scores.adjusted[length(old.scores.adjusted)]

  ## train and save
  training.stats=b_train_detective(detective.number=d.no,
                                   variables=my.vars,
                                   estimation.rounds=estimation.turns,
                                   estimation.method = the.model,
                                   dtm.incl=dt.incl,
                                   dtm.metric=dt.mtrc,
                                   dtm.terms=nr.of.terms,
                                   train.share=train.share,
                                   tested.models = bt.models,
                                   practice = F)

  t.stats=training.stats$performance
  t.stats$score=score_me(t.stats$capture, t.stats$reduction)
  t.stats$score.adjusted=score_me(t.stats$capture.adjusted, t.stats$reduction.adjusted)

  print(paste("old vs new score: ", round(max(old.scores, old.scores.adjusted),2)," vs ",round(max(t.stats$score, t.stats$score.adjusted),2),sep=""))

  ## storing result

  if(save.result){
    load("content/0 core/Classifier statistics & history.Rdata")

    t.stats$detective.no=d.no

    stats=rbind(stats, t.stats)
    word.score=training.stats$word.scores
    incumbent.classifier=training.stats$classifier

    save(model, stats, word.score, file="content/0 core/Classifier statistics & history.Rdata")

  }

  if(save.classifier){
    save(incumbent.classifier, file=paste("content/0 core/",Sys.Date()," - ",detective," classifier.Rdata", sep=""))
  }


  print(paste("Finish",detective))


}

