# Roxygen documentation

#' Bastiat, generate the detective stats for this model.
#'
#' @return A data frame including the model performance metrics.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

b_model_stats <- function(training.df="pred.train",
                          test.df="pred.test",
                          capture.cutoff=.97) {


  c.share=capture.cutoff

  # training set
  capture.train=subset(pred.train, obs==1)
  capture.train=capture.train[order(-capture.train$pred),]
  c.pos.train=round(nrow(capture.train)*c.share,0)
  c.pred.train=capture.train$pred[c.pos.train]

  # test set
  capture=subset(pred.test, obs==1)
  capture=capture[order(-capture$pred),]
  c.pos=round(nrow(capture)*c.share,0)
  c.pred=capture$pred[c.pos]

  stats=data.frame(round=rnd,
                   cutoff.train=c.pred.train,
                   cutoff.test=c.pred,
                   capture=nrow(subset(capture, pred>=c.pred.train))/nrow(capture),
                   cutoff.ratio=c.pred/c.pred.train)

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

  stats.new=merge(stats, evaluation[,c("round", "reduction","reduction.adjusted", "capture.adjusted")], by="round")



}
