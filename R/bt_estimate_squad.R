# Roxygen documentation

#' Bastiat, please estimate a new squad classifier based on the best detectives.
#'
#' @return Several data frames about how the detective did.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

bt_estimate_squad = function(squad.level=.7,
                             train.share=.82,
                             squad.model="XGB",
                             squad.robustness=0,
                             detective.log="content/0 core/Classifier statistics & history.Rdata",
                             squad.log="content/0 core/Squad statistics & history.Rdata",
                             estimation.data="data/classifier/training data.Rdata"
                                 ){

  load(detective.log)


  contenders=c("incumbent", setdiff(unique(subset(model, detective.no %in% subset(stats, date>="2019-03-08" & (score>=squad.level|score.adjusted>=squad.level))$detective.no)$name),
                                    c("Wallander","Tin Tin",model$name[nrow(model)])))


  model.files=list.files(path = "content/0 core/", pattern = ".Rdata",  full.names = T)
  model.files=model.files[grepl(paste(contenders, collapse="|"), model.files)]

  bt.squad=data.frame(member.name=contenders,
                      classifier.location=NA,
                      stringsAsFactors = F)

  for(i in 1:nrow(bt.squad)){
    bt.squad$classifier.location[i]=model.files[grepl( bt.squad$member.name[i], model.files)][1]
  }

  bt.squad$classifier.location[bt.squad$member.name=="incumbent"]="content/0 core/Bastiat classifier.Rdata"
  bt.squad$member.name[bt.squad$member.name=="incumbent"]=model$name[nrow(model)]

  if(any(is.na(bt.squad$classifier.location))){
    need.classifier=bt.squad$member.name[is.na(bt.squad$classifier.location)]
    bt_estimate_detective(detective.name=need.classifier)

    model.files=list.files(path = "content/0 core/", pattern = ".Rdata",  full.names = T)
    model.files=model.files[grepl(paste(contenders, collapse="|"), model.files)]

    for(i in 1:nrow(bt.squad)){
      bt.squad$classifier.location[i]=model.files[grepl( bt.squad$member.name[i], model.files)][1]
    }

    if(any(is.na(bt.squad$classifier.location))){
      still.need.classifier=bt.squad$member.name[is.na(bt.squad$classifier.location)]
      stop(paste("We are missing classifiers of",paste(still.need.classifier, collapse=",")))}

  }


  ## comparing with existing squad
  load(squad.log)
  if(length(setdiff(bt.squad$member.name,
                    subset(all.squads, squad.number=max(all.squads$squad.number))$member.name))==0 &
     length(setdiff(subset(all.squads, squad.number=max(all.squads$squad.number))$member.name,
                    bt.squad$member.name)==0)){
    s.no=max(all.squads$squad.number)
  } else {

    s.no=max(all.squads$squad.number)+1

    new.sq=bt.squad
    new.sq$squad.number=s.no
    new.sq$starting.date=Sys.Date()

    all.squads=rbind(all.squads, new.sq)
    rm(new.sq)
    save(all.squads, file="content/0 core/Squad statistics & history.Rdata")
  }


  ## getting squad member predictions
  squad.predictions=data.frame()

  load(estimation.data)
  for(squad.member in bt.squad$member.name){
    squad.predictions=rbind(squad.predictions,
                            bt_detective_prediction(detective=squad.member,
                                                    classifier.location=bt.squad$classifier.location[bt.squad$member.name==squad.member],
                                                    prediction.data.id=training$bid,
                                                    prediction.word.score=word.score,
                                                    prediction.data.text=training$text,
                                                    prediction.acting.agency=training$acting.agency,
                                                    train.share=train.share))

  }


  ## squad variable selection here
  # currently only use prediction probability, not whether it is classified as relevant.
  squad.predictions$relevant=NULL

  squad.predictions=merge(squad.predictions, unique(training[,c("bid", "evaluation")]), by="bid", all.x=T)

  squad.predictions=reshape(squad.predictions, idvar = c("bid", "evaluation"), timevar = "detective", direction="wide")

  # Estimating squad classifier
  squad.estimation=bt_estimate_classifier(training.data=squad.predictions,
                                          training.id="bid",
                                          training.y="evaluation",
                                          estimation.model=squad.model,
                                          robustness.turns=squad.robustness,
                                          train.share=train.share)

  classifier=squad.estimation$classifier
  cutoff=squad.estimation$cutoff
  squad.stats$performance

  print(paste("New score is",squad.stats$score," and ",squad.stats$score.adjusted, " (adjusted)"))
  save(classifier, bt.squad, cutoff, file="content/0 core/Bastiat squad classifier.Rdata")

  squad.stats$quad.no=s.no
  squad.stats$quad.no=date=Sys.Date()

  squad.statistics=rbind(squad.statistics,
                         squad.stats)
  ## saving the stats & squads
  save(squad.statistics, all.squads, file=squad.log)


}

