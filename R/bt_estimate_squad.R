# Roxygen documentation


#' Perform estimation of squad classifier
#' @return Several data frames about how the detective did.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

#' Bastiat, please estimate a new squad classifier based on the best detectives.
#'
#' @param detectives the squad to be estimated
#' @param squad.level min threshold for the squad score
#' @param train.share train:test split ratio
#' @param squad.model which model to use for the squad
#' @param squad.robustness
#' @param detective.log location of the log file
#' @param squad.log location of the squad log file
#' @param estimation.data the data to be used for testing
#' @param limit.classification reduces the amount of rows of data to test to reduce comp load (my 4GB ram laptop tends to error above ~50k rows hence this being the default value)
#'
#' @return Several data frames about how the detective did.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA
#'
#' @examples
bt_estimate_squad = function(detectives=NULL,
                             squad.level=.7,
                             train.share=.82,
                             squad.model="XGB",
                             squad.robustness=0,
                             detective.log="content/0 core/Classifier statistics & history.Rdata",
                             squad.log="content/0 core/Squad statistics & history.Rdata",
                             estimation.data="data/classifier/training data.Rdata",
                             limit.classification = 50000
                             ){

  #dbg
  # squad.level=.7
  # train.share=.82
  # squad.model="XGB"
  # squad.robustness=0
  # detective.log="content/0 core/Classifier statistics & history.Rdata"
  # squad.log="content/0 core/Squad statistics & history.Rdata"
  # estimation.data="data/classifier/training data.Rdata"

  load(detective.log)
  if(is.null(detectives)){

    #load(detective.log)
    #this is where the 'model' df is saved. not loading it here means the rest
    #doesn't work. hence why I move it outside the if statement.

    detectives=c("incumbent", setdiff(unique(subset(model, detective.no %in% subset(stats, date>="2019-03-08" & (score>=squad.level|score.adjusted>=squad.level))$detective.no)$name),
                                      c("Wallander","Tin Tin",model$name[nrow(model)])))

  }




  model.files=list.files(path = "content/0 core/", pattern = ".Rdata",  full.names = T)
  model.files=model.files[grepl(paste(detectives, collapse="|"), model.files)]

  bt.squad=data.frame(member.name=detectives,
                      classifier.location=NA,
                      stringsAsFactors = F)


  #for the squad members
  #this is very important. it loads the MOST RECENT classifier - i.e. the all.classifiers[num.total.classifiers] : the one at the end of this list
  for(i in 1:nrow(bt.squad)){
    if(bt.squad$member.name[i]!= "incumbent"){
      bt.squad$classifier.location[i]=model.files[grepl(bt.squad$member.name[i], model.files)][length(model.files[grepl(bt.squad$member.name[i], model.files)])]
    }
    }

  #for the incumbent
  bt.squad$classifier.location[bt.squad$member.name=="incumbent"]="content/0 core/Bastiat classifier.Rdata"
  bt.squad$member.name[bt.squad$member.name=="incumbent"]=model$name[nrow(model)]

  if(any(is.na(bt.squad$classifier.location))){
    need.classifier=bt.squad$member.name[is.na(bt.squad$classifier.location)]
    bt_estimate_detective(detective.name=need.classifier)

    model.files=list.files(path = "content/0 core/", pattern = ".Rdata",  full.names = T)
    model.files=model.files[grepl(paste(detectives, collapse="|"), model.files)]

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
    save(all.squads, squad.statistics, file=squad.log)
  }


  ## getting squad member predictions
  print(paste("Squad members are:",paste(bt.squad$member.name, collapse="; ")))

  squad.predictions=data.frame()

  load(estimation.data)

  #reduce the size of the set because my computer dies x_x
  #data.downscale = 3
  #data.downscale = 1
  #training = training[sample(nrow(training), (nrow(training)/data.downscale)),]

  if(nrow(training)>limit.classification){
    training = training[sample(nrow(training), limit.classification),]
  }
  for(squad.member in bt.squad$member.name){

    load(bt.squad$classifier.location[bt.squad$member.name==squad.member])
    rm(classifier, cutoff)
    squad.predictions=rbind(squad.predictions,
                            bt_detective_prediction(detective=squad.member,
                                                    classifier.location=bt.squad$classifier.location[bt.squad$member.name==squad.member],
                                                    prediction.data.id=training$bid,
                                                    prediction.word.score=word.score,
                                                    prediction.data.text=training$text,
                                                    prediction.acting.agency=training$acting.agency,
                                                    train.share=train.share))

  }

  print("Squad members made their predictions.")

  #dbg
  #sp2 = squad predictions

  ## squad variable selection here
  # currently only use prediction probability, not whether it is classified as relevant.
  squad.predictions$relevant=NULL

  squad.predictions=merge(squad.predictions, unique(training[,c("bid", "evaluation")]), by="bid", all.x=T)

  squad.predictions=reshape(squad.predictions, idvar = c("bid", "evaluation"), timevar = "detective", direction="wide")

  names(squad.predictions)=gsub(" ","", tolower(names(squad.predictions)))

  print("Estimating squad classifier ...")
  # Estimating squad classifier



  #there are a few NAs in the training data. very rare but causes classifier to break, so let's remove them

  #not sure why this line was here. caused the entire thing to break. CC 2021-11-02
  #squad.predictions = training.data[rowSums(is.na(training.data)) == 0,]

  squad.estimation=bt_estimate_classifier(training.data=squad.predictions,
                                          training.id="bid",
                                          training.y="evaluation",
                                          estimation.model=squad.model,
                                          robustness.turns=squad.robustness,
                                          train.share=train.share)

  classifier=squad.estimation$classifier
  cutoff=squad.estimation$cutoff
  squad.stats=squad.estimation$performance

  print(paste("New score is",squad.stats$score," and ",squad.stats$score.adjusted, " (adjusted)"))
  save(classifier, bt.squad, cutoff, file="content/0 core/Bastiat squad classifier.Rdata")

  squad.stats$squad.no=s.no
  squad.stats$date=Sys.Date()

  squad.statistics=rbind(squad.statistics,
                         squad.stats)
  ## saving the stats & squads
  save(squad.statistics, all.squads, file=squad.log)


}

