# Roxygen documentation

#' Bastiat, please send a squad to check these cases.
#'
#' @return Have the Bastiat Detective's School latest talent investigate your case.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

b_case_squad=function(value.capture, value.reduction){


  source("code/Bastiat base.R")


  load("data/classifier/training data.Rdata")
  training.base=training

  load("content/0 core/Classifier statistics & history.Rdata")

  contenders=c("incumbent", setdiff(unique(subset(model, detective.no %in% subset(stats, date>="2019-03-08" & (score>=.7|score.adjusted>=.7))$detective.no)$name),
                                    c("Wallander","Tin Tin",model$name[nrow(model)])))

  model.files=list.files(path = "content/0 core/", pattern = ".Rdata",  full.names = T)
  model.files=model.files[grepl(paste(contenders, collapse="|"), model.files)]


  result=data.frame()


  ## running all models
  ## set the dataset
  # competition.split=sample(unique(training.base$bid), 7000)
  # training=subset(training.base, bid %in% competition.split)

  training=training.base

  for(detective in contenders){

    print(paste("Starting",detective))

    ## loading the estimator
    if(detective=="incumbent"){
      load("content/0 core/Bastiat classifier.Rdata")

      my.vars=unlist(str_split(model$my.vars[nrow(model)], ";"))

      di=model$dtm.incl[nrow(model)]
      dm=model$dtm.metric[nrow(model)]
      dt=model$dtm.terms[nrow(model)]

    } else {

      load(model.files[grepl(detective, model.files)][1])

      e.m=model$estimation.method[max(which(grepl(detective, model$name)))]

      print(e.m)
      if(is.null(names(incumbent.classifier[["fitLibrary"]]$SL.randomForest_All$object$forest$xlevels))==F){

        my.vars=names(incumbent.classifier[["fitLibrary"]]$SL.randomForest_All$object$forest$xlevels)
      } else {
        if(is.null(incumbent.classifier[["fitLibrary"]]$SL.xgboost_All$object$feature_names)==F){

          my.vars=incumbent.classifier[["fitLibrary"]]$SL.xgboost_All$object$feature_names
        } else {

          print("plop")

        }


      }

      di=model$dtm.incl[max(which(grepl(detective, model$name)))]
      dm=model$dtm.metric[max(which(grepl(detective, model$name)))]
      dt=model$dtm.terms[max(which(grepl(detective, model$name)))]

    }

    if("acting.agency" %in% my.vars){
      aa=training$acting.agency
    }




    ## processing the text
    train=b_create_model_variables(bid=training$bid,
                                   text=training$text,
                                   train.share = .82,
                                   detective.number=999,
                                   acting.agency=NULL,
                                   variables=my.vars,
                                   dtm.incl=di,
                                   dtm.metric=dm,
                                   dtm.terms=dt,
                                   keywords=T,
                                   is.td=T)

    train=merge(train, training[,c("bid", "acting.agency")],by="bid")



    train.bid=train$bid

    train=train[,c(my.vars)]






    ## prediction
    result= rbind(result,
                  data.frame(contender=detective,
                             bid=train.bid,
                             pred=predict(incumbent.classifier, train)$pred[,1], stringsAsFactors = F))

    rm(incumbent.classifier)

    print(paste("Finish",detective))


  }



  result=merge(result, unique(training[,c("bid","evaluation")]), by="bid", all.x=T)

  result$contender[result$contender=="incumbent"]=paste(model$name[nrow(model)], " (*)", sep="")





}
