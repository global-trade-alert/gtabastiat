#' The classification part of bt_leads_core_update()
#'
#' Designed for use on DPA leads. Gives them a relevance probability using BT's standard squad prediction.
#' The code is taken directly from bt_leads_core_update to preserve similarity.
#'
#'
#' @param update.core standard update.core with the standard variables.
#' @param assign.relevance do you want to assign the 'relevant=1/0' from the classification process? default no
#'
#' @return a new update.core with probabilities
#'
#' @examples update.core = bt_leads_classify_only(update.core)
bt_leads_classify_only = function(update.core,
                                  assign.relevance=F){

  lc.update = update.core

  ## classifying results
  classify=subset(lc.update, lc.update$classify==1 &
                    lc.update$relevant==1 &
                    lc.update$country.lead!="Vatican")

  if(nrow(classify)>0){


    classify$text=classify$act.title.en
    classify$text[is.na(classify$act.description.en)==F]=paste(classify$text[is.na(classify$act.description.en)==F],
                                                               classify$act.description.en[is.na(classify$act.description.en)==F],
                                                               sep=" ")

    classify$text[is.na(classify$act.values)==F]=paste(classify$text[is.na(classify$act.values)==F],
                                                       classify$act.values[is.na(classify$act.values)==F],
                                                       sep=" ")

    # removing non-ASCII
    classify$text=stringi::stri_trans_general(classify$text, "latin-ascii")
    classify$text=gsub("[^\001-\177]","",classify$text)

    #check for bad chars
    potential.problems = gsub(pattern = "[^A-zÀ-ÿ]|_", #the regex engine in R treats _ as an alphanumeric, must include it explicitly
         replacement = " ",
         x = classify$text) %>%
      str_squish() %>%
      nchar() <= 1
    if(sum(potential.problems)>0){
      prob.msg = paste0("Warning! ", sum(potential.problems), " entries contain no ASCII chars and will not be classified!")
      message(prob.msg)
      classify = classify[!potential.problems,]

    }

    #dbg
    # prediction.data.id=classify$bid
    # prediction.data.text=classify$text
    # prediction.acting.agency=classify$acting.agency

    classification.result=bt_squad_prediction(prediction.data.id=classify$bid,
                                              prediction.data.text=classify$text,
                                              prediction.acting.agency=classify$acting.agency)

    classify$relevant=NULL
    classify$relevance.probability=NULL
    classify$text=NULL

    classify=merge(classify, classification.result, by.x="bid",by.y="text.id")

    classified.bids=classify$bid
    classified.lids=classify$lead.id
    classified.relevance=classify$relevant
    classified.rel.prob=round(classify$relevance.probability,4)

    lc.update$relevance.probability = NA

    lc.update=rbind(subset(lc.update, ! bid %in% classified.bids),
                    classify)

    #need to preserve the col order

    #this func is usually used on DPA leads where we only want the relevance prob as a continuous value
    #binary relevance is not appropriate for DPA leads as it is trained on GTA content
    if(assign.relevance){
    update.core$relevant = lc.update$relevant
    }


    update.core$relevance.probability = lc.update$relevance.probability

    return(update.core)





  }

 return(lc.update)


}
