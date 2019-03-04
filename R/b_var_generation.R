# Roxygen documentation

#' Bastiat, draw new variables for my model.
#'
#' Have Bastiat specify your learning model for you
#'
#' @return A vector of variable names.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

b_var_generation <- function(x, method="fully random") {

  ### ARSENAL
  deltas=c("delta.sum","delta.min","delta.max")
  scores=c("score.r.idf","score.r.oidf","score.ir.idf","score.ir.oidf")
  keys=c("pos.word","pos.word.char", "neg.word","neg.word.char")
  excl=c("exclusive.r.sum","exclusive.r.max","exclusive.ir.sum", "exclusive.ir.max",
         "gta.gini.all","gta.gini.source","gta.gini.title","gta.gini.description",
         "gta.share.all","gta.share.source","gta.share.title","gta.share.description",
         "gini.normalised","odds.relevant","odds.irrelevant", "odds.ratio")
  fe=c("acting.agency","is.td")

  arsenal=c(deltas, scores, keys, excl, fe)



  ### SELECTION [ADD ML here to make this non-random]

  if(method=="fully random"){
    return(sample(arsenal, x))
  }

  if(method=="random by category"){

    good.draw=F
    while(good.draw==F){

      # slicing the distance 0:x in 5 parts
      c= sample(2:(x-1),1)
      b= ifelse(c<=1,0,sample(0:(c-1),1))
      a= ifelse(b<=1,0,sample(0:(b-1),1))
      d =ifelse((c + 1)==x,x,sample((c + 1):x, 1))

      i=0
      while(i<5 & good.draw==F){
        print("Drawing variables by category ...")
        sample.categories=sample(c(a, b - a, c - b,d-c, x - d),5,replace=F)

        good.draw=sum(as.numeric(c(sample.categories[1]>length(fe),
                                   sample.categories[2]>length(deltas),
                                   sample.categories[3]>length(scores),
                                   sample.categories[4]>length(keys),
                                   sample.categories[5]>length(excl))))==0
        i=i+1


      }
    }
    print("Found some!")

    return(c(sample(fe, sample.categories[1]),
              sample(deltas, sample.categories[2]),
              sample(scores, sample.categories[3]),
              sample(keys, sample.categories[4]),
              sample(excl, sample.categories[5])))

  }

}
