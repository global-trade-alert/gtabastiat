# Roxygen documentation

#' Bastiat, please score this detective's performance.
#'
#' @return Get a value between 0 and 1 for how good he did.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

b_score_me=function(value.capture, value.reduction){
  if(round(value.capture, 2)<capture.cutoff){
    return(0)
  } else {
    return(2/(2+capture.weight*abs(log(value.capture+.0001))+ abs(log(value.reduction+.0001))))
  }
}
