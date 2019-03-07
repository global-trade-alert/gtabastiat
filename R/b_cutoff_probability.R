# Roxygen documentation

#' Bastiat, what's the minimum prediction probability for this model?
#'
#' @return A scalar.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

b_cutoff_probability <- function(observations=NULL,
                          predictions=NULL,
                          capture.cutoff=.97) {


  capture=data.frame(obs=observations, pred=predictions)

  capture=subset(capture, obs==1)
  capture=capture[order(-capture$pred),]
  capture.position=round(nrow(capture)*capture.cutoff,0)

  capture.cutoff=capture$pred[capture.position]

  return(capture.cutoff)

}
