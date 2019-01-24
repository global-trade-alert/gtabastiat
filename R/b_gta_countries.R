# Roxygen documentation

#' Bastiat, please convert this into GTA countries.

#' @return Output is two data frames. First data frame includes the share of each year that in intervention was in force. Second data frame states parameter choices.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

b_gta_countries = function(
  vector=NULL,
  from=NULL,
  to=NULL
) {


  b_eval(paste("return(gtabastiat::gta.countries$",to,"[gta.countries$",from,"==vector])",sep=""))



}
