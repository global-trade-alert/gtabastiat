# Roxygen documentation

#' Bastiat, please convert this into GTA countries.

#' @return Output is two data frames. First data frame includes the share of each year that in intervention was in force. Second data frame states parameter choices.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

b_gta_countries = function(
  convert.values=NULL,
  from=NULL,
  to=NULL
) {

  gta.countries=gtabastiat::gta.countries

  b_eval(paste("converted=sapply(convert.values, function(x) gta.countries$",to,"[gta.countries$",from,"==x])", sep=""))
  rm(gta.countries)
  return(converted)

}
