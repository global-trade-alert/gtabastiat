# Roxygen documentation

#' Bastiat, please convert this into GTA countries.
#' @param from Specify the origin classification.
#' @param to Specify the target classification.

#' @return Output is two data frames. First data frame includes the share of each year that in intervention was in force. Second data frame states parameter choices.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

b_gta_countries = function(
  convert.values,
  from=NULL,
  to=NULL
) {
  gta.countries=gtabastiat::gta.countries
  if(to=="gta.name"){
    if(from=="agnet.id"){
      return(sapply(convert.values, function(x) gta.countries$gta.name[gta.countries$agnet.id==x]))
    }

    if(from=="gain.name"){
      gta.countries=gtabastiat::gain.countries

      return(sapply(convert.values, function(x) gta.countries$gta.name[gta.countries$gain.name==x]))
    }
  }
  rm(gta.countries)
}
