# Roxygen documentation

#' Bastiat, clean my string.
#'
#' Performs some cleaning operation on a string. It's a wrapper that helps standardise code somewhat.
#'
#' @param data.path Specifies where the GTA data file is located (Default: 'data/master_plus.Rdata'). Set to 'online' to download the latest copy.

#' @return Output is two data frames. First data frame includes the share of each year that in intervention was in force. Second data frame states parameter choices.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

b_clean_string = function(
  variable=NULL,
  command=NULL,
  parameter=NULL
) {

  if(command %in% c("gsub", "gs")){
    return(gsub(parameter[1],parameter[2], variable))
  }

  if(command %in% c("date","dt")){
    return(as.Date(variable, parameter[1]))
  }

  if(command %in% c("extract_all","ea")){
    return(stringr::str_extract_all(variable, parameter[1]))
  }


  if(command %in% c("ul_extract_all","uea")){
    return(unlist(stringr::str_extract_all(variable, parameter[1])))
  }

  if(command %in% c("extract","e")){
    return(stringr::str_extract(variable, parameter[1]))
  }

  if(command %in% c("grepl","g")){
    return(grepl(parameter[1],variable))
  }

  if(command %in% c("grepl_ignore","gi")){
    return(grepl(parameter[1],variable, ignore.case = T))
  }
}
