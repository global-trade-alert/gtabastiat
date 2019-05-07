# Roxygen documentation

#' Bastiat, clean my string.
#'
#' Performs some cleaning operation on a string. It's a wrapper that helps standardise code somewhat.
#' @param variable Specify the vector on which to apply the cleaning operation.
#' @param command Specify the command you want to use for the cleaning.
#' @param parameter Give the parameter commands to do the cleaning.
#'
#' @return Clean text.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

b_clean = function(
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

  if(command %in% c("extract_all","sea")){
    return(stringr::str_extract_all(variable, parameter[1]))
  }

  if(command %in% c("ul_split","usp")){
    spt=unlist(data.table::strsplit(variable, parameter[1]))

    if(parameter[2]=="max"){
      n=length(spt)

    } else {
      if(parameter[2]=="min"){
        n=1
      } else {

        if(parameter[2]=="from.end"){

          n=length(spt)-as.numeric(parameter[3])


        } else {

          n=parameter[2]
        }


      }
    }

    return(spt[n])
  }

  if(command %in% c("ul_extract_all","usea")){
    return(unlist(stringr::str_extract_all(variable, parameter[1])))
  }

  if(command %in% c("extract","se")){
    return(stringr::str_extract(variable, parameter[1]))
  }

  if(command %in% c("grepl","g")){
    return(grepl(parameter[1],variable))
  }

  if(command %in% c("grepl_ignore","gi")){
    return(grepl(parameter[1],variable, ignore.case = T))
  }

  if(command %in% c("numeric","n")){
    return(as.numeric(as.character(variable)))
  }

  if(command %in% c("country","cty")){
    variable<<-variable
    return(b_gta_countries(variable, parameter[1],parameter[2]))
    rm(variable)
  }

}
