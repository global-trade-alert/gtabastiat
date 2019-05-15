# Roxygen documentation

#' Bastiat, extract from an XPATH but keep missing nodes as NA's.
#'
#' Performs some cleaning operation on a string. It's a wrapper that helps standardise code somewhat.
#' @param path.base Enter a portion of the path that you are sure that exists.
#' @param path.extension Enter the remainder where there may be gaps wrt to final node you are interested in.
#' @param get.attribute Do you want to extract the value from an attribute? Default is 'FALSE'.
#' @param the.attribute Name the attribute you want to extract the value from. Default is 'href'.
#'
#' @return A vector of values.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA based on 'hrbrmstr' in https://stackoverflow.com/questions/32429574


# Function infos and parameters  --------------------------------------------
b_xpath=function(html.doc="html",
                 path.base=NULL,
                  path.extension=NULL,
                  get.attribute=F,
                  the.attribute="href"){

  eval(parse(text=paste("html.dom=",html.doc, sep="")))

  return(xpathSApply(html.dom, path.base, function(x) {

    if(get.attribute){

      if(xpathSApply(x, paste("boolean(./",path.extension,")",sep=""))){

        xpathSApply(x, paste("./",path.extension,sep=""), xmlGetAttr, the.attribute)

      } else {
        NA
      }


    } else {

      if(xpathSApply(x, paste("boolean(./",path.extension,")",sep=""))) {

        xpathSApply(x, paste("./",path.extension,sep=""), xmlValue)

      } else {
        NA
      }


    }


  }))

}
