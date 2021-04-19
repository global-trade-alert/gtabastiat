# Roxygen documentation

#' An updated version of b_load_site
#'
#' Waits for the specified xpath node to be present in the HTML of webdriver's
#' current page.
#'
#' Requires webdriver to be in use, assigned to an object called 'remDr'.
#'
#'
#' @param xpath Provide the XPATH for which the function should wait.
#' @param wait Specify the number of seconds you want to wait. Default is 60.
#' @param abort Specifiy whether to cause an error in case path is not find
#'   after waiting time. Default is FALSE.
#'
#' @return Pauses the algorithm until XPATH appears.
#' @references www.globaltradealert.org



# Function infos and parameters  --------------------------------------------
bt_load_site = function(xpath=NULL,
                        wait=60,
                        abort=F,
                        wait.interval=1.5,
                        assign.html=F){

  html.load= htmlParse(remDr$getSource()[[1]], asText=T)
  message("Loading site...")

  refreshed=FALSE
  t=Sys.time()
  cat("waiting")
  while(refreshed==F & as.numeric(Sys.time()-t)<=wait){
    cat(".")
    Sys.sleep(wait.interval)
    html.load = htmlParse(remDr$getSource()[[1]], asText=T)

    refreshed=length(xpathSApply(html.load, xpath, xmlValue))>0

  }

  cat("\n")

  if(refreshed){

    message("Site's fresh!")
    if(assign.html){
      assign("html", html.load, envir=.GlobalEnv)
    }

  }else {

    if(abort){
      stop("Site does not respond")
    }else{
      message("Site does not respond")
    }

  }




  # legacy version
  # html <- htmlParse(remDr$getSource()[[1]], asText=T)
  # load=length(xpathSApply(html, xpath, xmlValue))
  #
  # sleep=1
  # while(load==0){
  #   print(paste("waiting ", sleep, sep=""))
  #   Sys.sleep(1)
  #
  #   html <- htmlParse(remDr$getSource()[[1]], asText=T)
  #   load=length(xpathSApply(html, xpath, xmlValue))
  #   sleep=sleep+1
  #
  #   if(sleep>wait){
  #
  #     if(abort){
  #       stop("site does not respond")
  #     }else{
  #       print("site does not respond")
  #     }
  #
  #     load=1
  #   }
  # }
  # print("loading complete")
}

