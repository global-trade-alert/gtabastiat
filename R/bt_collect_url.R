# Roxygen documentation

#' Bastiat, collects a URL and stores it locally using file name. Websites are printed into PDF, URLS to files are downloaded.
#'
#'
#' @return #returns a list of 4 objs: new.file.name, file.suffix, url, status {see gta_prod.gta_url_status_list for these}
#' @references www.globaltradealert.org
#' @author Johannes Fritz, Robin Scherrer and Callum Campbell for GTA


# Function infos and parameters  --------------------------------------------


bt_collect_url = function(url=NULL,
                          file.name=NULL,
                          store.path=NULL,
                          add.timestamp=F,
                          update.file.name=F,
                          js.path="setup/rasterize.js",
                          phantom.path="~/bin/phantomjs"){


  # example vars for debugging
  # file.name="2022-10-12-10-30-30_eur-lexeuropaeu-4c66d7945bed408c1c7acefe8ddd4c7e"
  # store.path="0 source completion/temp"
  # url="http://web.archive.org/web/20130305174905/http://eur-lex.europa.eu:80/JOHtml.do?uri=OJ:C:2013:056:SOM:EN:HTML"
  # phantom.path="~/bin/phantomjs"

  library(httr)

  # send request to check if page is redirecting or broken
  r = tryCatch({
    httr::HEAD(url=url, timeout(30))
  }, error = function(e) {
    FALSE
  })

  if(is.logical(r)) {
    return(list("new.file.name"=NA,
                "file.suffix"=NA,
                "url"=url,
                status = 1)) #GET returned error
  } else if (((r$url != url) & #check if we have been redirected
              (paste0(r$url, "/") != url)) &
             (gsub("www\\.", "", r$url) != url)){

    #IF REDIRECTD, THEN...
    if(grepl("(\\D404\\D)|(\\D404$)", r$url) & #check for 'soft 404'
       r$status_code == 200){
      return(list("new.file.name"=NA,
                  "file.suffix"=NA,
                  "url"=url,
                  status = 3))
    }else{
      return(list("new.file.name"=NA, #if not 'soft 404', return redirected status and new url
                  "file.suffix"=NA,
                  "url"=r$url,
                  status = 2))
    }

  } else if(r$status_code == "404"){ #chcek for proper 404
    return(list("new.file.name"=NA,
                "file.suffix"=NA,
                "url"=url,
                status = 4))
  } else if(r$status_code == "405"){ #chcek for 405 (blocked)
    return(list("new.file.name"=NA,
                "file.suffix"=NA,
                "url"=url,
                status = 8))
  } else if(r$status_code != "200"){ #chcek for other statuses (eg 521)
    return(list("new.file.name"=NA,
                "file.suffix"=NA,
                "url"=url,
                status = 3))
  }




  # define where the file should be stored
  if(is.null(store.path)){
    file.path=file.name
  } else {
    file.path=paste0(gsub("/$", "", store.path), "/", file.name)
  }

  # create the time stamp to include it in the file name
  if(add.timestamp){
    t.stamp=paste0(" - ", gsub("\\D", "-", Sys.time()))
  } else {
    t.stamp=""
  }

  # check whether URL leads to a file
  file.types=c("doc", "pdf","xls","txt","csv","rdata")
  is.file=grepl(paste(file.types, collapse="|"), str_extract(url, "\\.[A-Za-z]{1,5}$"), ignore.case = T)

  # download the file if there is one or save a screenshot of the webpage
  if(is.file) {
    file.suffix=str_extract(url,"\\.[A-Za-z]{1,5}$")
    GET(url, write_disk(paste0(file.path, t.stamp, file.suffix), overwrite=TRUE))

    if(update.file.name){
      new.file.name=paste0(file.path, t.stamp, file.suffix)
    } else {
      new.file.name=file.name
    }

    return(list("new.file.name"=new.file.name,
                "file.suffix"=file.suffix,
                "url"=url,
                status = 6))
  } else {
    file.suffix=".pdf"
    cmd=paste(phantom.path,
              js.path,
              paste0("'",url,"'"),
              paste0("'",file.path, t.stamp, file.suffix,"'"),
              "'A1'")
    #run the command and save result to a variable
    output = system(cmd,
                    intern = T,
                    timeout = 120)
  }

  if(length(output)>0){
    print("rasterize.js failed, output was:")
    print(paste(output, collapse = "\n", sep ="\n"))


    library(webdriver)
    pjs <- run_phantomjs()
    remDr=Session$new(port=pjs$port)
    remDr$go(url)
    w = remDr$getWindow()
    curr.width = w$getSize()$width

    #set to 1920 width for standard HD res
    w$setSize(width = 1920, height = curr.height*(1920/curr.width))

    file.suffix = ".PNG"

    if(update.file.name){
      new.file.name=paste0(file.path, t.stamp, file.suffix)
    } else {
      new.file.name=file.name
    }

    remDr$takeScreenshot(file = paste0(file.path, ".PNG"))


    return(list("new.file.name"=new.file.name,
                "file.suffix"=file.suffix,
                "url"=url,
                status = 11))
  }

  file.saved.check = grepl(file.name, list.files(file.path)) %>% any()

  if(!file.saved.check){

    print("successful scrape, but rasterize.js failed to save the PDF for an unknown reason. (this is a very fiendish error)")
    return(list("new.file.name"=NA,
                "file.suffix"=NA,
                "url"=r$url,
                status = 9))
  }

  # return the file name
  if(update.file.name){
    new.file.name=paste0(file.path, t.stamp, file.suffix)
  } else {
    new.file.name=file.name
  }
  return(list("new.file.name"=new.file.name,
              "file.suffix"=file.suffix,
              "url"=url,
              status=0))
}

