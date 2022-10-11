bt_get_archive_url = function(tgt.url, tgt.date = NA){

  library(archiveRetriever)
  library(glue)

  if(is.na(tgt.date)){
    tgt.date = "2010-01-01"
  }

  tryCatch(expr = {
    as.Date(tgt.date)
  },
  error=function(e){
    stop("date error: must be in YYYY-MM-DD format")
  })


  avail = retrieve_urls(tgt.url, startDate = tgt.date,
                        endDate = as.character(Sys.Date()))


  if(nrow(avail)==0){

    err.msg=glue("no snapshots available after date: {tgt.start} for URL: {tgt.url}")

    stop(err.msg)
  }


  #first one will be the closest one to the specified date
  return(avail[1])
}
