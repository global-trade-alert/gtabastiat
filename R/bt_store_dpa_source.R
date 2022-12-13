#' FOR DPA ONLY! Main function of the source collection process. Uses
#' bt_collect_url() to check if they are working. Gets a PDF if possible,
#' uploads to the bucket and updates gta_files. Then adds the link to all the
#' state acts which use that source.
#'
#' @param timeframe how long ago to check
#' @param update.source.log whether to update the master table
#' @param bad.urls specify any URLs not to do automatic source completion on.
#'   b/c this function uses a system() call, if it times out sometimes we can't
#'   catch it (withTimeout() only works for calls within R)
#' @return
#' @export
#'
#' @examples
bt_store_dpa_source = function(timeframe = 365*3,
                              establish.connection = T,
                              recheck.existing.sources = F,
                              use.wayback = T,
                              bad.urls = ""){
  
  path.root = "0 source completion/temp"
  
  # dbg dbg dbg
  # timeframe = 365*3
  # establish.connection = T
  # recheck.existing.sources = F
  # use.wayback = T
  # bad.urls = ""
  
  # add any URLs here that are just always naughty
  # the reason we need these is because the source collection uses a system() call, if it times out sometimes we can't catch it (withTimeout() only works for calls within R)
  bad.urls = c(bad.urls, "")
  
  
  #check we are in BT WD
  bastiat.wd = str_extract(getwd(), ".+Bastiat")
  setwd(bastiat.wd)
  
  library(gtalibrary)
  library(gtasql)
  library(pool)
  library(RMariaDB)
  library(data.table)
  library(stringr)
  #library(pdftools)
  library(glue)
  library(R.utils)
  library(digest)
  
  if(establish.connection){
    #library(gtalibrary)
    library(DBI)
    library(pool)
    library(RMariaDB)
    library(data.table)
    
    
    
    #for gtamain
    database <- "gtamain"
    
    con <- dbConnect(drv = RMariaDB::MariaDB(),
                      user = gta_pwd(database)$user,
                      password = gta_pwd(database)$password,
                      dbname = gta_pwd(database)$name,
                      host=gta_pwd(database)$host)
    
    
  }
  
  
  
  
  
  
  ### Collecting new URLs

    missing.sources=dbGetQuery(con, glue("
    SELECT lsl.*, gusl.is_terminal, gusl.is_success
    FROM lux_source_log lsl LEFT JOIN gta_url_status_list gusl ON lsl.check_status_id = gusl.id
    WHERE (
		  lsl.last_check IS NULL #not yet attempted scrape
		  OR (
		    lsl.last_check > (SELECT NOW() - INTERVAL 365 DAY)
		    AND lsl.check_status_id IN
		    #status ID where the scrape fininshed in a non-successful way
		    (
		      SELECT gusl2.id
      		FROM gta_url_status_list gusl2
      		WHERE gusl2.is_success = 0
      		#AND gusl2.is_terminal = 0
		    )
		  )
		);"))
    #yes, this is the one error I could not solve as it causes the system call to die in an uncatchable way
    missing.sources = subset(missing.sources, ! source_url %in% bad.urls)
    
    missing.sources = subset(missing.sources, ! is.na(source_url))
    
    # remove the rows where sources were successfully added if we are NOT forcing a total recheck of all the sources
    if(!recheck.existing.sources){
      
      missing.sources = subset(missing.sources, (is.na(check_status_id)) )
      
    
    }
  
  
  
  #must select executable path. depending on the OS
  #could maybe use a switch?
  
  #WINDOWS
  if(grepl("Windows", Sys.info()['sysname'])){
    
    #this assumes your wd and pjs exe are on the same disk and installed in the default directory
    
    pfx = str_extract(getwd(), "^.+Users/[^/]+")
    
    phantom.path.os = paste0(pfx, "/AppData/Roaming/PhantomJS/phantomjs")
    #e.g. C:\Users\d-trump\AppData\Roaming\PhantomJS
    
  } else if(grepl("Darwin", Sys.info()['sysname'])){
    
    phantom.path.os = "~/Library/Application\\ Support/PhantomJS/phantomjs"
    
  } else if(grepl("Linux", Sys.info()['sysname'])){
    
    phantom.path.os = "~/bin/phantomjs"
    
  }
  
  
  # begin looping through each URL ------------------------------------------
  
  
  if(nrow(missing.sources)>0){
    
    completed = subset(missing.sources, is_success == 1)
    
    missing.sources = subset(missing.sources, ! source_url %in% completed$url)
    
    n.remaining = length(unique(missing.sources$source_url))
    print(glue("beginning source completion process for {n.remaining} url(s)"))
    
    for(url in unique(missing.sources$source_url)){
      
      
      src.url = url #vestige from source collector v.01a :)
      
      if(src.url!="NA"){
        
        url.timestamp = gsub("\\D", "-", Sys.time())
        url.hash = digest(src.url)
        url.quick.id = gsub("\\.","",str_extract(str_extract(src.url,"((https?://)|(www\\.))[A-Za-z\\.\\-_0-9]+"), "[A-Za-z\\.\\-_0-9]+$")) %>%
          str_trunc(35, ellipsis = "")
        
        base.file.name = glue("motoko_{url.timestamp}_{url.quick.id}-{url.hash}")
        
        #I added a str_trunc above, but keep this here for redundancy
        #db/website only allows up to 100chr filename
        if(nchar(base.file.name)>90){
          base.file.name = glue("motoko_{url.timestamp}_{url.hash}")
        }
        
        
        most.recent.attempt = subset(missing.sources, source_url == src.url) %>% last()
        
        
        # INTERNET ARCHIVE CHECK --------------------------------------------------
        wayback.fail = F
        if(use.wayback){
          if((most.recent.attempt$is_success==0 & most.recent.attempt$check_status_id!=12)){
            
            
            source.date = missing.sources$source_date[missing.sources$source_url == url] %>%
              as.character()
            
            if(length(source.date)>1) source.date = source.date[1]
            
            if(is.na(source.date)) source.date = Sys.Date()
            
            tryCatch(expr={
              src.url=bt_get_archive_url(src.url,tgt.date = measure.date)
            },
            error=function(e){
              print(glue("Failed to find internet archive snapshot of {src.url}."))
              wayback.fail <<- T
              scrape.result <<- list("new.file.name"=NA,
                                     "file.suffix"=NA,
                                     "url"=src.url,
                                     status=12)
            }
            )
            if(!wayback.fail){
              print(glue("Broken link detected. Closest Internet archive link to {measure.date} found at URL:\n {src.url}"))
              base.file.name = paste0("WBK_", base.file.name)
            }
            #TODO add the internet archive URL to gta_url_log and gta_measure_url... maybe as separate col?
            #does this need to be done?
            
          }
        }
        if(!wayback.fail){
          # do the scraping
          print(glue("Attempting scrape of {src.url}... "))
          
          
          #I leave the timeout code here for reference. However, withTimeout fn
          #doesn't work with system(cmd) calls because it relies on it running
          #native C/fortran code... from the documentation:
          
          
          # "Time limits are checked whenever a user interrupt could occur. This
          # will happen frequently in R code and during Sys.sleep(*), but only at
          # points in compiled C and Fortran code identified by the code author."
          #
          # More precisely, if a function is implemented in native code (e.g. C)
          # and the developer of that function does not check for user interrupts,
          # then you cannot interrupt that function neither via a user interrupt
          # (e.g. Ctrl-C) nor via the built-in time out mechanism. To change this,
          # you need to contact the developer of that piece of code and ask them
          # to check for R user interrupts in their native code.
          #
          # Furthermore, it is not possible to interrupt/break out of a "readline"
          # prompt (e.g. readline() and readLines()) using timeouts; the timeout
          # exception will not be thrown until after the user completes the prompt
          # (i.e. after pressing ENTER).
          #
          # System calls via system() and system2() cannot be timed out via the
          # above mechanisms. However, in R (>= 3.5.0) these functions have
          # argument timeout providing their own independent timeout mechanism.
          
          # tryCatch(
          #   withTimeout( {
          #     print("scraping...")
          
          scrape.result= bt_collect_url(file.name=base.file.name,
                                        store.path = path.root,
                                        url=as.character(src.url),
                                        phantom.path = phantom.path.os)
          #   },
          #
          #     timeout = 120),
          #
          #   TimeoutException = function(ex){
          #     cat("[Skipped due to timeout]\n")
          #     scrape.result <<- list("new.file.name"=NA,
          #          "file.suffix"=NA,
          #          "url"=src.url,
          #          status=10)
          #     },
          #   error=function(cond){
          #     message("[caught error] timeout, skipping")
          #   },
          #   warning=function(cond){
          #     message("[caught warning] timeout, skipping")
          #   }
          # )
        }
        
        
        
        #returns a list of 4 objs:
        # new.file.name
        # file.suffix
        # url
        # status
        
        if(scrape.result$status %in% c(0, 6)){
          cat("Success! Uploading to S3...")
          aws.file.name = paste0(scrape.result$new.file.name, scrape.result$file.suffix)
          
          aws.url=bt_upload_to_aws(upload.file.name = aws.file.name,
                                   upload.file.path = path.root)
          
          
          n.measures = unique(missing.sources$source_id[missing.sources$source_url == url]) %>% length()
          add.id = missing.sources$source_id[missing.sources$source_url == url] %>% unique()
          print(glue("Adding url ID {add.id} to {n.measures} source(s)..."))
          
          #add appropriate status if this was an internet archive scrape.
          if(grepl("web\\.archive\\.org", src.url)){
            scrape.result$status = 11
          }
          
          for(source.id in missing.sources$source_id[missing.sources$source_url == url]){
            cat(glue("Source ID {source.id}: "))
            # update lux_file_log
            lux.files.sql = glue("INSERT INTO lux_file_log (file_url, file_name)
                                   VALUES ('{aws.url}', '{base.file.name}{scrape.result$file.suffix}')") #id = AUTO_INCREMENT; is_delete default = 0
                                    #VALUES (",sa.id,",'measure','",aws.url,"','",gsub("temp/","", base.file.name), scrape.result$file.suffix,"',0);") # orig gta_files version
            
            lux.files.sql.upload = dbExecute(con, lux.files.sql)
            cat(glue("lux_file_log upload status: {lux.files.sql.upload}  ///  "))
            
            # update lux_source_file
            #get the file ID
            aws.file.id=dbGetQuery(con, glue("SELECT MAX(id)
                                              FROM lux_file_log WHERE file_url = '{aws.url}';"))
            
            #add correspondence
            lux.source.file.sql = glue("INSERT INTO lux_source_file (source_id, file_id)
                                       VALUES ({source.id}, {aws.file.id})")
            
            lux.source.file.sql.upload = dbExecute(con, lux.source.file.sql)
            
            cat(glue("lux_source_file upload status: {lux.source.file.sql.upload}  ///  "))
            
            lux.source.log.sql = glue("UPDATE lux_source_log
                           SET last_check = CURRENT_TIMESTAMP, check_status_id = {scrape.result$status}
                           WHERE source_url = '{url}';")
            
            lux.source.log.sql.upload = dbExecute(con, lux.source.log.sql)
            cat(glue("lux_source_log upload status: {lux.source.log.sql.upload}"))
            
            cat("\n")
            # gta_sql_multiple_queries(url.log.sql, 1)
            
            # gta_sql_update_table(paste0("INSERT INTO gta_sa_source_url
            #                             VALUES (",sa.id,",'",src.url,"',1,1,CURRENT_TIMESTAMP,1,",aws.file.id,");"))
            
          }
        }else if(scrape.result$status == 2){
          
          #add all the info if redirected
          cat(glue("REDIRECTED - "))
          for(sa.id in unique(subset(missing.sources, url == src.url)$measure_id)){
            #TODO not sure how to tackle these yet
            cat(glue("AMENDING SOURCE ID {source.id}: "))
            url.log.sql = glue("UPDATE lux_source_log
                             SET last_check = CURRENT_TIMESTAMP, check_status_id = {scrape.result$status}
                             WHERE source_url = '{src.url}';")
            
            url.log.sql.status = dbExecute(con, url.log.sql)
            cat(url.log.sql.status)
            
            # #add the new url after redirect for later scraping
            cat("\nADDING NEW URL: ")
            
            lux.event.id = glue("SELECT event_id
                                        FROM lux_event_source
                                        WHERE source_id = {source.id}")
            
            this.source.name = missing.sources$source_name[missing.sources$source_url == url] %>% last()
            this.source.type.id = missing.sources$source_type_id[missing.sources$source_url == url] %>% last()
            this.source.institution.name = missing.sources$institution_name[missing.sources$source_url == url] %>% last()
            this.source.date = glue("'{source.date}'")
            if(is.na(source.date)) this.source.date = 'NULL'
            
            
            #in very rare cases the new url may have an apostrophe in it...
            escaped.url = gsub("'", "\\\\'", scrape.result$url)
            
            
            url.exist.check = dbGetQuery(con, glue("SELECT * FROM gta_url_log gul
                                                   WHERE gul.url = '{escaped.url}'"))
            if(nrow(url.exist.check)==0){
              
              lux.source.log.add.sql = glue("INSERT INTO lux_source_log (source_name, source_url, source_type_id, institution_name, source_date)
                                          VALUES ({this.source.name}, {escaped.url}, {this.source.type.id}, {this.source.institution.name}, {this.source.date})")
              
              lux.source.log.add.upload = dbExecute(con, lux.source.log.add.sql)
              
              lux.source.id.just.added = dbGetQuery(con, glue("SELECT source_id FROM lux_source_log WHERE source_url = '{escaped.url}'"))
              
              # new.url.log.upl = dbExecute(con, glue("INSERT INTO gta_url_log (url)
              #                         VALUES ('{escaped.url}');"))
              cat(glue("new lux_source_log status = {lux.source.log.add.upload}  ///  "))
              
            }
            
            lux.event.source.sql = glue("INSERT INTO lux_event_source (event_id, source_id)
                                        VALUES ({lux.event.id}, {lux.source.id.just.added})")
            
            lux.event.source.sql.upl = dbExecute(con, lux.event.source.sql.upl))
            cat(glue("new measure_url status = {lux.event.source.sql.upl}"))
            cat("\n")
          }
          
        }else{
          cat("updating lux_source_log with failed status: ")
          lux.source.log.sql = glue("UPDATE lux_source_log
                           SET last_check = CURRENT_TIMESTAMP, check_status_id = {scrape.result$status}
                           WHERE source_url = '{url}';")
          
          url.log.sql.status = dbExecute(con, lux.source.log.sql)
          cat(glue("{url.log.sql.status}"))
          cat("\n")
        
        }
      }
      
      
    }
  }
  
  
 
  
  
  #set trigger on gtamain that marks table as re-check.
  
  #set CRON
  
  print("reached the end! disconnecting")
  dbDisconnect()
  
}
