#' Show/update DB for the master list of GTA scrapers
#'
#' Analyses codes in the daily/running folder to show an overview of all GTA scrapers.
#'
#' @param show.stock.data.errors show scrapers for which stock data could not be loaded. Inaccurate as some scrapers work differently (e.g. EIB).
#' @param update.db send the updated list of scrapers to the database.
#'
#' @return a data.frame with the collated scraper information.
bt_scraper_master_list = function(show.stock.data.errors = F,
                                  update.db = F,
                                  quiet = T,
                                  incl.under.maintenance = F){

# Dependencies ------------------------------------------------------------


  library(dplyr)

  library(xlsx)

  library("lubridate")
  library("textcat")
  library(stringr)
  library(tidytext)

  library(gtasql)
  library(pool)
  library(RMariaDB)

  library(gtabastiat)
  library(gtalibrary)


# set the wd --------------------------------------------------------------


  curr.wd = getwd()

  bt.wd = str_extract(curr.wd, ".+GTA data team Dropbox") %>%
    paste0(., "/Bastiat")

  setwd(bt.wd)


# PROCESS SCRIPTS + COLLATE IN ONE DATAFRAME ------------------------------



  scraper.master = data.frame(
    #scraper.name = c(list.files(path = "code/daily/running", pattern = "R",  full.names = F), list.files(path = "code/daily/under maintenance", pattern = "R",  full.names = F)),
    scraper.path = c(list.files(path = "code/daily/running", pattern = ".R",  full.names = T), list.files(path = "code/daily/under maintenance", pattern = "R",  full.names = T)),
    multiple.urls = F,
    main.url = "",
    specialised.scraper = F,
    stock.data.file = "",
    bid.stem = "",
    stringsAsFactors = F
  )

  scraper.master = subset(scraper.master, grepl("\\.R$", scraper.path))

  if(incl.under.maintenance){
    scraper.master$under.maintenance = grepl("under maintenance", scraper.master$scraper.path)
  } else {
    scraper.master = subset(scraper.master, ! grepl("under maintenance", scraper.master$scraper.path))
    scraper.master$under.maintenance = F
  }
  errorifications = c()

  bid.stem.master = list()



  #if the code has a 'main.url', it is scraping from one URL
  for(scraper.path in scraper.master$scraper.path){

    ###load the code as a string

    #could use 'source()' but I don't want to run the codes here
    #scraper.code = paste(readLines(scraper.path), collapse = "\\n")

    scraper.code = readLines(scraper.path)

    Encoding(scraper.code) <- "UTF8"

    ###get rdata filename
    stock.data.line = scraper.code[grepl("\\.rdata", scraper.code, ignore.case = T)][1]
    stock.data.path = str_extract(stock.data.line, "(?<=\").+\\.[Rr]data")
    scraper.master$stock.data.file[scraper.master$scraper.path == scraper.path] = stock.data.path

    #don't try and do it for the under maintenance ones
    if(!scraper.master$under.maintenance[scraper.master$scraper.path == scraper.path]){



      tryCatch(
        {
          load(stock.data.path)
          if(!quiet){
            print(paste("loaded:", stock.data.path))
          }
          if(!exists("table.main") & exists("table.stock")){
            table.main = table.stock
            rm(table.stock)
          } else if(exists("table") & typeof(table) != "closure" ){ #the issue is that there's a base fn called "table"
            table.main = table
            rm(table)
          }
        },
        error=function(cond){
          if(!quiet){
            message(paste("[err]: problem loading stock data for", scraper.path))
          }
          errorifications %>% c(scraper.path) ->> errorifications
        }
      )


      #special eib
      if(exists("eib")){
        table.main = eib
        rm(eib)
      }


      #(try to) get most recent lead date
      if(exists("table.main")){
        if(typeof(table.main)=="list"){
          if("act.date" %in% colnames(table.main)){
            most.recent = max(table.main$act.date, na.rm = T)
          }else{
            date.column.idx = match(colnames(table.main)[grepl("date", colnames(table.main))][1],
                                    colnames(table.main)
            )
            most.recent = max(table.main[,date.column.idx], na.rm = T)
          }
          scraper.master$most.recent[scraper.master$scraper.path == scraper.path] = most.recent
        }
        rm(table.main)
      }

      ###add last check date
      scraper.master$last.check[scraper.master$scraper.path == scraper.path] = last.check


      ###check for a 'main.url'
      if(any(grepl("^main\\.url", scraper.code))){

        #finds the first line that starts with 'main.url...', and parses it.
        eval(parse(text = scraper.code[grepl("^main\\.url", scraper.code)][1]))

        # 'main.url' now in the local env so it can be referenced...

        scraper.master$main.url[scraper.master$scraper.path == scraper.path] = main.url
      } else {
        scraper.master$multiple.urls[scraper.master$scraper.path == scraper.path] = T
      }



      ###check for 'specialised.scraper' (it just has to be there so leave it out if not specialised!)
      if(any(grepl("^specialised\\.scraper", scraper.code))){

        #works the same way as the url finding part above
        eval(parse(text = scraper.code[grepl("^specialised\\.scraper", scraper.code)][1]))
        scraper.master$specialised.scraper[scraper.master$scraper.path == scraper.path] = specialised.scraper
      }

      ###bid stem
      #must be done like this as there are a few scrapers that do not save BID in the table.stock
      #update.table$bid=paste("CAN-EDC-", (nrow(table.stock)+1):(nrow(table.stock)+nrow(update.table)), sep="")

      tryCatch(expr={bid.stem = str_extract(string = scraper.code,
                                            pattern = "\\$bid ?= ?paste0?\\(\\\".+?\\\"") %>%
        str_extract(pattern = "(?<=\\\").+(?=\\\")") %>%
        purrr::discard(is.na) %>%
        unique()# %>%
        #list() #because some have several bids per scraper

      names(bid.stem) = scraper.path

      bid.stem = bid.stem[1]
      scraper.master$bid.stem[scraper.master$scraper.path == scraper.path] = bid.stem

      bid.stem.master = c(bid.stem.master, bid.stem)},
      error = function(e){
        if(!quiet){
          message("can't find bid stem for this one")
        }
      })

      #for testing
      # str_extract(string = scraper.code,
      #             pattern = "\\$bid.+")


      if(!quiet){
        print("done, moving on")
      }

      prog = match(scraper.path, scraper.master$scraper.path)
      cat(prog)
      if(quiet){
        if(prog<nrow(scraper.master)){
          cat(", ")
        } else{
          cat("\n")
          cat("Done!\n")
        }

      }
    }

  }

  if(show.stock.data.errors){
    message("Stock data problems:")
    for(err in errorifications){
      message(err)
    }
  }

  #fix integer dates
  scraper.master$most.recent = as.Date(scraper.master$most.recent, origin = "1970-01-01")
  scraper.master$last.check = as.Date(scraper.master$last.check, origin = "1970-01-01")

  scraper.master$display.name = str_extract(scraper.master$scraper.path, "(?<=/)[^/]+(?=\\.)")

  #Files must be named with the correct syntax! e.g. Jurisdiction Name - <optional>Ministry Name - </optional>Scraper name.R

  scraper.master$scraper.jurisdiction = str_extract(scraper.master$scraper.path, "(?<=/)[^/-]+(?=\\ - )")
  #%>% sapply(function(x) bt_guess_country(tgt.string = x))# can't do this
  #because yes, each lead pertains to a country. But scrapers may cover entire
  #regions, e.g. Africa

  scraper.master$scraper.official = 1


  save(scraper.master, file = "content/scraper master/scraper_master.Rdata")




# SQL prep and DB upload --------------------------------------------------


  if(update.db){
    scraper.master.sql = data.frame(scr_name = scraper.master$display.name,
                                    scr_jurisdiction = scraper.master$scraper.jurisdiction,
                                    scr_last_run = scraper.master$last.check,
                                    scr_most_recent_lead = as.character(scraper.master$most.recent),
                                    scr_official = scraper.master$scraper.official,
                                    scr_type_id = 1,
                                    under.maintenance = scraper.master$under.maintenance,
                                    scr_url = scraper.master$main.url,
                                    stringsAsFactors = F)

    scraper.master.sql = scraper.master.sql[!scraper.master.sql$under.maintenance,]
    scraper.master.sql$under.maintenance = NULL


    #tidy up for SQL readiness, particularly the flipping dates
    # doing it row-by-row is much more flexible and reliable.

    sql.rows = c()
    sql.rows.url = c()

    for(i in 1:nrow(scraper.master.sql)){
      #print(i)
      this.row = scraper.master.sql[i,]

      this.row$scr_name = paste("'", this.row$scr_name, "'", sep = "")
      this.row$scr_jurisdiction = paste("'", this.row$scr_jurisdiction, "'", sep = "")
      this.row$scr_url = paste("'", this.row$scr_url, "'", sep = "")


      if(!is.na(scraper.master.sql$scr_most_recent_lead[i])){

        tryCatch(expr = {
          this.row$scr_most_recent_lead = as.numeric(scraper.master.sql$scr_most_recent_lead[i]) %>%
            as.Date(origin = "1970-01-01")
        },
        warning = function(e){
          message("Oops, maybe this is NOT an integer date...")
          print("go string conv")
          tryCatch(expr = {
            print("string conv start")
            this.row$scr_most_recent_lead = as.Date(scraper.master.sql$scr_most_recent_lead[i],
                                                    origin = "1970-01-01")
          },
          error = function(e){
            message("Date conversion failed, inserting NULL")
            this.row$scr_most_recent_lead = "NULL"
          })

        })

        #if you don't do this AGAIN it goes back to an integer
        this.row$scr_most_recent_lead = as.character(this.row$scr_most_recent_lead) %>%
          paste("'", ., "'", sep = "")

      }


      if(!is.na(scraper.master.sql$scr_last_run[i])){
        this.row$scr_last_run = as.Date(scraper.master.sql$scr_last_run[i], origin = "1970-01-01") %>%
          as.character() %>%
          paste("'", ., "'", sep = "")
      }

      this.row[which(is.na(this.row))] = "NULL"

      name.col.idx = which(grepl("scr_name", names(this.row)))
      url.col.idx = which(grepl("scr_url", names(this.row)))#because URLs must be added to a separate table

      sql.rows = c(sql.rows, paste(this.row[-url.col.idx], sep = "), (", collapse = ", "))
      sql.rows.url = c(sql.rows.url, paste(this.row[c(name.col.idx, url.col.idx)], sep = "), (", collapse = ", "))

    }

    sql.insert.statement = paste("(", sql.rows, ")", sep="", collapse = ", ")
    sql.insert.statement.url = paste("(", sql.rows.url, ")", sep="", collapse = ", ")

    #send main table update query
    gta_sql_update_table(query = "DELETE FROM bt_scraper_log;")
    gta_sql_update_table(query = paste0("INSERT INTO bt_scraper_log(scr_name, scr_jurisdiction, scr_last_run, scr_most_recent_lead, scr_official, scr_type_id)
VALUES", sql.insert.statement))

    #send URL table update query
    gta_sql_update_table(query = "DELETE FROM bt_scraper_url_log;")
    gta_sql_update_table(query = paste0("INSERT INTO bt_scraper_url_log(scr_name, scr_url)
VALUES", sql.insert.statement.url))

  }







  ##### XLSX EXPORT #####

  # using the SQL should suffice here.
  #
  #
  # for.export = T
  # if(for.export){
  #   scraper.master.for.editors = data.frame(display.name = scraper.master$display.name,
  #                                           jurisdiction = scraper.master$scraper.jurisdiction,
  #                                           main.url = scraper.master$main.url,
  #                                           last.ran = scraper.master$last.check,
  #                                           most.recent = scraper.master$most.recent,
  #                                           under.maintenance = scraper.master$under.maintenance,
  #                                           stringsAsFactors = F)
  #   scraper.master.for.editors = scraper.master.for.editors[!scraper.master.for.editors$under.maintenance,]
  #   scraper.master.for.editors$under.maintenance = NULL
  #
  #   write.xlsx(scraper.master.for.editors, file = "content/scraper master/master list for editors.xlsx")
  # }



# tidy up and finish ------------------------------------------------------

  setwd(curr.wd)

  return(scraper.master)

}

