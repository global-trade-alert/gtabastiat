#' Checks state acts for URLs and adds them to the tables for scraping later on.
#'
#' @param establish.connection If T, opens the DB connection.
#' @param timeframe Search for sources in the last {timeframe} days.
#' @param recheck.existing.sources If T, try to scrape sources for entries which already have an attached PDF.
#'
#' @return
#' @export
#'
#' @examples
bt_sa_record_new_source=function(establish.connection=T,
                                 timeframe = NA,
                                 recheck.existing.sources = F){

  library(glue)

  if(establish.connection){
    library(gtalibrary)
    library(gtasql)
    library(pool)
    library(RMariaDB)
    library(data.table)

    # gta_sql_pool_open(db.title="gtamain",
    #                   db.host = gta_pwd("gtamain")$host,
    #                   db.name = gta_pwd("gtamain")$name,
    #                   db.user = gta_pwd("gtamain")$user,
    #                   db.password = gta_pwd("gtamain")$password,
    #                   table.prefix = "gta_")

    #for gtamain
    database <<- "gtamain"

    con <<- dbConnect(drv = RMariaDB::MariaDB(),
                    user = gta_pwd(database)$user,
                    password = gta_pwd(database)$password,
                    dbname = gta_pwd(database)$name,
                    host=gta_pwd(database)$host)


#    test = dbGetQuery(con, statement = query)
  }

  # updated.sa.sources=gta_sql_get_value("SELECT DISTINCT(state_act_id)
  #                                       FROM gta_source_temp
  #                                       WHERE source_processed=0;")


  #this will only look for measures that have NEVER been added before
  if(is.na(timeframe)){
    sa.upd.sql = glue("SELECT gm.id, gm.source
                      FROM gta_measure gm")
  }else{
    sa.upd.sql = glue("SELECT gm.id, gm.source
                      FROM gta_measure gm
                      WHERE gm.creation_date > (SELECT NOW() - INTERVAL {timeframe} DAY)")
  }

  if(!recheck.existing.sources){
    sa.upd.sql = paste0(sa.upd.sql, " AND gm.id NOT IN (SELECT gmu.measure_id
                                        FROM gta_measure_url gmu)")


  }

  sa.sources.update = dbFetch(con, sa.upd.sql)

  rows.updated = 0

  if(!all(is.na(sa.sources.update))){

    ## extract the URLs
    # sources.to.parse=gta_sql_get_value(paste0("SELECT id, source FROM gta_measure WHERE id IN (",paste(updated.sa.sources, collapse=","),");"))

    sources.to.parse = sa.sources.update
    new.urls=data.frame()

    #the below loop constructs the many-to-one relationship between urls and sa ids
    print("constructing URL-SA ID dataframe")

    for(i in 1:nrow(sources.to.parse)){
      new.urls=rbind(new.urls,
                     data.frame(state.act.id=sources.to.parse$id[i],
                                url=bt_extract_url(sources.to.parse$source[i]),
                                stringsAsFactors = F))

    }

    new.urls$url.log.updated = 0
    new.urls$measure.url.updated = 0


    rnds.max=length(unique(new.urls$url))
    rnds=1

    #must go through the table row by row to ensure that:
    #measures with multiple sources are all added
    #sources with multiple measures are all added
    for(i in 1:nrow(new.urls)){


      ## Adding URL if not already in gta_url_log
      if(is.na(gta_sql_get_value(glue("SELECT id FROM gta_url_log gul
                                        WHERE gul.url  = '{new.urls$url[i]}';")))){

        new.urls$url.log.updated[i] = dbExecute(con, glue("INSERT INTO gta_url_log (url)
                                    VALUES ('{new.urls$url[i]}');"))


        }


      #add sa id-url id tuple to measure_url table
      #this should be done even if the URL exists in the URL table already, because all the tuples are new
      {
        new.urls$measure.url.updated[i] = dbExecute(con, glue("INSERT INTO gta_measure_url (measure_id, url_id)
                                    VALUES ({new.urls$state.act.id[i]}, (SELECT gul.id FROM gta_url_log gul WHERE gul.url = '{new.urls$url[i]}'));"))

      }



      if(i < nrow(new.urls)){
        cat(glue("{i}, "))
      }else{
        cat(glue("{i}\n"))
      }
      # print(rnds/rnds.max)
      # rnds=rnds+1
    }


    problems = subset(new.urls, url.log.updated == 0)
    problems = rbind(problems,subset(new.urls, measure.url.updated == 0))

    if(nrow(problems)>0){

      print("There was a problem updating the following URL-SA ID tuples, check the SQL:")
      return(problems)

    }else{
      print("Recorded all new URLs successfully!")
    }

#
#
#     ## mark as processed
#     gta_sql_get_value(paste0("UPDATE gta_source_temp
#                              SET source_processed=1
#                              WHERE state_act_id IN(",paste(updated.sa.sources, collapse=","),");"))


  }
}
