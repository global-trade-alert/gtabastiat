#' Checks state acts for URLs and adds them to the tables for scraping later on.
#' Step 1 in source collection process.
#'
#' @param establish.connection If T, opens the DB connection.
#' @param timeframe Search for sources in the last {timeframe} days.
#' @param recheck.existing.sources If T, try to scrape sources for entries which have already been added to gta_url_log.
#' @param ignore.manually.added If T, do not search for sources for measures that have an associated file in gta_files. Note that there is no way to distinguish if a file is present for a particular SOURCE.
#'
#' @return
#' @export
#'
#' @examples
bt_sa_record_new_source=function(establish.connection=T,
                                 timeframe = NA,
                                 recheck.existing.sources = F,
                                 ignore.manually.added = T){


    library(glue)



# Get sources -------------------------------------------------------------


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


  }



  #this will only look for measures that have NEVER been added before
  if(is.na(timeframe)){
    sa.upd.sql = glue("SELECT gm.id, gm.source
                      FROM gta_measure gm
                      WHERE 1=1") #need a WHERE clause to make adding AND/OR easier later
  }else{
    sa.upd.sql = glue("SELECT gm.id, gm.source
                      FROM gta_measure gm
                      WHERE gm.creation_date > (SELECT NOW() - INTERVAL {timeframe} DAY)")
  }

  if(!recheck.existing.sources){
    sa.upd.sql = glue("{sa.upd.sql}
                      AND gm.id NOT IN (SELECT gmu.measure_id
                                        FROM gta_measure_url gmu)")


  }

  if(ignore.manually.added){
    sa.upd.sql = glue("{sa.upd.sql}
                      AND gm.id NOT IN (
                        SELECT DISTINCT gf.field_id
                        FROM gta_files gf
                        WHERE gf.field_type = 'measure'
                      )")


  }

  sa.sources.update = dbGetQuery(con, sa.upd.sql)

  rows.updated = 0

  if(!all(is.na(sa.sources.update))){


# Extract URLs ------------------------------------------------------------


    # extract the URLs
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

    #some sources have duplicated URLs
    new.urls = subset(new.urls, !duplicated(new.urls[,c("state.act.id", "url")]))






# Add URL-SA ID correspondence --------------------------------------------


    new.urls$url.log.updated = 0
    new.urls$measure.url.updated = 0

    rnds.max=length(unique(new.urls$url))
    rnds=1

    #must go through the table row by row to ensure that:
    #measures with multiple sources are all added
    #sources with multiple measures are all added
    for(i in 1:nrow(new.urls)){


      ## Adding URL if not already in gta_url_log
      if(is.na(dbExecute(con, glue("SELECT id FROM gta_url_log gul
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
      print(problems)
      return(problems)

    }else{
      print("Recorded all new URLs successfully!")
    }


    dbDisconnect(conn = con)


  }
}
