#' Get the maximum numbered entry for a given BID stem
#'
#' @param bid.stem the stem to search for in the db
#' @param force.con.open open the db connection in the function call
#'
#' @examples
bt_get_max_bid = function(bid.stem, force.con.open = F){


  library(gtasql)
  library(gtalibrary)
  library(pool)
  library(RMariaDB)
  library(gtabastiat)
  library(stringr)

  if(force.con.open){
    database <<- "ricardomain"

    gta_sql_pool_open(db.title=database,
                      db.host = gta_pwd(database)$host,
                      db.name = gta_pwd(database)$name,
                      db.user = gta_pwd(database)$user,
                      db.password = gta_pwd(database)$password,
                      table.prefix = "bt_")
  }


  max.bid = gta_sql_get_value(paste0("SELECT bid
                              FROM bt_hint_bid
                              WHERE bid LIKE '", bid.stem, "%';")) %>%
    str_extract(pattern = "\\d+$") %>%
    as.numeric() %>%
    max()

  return(max.bid)




}
