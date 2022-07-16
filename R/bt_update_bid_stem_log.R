#' Update the list of BID stems
#'
bt_update_bid_stem_log = function(){

  library(DBI)
  library(stringr)
  library(gtalibrary)

  bastiat.wd = str_extract(getwd(), ".+Bastiat")
  setwd(bastiat.wd)

  con=dbConnect(drv = RMariaDB::MariaDB(),
                host = gta_pwd("ricardomain")$host,
                username = gta_pwd("ricardomain")$user,
                password = gta_pwd("ricardomain")$password,
                dbname = gta_pwd("ricardomain")$name)

  bid.master = dbGetQuery(con, statement = "SELECT * FROM bt_hint_bid;")

  DBI::dbDisconnect(con)

  bid.stem.master = str_extract(bid.master$bid, ".+?(?=\\d+$)")

  # slower way for dbg/monitoring progress
  # chunk = (nrow(bid.master)/10) %>% ceiling()
  # bid.stem.master = c()
  # for(i in c(1:10)){
  #
  #   this.chunk = bid.master$bid[(((i-1)*chunk)+1):(i*chunk)]
  #
  #   bid.stem.master = c(bid.stem.master,#
  #                       str_extract(this.chunk, ".+?(?=\\d+$)"))
  #   print(i)
  # }

  bid.stem.master = bid.stem.master %>% unique()

  message(paste0("Total bid stems: ", length(bid.stem.master)))

  save(bid.stem.master, file = "0 gtabastiat/data/bid_stem_master.Rda")


}
