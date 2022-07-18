bt_check_bid_stem = function(query){

  bid.stem.master = gtabastiat::bid.stem.master
  return(bid.stem.master[grepl(query, bid.stem.master, ignore.case = T)])


}
