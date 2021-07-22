#' Tries to kill extraneous PJS processes on UNIX
#'
#' Sometimes, pjs processes are left running after crashing or not being
#' terminated properly and can eat up the CPU. This function tries to kill them
#' to free up memory.
#'
#'
#' @return
#' @export
#'
#' @examples
bt_kill_runaway_pjs = function(){

  if(Sys.info()['sysname'] == "Windows"){
    stop("this function only works on UNIX OSes")
  }

  #get the running processes as a df on the server
  devtools::source_url("https://github.com/mathosi/cluster_check/blob/master/ps_to_df.R?raw=TRUE")
  #ps.to.df() #List All processes sorted by %CPU usage
  n.pjs.ps =nrow(ps.to.df(bylist.selection = "-C phantomjs")) #List All processes named 'rsession' sorted by %CPU usage
  if(n.pjs.ps > 0){
    print(paste(n.pjs.ps, "PhantomJS processes found. Attempting to kill..."))
    system("killall phantomjs -v")

    n.pjs.ps2 =nrow(ps.to.df(bylist.selection = "-C phantomjs"))
    if(n.pjs.ps2 > 0){
      print(paste(n.pjs.ps2, "PhantomJS processes found after killing. A cronjob is probably in the process of scraping."))
    }
  }

}
