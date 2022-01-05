rpm_delay = function(){
  if (exists("t1")){
    t2 <<- Sys.time()
    interval <- as.numeric(t2 - t1, units = "secs")
    delay <- 2 - min(1, interval)
    Sys.sleep(delay)
    t1 <<- t2

  } else{
    t1 <<- Sys.time()
  }

}
