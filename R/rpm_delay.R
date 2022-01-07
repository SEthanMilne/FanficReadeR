# https://github.com/otwcode/otwarchive/blob/master/config/initializers/rack_attack.rb

# 60rpm limit -- chose to cap FFR at 30rpm to be safe


rpm_delay = function(){
  if (exists("t1_short")){
    t2_short <<- Sys.time()
    interval <- as.numeric(t2_short - t1_short, units = "secs")
    delay <- max(0, (3 - min(1, interval))) #2sec delay per req

    Sys.sleep(delay)
    t1_short <<- t2_short

  } else{
    t1_short <<- Sys.time()
  }

  if (exists("t1_long")){
    t2_long <<- Sys.time()
    interval <- as.numeric(t2_long - t1_long, units = "secs")

    if (interval > 1170){
      print("5.5 minute RPM pause -- don't overwhelm AO3 with requests")
      Sys.sleep(330)
      t1_long <<- Sys.time()
    }

  } else{
    t1_long <<- Sys.time()
  }


}

rpm_delay_alt <- function(){

  # Count of Requests
  if (exists("req_count")){

    req_count <<- req_count + 1

    if (req_count %% 60 == 0){
      Sys.sleep(300)
    }

  } else{

    req_count <<- 1
  }

  # Time Period counting
  if (exists("t1_short")){
    t2_short <<- Sys.time()
    interval <- as.numeric(t2_short - t1_short, units = "secs")
    delay <- max(0, (4 - min(1, interval))) #2sec delay per req

    Sys.sleep(delay)
    t1_short <<- t2_short

  } else{
    t1_short <<- Sys.time()
  }


}
