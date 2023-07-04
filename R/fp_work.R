#FP Database
fp_time <- function(x){
  x_1=format(strptime(x, format="%I:%M %p"),
             format="%H:%M")
  return(x_1)
}

hmrange <- function(x){
  x_1 =(as.numeric(sub(":","",x)))
  x_2=ifelse((x_1<=530 | x_1>=1930),1,NA)
  return(x_2)
}

tm_format <- function(x){
  x_1=substr(x,nchar(x)-1,nchar(x))
  x_2=ifelse(!(x_1 %in% c("AM", "PM", "",NA)),1,NA)
  return(x_2)
}

fp_date <- function(dt, tm){
  dt=lubridate::ymd(dt)
  tm=format(strptime(tm, format="%I:%M %p"),
            format="%H:%M")
  format = "%Y-%m-%d %H:%M"
  y_dt_tm = as.POSIXct(paste(dt, tm), format = format)
  y_dt_tm_1 = substr(y_dt_tm, 1, 16)
  return(y_dt_tm_1)
}
