require(dplyr)
require(lubridate)
require(data.table)

formatting_tm <- function(x,y){
  y=factor(y, levels = c(1,2), labels = c("AM","PM"))
  x_1=format(strptime(paste(substr(x,1,5), y, sep=" "),
                      format = "%I:%M %p"), format = "%H:%M")
  return(noquote(x_1))
}

tm_diff <- function(coll_tm,recv_tm){
  x_1=as.numeric(as.difftime(recv_tm,format ="%H:%M") -
                   as.difftime(coll_tm, format = "%H:%M", units = "mins"))
  return(noquote(x_1))
}

non_sputum_coll <- function(na,nc,dt){
  x_1=ifelse(((is.na(na) | is.na(nc)) & str_count(dt)>0),1,2)
  return(x_1)
}

formatting_dt <- function(tm,ampm,dt){
  ampm=factor(ampm, levels = c(1,2), labels = c("AM","PM"))
  x_1=format(strptime(paste(substr(tm,1,5), ampm, sep=" "),
                      format = "%I:%M %p"), format = "%H:%M")
  dt=ymd(dt)
  format="%Y-%m-%d %H:%M"
  y_dt_tm=as.POSIXct(paste(dt, x_1), format=format)
  y_dt_tm_1=substr(y_dt_tm,1,16)
  return(y_dt_tm_1)
}

formatting_dt_1 <- function(tm,ampm,dt){
  ampm=factor(ampm, levels = c(1,2), labels = c("AM","PM"))
  x_1=format(strptime(paste(substr(tm,1,5), ampm, sep=" "),
                      format = "%I:%M %p"), format = "%H:%M")
  dt=dmy(dt)
  format="%Y-%m-%d %H:%M"
  y_dt_tm=as.POSIXct(paste(dt, x_1), format=format)
  y_dt_tm_1=substr(y_dt_tm,1,16)
  return(y_dt_tm_1)
}


missing_fields <- function (.data)
{
  df.in=.data
  df.in_1=df.in[1:4]
  df.out = df.in  %>% is.na() %>%
    data.frame() %>%
    dplyr::mutate_all(factor,
                      levels = c("FALSE", "TRUE"), labels = c("", "Miss"))
  names(df.out) = paste0(names(df.out), "_na")
  df.return = cbind(df.in_1,df.out)
  df.return_1=df.return %>%
    pivot_longer(cols = ends_with("_na"), names_to = "Variables", values_to = "Error",
                 values_drop_na = TRUE) %>%
    mutate(Variables=substr(Variables,1,(nchar(Variables)-3)))
  return(df.return_1)
}

dt_tm_diff <- function(coll_dt_tm, recv_dt_tm){
  x_1=as.numeric(difftime(recv_dt_tm,coll_dt_tm,units = "mins"))
  return(x_1)
}

run_source <- function(path){
  runlist=list.files(path, pattern = ".R")
  lapply(as.list(runlist), source)
}

read_allsheets_sk <- function(filename, tibble = TRUE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  unlist(x)
}
