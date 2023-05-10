
require(dplyr)
require(lubridate)

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
  y=factor(y, levels = c(1,2), labels = c("AM","PM"))
  x_1=format(strptime(paste(substr(x,1,5), y, sep=" "),
                      format = "%I:%M %p"), format = "%H:%M")
  return(noquote(ymd_hm(z, x_1)))
}

formatting_dt_1 <- function(tm,ampm,dt){
  y=factor(y, levels = c(1,2), labels = c("AM","PM"))
  x_1=format(strptime(paste(substr(x,1,5), y, sep=" "),
                      format = "%I:%M %p"), format = "%H:%M")
  return(noquote(ymd_hm(z, x_1)))
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
