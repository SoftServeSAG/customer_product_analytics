convert_date_to_index <- function(date_) {
  as.integer(date_ - as.Date("2014/03/19", format="%Y/%m/%d")) + 1
}

convert_index_to_date <- function(index_) {
  as.Date("2014/03/19", format="%Y/%m/%d") + index_ - 1
}
