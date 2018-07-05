kRecentMonths <- function(k, dat) {
  if (k + 3 > ncol(dat) | k < 1) {
    cat("The k you selected is not able to be calculated with the current data.")
  } else {
    rows <- which(
      rowSums(dat[,c(2:(ncol(dat)-k-1))]) > 0 &
        rowSums(dat[,c((ncol(dat)-k):(ncol(dat)-1))]) == 0 &
        rowSums(dat[,ncol(dat)]) > 0
    )
    dat <- dat[rows,]
    return(dat[order(-dat[,ncol(dat)]),])
  }
}