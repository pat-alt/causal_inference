within_groups <- function(X,y,index) {
  index_vars <- colnames(index)
  dt <- data.table(y,X,index)
  demean <- function(i,...) {i - mean(i,...)}
  dt <- dt[,lapply(.SD,demean),by=index_vars] # demean
  cols <- colnames(dt)[!colnames(dt) %in% colnames(index)]
  dt <- dt[,.SD,.SDcols=cols]
  mod <- lm(y~.,dt)
  return(mod)
}