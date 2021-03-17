lsdv <- function(X,y,index) {
  dt <- data.table(y,X,index)
  dt[,I:=.I]
  setkey(dt, I)
  for (i in colnames(index)) {
    dummies <- dcast(dt[,.(.I, var = get(i))], ... ~ var, fun.aggregate = length) # one-hot encode
    cols <- colnames(dt)[colnames(dt) != i]
    dt <- dt[,.SD,.SDcols=cols]
    setkey(dummies, I)
    dt <- dt[dummies]
  }
  dt[,I:=NULL]
  mod <- lm(y~0+.,dt)
  return(mod)
}