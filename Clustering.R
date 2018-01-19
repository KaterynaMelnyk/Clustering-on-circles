make.rings <- function(cx, cy, obsnum, ringsnum, ringwidth, gapwidth, wavity = 0){
  dfrm <- data.frame(x = rep(0, obsnum), y = rep(0, obsnum))
  rnumdistr <- c()
  for(i in 1:ringsnum){
    rnumdistr <- c(rnumdistr, rep(i, i))
    }
  kring <- rnumdistr[1+floor(runif(obsnum, max = 0.5*ringsnum*(ringsnum+1)))]
  radius <- (kring - 1)*(gapwidth+ringwidth)+gapwidth+runif(obsnum, max = ringwidth)
  angle <- runif(obsnum,min = -pi,max = pi);
  wavmult <- 1.0 + 0.5*wavity*(1 + cos(angle*8))
  dfrm$x <- cx + radius*cos(angle)*wavmult
  dfrm$y <- cy + radius*sin(angle)*wavmult
  return (dfrm)
}

draw.colored.clust <- function(dfrm){
  old.pars <- par(no.readonly = TRUE)
  par(bg = "black", fg = "white", col.axis = "white", col.lab = "white", col.main = "white", col.sub = "white")
  clrnum <- length(table(dfrm$cls));
  plot(dfrm$x,dfrm$y, col = rainbow(clrnum)[dfrm$cls], pch = 20, xlab = "x", ylab = "y", main = "Colored Clusters", sub = paste0("Clusters: ", clrnum))
  par(old.pars)  
}

clust.rings <- function(dfrm, scalarparam){
  const.x <- (max(dfrm$x)+min(dfrm$x))/2
  const.y <- (max(dfrm$y)+min(dfrm$y))/2
  #dfrm.new <- data.frame(x= rep(0,length(dfrm$x)),y=rep(0,length(dfrm$y)))
  for(i in length(dfrm$x)){
    dfrm$x.new <- dfrm$x-const.x
    dfrm$y.new <- dfrm$y-const.y
  }
  dist.x <- abs(max(dfrm$x.new)-min(dfrm$x.new))
  dist.y <- abs(max(dfrm$y.new)-min(dfrm$y.new))
  if(dist.x >= dist.y){
    coeff <- dist.y/dist.x
    dfrm$x.new <- dfrm$x.new*coeff
  }else{
    coeff <- dist.x/dist.y
    dfrm$y.new <- dfrm$y.new*coeff
  }
  for (i in 1:length(dfrm$x)){
    dfrm$radius[i] <- sqrt((dfrm$x.new[i])^2+(dfrm$y.new[i])^2)
  }
  dfrm.sort <- dfrm[order(dfrm$radius), ]
  arrayDiff_Index <- data.frame(different = rep(0, length(dfrm.sort$x)-1), index = rep(0, length(dfrm.sort$x)-1))
  for (i in 1:(length(dfrm.sort$x)-1)){
    arrayDiff_Index$different[i] <- dfrm.sort$radius[i+1]-dfrm.sort$radius[i]
    arrayDiff_Index$index[i] <- i
  }
  arrayDiff_Index.sort <- arrayDiff_Index[order(arrayDiff_Index$different, decreasing = TRUE), ]
  maxDiff_index.new <- arrayDiff_Index.sort$index[1:scalarparam-1]
  maxDiff_index <- sort(maxDiff_index.new)
  dfrm.sort$cls <- 0
  dfrm.sort$cls[1:maxDiff_index[1]] <- 1
  for (i in 2:(scalarparam-1)){
    dfrm.sort$cls[(maxDiff_index[i-1]+1):maxDiff_index[i]] <- i
  }
  dfrm.sort$cls[(maxDiff_index[length(maxDiff_index)]+1):length(dfrm.sort$cls)] <- scalarparam
  return (dfrm.sort[ , c('x','y','cls')])
}

dfrm <- make.rings(-2,9,500,3,1,2)
plot(dfrm$x, dfrm$y)
dfrm.new <- clust.rings(dfrm, 3)
draw.colored.clust(dfrm.new)

