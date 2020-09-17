### Calculating overlap ###

#Here we provide R code for functions that we use for calculating overlap of VSTs.
#those functions are: 
# - oa2
# - simul_overlap2
# 

#custom made function "oa2" to calculate overlap of VSTs in time and frequency and time only
#' Calculate total area of ovrlapped rectangles
#'
#' @details Calculate total area of overlapped rectangles, that are defined by vectors of bottom-left and top-right vertices.
"' Bottom-left and top-right vertices are given by vectors (\code{l}, \code{b}) and (\code{r},\code{t}) respectively. 
"'
#' @param l, b, r, t Numeric vectors with rectangle defining coordinates (see Details).
#' @return A numeric vector with total overlapped area of rectangles (aka overlap) and total overlap of rectangle widths (aka time).
#' @export
#' @note In our application, \code{l} and \code{r} represent signal start and end time, while \code{b} and code{t} represent 
#' minimal amd maximal observed frequency.
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' l <- c(1,2,3)
#' b <- c(1,2,3)
#' r <- c(2.5,4.5,5)
#' t <- c(3,4,5)
#' plot(0,0
#'    , xlim=c(0,6)
#'    , ylim=c(0,6)
#'    , ann=FALSE
#'    , type = "n")
#'    box()
#'    rect(l,b,r,t)
#' oa2(l, b, r, t) 
#' 
oa2 <- function(l,b,r,t)   #l, b, r, t must be vectors of the same length. Values in this vectors on same position are limits of one recangle that limit one VST on spectrogram (l =left, b= bottom, r= right, t=top). 
  {
  di=10
  ord <- order(l)
  l <- l[ord]
  b <- b[ord]
  r <- r[ord]
  t <- t[ord]
  n <- length(l)
  orig <- seq(1,n*n,n+1)  # indices to the observed rectangles
  my.expand.grid <- function(x,di=10) t(expand.grid(x, min(n,(x+1)):min(n,(x+di))))
  what <- t(as.data.frame(lapply(1:(n-1), my.expand.grid, di=di)))
  dimnames(what)[[1]] <- 1:nrow(what)
  left <- apply(cbind(l[what[,1]],l[what[,2]]),1,max)
  right <- apply(cbind(r[what[,1]],r[what[,2]]),1,min)
  bottom <- apply(cbind(b[what[,1]],b[what[,2]]),1,max)
  top <- apply(cbind(t[what[,1]],t[what[,2]]),1,min)
  width <- right - left
  height <- top - bottom
  over <- (width>0)&(height>0)
  width[!over] <- 0
  height[!over] <- 0
  overlap <- sum(width*height)
  over_time <- sum(width)
  return(c(overlap=overlap, time=over_time))  #output of function are two values: "overlap" (overlap in time nad frequency) and "time" (overlap in time). 
}

# custom made function "simul_overlap2" to calculate overlap in randomly distributed rectangles 
# See function oa2() for arguments.
simul_overlap2 <- function(l,b,r,t){
  n <- length(l)
  w <- r-l
  h <- t-b
  l_new <- runif(n,min(l),max(l))
  oa2(l_new,b,l_new+w,t)
}

# make data frame "areas2" with values of 1000 times replicated function "simul_overlap2" 
areas2 <- t(replicate(1000, simul_overlap2(l[filter], b[filter], r[filter], t[filter], plt=FALSE, col="blue")))



