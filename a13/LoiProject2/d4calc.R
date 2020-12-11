#d4calc.R - Symmetries of the equilateral triangle

D4.makeDataFrame <- function() {
  DF <- data.frame(name=rep("",8),cfg=rep("",8),stringsAsFactors = FALSE)
  DF[1,] <- c("i","ABCD")
  DF[2,] <- c("r","DABC")
  DF[3,] <- c("s","CDAB")  
  DF[4,] <- c("t","BCDA")
  DF[5,] <- c("w","BADC")
  DF[6,] <- c("x","DCBA")
  DF[7,] <- c("y","ADCB")
  DF[8,] <- c("z","CBAD")
  return(DF)
}

shift <- 2

#BiggsDF <- D4.makeDataFrame()
#BiggsDF
D4.showConfigs <- function(DF) {
  par(mar=c(1,1,1,1))
  plot(NULL,xlim=c(-2,21),ylim = c(-1,3), asp = 1, axes = FALSE)
  for (i in 0:7) {
    points(c(0,0,2,2,0)+3*i-shift,c(0,2,2,0,0),type = "l")
    lbl <- strsplit(DF[i+1,2],"")[[1]]
    s = 0.3
    text(c(0+s,0+s,2-s,2-s)+3*i-shift,c(0+s,2-s,2-s,0+s),lbl,cex=1.5)
    text(1+3*i-shift,-0.5,DF[i+1,1],cex=1.5)
    ext = 0.2
    segments(10-ext,1,x1=12+ext,y1=1,lty = 2) #w
    segments(14,0-ext,x1=14,y1=2+ext,lty = 2) #x
    segments(16-ext,0-ext,x1=18+ext,y1=2+ext,lty = 2) #y
    segments(19-ext,2+ext,x1=21+ext,y1=0-ext,lty = 2) #z
  }
  "rotations label"
  text(2,1,"cw90")
  text(5,1,"cw180")
  text(8,1,HTML("cw270"))
}
#D4.showConfigs(BiggsDF)

D4.showSquare <- function(cfg){
  par(mar=c(1,1,1,1))
  plot(NULL,xlim=c(0,3),ylim = c(-1,2), asp = 1, axes = FALSE)
  points(c(0,0,2,2,0),c(0,2,2,0,0),type = "l", lwd = 2)
  lbl <- strsplit(cfg,"")[[1]]
  s = 0.2
  text(c(0+s,0+s,2-s,2-s),c(0+s,2-s,2-s,0+s),lbl,cex=1.5)
}
#D4.showSquare("ABCD")

#a is one of the Biggs symbols for an operation.

#The return value is the new configuration
D4.apply <- function(a,cfg){
  v <-strsplit(cfg,"")[[1]]   #select first component of list
  w <- switch(a,
              "i" = v,
              "r" = c(v[4],v[1],v[2],v[3]),
              "s" = c(v[3],v[4],v[1],v[2]),
              "t" = c(v[2],v[3],v[4],v[1]),
              "w" = c(v[2],v[1],v[4],v[3]),
              "x" = c(v[4],v[3],v[2],v[1]),
              "y" = c(v[1],v[4],v[3],v[2]),
              "z" = c(v[3],v[2],v[1],v[4])
  )
  s <- paste(w,collapse="") 
  return(s)
  # DF[1,] <- c("i","ABCD") 1234
  # DF[2,] <- c("r","DABC") 4123
  # DF[3,] <- c("s","BCDA") 2341
  # DF[4,] <- c("t","CDAB") 3412
  # DF[5,] <- c("w","BADC") 2143
  # DF[6,] <- c("x","DCBA") 4321
  # DF[7,] <- c("y","ADCB") 1432
  # DF[8,] <- c("z","CBAD") 3214
}
D4.apply("r","ABCD")


D4.multiply <- function(DF,a,b){
  #Look up the name, which occurs once and only once
  idx <- which.max(DF$name==b)
  #Find the corresponding configuration
  cfg <- DF$cfg[idx]
  #Apply the group operation to it
  newcfg <- D4.apply(a,cfg)
 # Look up the configuration
  idx <- which.max(DF$cfg==newcfg)
  return (DF$name[idx])
}
#D4.multiply(BiggsDF,"r","r")

#To use this with outer() we must vectorize it
vD4.multiply <- Vectorize(D4.multiply,c("a","b"))
#outer(c("i","r","s","x","y","z"),c("i","r","s","x","y","z"),"vD4.multiply", DF = BiggsDF)

