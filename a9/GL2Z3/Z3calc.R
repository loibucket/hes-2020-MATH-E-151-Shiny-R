#Matrix and vector calculations in Z3
#These functions use -1 and -2 aas names instead of 4 and 3
Z3Sum <- function(a,b){
  s <-(a+b)%%3
  if (s > 1) s <- s-3
  return (s)
}

Z3Prod <- function(a,b){
  s <-(a*b)%%3
  if (s > 1) s <- s-3
  return (s)
}

Z3Inv <- function(a){
  validate(need(a != 0, "Cannot divide by zero"))
  if (a == 1) return(1)
  if (a == -1) return(-1)
}

#These vectorized functions make it possible 
#to multiply a matrix or a vector by a constant
vZ3Sum <- Vectorize(Z3Sum,c("a","b"))
vZ3Prod <- Vectorize(Z3Prod,c("a","b"))

#5 rows for each of the 6 subspaces -- permits easy lookup
makeVecList <- function() {
  m <- matrix(ncol = 2, nrow = 12)
  v <- c(1,0)
  m[1,] <- vZ3Prod(0,v)
  m[2,] <- vZ3Prod(1,v)
  m[3,] <- vZ3Prod(-1,v)
  v <- c(0,1)
  m[4,] <- vZ3Prod(0,v)
  m[5,] <- vZ3Prod(1,v)
  m[6,] <- vZ3Prod(-1,v)
  v <- c(1,1)
  m[7,] <- vZ3Prod(0,v)
  m[8,] <- vZ3Prod(1,v)
  m[9,] <- vZ3Prod(-1,v)
  v <- c(1,-1)
  m[10,] <- vZ3Prod(0,v)
  m[11,] <- vZ3Prod(1,v)
  m[12,] <- vZ3Prod(-1,v)
  return (m)
}

library(prodlim) #needed for row.match
#Multiply a matrix by a vector in Z3
ActOnVector <- function(A,v){
  return(vZ3Sum(vZ3Prod(v[1],A[,1]),vZ3Prod(v[2],A[,2])))
}

#Applies matrix A to a vector from subspace idx
#Looks up the result to find what subspace it is in.
Transform <- function(A,idx){
  m <- makeVecList()
  v <- m[3*idx-1,]   #second vector (first nonzero one) in subspace idx
  x <- vZ3Sum(vZ3Prod(v[1],A[,1]),vZ3Prod(v[2],A[,2]))
  #x <- ActOnVector(A, v)
  #Find what row in matrix m amatches vector v
  r <- row.match(x,m)
  return(floor((r+2)/3))
}

#Multiply 2x2 matrices in Z3
Z3MatProd <- function(A,B) {
  v1 <- B[,1]
  x1 <- vZ3Sum(vZ3Prod(v1[1],A[,1]),vZ3Prod(v1[2],A[,2]))
  v2 <- B[,2]
  x2 <- vZ3Sum(vZ3Prod(v2[1],A[,1]),vZ3Prod(v2[2],A[,2]))
  return(cbind(x1,x2))
}

Z3Det <- function(A) {
  Z3Sum(Z3Prod(A[1,1],A[2,2]),Z3Prod(A[1,2],A[2,1]))
}

#Create a random matrix with specified determinant and trace
Z3CreateMatrix <- function(det,trc){
  elements <- c(0,1,-1)
#generate random top left entry
  a11 <- as.numeric(sample(elements,1))
#make the trace correct
  a22 <- Z3Sum(-a11,trc)
#choose a random nonzero element
  b1 <- sample(elements[2:3],1)   #nonzero
  #Required product for the off-diagonal elements
  offDiag <- Z3Sum(-det,Z3Prod(a11,a22))    #could be zero
  #Multiply it by the inverse of the known element
  b2 <- Z3Prod(offDiag,Z3Inv(b1))     #could be zero
  #The following makes it possible to have a zero in either off-diagonal position
  if (sample(2,1)==1)
    return (matrix(c(a11,b1,b2,a22),2))
  return (matrix(c(a11,b2,b1,a22),2))
}

#A <- Z3CreateMatrix(1,0)

