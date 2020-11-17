#fitcalc.R

#Given a vector of n x values and a vector of n y values,
#returns a vector of n coefficients for a polynomial of degree n-1

x <- c(0,1,2)   #the evaluation points
y <- c(1,4,9)   #desired values at these points
#Find the coefficients of a polynomial that gives y when evaluated at e
fit.makePoly <- function(x,y){
  n <- length(x)
  stopifnot(length(y)==n)
  P <- matrix(nrow = n, ncol = n)   #the change-of-basis matrix
  for (i in 1:n) {
    P[,i] <- x^(i-1)
  }
  P   #each column gives the values of a monomial at 0, 1, and 2
  PInv <- solve(P);PInv   #the inverse matrix
  #Each column gives the coefficients of a polynomial that is 1 at one point, 0 at the others.
  #p(x) = 1 - 1.5x +0.5 x^2 has p(0)=1 p(1)=p(2)=0
  #p(x) = 2x - x^2 has p(1)=1 p(0)=p(2)=0
  #p(x) = 0.5x +0.5 x^2 has p(2)=1 p(0)=p(1)=0
  coeff <- PInv%*%y #multiply each by the desired value
  return(coeff)
}
fit.makePoly(x,y)

#Evaluate a polynomial with the specified coefficients at the specified x
eval <- function(coeff,x){
  n <- length(coeff)
  xpower <- numeric(n)
  #Calculate the powers 0 though n-1 for x
  for (i in 1:n) 
    xpower[i] <- x^(i-1)
  #Add up the terms in the polynomial
  return (sum(coeff*xpower))
}
#Vectorize this function so we can evaluate at many points
fit.eval <- Vectorize(eval,"x")


coeff <- fit.makePoly(x,y); coeff   #find the coefficients
fit.eval(coeff,x)   #check that we got it right
curve(fit.eval(coeff,x), from = 0, to = 4)

#Points where we know the value of the sine function
x <- c(0,pi/6,pi/4,pi/3,pi/2, 2*pi/3)
y <- c(0,1/2,sqrt(2)/2, sqrt(3)/2,1,sqrt(3)/2)  #the values we know
coeff <- fit.makePoly(x,y);coeff    #coefficients for a degree 5 polynomial
curve(fit.eval(coeff,x), from = -1, to = 3)   #graph it in black
curve(sin, add = TRUE, col = "red")           #graph the sine function in red to compare


#Given a polynomial as a vector of coefficients, converts it to a printable string
convertPoly <- function(coeff) {
  v <- character(0)
  n <- length(coeff)
  if (coeff[n] < 0) v <- c(v, "-")
  while(n > 0) {
    if (n > 2 && coeff[n] != 0){
      if (coeff[n] != 1) v <- c(v, abs(coeff[n]))
      v <- c(v,"x","<sup>",n-1,"</sup>",ifelse(coeff[n-1]>0,"+","-"))
    }
    
    if (n== 2 && coeff[2] != 0) {
      if (coeff[2] != 1) v <- c(v, abs(coeff[2]))
      v <- c(v,"x",ifelse(coeff[n-1]>0,"+","-"))
    }
    if ((n == 1) && (length(v) == 0 || coeff[1]!=0)) v <- c(v, abs(coeff[1]))
    n <- n-1
  }
  if (tail(v,1)=="+") v <- head(v,-1)
  return(paste(v,sep="",collapse=""))
}
convertPoly(c(1,0,0,2))
