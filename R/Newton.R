
# The comments for the inputs: 
# 1. f is a single-argument fuction;
# 2. x0 is the starting point of Newton method;
# 3. delta is the threshold of accepting the result of Newton method;
# 4. N is the maximum of iteration;
# 5. d is used for approximizing f'(x);
# 6. p represents the number of decimals


Newton <- function (f, x0, delta, N, d, p)
{
  x <- c()
  y <- c()
  x[1] <- x0
  y[1] <- f(x[1])
  i <- 1
  Solumatrix <- matrix(c(0,x[1],y[1]),nrow=1,ncol=3,byrow=TRUE) 

  while (abs(y[i]) > delta && i <= N)
  {
    i <- i+1
    x[i] <- x[i-1]-f(x[i-1])*d/(f(x[i-1]+d)-f(x[i-1]))
    y[i] <- f(x[i])
    Solumatrix <- rbind(Solumatrix, c(i-1, x[i], y[i]))
  }
  
  if (p > 0){
    Approximate <- round(Solumatrix, digit=p)
    Showmatrix <- data.frame(Approximate)
    names(Showmatrix) <- c("iter.","x","f(x)")
    print(Showmatrix)}
  
    return(Solumatrix)
}
  
