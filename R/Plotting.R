

plotting <- function (Solumatrix)
{
  plot(Solumatrix[,1],Solumatrix[,3], type="l",main="Newton Method to Find the Solution to a Function",
       xlab="Number of iterations",ylab="Corresponding value of f(x)")
  
}