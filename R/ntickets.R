#' @title ntickets function
#'
#' @param N # Number of seats
#' @param gamma # Probability of a full plain
#' @param p # Probability of show
#'
#' @importFrom stats dbeta dnorm optimize pbinom pnorm quantile rbinom
#' @importFrom graphics abline axis barplot contour curve hist points polygon segments text
#' @importFrom grDevices rainbow
#'
#'
#'
#' @return # 2 plots and their minimum value one discrete and other continuous
#' @export
#'
#' @examples
#' ntickets(400,0.02,0.95)
ntickets <- function(N,gamma,p)
{
  # Calculating the probability of a no show
  q = 1-p

  # Plotting the discrete distribution and calculating the 0 for nd
  x = seq(N-20,N+80,1)

  plot(x,1-gamma-pbinom(N,x,p), main = "Discrete Objective vs n", xlab = "n", ylab = "Objetive", col = "green")

  x_min = which.min(abs(1-gamma-pbinom(N,x,p)))

  nd = x[x_min]

  abline(v=nd, col = "blue")


  # Plotting the continuous distribution and optimizing to calculate the 0 for nd
  curve(1-gamma-pnorm(N,x*p,sqrt(x*p*q)), xlim = c(N-20,N+80), main = "Continuous Objective vs n", xlab = "n", ylab = "Objective", col = "green")

  f <- function(x)
  {
    N = N
    gamma = gamma
    p = p
    q = q
    abs(1-gamma-pnorm(N,x*p,sqrt(x*p*q)))
  }
  op = optimize(f, interval = c(N,N+20))

  nc = op$minimum
  abline(v = nc, col = "blue")


  # Printing a named list of the calculated and input values used
  print(paste("N = ", N), quote = FALSE)
  print(paste("gamma = ", gamma), quote = FALSE)
  print(paste("p = ", p), quote = FALSE)
  print(paste("nd = ", nd), quote = FALSE)
  print(paste("nc = ", nc), quote = FALSE)


}
