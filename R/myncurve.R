#' Title
#'
#' @param mu the average value
#' @param sigma standard deviation
#' @param a the interval from left critical to a
#'
#' @return A graph of a normal curve with a shaded region and the area
#' @export
#'
#' @examples
#' myncurve(mu=2,sigma=7,a=3)
myncurve = function(mu, sigma, a)
{

  #Make a normal curve
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  #Set shape for shading
  xcurve=seq(mu-3*sigma,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  # Fill in the polygon with the given vertices
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Purple")


  # Area of the shaded region
  prob=pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)
  prob


}
