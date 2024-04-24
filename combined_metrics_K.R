#' combined relerr and nse function
#'
#'ompute NSE between observation and model and
#' @param m 
#' @param o 
#'
#' @return
#' @export
#'
#' @examples
#' 
combined_function <- function(m, o) {
  
  # Calculate error between model and observations
  err = m - o
  
  # Calculate mean of observations
  meanobs = mean(o)
  
  # Calculate relative error
  meanerr <- mean(err)
  
  # Calculate mean squared error (MSE)
  mse = sum(err * err)
  
  # Calculate observed variance
  ovar = sum((o - meanobs) * (o - meanobs))
  
  # Calculate NSE
  nse = 1.0 - mse / ovar
  
  res = meanerr / meanobs
  
  # Return both NSE and relative error
  return(list(nse = nse, relative_error = res))
}
