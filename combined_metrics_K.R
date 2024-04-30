#' combined relerr, nse, and rmse function - calculates metric as part of combined metric that needs to be transformed 
#' @param m model estimates 
#' @param o model observations 
#' @return combined performance 0-1 


combined_function = function(m, o, weight.nse=0.33, weight.rmse=0.33, weight.relerr=0.34) {
  nse = nse(m, o)
  mnse = max(nse, 0)
  
  rmse = sqrt(mean((m - o)^2))
  mrmse = 1 - rmse / max(m, o)
  
  rel.err = relerr(m, o)
  merr = 1.0 - min(1.0, abs(rel.err) / max(abs(rel.err)))
  
  combined = weight.nse * mnse + weight.rmse * mrmse + weight.relerr * merr
  return(combined)
}
