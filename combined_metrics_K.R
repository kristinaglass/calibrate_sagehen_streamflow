#' combined relerr and nse function - calculates metric (as part of your combined metric) that needs to be transformed 
#'
#' Compute NSE between observation and model and transform to normalize 
#' @param m model estimates 
#' @param o model observations 
#'
#' @return combined performance 0-1 


combined_function = function(m, o, weight.nse=0.33, weight.rmse=0.33, weight.relerr=0.34) {
  nse = nse(m, o)
  mnse = max(nse, 0)
  
  rmse = sqrt(mean((m - o)^2))
  mrmse = 1 - rmse / max(m, o)
  
  rel.err = relerr(m, o)
  merr = 1.0 - min(1.0, abs(rel.err) / max(abs(rel.err)))
  
  combined = weight.nse * mnse + weight.rmse * mrmse + weight.relerr * merr
  #return(list(nse = nse, relative_error = res, combined = combined))
  return(combined)
}


# 
# 
# perf <-  high_flow_metrics_K(m=sager$model,o=sager$obs, month=sager$month, day=sager$day, year=sager$year, wy=sager$wy, high_flow_months = 8)
# 
# perf <-  as.data.frame((perf))
# # remember you want error to be low but correlation to be high 
# # so we need to transform in some way
# 
# # normalize by max error = if error is greater than this we don't care
# # many ideas -  maybe 50% of mean daily summer observed low flow
# tmp = sager %>% subset(month == 8) 
# errmin = mean(tmp$obs)*0.5
# 
# perf = perf %>% mutate(annual_max_err_trans = min(0,(1-abs(annual_max_err/errmin) )))
# 
# # for monthly we can do a simpler thing to find maximum allowable errror   
# tmp = sager %>% subset(month == 8) %>% group_by(wy, month) %>% summarize(obs=sum(obs))
# 
# errmax = mean(tmp$obs)*0.5
# 
# perf = perf %>% mutate(high_month_err_trans = min(0,(1-abs(high_month_err/errmin) )))
# 
# # now we have 4 measures that we can combine together
# 
# perf = perf %>% mutate(combined = (annual_max_cor + annual_max_err_trans + high_month_err_trans + high_month_cor)/4)
# perf
# 
# # or weight differently - we know that minimum flows are hard to get to weight those differently
# 
# perf = perf %>% mutate(combined2 = 0.1*annual_max_cor + 0.1*annual_max_err_trans + 0.4*high_month_err_trans+ 0.4*high_month_cor)
# 
# #perf
# 
# # easier to put all this in a function

# combined_function <- function(m, o) {
#   
#   # Calculate error between model and observations
#   err = m - o
#   
#   # Calculate mean of observations
#   meanobs = mean(o)
#   
#   # Calculate relative error
#   meanerr <- mean(err)
#   
#   # Calculate mean squared error (MSE)
#   mse = sum(err * err)
#   
#   # Calculate observed variance
#   ovar = sum((o - meanobs) * (o - meanobs))
#   
#   # Calculate NSE
#   nse = 1.0 - mse / ovar
#   
#   res = meanerr / meanobs
#   
#   # Return both NSE and relative error
#   return(list(nse = nse, relative_error = res))
# }


# combined_function = function(m,o,weight.nse=0.5, weight.relerr=0.5) {
#   
#   nse = nse(m,o)
#   mnse = max(nse,0)
#   
#   rel.err = relerr(m,o)
#   merr = 1.0-min(1.0, abs(rel.err)/max(abs(rel.err)))
#   
#   combined = weight.nse*mnse + weight.relerr*merr
#   
#   return(combined)
# }






