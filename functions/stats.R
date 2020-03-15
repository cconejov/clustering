# Basic statistics
# Input: x vector
# Output: Summary of statistics of the input


cc_stats <- function(x){
  
  #NA Values
  nas = sum(is.na(x))
  
  # Vector with complete values
  a = x[!is.na(x)]
  
  # Properties
  
  m   = mean(a)
  min = min(a)
  max = max(a)
  s   = sd(a)
  
  # Stats
  stats <- boxplot.stats(a)
  n     <- stats$n
  out   <- length(stats$out)
  
  Q  = quantile(a, 0.95)
  UL = m + 3*s
  
  return(c(n     = n,
           nas   = nas,
           Mean  = m,
           StDev = s,
           Q_out = out,   
           Min   = min,
           Max   = max,
           Q     = Q,
           Upper_Limit = UL))
}
