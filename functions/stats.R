# Basic statitcs

cc_stats <- function(x){
  #NA Values
  nas = sum(is.na(x))
  
  # Vector with complete values
  a = x[!is.na(x)]
  
  # Properties
  
  m = mean(a)
  min = min(a)
  max = max(a)
  s = sd(a)
  
  # Stats
  stats <- boxplot.stats(a)
  n <- stats$n
  out <- length(stats$out)
  
  Q95 = quantile(a, 0.95)
  UL = m + 3*s
  
  
  return(c(n     = n,
           nas   = nas,
           Mean  = m,
           StDev = s,
           Q_out = out,   
           Min   = min,
           Max   = max,
           Q95   = Q95,
           Upper_Limit = UL))
}
