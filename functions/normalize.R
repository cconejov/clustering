## Normalize
## Input: Numeric vector
## Output: Vector normalized.

normalize <- function(x){
  
  min_x <- min(x)
  max_x <- max(x)
  
  return((x - min_x)/(max_x - min_x))
  
}