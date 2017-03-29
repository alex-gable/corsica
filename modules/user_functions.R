### USER FUNCTIONS ###
# Last edit: Manny (2017-03-28)


## Description
# User Functions are meta functions and methods for more efficient code writing
# Dependencies: dplyr


## Dependencies
require(dplyr)


## General Functions
# Numeric Absolute
nabs <- function(x) {
  
  ## Description
  # nabs() returns x after first converting it to class numeric via character
  # Its primary use is converting objects of class factor to numeric
  # It also provides a more concise wrapper for standard numeric conversion
  
  return(as.numeric(as.character(x)))
  
}

# Logarithmic Loss
log_loss <- function(act, pred, allow_inf = FALSE) {
  
  ## Description
  # Log_loss() returns the logarithmic loss obtained from a given prediction and known result
  # The allow_inf parameter controls whether infinite loss is allowed (default is FALSE)
  # Setting allow_inf to FALSE will cause large but finite penalties at the extremes
  
  eps = as.numeric(!allow_inf)*1e-15
  
  pred = matrix(sapply(pred, function(x) max(eps, x)),
                nrow = nrow(pred)
                )      
  pred = matrix(sapply(pred, function(x) min(1 - eps, x)), 
                nrow = nrow(pred)
                )
  
  ll = sum(act*log(pred) + (1 - act)*log(1 - pred))
  ll = -ll/(nrow(act))    
  
  return(ll)
  
}

# Moving 
moving <- function(x, n = 5) {
  
  ## Description
  # moving() returns a vector of averages obtained from the n elements of x preceding and including the element \
  # at each respective index
  
  if(length(x) < n) {
    
    v <- NA
    
  } else {
  
    stats::filter(x,
                  rep(1/n, n),
                  sides = 1
                  ) -> 
      v
    
  }
  
  return(as.numeric(v))
  
}

# Brier Score
brier <- function(act, pred) {
  
  ## Description
  # brier() returns the Brier score obtained from a given prediction and known result
  
  bri <- sum((act - pred)^2)/length(act)
  
  return(bri)
  
}

# NA if NULL
na_if_null <- function(x) {
  
  ## Description
  # na_if_null() returns an object's value if it is not NULL and NA otherwise
  
  return(ifelse(is.null(x) == TRUE,
                NA,
                x
                )
         )
  
}

# Do Call Apply
dcapply <- function(x, fun, combine, cores, ...) {
  
  ## Description
  # dcapply() uses do.call() to merge the products of an applied function according to specifications
  # The function will be applied in parallel if cores >= 1
  
  if(cores > 1) {
    
    registerDoMC(cores)
  
    chunks <- split(x, cut(1:length(x), cores))
    
    foreach(i = 1:cores, .combine = c) %dopar% {
      
      chunks[[i]] %>%
        lapply(fun, ...) 
      
    } -> list
    
    combined <- do.call(combine, list)
    
  } else {
    
    
    list <- lapply(x, fun, ...)
    
    combined <- do.call(combine, list)
    
  }
  
  return(combined)
  
}
