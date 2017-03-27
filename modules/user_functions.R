### USER FUNCTIONS ###
# Last edit: Manny (2017-03-27)

## Description
# User Functions are meta functions and methods for more efficient code writing
# Dependencies: dplyr

## Dependencies
require(dplyr)

## Functions
# Numeric Absolute
nabs <- function(x) {
  
  ## Description
  # Numeric Absolute returns x after first converting it to class numeric via character
  # Its primary use is converting objects of class factor to numeric
  # It also provides a more concise wrapper for standard numeric conversion
  
  return(as.numeric(as.character(x)))
  
}