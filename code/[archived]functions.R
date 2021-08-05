# Notes
# Adapted from UDP--HPRM
# https://github.com/urban-displacement/hprm/

# Calculations ----
# Relative Risk
RR <- function(evicted, renters){
  o = evicted
  r = sum(evicted, na.rm = TRUE)/sum(renters, na.rm = TRUE)
  e = renters*r
  rr = o/e
  rr
}

