# Set directory to source file location

#setwd(getSrcDirectory()[1])

# If using RStdio use this:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
if(!require(pacman)) install.packages('pacman')
pacman::p_load(colorout, tidycensus, googledrive, sf, tigris, rmapshaper, tidyverse, data.table)
options(gargle_oob_default = TRUE, scipen=10, tigris_use_cache = TRUE) # avoid scientific notation, oob = out-of-bound auth, set to TRUE when using RStudio Server
drive_auth(use_oob = TRUE)


