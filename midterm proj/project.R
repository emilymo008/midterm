setwd("/Users/emily/Documents/ma415/midterm proj")
library(foreign)

a <- read.dbf("accid.dbf")
# aFac <- read.dbf("lookups/acc.dbf") don't have this directory yet
v <- read.dbf("viol.dbf")

lookAcc <- read.dbf("lookups/acc.dbf")

o <- read.dbf("osha.dbf")
