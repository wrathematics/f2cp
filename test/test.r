source("../R/make_headers.r")
setwd("../test")

f90 <- system("ls *.f90", intern=TRUE)

filename <- f90[1]

parse.f90(filename)

