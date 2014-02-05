library(f2cp)

#f90 <- system("ls *.f90", intern=TRUE)
#filename <- f90[1]
filename <- "test.f90"


setwd("~/dev/f2cp/test")
#debug(f2cp:::find.type)

parse.f90(filename)

