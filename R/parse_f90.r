# Assumptions: 
# subroutines are ended by the declaration "end subroutine"
# arguments are all on the initial subroutine line FIXME
# all input args are typed (no implicit declarations)

parse.f90 <- function(filename)
{
  raw <- readLines(filename)
  raw <- tolower(remove.comments(raw=raw))
  
  sl <- which(grepl(x=raw, pattern="^(?!end subroutine)subroutine", perl=TRUE))
  el <- which(grepl(x=raw, pattern="end subroutine"))
  
  if (length(sl) != length(el))
    stop("Couldn't match each 'subroutine' call with 'end subroutine'")
  
  ret <- list(length(sl))
  
  for (ind in 1L:length(sl))
  {
    start <- sl[ind]
    end <- el[ind]
    
    subr <- raw[start:end]
    subr <- clean.subr(subr=subr)
    
    # name
    nm <- sub(x=subr[1L], perl=TRUE, pattern="^[subroutine ]*", replacement="")
    nm <- sub(x=nm, perl=TRUE, pattern="\\((.*)", replacement="")
    
    # arguments
    args <- get.args(subr=subr)
    args <- type.args(args=args, subr=subr)
    
    # C prototype
    vars <- paste(args, collapse=", ")
    prototype <- paste(nm, "_(", vars, ");", sep="")
    
    ret[ind] <- prototype
  }
  
  return( ret )
}

