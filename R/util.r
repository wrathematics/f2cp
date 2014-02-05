nmatches <- function(x, pattern, ignore.case=FALSE)
{
  ret <- grep(x=x, pattern=pattern, ignore.case=ignore.case)
  if (length(ret) == 0)
    return( 0L )
  else
    return( ret )
}



get.format <- function(filename)
{
  if (nmatches(x=filename, pattern="[.]f$", ignore.case=TRUE))
    return( "f77" )
  else if (nmatches(x=filename, pattern="[.]f90$", ignore.case=TRUE))
    return( "f90" )
  else
  {
    warn(paste("Unable to determine format of ", filename, "; skipping", sep=""))
    return( NULL )
  }
}



# control for continuation lines
continuation <- function(loc, subr, format)
{
  if (format == "f90")
  {
    symbol <- "&"
    adj <- 0L
  }
  else
  {
    symbol <- "[$]"
    adj <- 1L
  }
  
  ct <- 0L
  vars <- ""
  n <- nmatches(x=subr[loc+adj], pattern=symbol)
  
  while (n)
  {
    ct <- ct + 1L
    n <- nmatches(x=subr[loc+ct+adj], pattern=symbol)
  }
  
  vars <- subr[loc:(loc+ct)]
  
  vars <- gsub(x=vars, pattern=symbol, replacement=" ")
  vars <- paste(vars, collapse="")
  
  return( vars )
}

