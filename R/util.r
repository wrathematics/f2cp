nmatches <- function(x, pattern)
{
  ret <- grep(x=x, pattern=pattern)
  if (length(ret) == 0)
    return( 0L )
  else
    return( ret )
}
