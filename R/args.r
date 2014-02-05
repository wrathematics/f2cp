get.args <- function(subr, format)
{
  arg.dec <- continuation(loc=1L, subr=subr, format=format)
  
  # Remove parens
  args <- sub(x=arg.dec, perl=TRUE, pattern="^[^\\(]*", replacement="")
  args <- sub(x=args, pattern="\\(", replacement="")
  args <- sub(x=args, pattern="\\)", replacement="")
  
  args <- unlist(strsplit(x=args, split=","))
  args <- gsub(x=args, pattern=" ", replacement="")
  
  return( args )
}

