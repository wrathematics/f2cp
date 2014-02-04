find.type <- function(arg, subr)
{
  types <- c("integer", "double precision", "real", "character")
  ctypes <- c("int", "double", "float", "char")
  
  
  for (ind in 1L:length(types))
  {
    type <- types[ind]
    
    type.locs <- which(grepl(x=subr, pattern=type))
    
    for (loc in type.locs)
    {
      # throw away array information on each var
      vars <- gsub(x=subr, replacement="", pattern="\\([^\\)]*\\)")
      
      # get array of vars of given type
      vars <- unlist(strsplit(vars[loc], split=","))
      vars <- sub(x=vars, pattern=type, replacement="")
      vars <- unlist(strsplit(x=vars, split=" "))
      vars <- gsub(x=vars, pattern=" ", replacement="")
      vars <- vars[which(vars!="")]
      
      
      if (arg %in% vars)
        return( ctypes[ind] )
    }
  }
  
  
  stop("Couldn't determine variable type")
}



get.args <- function(subr)
{
  # FIXME this assumes all args are on line 1
  args <- sub(x=subr[1L], perl=TRUE, pattern="^[^\\(]*", replacement="")
  args <- sub(x=args, pattern="\\(", replacement="")
  args <- sub(x=args, pattern="\\)", replacement="")
  
  args <- unlist(strsplit(x=args, split=","))
  args <- gsub(x=args, pattern=" ", replacement="")
  
  return( args )
}



type.args <- function(args, subr)
{
  l <- list(length(args))
  
  for (i in 1L:length(args))
  {
    arg <- args[i]
    type <- find.type(arg=arg, subr=subr)
    
    l[[i]] <- paste(type, paste("*", arg, sep=""))
  }
  
  return( l )
}

