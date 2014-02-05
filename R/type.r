find.type <- function(arg, subr, format)
{
  types <- c("integer", "double precision", "real", "character", "logical")
  ctypes <- c("int", "double", "float", "char", "int")
  
  
  for (ind in 1L:length(types))
  {
    type <- types[ind]
    
    type.locs <- which(grepl(x=subr, pattern=type))
    
    for (loc in type.locs)
    {
      vars <- continuation(loc=loc, subr=subr, format=format)
      
      # throw away array information on each var
      vars <- gsub(x=vars, replacement="", pattern="\\([^\\)]*\\)")
      vars <- sub(x=vars, replacement=" ", pattern="::")
      
      # get array of vars of given type
      vars <- unlist(strsplit(x=vars, split=","))
      vars <- sub(x=vars, pattern=type, replacement="")
      vars <- unlist(strsplit(x=vars, split=" "))
      vars <- gsub(x=vars, pattern=" ", replacement="")
      vars <- vars[which(vars!="")]
      
      
      if (arg %in% vars)
        return( ctypes[ind] )
    }
  }
  
  
  stop(paste("Couldn't determine variable type of", arg))
}



type.args <- function(args, subr, format)
{
  l <- list(length(args))
  
  for (i in 1L:length(args))
  {
    arg <- args[i]
    type <- find.type(arg=arg, subr=subr, format=format)
    
    l[[i]] <- paste(type, paste("*", arg, sep=""))
  }
  
  return( l )
}

