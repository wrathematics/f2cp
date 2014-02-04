# Assumptions: 
# subroutines are ended by the declaration "end subroutine"
# arguments are all on the initial subroutine line FIXME
# all input args are typed (no implicit declarations)

nmatches <- function(x, pattern)
{
  ret <- grep(x=x, pattern=pattern)
  if (length(ret) == 0)
    return( 0L )
  else
    return( ret )
}

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

clean.subr <- function(subr)
{
  # remove multiple spaces
  subr <- gsub(x=subr, pattern=" +", replacement=" ")
  # remove leading spaces
  subr <- sub(x=subr, pattern="^[ ]", replacement="")
  # assignment information removed
  subr <- sub(x=subr, pattern="=", replacement=" ")
  # remove empty lines
  blanks <- which(subr=="")
  if (length(blanks) > 0)
    subr <- subr[-which(subr=="")]
  
  return( subr )
}

remove.comments <- function(raw)
{
  # !
  clean <- gsub(x=raw, pattern="!.*$", replacement="")
  # leading C
  clean <- gsub(x=clean, pattern="^C.*$", replacement="")
  # *
  clean <- gsub(x=raw, pattern="^[*].*$", replacement="")
  
  return( clean )
}

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

