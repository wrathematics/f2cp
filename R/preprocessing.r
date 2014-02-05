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
  clean <- gsub(x=clean, pattern="^[*].*$", replacement="")
  
  return( clean )
}

