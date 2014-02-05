get.fmt <- function(format, filename)
{
  if (format == "check")
    fmt <- get.format(filename=filename)
  else
    fmt <- format
  
  return( fmt )
}



f2cp.dir <- function(directory, format)
{
  # Get list of files and their formats
  files <- dir()
  files <- files[which(grepl(x=files, ignore.case=TRUE, pattern="([.]f$|[.]f90$)"))]
  
  if (length(files) == 0)
  {
    warn(paste("No suitable files found in directory", directory))
    return( NULL )
  }
  
  if (format == "check")
    formats <- sapply(X=files, FUN=get.format)
  else
    formats <- rep(x=format, length(files))
  
  # Parse and return
  ret <- lapply( X=1L:length(files), FUN=function(i) parse.fortran(files[i], formats[i]) )
  
  return( ret )
}



f2cp.file <- function(filename, format)
{
  fmt <- get.fmt(format=format, filename=filename)
  
  ret <- parse.fortran(filename=filename, format=fmt)
  
  return( ret )
}



f2cp <- function(filename, directory, output="single", format="check")
{
  if (length(format) > 1L)
    stop("argument 'format' must be length 1; see the documentation")
  
  format <- match.arg(tolower(format), c("check", "f90", "f77"))
  
  if (missing(filename))
  {
    if (missing(directory))
      stop("You must supply either 'filename' or 'directory'")
    else
      ret <- f2cp.dir(directory=directory, format=format)
  }
  else if (missing(directory))
    ret <- f2cp.file(filename=filename, format=format)
  else
    stop("You must supply only one of 'filename' or 'directory'")
  
  
  
  return( ret )
}
