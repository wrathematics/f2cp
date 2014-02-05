find.linenums <- function(raw, format, filename)
{
  sl.regex <- "(?!end subroutine)(      subroutine|^subroutine|recursive subroutine)"
  el.regex <- "(^| )(?!end if|endif|end do|enddo)(end($| ))"
  
  sl <- which(grepl(x=raw, pattern=sl.regex, perl=TRUE))
  el <- which(grepl(x=raw, pattern=el.regex, perl=TRUE))
  
  if (length(sl) != length(el))
    stop(paste("Couldn't match each 'subroutine' call with its 'end' in file", filename))
  
  return( list(sl=sl, el=el) )
}
