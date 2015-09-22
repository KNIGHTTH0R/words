# load wordlists into workspace when package is attached
#

.onLoad <- function(lib, pkg) 
{
  l <- list()
  class(l) <- "wordlist"
  assign(".words", l, envir = .GlobalEnv)
  load_languages("en")
}


# clean up workspace
#
.onDetach <- function (libpath)
{
  rm(".words", envir = .GlobalEnv)
}