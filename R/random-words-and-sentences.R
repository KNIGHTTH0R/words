#//////////////////////////////////////////////////////////////////////////////
#
#                           RANDOM WORDS AND SENTENCES
#
#//////////////////////////////////////////////////////////////////////////////



#### +---------- LANGUAGES ---------------- ####


#' Get languages for which wordlists are available
#' @export
#' 
get_languages <- function()
{
  #path <- system.file("inst/extdata/wordlists", package = "words")
  path <- file.path("inst/extdata/wordlists")
  files <- list.files(path, pattern = ".txt")
  gsub(".txt", "", files)
}



# Load a word list
# 
# A word list is loaed by copying it into the .words object in the global
# environment.
# 
load_word_list <- function(lang="en")
{
  lang <- tolower(lang)
  langs <- get_languages()
  if (all(lang != langs))
    stop("Language '", lang, "' is not known. Please choose from: ", paste(langs,""), call. = FALSE)
  
  #file <- system.file("inst/extdata/wordlists", paste0(lang, ".txt"), package = "words")
  file <- file.path("inst/extdata/wordlists", paste0(lang, ".txt"))
  x <- read.table(file, stringsAsFactors = FALSE)[[1]]    # read and convert to vector
  .words[[lang]] <<- x                                    # add list to hidden object in workspace
}


#' Load language reference lists into memory
#' 
#' By default an English wordlist is loaded. To show available languages use 
#' \code{\link{get_languages}}.
#' 
#' @param ... Language shortcodes as character vector(s), e.g. \code{("en",
#'   "de")}. See \code{\link{get_languages}} for available shortcodes.
#' @examples
#' load_languages("de", "nl")
#' @export
#' 
load_languages <- function(...)
{
  dots <- list(...)
  langs <- unlist(dots)
  dummy <- sapply(langs, load_word_list)
}


#' Print method for wordlist object
#' 
#' Object is created in zzz.R.
#' 
#' @export
#' @keywords internal
#' 
print.wordlist <- function(x)
{
  cat("A list with the language word lists. Loaded languages are:", names(x))
}


# Get loaed languages
#
loaded_languages <- function()
{
  names(.words)
}


# get the word list for a specific language
#
get_word_list <- function(lang)
{
  loaded <- lang %in% loaded_languages()
  if (!loaded)
    load_languages(lang)
  .words[[lang]]
}




#### +---------- WORDS AND SENTENCES ---------------- ####


#' Random words
#' 
#' \code{words} generates a vector of random words taken from a reference list 
#' in the prompted language.
#' 
#' @param n Number of words to be generated (integer).
#' @param lang Language code (default \code{"en"}). See \code{\link{get_languages}} for available 
#'   language codes.
#' @return A vector of random words.
#' @export
#' @examples
#' words(10)  # 10 random words
#' 
words <- function(n=1, lang="en")
{ 
  if (! is.numeric(n))
    stop("'n' must be numeric", call. = FALSE)
  wordlist <- get_word_list(lang)
  sample(wordlist, n, replace=FALSE)
}



#' Random sentence with n words
#'
#' @param w Number of words in sentence.
#' @param maxchar Maximal number of characters per sentence. Note that whole 
#' words (not part of words) are excluded if the maximal number 
#' is exceeded.
#' @return a string with n words (if length is not constrained)
#' @export
#' @keywords internal
#' @examples  
#' sentence(5)            # random sentence with 5 words
#' sentence(5, max=20)   # random sentence cut off after 20 chars
#' 
sentence <- function(w, lang="en", maxchar=Inf)
{
  x <- words(w, lang=lang)
  chars <- sapply(x, nchar)
  keep <- x[cumsum(chars) < maxchar]
  paste(keep, collapse = " ")
}


#' Random sentences
#'
#' Create random sentences with a given or random number of words.
#' 
#' @param w Number of words per sentence. If more than one value
#'  lengths is randomly drawn from \code{w}.
#' @param s Number of sentences to generate (integer).
#' @inheritParams sentence
#' @return A vector of random sentences.
#' @export
#' @examples
#' # one sentence
#' sentences(5)            # random sentence with 5 words
#' sentences(5, max=30)    # cut off after 20 chars
#' sentences(3:5)          # between 3 and 5 words
#' 
#' # many sentences
#' sentences(3, 2)         # 2 sentences each 3 words long
#' sentences(3:5, 2)       # 2 sentences each between 3 and 5 words long
#' 
sentences <- function(w, s=1, lang="en", maxchar=Inf)
{
  len <- length(w)
  if (len == 1) {
    ww <- rep(w, s)
  } else {
    ww <- sample(w, s, replace = TRUE) 
  }
  sapply(ww, sentence, lang=lang, maxchar = maxchar)
}


