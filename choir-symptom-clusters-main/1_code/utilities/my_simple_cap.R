# capitalization function for pretty titles
simpleCap <- function(x) {
  x <- tolower(x)
  s <- strsplit(x, "_")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}