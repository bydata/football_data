
library(tidyverse)
library(rvest)

url_vector <- c(base = "http://www.kicker.de/news/fussball/dfbpokal/spielrunde/dfb-pokal", 
                season = "2017-18",
                round = 1,
                suffix = "0/spieltag.html"
                )

# retrieve content
get_content <- function(url) {
  if (is.vector(url)) {
    url <- str_c(url, collapse = "/")
  }
  url %>%
      read_html()
}

page <- get_content(url_vector)
page

