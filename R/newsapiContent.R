#' newsapiArticles
#'
#' A function to retrieve articles from newsapi.org
#'
#' @param url (required) the url of the page from which to get the content
#' @param tag (required) the html tag from which to get the content
#'                       e.g. "p" passed to \code{\link[xml2]{read_html}}.
#'
#' @keywords news, api
#'
#' @import tidytext
#' @import rvest
#' @import dplyr
#' @import xml2
#' @export
newsapiContent <- function(url, tag) {
  page <- xml2::read_html(url)
  text <- page %>%
    rvest::html_nodes(tag) %>%
    rvest::html_text() %>%
    as.data.frame
  names(text) <- "text"
  text$text <- as.character(text$text)
  token <- tidytext::unnest_tokens(text, word, text) %>%
    dplyr::anti_join(tidytext::stop_words, by = "word")
  return(token$word)
}
