% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/newsapiArticles.R
\name{newsapiArticles}
\alias{newsapiArticles}
\title{newsapiArticles}
\usage{
newsapiArticles(key = Sys.getenv("NEWSAPI_KEY"), source, sortBy = "top")
}
\arguments{
\item{key}{(required) - Your API key. Alternatively you can provide this via the X-Api-Key HTTP header.}

\item{source}{(required) - The identifer for the news source or blog you want headlines from.
Use \code{\link{newsapiSources}} endpoint to locate this or use the sources index.}

\item{sortBy}{(optional) - Specify which type of list you want.
                            The possible options are "top", "latest" and "popular".
                            Note: not all options are available for all sources. Default: top.

                            "top" - Requests a list of the source's headlines sorted in the order they appear on its homepage.

                            "latest" - Requests a list of the source's headlines sorted in chronological order, newest first.

                            "popular" -    Requests a list of the source's current most popular or currently trending headlines.}
}
\description{
A function to retrieve articles from newsapi.org
}
\keyword{api}
\keyword{news,}
