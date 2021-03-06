% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contextractr-class.R
\name{Contextractr$add_json}
\alias{Contextractr$add_json}
\title{Add a mapping}
\arguments{
\item{path}{the path to the mapping file}
}
\description{
Given a path to a mapping file, add that file's contents to the
mapping
}
\section{Mapping Rules}{

For a given entry, corresponding to a a row in a tibble, or a dict/named
list in a list-of-dicts, the following must be specified:\preformatted{* title: The output grouping for the item (string)
* keywords: (list of strings) a list of keywords by which to identify
  this group
* approx.match: (list of decimal numbers) (optional - sets to 0 if not set)
}
}

\seealso{
Other Contextractr: \code{\link{Contextractr$add_serial}},
  \code{\link{Contextractr$add_yaml}},
  \code{\link{Contextractr$locate_keywords}},
  \code{\link{Contextractr$new}},
  \code{\link{Contextractr}}
}
\concept{Contextractr}
