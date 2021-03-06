% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/encode_list_columns.R
\name{encodeListColumns}
\alias{encodeListColumns}
\title{Encode list-columns in a dataframe as character using base64 encoding}
\usage{
encodeListColumns(df)
}
\arguments{
\item{df}{dataframe}
}
\value{
A dataframe where list-columns are encoded using base64 and those
  column names are appended by "__encoded"
}
\description{
Encoded columns are detected, encoded with base64 and column
  names are appended with "__encoded" so that the decoder knows it. Other
  columns are not transformed
}
\details{
This is helpful in writing dataframes with list-columns to disk as
  delimited formats and read them back
}
\examples{
library("magrittr")
trialDF <- tibble::tibble(x = 1:10
                          , y = c(list(list(iris)), as.list(2:10))
                          )
trialDF
trialDF \%>\% encodeListColumns()
trialDF \%>\% encodeListColumns() \%>\% decodeListColumns()
}
