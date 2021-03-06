% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heatmap_binary.R
\name{bin_binary}
\alias{bin_binary}
\title{Bin data in a binary fashion.}
\usage{
bin_binary(data, by, threshold, levels = c("Acceptable", "Not Acceptable"),
  reverse = FALSE)
}
\arguments{
\item{data}{The dataframe or related data structure containing the data.}

\item{by}{The column that we will evaluate against the threshold. Pass as
string.}

\item{threshold}{The value we evaluate our "by" column with.}

\item{levels}{The values that will be used to fill the "bin" column.}

\item{reverse}{logical; Should values below or equal to the threshold be considered
good?}
}
\value{
The same dataframe, but with an additional column indicating the
  bins our data falls into.
}
\description{
Adds a "bin" column to a dataframe or related data strucutre which indicates
whether or not another column meets a given threshold.
}
