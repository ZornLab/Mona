% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_data.R
\name{mona_annotate}
\alias{mona_annotate}
\title{Mona annotation}
\usage{
mona_annotate(mona_ref, anno, exp, species)
}
\arguments{
\item{mona_ref}{A Mona reference object}

\item{anno}{Which annotation within the reference to use}

\item{exp}{The current Mona dataset counts}

\item{species}{The current Mona dataset species}
}
\value{
A list of labels
}
\description{
Function for transferring cell labels from a reference to the current Mona dataset
Users must ensure the reference and query share the same type and normalization
}
