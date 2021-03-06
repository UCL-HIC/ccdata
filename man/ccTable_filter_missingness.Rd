% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.missingness.R
\name{ccTable_filter_missingness}
\alias{ccTable_filter_missingness}
\title{Data missing filter}
\arguments{
\item{recount}{logical value. Recount the missingness if TRUE.}
}
\description{
Deal with data when insufficient data points are supported. There are 
two key items to be set in the YAML configuration file. 
1) labels -- time interval. 2) accept_2d -- the accept present ratio. 
So if we set the labels is 24, and accept_2d is 70. It means we accept 
all the missing rate that is lower than 30% every 24 data points.
}

