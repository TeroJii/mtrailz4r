% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{mockdata}
\alias{mockdata}
\title{Mock data for testing}
\format{
A data frame with 51 rows and 6 columns:
\describe{
\item{event_date}{Event date as character string ("YYYYMMDD")}
\item{event_timestamp}{Timestamp as character string. Microseconds from
the Unix epoch}
\item{event_name}{Name of the event}
\item{event_params}{List of parameters for the event (nested data.frame)}
\item{user_pseudo_id}{Pseudo user id}
\item{user_properties}{List of user properties (nested data.frame)}
}
}
\usage{
mockdata
}
\description{
Mock data mimicking the properties of actual data. This data is used for
testing purposes, and does not contain all the columns which are present in
the actual data.
}
\keyword{datasets}
