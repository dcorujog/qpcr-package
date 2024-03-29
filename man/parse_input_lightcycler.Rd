% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_input_lightcycler.R
\name{parse_input_lightcycler}
\alias{parse_input_lightcycler}
\title{parse_input_lightcycler}
\usage{
parse_input_lightcycler(infile)
}
\arguments{
\item{infile}{character, path to input file}
}
\value{
A data frame with the fluorescence signal values for every cycle (rows)
for each sample (columns).
}
\description{
Parses an input xml file exported from the lightcycler software with the qpcr
curve data.
}
