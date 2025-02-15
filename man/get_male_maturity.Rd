% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_male_maturity.R
\name{get_male_maturity}
\alias{get_male_maturity}
\title{Pull proportion morphometrically mature Chionoecetes spp. males by size}
\usage{
get_male_maturity(
  species = NULL,
  region = c("EBS", "NBS")[1],
  district = NULL,
  channel = NULL
)
}
\arguments{
\item{species}{Character string. One of \code{c("TANNER", "SNOW")}.}

\item{region}{Character string describing the region of interest. One of
\verb{c("EBS", "NBS"}). Defaults to \code{"EBS"} for Eastern Bering Sea.}

\item{district}{Character string. One or many of \code{c("ALL", "E166", "W166")}. Defaults to
\code{"ALL"}; \code{"E166"} and \code{"W166"} are used for Tanner Crab only.}

\item{channel}{Character string or Oracle connection. Defaults to an API connection,
(\code{channel = "API"}), allowing for public data access. To use an
Oracle database connection, set \code{channel} to an object created
via \code{crabpack::get_connected()} or \code{DBI::dbConnect()}. Local AFSC
Kodiak users can also set \code{channel = "KOD"} to access data on the
network drives (requires VPN connection). This option will pull all
available years and districts for the given species and region.}
}
\value{
A named list containing the proportion of male Chionoecetes spp. crab
that are morphometrically mature in a given 10mm size bin, and yearly
model parameter estimates for 50\% probability of maturity at size for
the species, region, and district of interest.
}
\description{
A time series of model estimated proportion of morphometrically mature
male Tanner Crab and Snow Crab by 10 millimeter size bin for a given
region or district, based on chela height measurements. Yearly model
parameter estimates of 50\% probability of maturity at size are also
provided.
}
