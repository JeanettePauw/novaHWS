% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_measure_cat_hh.R
\name{make_measure_cat_hh}
\alias{make_measure_cat_hh}
\title{make_measure_cat_hh}
\usage{
make_measure_cat_hh(
  loopdf = NULL,
  loopname = NULL,
  namewash = c("", "waste_type_non_recycling_weight_")[1],
  idvar = "ID",
  indicatorOut = TRUE,
  bycat = c(NA_character_, "BYCAT_", "BYINTVCAT")[1],
  measure = c("MASS", "VOLUME")[1],
  group = c("HH"),
  category = c("", "RCLB", "NONRCLB")[1],
  pername = c("", "household", "capita")[1],
  period = "WEEK",
  unt = "kg",
  srcpth = "",
  digits = 2,
  dataset = NULL,
  ...
)
}
\arguments{
\item{loopdf}{}

\item{loopname}{}

\item{namewash}{}

\item{idvar}{Character.}

\item{indicatorOut}{}

\item{bycat}{}

\item{measure}{}

\item{group}{}

\item{category}{}

\item{pername}{}

\item{period}{}

\item{unt}{}

\item{srcpth}{}

\item{digits}{}

\item{dataset}{}

\item{...}{}
}
\value{
tibble
}
\description{
make_measure_cat_hh
}
\examples{
make_measure_cat_hh(loopdf = df_ana$loops[[1]][[1]], namewash = "waste_type_non_recycling_weight_", idvar = "ID")
}
