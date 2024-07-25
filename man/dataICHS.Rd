\name{dataICHS}
\alias{dataICHS}
\docType{data}
\title{
Data Sets
}
\description{
Data.frame with data for testing the ICHS() (Ionic Charge Summation) function. 
Column 1: sample identification code. 
Column 2: measurement of water conductivity. 
Column3 onwards: measurement of ionic chemical components of water expressed in milliequivalent per litre.
}
\usage{data("dataICHS")}
\format{
  A data frame with 411 observations on the following 14 variables.
  \describe{
    \item{\code{codigo}}{a character vector}
    \item{\code{conductividad}}{a numeric vector}
    \item{\code{cargacloruro}}{a numeric vector}
    \item{\code{cargacarbonato}}{a numeric vector}
    \item{\code{cargabicarbonato}}{a numeric vector}
    \item{\code{cargafosfato}}{a numeric vector}
    \item{\code{carganitrato}}{a numeric vector}
    \item{\code{carganitrito}}{a numeric vector}
    \item{\code{cargafloruro}}{a numeric vector}
    \item{\code{cargaarcenico}}{a numeric vector}
    \item{\code{cargaamonio}}{a numeric vector}
    \item{\code{cargasulfato}}{a numeric vector}
    \item{\code{cargasodio}}{a numeric vector}
    \item{\code{cargacalcio}}{a numeric vector}
  }
}

\examples{
# Including data.frame: data in workspace.
data("dataICHS")
# Column names of data.frame: data
names(dataICHS) 
# Data set type of columns of data.frame: data.
str(dataICHS)
# Visualization of sample A45
#The following code should display a graphic with all samples in green dots and sample
# A45 as red big dot
ICHS("A45",dataICHS)
}
\keyword{datasets}

