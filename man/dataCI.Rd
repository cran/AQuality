\name{dataCI}
\alias{dataCI}
\docType{data}
\title{
Data Sets~~
}
\description{
Data.frame with data for testing the CI() (Contamination Index) function. 
Column 1: sample identification code. 
Column 2: onwards: measurement of chemical components of water used to calculate CI, expressed in ppm.
}
\usage{data("dataCI")}
\format{
  A data frame with 6 observations on the following 9 variables.
  \describe{
    \item{\code{code}}{a character vector}
    \item{\code{phosphate}}{a numeric vector}
    \item{\code{nitrate}}{a numeric vector}
    \item{\code{nitrite}}{a numeric vector}
    \item{\code{ammonium}}{a numeric vector}
    \item{\code{dqo}}{a numeric vector}
    \item{\code{tkn}}{a numeric vector}
    \item{\code{organicmatter}}{a character vector}
    \item{\code{dbo}}{a numeric vector}
  }
}

\examples{
# Including data.frame: dataCI in workspace.
data("dataCI")
# Column names of data.frame: dataCI
names(dataCI) 
# Data set type of columns of data.frame: dataCI.
str(dataCI)
# Calculation of CI for the sample A1
#The following code should calculate the CI for the sample A1 included in dataCI, which 
# is not acceptable as drinking water and is possibly contaminable. 
CI("A1",dataCI)
#The following code should calculate the CI for the sample A2 included in dataCI, which 
#is acceptable as drinking water and is hardly contaminable. 
CI("A3",dataCI)

}
\keyword{datasets}
