\name{ICHS}
\alias{ICHS}
\title{
Ionic Charge Summation
}
\description{
Plots ionic charge summation as a function of conductivity.
}
\usage{
ICHS(sample, data, conflevel = 0.95, pchdata = 19, coldata = "green", cexdata = 0.5,
 pchsample = 19, colsample = "red", cexsample = 3, xaxis = "CONDUCTIVITY", 
yaxis = "IONIC CHARGE SUMMATION", title = paste("Sample ", as.character(sample)),
 linetyprediction = 2, linewidthprediction = 1, linecolorprediction = 5)
}
\arguments{
  \item{sample}{
Code of the sample whose quality you want to know.
}
  \item{data}{
Data.frame containing code of the database samples, conductivity, measurements 
of ionic water components.
}
  \item{conflevel}{
Significance level used in the predict function.
}
  \item{pchdata}{
Symbol used to graph all the data in the data.frame.
}
  \item{coldata}{
Color of the symbols of all the data in the data.frame.
}
  \item{cexdata}{
Symbol size of all data in the data frame.
}
  \item{pchsample}{
Symbol chosen to represent the point whose measurement quality is to be represented.
}
  \item{colsample}{
Color chosen to represent the point whose measurement quality is to be represented.
}
  \item{cexsample}{
Size of the symbol chosen to represent the point whose measurement quality is to be 
represented.
}
  \item{xaxis}{
X axis label.
}
  \item{yaxis}{
Y axis label.
}
  \item{title}{
Title of the graph including the code of the chosen sample.
}
  \item{linetyprediction}{
Linear model prediction line type.
}
  \item{linewidthprediction}{
Linear model prediction line thickness.
}
  \item{linecolorprediction}{
Linear model prediction line color.
}
}

\details{
The ICHS() function performs a linear model using column 2 (conductivity) as the 
independent variable and the other components of water as dependent variables (columns 
3 onwards). Based on the linear model, a data prediction interval is obtained with a 
certain confidence level (conflevel). Then, ICHS() graphs the values of the entire 
database and finally graphs as a point with different color, the sample whose measurement quality you want to observe.

}
\value{
The ICHS() function returns a graph of the sum of ionic chemical components as a function of the measurement of conductivity for each sample. It contains the confidence interval indicated in a dotted line, and the sample under observation. If the point that 
represents the sample is within the region delimited by the lines of the confidence 
interval, it is presumed that there were no serious measurement errors of the components 
analyzed.
}
\author{
Maela Lupo, Andrea Porpatto, Alfredo Rigalli
}

% \keyword{methods}
% \keyword{misc}
