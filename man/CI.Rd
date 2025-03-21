\name{CI}
\alias{CI}
\title{
Contamination Index
}
\description{
Calculate an index that allows to estimate the possibility of microbiological contamination of a water sample after being obtaines.
}

\usage{
CI(sample,data)
}
\arguments{
  \item{sample}{
Code of the sample whose quality you want to know.
}
  \item{data}{
Data.frame containing code of the database samples,and de concentration of the following chemical components: phosphate, nitrate, nitrite, tkn, ammonium, chemical demand of oxygen (dqo), biological demand of oxygen (dbo) and organic matter.
}
}

\details{
The CI() function performs the calculation of a score whose value allows to estimate the possibility of microbiological contamination of a water sample after being obtained. 
}
\value{
The CI() function returns a number (score). If score>=0 and score<= 2, the sample is not contaminable. If score>2 and score<= 4, the sample is hardly contaminable. If score>4 and score <= 6), the sample is possibly contaminable. If score>6 and score<= 8, the sample is easily contaminable. 
}

\author{
Maela Lupo, Andrea Porpatto, Rosa Marzullo, Alfredo Rigalli
}

% \keyword{methods}
% \keyword{misc}