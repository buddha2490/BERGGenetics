\name{findSNP}
\alias{findSNP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find a vector of SNP names in the CPS2 genetics studies
}
\description{
Input a vector of RS#s, the function will search the genetics data and return their locations (if available)
}
\usage{
findSNP(findthesesnps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{findthesesnps}{
Character vector of RS#s
}
}
\details{
This function will answer the question: "Have we genotyped this SNP in CPS2"
}
\value{
A data frame with the vector of SNPs and their locations
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Brian Carter
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
findSNP("rs9442372")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
