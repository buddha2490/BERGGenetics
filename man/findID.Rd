\name{findID}
\alias{findID}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Searches for a vector of IDs in our genetics studies.
}
\description{
Function will answer the question: "Have these samples been genotyped?"
}
\usage{
findID(idv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{idv}{
Character vector of 14-digit CPS2 IDs
}
}
\details{
A common question is to ask whether some set of IDs have been genotyped and in which project were they genotyped.  This function will require a vector of IDs and search the metadata files for the locations of these IDs.
}
\value{
A data frame of the input IDs and their locations.
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
findID("12065001008051")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
