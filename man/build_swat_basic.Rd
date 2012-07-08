\name{build_swat_basic}
\alias{build_swat_basic}
\title{
Extracts a simple 9 HRU, 3 subbasin SWAT initialization
}
\description{
Extracts a simple 9 HRU, 3 subbasin SWAT initialization
}
\usage{
build_swat_basic(dirname, iyr, nbyr, wsarea, elev, declat, declon, hist_wx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dirname}{
Describe \code{dirname}
}
  \item{iyr}{
Describe \code{iyr} 
}
  \item{nbyr}{
Describe \code{nbyr} 
}
  \item{wsarea}{
Describe \code{wsarea} 
}
  \item{elev}{
Describe \code{elev} 
}
  \item{declat}{
Describe \code{declat}
}
  \item{declon}{
Describe \code{declon} 
}
  \item{hist_wx}{
Describe \code{hist_wx} 
}
}
\author{
Daniel Fuka
}
\examples{

\dontrun{
build_swat_basic(dirname="test", iyr="1979", nbyr=13, wsarea=40, elev=200, declat=45, declon=-72, hist_wx=hist_wx) 
}
}
