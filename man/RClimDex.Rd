\name{RClimDex-package}
\alias{RClimDex-package}
\alias{RClimDex}
\docType{package}
\title{
RClimDex, Climate change indecies calculation for all 27 core indices defined by ETCCDI.
}
\description{
The RClimDex provides a friendly graphical user interface to compute all 27 core 
climate change indices defined by ETCCDI. It also conducts simple quality control 
on the input daily data. It has been developed and maintained by Xuebin Zhang and 
Yang Feng at Climate Research Division. The software was used first at the South 
Africa Workshop in Cape Town, South Africa, in June 2004 and has been used in 
other ET workshops.
}
\details{
\tabular{ll}{
Package: \tab RClimDex\cr
Type: \tab Package\cr
Version: \tab 1.8-2\cr
Date: \tab 2015-11-06\cr
Depends: \tab R (>= 2.15.2), climdex.pcic (>= 1.1-6), PCICt (>= 0.5-1)\cr
Imports: \tab tcltk (>= 2.15.2)\cr
License: \tab LGPL-2.1\cr
}

Index:
\preformatted{
rclimdex.start()          Start Graphical User Interface of RClimDex
}

}
\author{
Xuebin Zhang, Yang Feng, Rodney Chan, Yujun Ouyang

Maintainer: Yang Feng \email{yfeng@ec.gc.ca}
}
\references{
\url{http://etccdi.pacificclimate.org/list_27_indices.shtml}

Karl, T.R., N. Nicholls, and A. Ghazi, 1999:
CLIVAR/GCOS/WMO workshop on indices and indicators for
climate extremes: Workshop summary. Climatic Change, 42,
3-7.

Peterson, T.C., and Coauthors: Report on the Activities
of the Working Group on Climate Change Detection and
Related Rapporteurs 1998-2001. WMO, Rep. WCDMP-47, WMO-TD
1071, Geneve, Switzerland, 143pp.

Zhang, X., 2005: Avoiding inhomogeneity in
percentile-based indices of temperature extremes. Journal
of Climate 18.11 (2005):1641-.
}
\seealso{
\code{\link{rclimdex.start}}.
}
\keyword{package}
\examples{
#Start GUI
rclimdex.start()
}
