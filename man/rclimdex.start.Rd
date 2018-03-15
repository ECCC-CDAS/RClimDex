\name{rclimdex.start}
\alias{rclimdex.start}
\title{
Start Graphical User Interface of RClimDex
}
\description{
Start graphical user interface to compute all 27 core climate change indices defined
by ETCCDI. It also conducts simple quality control on the input daily data.
}
\usage{
rclimdex.start()
}
\details{
RClimDex has 2 main functions. The first is input data quality control and the other
is climate change indices calulation. It is highly recommended to performed quality
control or used post quality control dataset before processing climate change indices
calulation. RClimDex supports proccessing multiple input data in a queue.

The many outputs of RClimDex are stored as follow:
\preformatted{
"user_defined_output_path"/               Post Quality Control Dataset
"user_defined_output_path"/log/           Logs of Quality Control
"user_defined_output_path"/indices/       Indices Calulation Results
"user_defined_output_path"/plot/          Plots of Indices Calulation Result
"user_defined_output_path"/trend/         Trend Analysis of Indices Calulation Results
}
}
\note{
Climate change indices calualtion are processed by the climdex.pcic package. RClimDex
supports ".txt" and ".csv" data types for input. Input daily data must be 6 column in the
order of "year month day prcp tmax tmin" with no header or row names. All non-numeric
values are considered as undefined values.
}
\seealso{
\code{\link{rclimdex.start}}.
}
\keyword{methods}
\examples{
#Start GUI
rclimdex.start()
}
