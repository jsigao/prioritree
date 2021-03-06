% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tex_helpr.R
\name{tex_data}
\alias{tex_data}
\title{Generate description (in the selected source format) for the data used in the study}
\usage{
tex_data(taxa_num = 1, states_num = 1, format = "HTML")
}
\arguments{
\item{taxa_num}{Number of taxa in the study data}

\item{states_num}{Number of discrete states of the study trait.}
}
\value{
a paragraph (in the selected source format) describing the data used in the study
}
\description{
Generate description (in the selected source format) for the data used in the study
}
\examples{
tex_data(1000, 10, "HTML")
}
