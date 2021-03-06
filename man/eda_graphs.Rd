% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eda_graphs.R
\name{eda_graphs}
\alias{eda_graphs}
\title{Automates Exploratory Data Analysis Process for a Data}
\usage{
eda_graphs(data, var = 0, folder = getwd())
}
\arguments{
\item{data}{Data frame object}

\item{var}{Index of variables for which EDA plots are required. Default is All.}

\item{folder}{Export location of all the plots. Defaults to current working directory.}
}
\description{
The process of exploration might become especially when we are dealing with data sets that contains a large number of variables. This package helps to reduce the work and generate plots for numerical and categorical variables.
}
\examples{
eda_graphs(Boston)

}
