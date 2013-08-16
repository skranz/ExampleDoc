\name{get.examples}
\alias{get.examples}
\title{Extract all example functions from a set of files}
\usage{
  get.examples(package.path = NULL, files = NULL,
    path = NULL)
}
\arguments{
  \item{files}{a list of R files}

  \item{path}{a path from which all R files are taken}

  \item{package}{take all R files from the source directory
  in the package}
}
\description{
  Extract all example functions from a set of files
}

\examples{\dontrun{
  path = "C:/libraries/ExampleDoc/ExampleDoc/R"
  get.examples(path=path)
  
}}
