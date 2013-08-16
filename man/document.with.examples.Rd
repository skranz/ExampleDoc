\name{document.with.examples}
\alias{document.with.examples}
\title{Build package documentation with examples}
\usage{
  document.with.examples(package,
    package.path = path.package(package), clean = TRUE,
    roclets = c("namespace", "rd"), ...)
}
\description{
  Build package documentation with examples
}

\examples{\dontrun{
  library(restorepoint)
  
  document.with.examples("ExampleDoc", "C:/libraries/ExampleDoc/ExampleDoc")  
  document.with.examples("RMaxima", "C:/libraries/RMaxima/RMaxima")  
  
}}
