\name{add.example.to.Rd}
\alias{add.example.to.Rd}
\title{Adds examples to an Rd file}
\usage{
  add.example.to.Rd(ex, package,
    package.path = path.package(package),
    fun.name = ex$fun.name,
    example.source = ex$example.source,
    remove.top.lines = 1, remove.bottom.lines = 1,
    all.dont.run = TRUE)
}
\arguments{
  \item{ex}{a list with elements fun.name and
  example.source}
}
\description{
  Not very stable yet. Assume that the Rd file has no
  examples section yet. Simple add examples at the end
}

