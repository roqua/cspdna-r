#! /usr/bin/env Rscript

message(R.version.string)
options(repos=structure(c(CRAN="https://cloud.r-project.org")))
install.packages('packrat')
packrat::restore()
