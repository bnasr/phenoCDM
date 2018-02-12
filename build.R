if(!require("utils")) {install.packages("devtools"); library(devtools)}
if(!require("devtools")) {install.packages("devtools"); library(devtools)}
if(!require("roxygen2")) {devtools::install_github("klutometis/roxygen"); library(roxygen2)}

PACKAGE.NAME = 'phenoCDM'
# create('~/Projects/phenoCDM/')
document(pkg = '.')
install(pkg = '.')
f <- build(pkg = '.')

file.copy(from = f, to = paste0(PACKAGE.NAME, '.tar.gz'), overwrite = T)
file.remove(f)
system(command = paste0('R CMD check --as-cran ', PACKAGE.NAME, '.tar.gz'))
