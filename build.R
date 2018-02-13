if(!require("utils")) {install.packages("devtools"); library(devtools)}
if(!require("devtools")) {install.packages("devtools"); library(devtools)}
if(!require("roxygen2")) {devtools::install_github("klutometis/roxygen"); library(roxygen2)}

PACKAGE.NAME = 'phenoCDM'
# create('~/Projects/phenoCDM/')
document(pkg = '.')
install(pkg = '.')

system('mkdir toCRAN')
system('cp -r R man DESCRIPTION NAMESPACE toCRAN')
f <- build('toCRAN')
system('rm -r toCRAN')
system(command = paste0('R CMD check --as-cran ', basename(f)))

# devtools::revdep_check()
# devtools::submit_cran()
