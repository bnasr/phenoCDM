if(!require("utils")) {install.packages("devtools"); library(devtools)}
if(!require("devtools")) {install.packages("devtools"); library(devtools)}
if(!require("roxygen2")) {devtools::install_github("klutometis/roxygen"); library(roxygen2)}

PACKAGE.NAME = 'phenoCDM'
# create('~/Projects/phenoCDM/')
document(pkg = '.')
install(pkg = '.')

system('mkdir toCRAN')
system(paste0('rm ', PACKAGE.NAME, '*.tar.gz'))
system('cp -r R man DESCRIPTION NAMESPACE LICENSE inst toCRAN')
f <- build('toCRAN')
system(command = paste0('R CMD check --as-cran ', basename(f)))

devtools::check('toCRAN')
devtools::revdep_check('toCRAN')

# system('rm -r toCRAN')
# devtools::submit_cran('toCRAN')
