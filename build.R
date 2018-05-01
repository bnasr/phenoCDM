if(!require("utils")) {install.packages("devtools"); library(devtools)}
if(!require("devtools")) {install.packages("devtools"); library(devtools)}
if(!require("roxygen2")) {devtools::install_github("klutometis/roxygen"); library(roxygen2)}

PACKAGE.NAME = 'phenoCDM'
# create('~/Projects/phenoCDM/')
# use_vignette("getting-started")
document(pkg = '.')
install(pkg = '.')

system('rm -r toCRAN')
system('mkdir toCRAN')
system(paste0('rm ', PACKAGE.NAME, '*.tar.gz'), ignore.stderr = TRUE)
system('cp -r R man vignettes DESCRIPTION NAMESPACE LICENSE inst toCRAN')
f <- build('toCRAN')

# devtools::revdep_check('toCRAN')
devtools::check('toCRAN')
system(command = paste0('R CMD check ', basename(f)))
system(command = paste0('R CMD check --as-cran ', basename(f)))

# system('rm -r toCRAN')
# devtools::submit_cran('toCRAN')
