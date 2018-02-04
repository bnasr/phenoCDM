if(!require("devtools")) {install.packages("devtools"); library(devtools)}
if(!require("roxygen2")) {devtools::install_github("klutometis/roxygen"); library(roxygen2)}

# create('~/Projects/phenoCDM/')
document('~/Projects/phenoCDM/')
install('~/Projects/phenoCDM/')
