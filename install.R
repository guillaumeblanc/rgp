# Setup and install all project dependencies

install.packages('RCurl')
install.packages('rjson')
install.packages('RUnit')
install.packages('rvest')
install.packages('htmltidy')
install.packages('stringr')

# devtools::install_github("rstudio/tensorflow")
# devtools::install_github("rstudio/tfestimators")
# devtools::install_github("rstudio/keras")
# library(tensorflow)
# library(tfestimators)
# install_tensorflow()

devtools::install_github("rstudio/keras")
library(keras)
install_keras()
