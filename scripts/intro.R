# from CRAN
install.packages("blogdown")
install.packages("htmltools")
library(htmltools)
install.packages("bslib")
library(bslib)
install.packages("bslib", dependencies = TRUE)
# or the development version from GitHub
devtools::install_github("rstudio/blogdown")

remove.packages(all = TRUE)
q()
