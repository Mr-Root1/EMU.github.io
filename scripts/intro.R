# from CRAN
install.packages("blogdown")
install.packages("htmltools")
library(htmltools)
install.packages("bslib")
library(bslib)
# install.packages("bslib", dependencies = TRUE)
# or the development version from GitHub
devtools::install_github("rstudio/blogdown")

# remove.packages(all = TRUE)
# q()
install.packages(c("rmarkdown", "tufte", "natbib", "htmltools"))
unloadNamespace("htmltools")
install.packages("xfun")


update.packages(ask = FALSE, checkBuilt = TRUE)

install.packages("xfun")
install.packages("rlang")
install.packages("rmarkdown")
packageVersion("rmarkdown")
