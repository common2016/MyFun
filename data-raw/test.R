## code to prepare `test` dataset goes here
devtools::load_all()
library(vars)
library(tidyverse)
data(Canada, package = 'vars')
ar <- vars::VAR(Canada, p = 2, type = "none") %>% vars::Bcoef()
ar2ma(ar)

# usethis::use_data("test")
