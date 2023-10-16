# rusfmd-mortality
An application that allows you to explore mortality patterns in Russian regions. For the calculations I used data from the Russian Fertility and Mortality database[1]. To run the app execute the R code below on your machine.
``` r
# install packages (if necessary)
install.packages(c("shiny","readr","readxl","dplyr","tidyr","ggplot2","thematic","bslib","scales"))

# load shiny
library(shiny)

# run the app from the GitHub repository
runGitHub(repo = "rusfmd-mortality", username = "ilyaklimkin")
```
[1] Russian Fertility and Mortality Database. Center for Demographic Research, Moscow (Russia). Available at http://demogr.nes.ru/index.php/ru/demogr_indicat/data (data downloaded on [20-09-2023]).
