# install others

install.packages(c("plumber","mongolite","httr","jsonlite","jose","sodium","tidyverse","future","promises","rlang","remotes"))

# install finman
remotes::install_github("statisticsguru1/personal-finance-manager", 
                        subdir = "finman", 
                        upgrade = "never")