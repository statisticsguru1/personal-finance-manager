# install others

install.packages(c("plumber","mongolite","httr","jsonlite","jose","sodium","tidyverse","future","promises","rlang","remotes"))

# install finman
remotes::install_github("statisticsguru1/personal-finance-manager@v1.2.0", 
                        subdir = "finman", 
                        upgrade = "never")