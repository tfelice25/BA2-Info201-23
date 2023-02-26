library(tidyverse)
bikes <- read_delim("SeoulBikeData3.csv")
## How does solar radiation affect bike rentals on a given day? 
bikes %>% 
  distinct(date)

