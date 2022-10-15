

Printsonnet<-function(input_vector){
  library(tidyverse)
  sonnets <- read_csv("https://blades.byu.edu/hon290/queneauEng.txt")
  mylines <- c(input_vector)
  posonnet <- NULL
  for (i in 1:14){
  nextline <- sonnets %>%
    filter (sonnet == mylines[i] & line == i)
  posonnet <- bind_rows(posonnet, nextline)
  }
  posonnet%>%
    select(text)%>%
    print()
}
v <- c(1,2,3,4,3,7,1,1,2,2,3,10,5,2)
Printsonnet(v)
