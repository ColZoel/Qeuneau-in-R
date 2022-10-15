printSonnet <-function(whichsonnet){
library(tidyverse)
sonnets <- read_csv("https://blades.byu.edu/hon290/queneauEng.txt")
sonnets %>%
  filter(sonnet==whichsonnet) %>%
  select(text) %>%
  print()
}

printSonnet(4)

## making sonnets from arbitrarily chosen lines ##
mylines <- c(4,3,6,5,1,9,3,4,5,6,7,2,3,4)
for(i in 1:10){
sonnets %>%
  filter(sonnet ==mylines[i], line ==i)%>%
  print()
}

posonnet <- NULL
for (i in 1:14){
  nextline <- sonnets %>%
    filter (sonnet == mylines[i] & line == i)
  posonnet <- bind_rows(posonnet, nextline)
}
posonnet%>%
  select(text)%>%
  print()


##randomly chosen line combinations##
rando <- function(){
  vect<-NULL
  vect<-sample(1:10, 14, replace = T)
  posonnet <- NULL
  for (i in 1:14){
    nextline <- sonnets %>%
      filter (sonnet == vect[i] & line == i)
    posonnet <- bind_rows(posonnet, nextline)
  }
  posonnet%>%
    print()
}
rando()

