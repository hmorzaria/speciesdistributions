install.packages("biomod2")
install.packages("dismo")
install.packages("XML")

library (biomod2)
library (dismo)
library(XML)

ruber<-gbif('Batrachoseps', '*' , geo=T, down=F)
