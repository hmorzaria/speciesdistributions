#Ejemplo mapas distribucion de especies
#Hem Nalini Morzaria Luna
#Marzo 2015
#hmorzarialuna@gmail.com

install.packages("biomod2")
library(biomod2) # paquete para SDMs

#importar datos distribucion de especies
path.datos.distribucion = ""
setwd(path.datos.distribucion)

DataSpecies = read.csv(, header = T)
head(DataSpecies)

#Nombre de la especie
myRespName <- 'Totoaba'

# datos de presencia ausencia para la especie
myResp <- as.numeric(DataSpecies[,myRespName])
# coordenadas XY de la especie
myRespXY <- DataSpecies[,c("X_WGS84","Y_WGS84")]
#datos ambientales
myExpl = stack()


#transformar al formato correcto

myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName,
                                     PA.strategy = random)

myBiomodData
