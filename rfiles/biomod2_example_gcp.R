#Ejemplo mapas distribucion de especies
#Hem Nalini Morzaria Luna
#Marzo 2015
#hmorzarialuna@gmail.com

install.packages("biomod2")
library(biomod2) # paquete para SDMs

#para accesar manual 
#browseVignettes(package='biomod2') 

#importar datos distribucion de especies
path.datos.distribucion = "C:/Users/Gaby/Desktop/SDM_Totoaba"
setwd(path.datos.distribucion)

DataSpecies = read.csv("Tmacdonaldi.csv", header = T)
head(DataSpecies)

DataSpecies$Totoaba = 1

#Nombre de la especie
myRespName <- 'Totoaba'

# datos de presencia ausencia para la especie
myResp <- as.numeric(DataSpecies[,myRespName])
# coordenadas XY de la especie
myRespXY <- DataSpecies[,c("Longitude","Latitude")]
#datos ambientales
myExpl = stack("Temp_media.asc", "Nitratos_media.asc", "Fosfato_media.asc", "batimetria_tierra.asc", "Sal_media.asc", "Kd_media.asc") 

#transformar al formato correcto

myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName,
                                     PA.strategy = random)

### Modelaje

myBiomodOption <- BIOMOD_ModelingOptions()

myBiomodModelOut <- BIOMOD_Modeling(
  myBiomodData,
  models = c('SRE','CTA','RF','MARS','FDA'),
  models.options = myBiomodOption,
  NbRunEval=3,
  DataSplit=80,
  Prevalence=0.5,
  VarImport=3,
  models.eval.meth = c('TSS','ROC'),
  SaveObj = TRUE,
  rescal.all.models = TRUE,
  do.full.models = FALSE,
  modeling.id = paste(myRespName,"FirstModeling",sep=""))

#Unir los distintos modelos en un ensamblaje
myBiomodEM <- BIOMOD_EnsembleModeling(
  modeling.output = myBiomodModelOut,
  chosen.models = 'all',
  em.by='all',
  eval.metric = c('TSS'),
  eval.metric.quality.threshold = c(0.7),
  prob.mean = T,
  prob.cv = T,
  prob.ci = T,
  prob.ci.alpha = 0.05,
  prob.median = T,
  committee.averaging = T,
  prob.mean.weight = T,
  prob.mean.weight.decay = 'proportional' )

#Hacer proyecciones bajo condicciones actuales
myBiomodProj <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = myExpl,
  proj.name = 'current',
  selected.models = 'all',
  binary.meth = 'TSS',
  compress = 'xz',
  clamping.mask = F,
  output.format = '.grd')

myBiomodEF <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEM,
  projection.output = myBiomodProj)

plot(myBiomodEF)


plot(myBiomodProj,  str.grep = 'MARS')

