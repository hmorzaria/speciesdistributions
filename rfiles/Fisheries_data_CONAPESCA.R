#' Hem Nalini Morzaria Luna
#' March 2016
#' organize data from excel spreadsheets
#' catch data for 2014-106
#' http://datos.gob.mx/

# List of packages for session
.packages = c("gdata","data.table","stats","magrittr","dplyr")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

#' clean up the space

rm(list=ls())

# USER BLOCK: CHECK AND CHANGE OPTIONS HERE  
#_________________________________________________________________________

#this should match the path where your files directories are stored
#note the "/" go in the opposite direction than in Windows explorer
#' path
datafiles='E:/Archivos/1Archivos/Articulos/En preparacion/Climate_change_species_distribution/Data/CONAPESCA/Produccion_Pesquera_2006_2014'
workpath='E:/Archivos/1Archivos/Articulos/En preparacion/Climate_change_species_distribution/Data/CONAPESCA'


setwd(workpath)
nombres_validos = fread("nombres_cientificos_sinonimias.txt",header= TRUE, select = 1:3, encoding = "UTF-8") %>% 
  tbl_df %>% setNames(c("NOMBRE_COMUN","NOMBRE_VALIDO","SINONIMIA")) 

nombres_comunes = fread("nombres_cientificos_sinonimias.txt",header= TRUE, select = 1:3, encoding = "UTF-8") %>% 
  tbl_df %>% setNames(c("NOMBRE_COMUN","NOMBRE_VALIDO","SINONIMIA")) %>% .$NOMBRE_COMUN %>% unique

setwd(datafiles)
files <- list.files(pattern = "*.csv$")


conapesca.arrange <- function(x){
  fread(x, header= TRUE, encoding = "UTF-8") %>% tbl_df %>% 
    setNames(c("ENTIDAD_NO", "ENTIDAD", "OFICINA", "OFICINA_NO", "MES", "DESCRIPCION", "NOMBRE_COMUN", "PESO_VIVO", "PESO_DESEMBARCADO", "VALOR", "NOMBRE_PRINCIPAL", "FAMILIA", "NOMBRE_CIENTIFICO", "YR")) %>% 
  left_join(nombres_validos, by="NOMBRE_COMUN") 
  }

conapesca.data <- lapply(files, function (x) conapesca.arrange(x)) %>% 
rbindlist (fill=TRUE) %>% 
  tbl_df


#write.csv(Conapesca, file="Conapesca_produccion_2006_2014.csv")




#' pacific.states = 