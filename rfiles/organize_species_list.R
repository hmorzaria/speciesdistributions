#' Hem Nalini Morzaria Luna hmorzarialuna@gmail.com
#' January 2016
#' Script to read in list of species from CONAPESCA catch records
#' Remove authorities and years

if(!require(data.table)){install.packages("data.table"); library(data.table)}
if(!require(XML)){install.packages("XML"); library(XML)}
if(!require(magrittr)){install.packages("magrittr"); library(magrittr)}
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(readxl)){install.packages("readxl"); library(readxl)}
if(!require(stringr)){install.packages("stringr"); library(stringr)}

rm(list=ls())
datapath = "E:/Archivos/1Archivos/Articulos/En preparacion/Climate_change_species_distribution/Data/CONAPESCA"

setwd(datapath)
species.list = read_excel("Busqueda_terminada_especies_comerciales.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "",skip = 0) 
#species.conapesca = read_excel("Busqueda_terminada_especies_comerciales.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "",skip = 0) 

species.clean = species.list%>%.$Nombre_valido_WORMS%>%
gsub("\\(.*","",.)%>%
gsub("(.*?)W[.].*", "\\1", .)%>%
gsub("(.*?)G[.].*", "\\1", .)%>%
gsub("(.*?)A[.].*", "\\1", .)%>%
gsub("(.*?)B[.].*", "\\1", .)%>%
gsub("(.*?)Baird &.*", "\\1", .)%>%
gsub("(.*?)Reeve.*", "\\1", .)%>%
gsub("(.*?)Linnaeus.*", "\\1", .)%>%
gsub("(.*?)Boulenger.*", "\\1", .)%>%
gsub("(.*?)Meek.*", "\\1", .)%>%
gsub("(.*?)Woolman.*", "\\1", .)%>%
gsub("(.*?)Philippi.*", "\\1", .)%>%
gsub("(.*?)Wood.*", "\\1", .)%>%
gsub("(.*?)Leach.*", "\\1", .)%>%
gsub("(.*?)Swainson.*", "\\1", .)%>%
gsub("(.*?)Bartsch.*", "\\1", .)%>%
gsub("(.*?)Dall.*", "\\1", .)%>%
gsub("(.*?)Carpenter.*", "\\1", .)%>%
gsub("(.*?)Conrad.*", "\\1", .)%>%
gsub("(.*?)Baldwin.*", "\\1", .)%>%
gsub("(.*?)Girard.*", "\\1", .)%>%
gsub("(.*?)Bonnaterre.*", "\\1", .)%>%
gsub("(.*?)Hildebrand.*", "\\1", .)%>%
gsub("(.*?)Jordan.*", "\\1", .)%>%
gsub("(.*?)Kishinouye.*", "\\1", .)%>%
gsub("(.*?)Breder.*", "\\1", .)%>%
gsub("(.*?)Gill.*", "\\1", .)%>%
gsub("(.*?)Berry.*", "\\1", .)%>%
gsub("(.*?)Boone.*", "\\1", .)%>%
gsub("(.*?)Lockington.*", "\\1", .)%>%
gsub("(.*?)Faxon.*", "\\1", .)%>%
gsub("(.*?)Burkenroad.*", "\\1", .)%>%
gsub("(.*?)Bouvier.*", "\\1", .)%>%
gsub("(.*?)Chace.*", "\\1", .)%>%
gsub("(.*?)Sowerby.*", "\\1", .)%>%
gsub("(.*?)Boderip.*", "\\1", .)%>%
gsub("(.*?)Garman.*", "\\1", .)%>%
gsub("(.*?)Gilbert.*", "\\1", .)%>%
gsub("(.*?)Ayres.*", "\\1", .)%>%
gsub("(.*?)Valenciennes.*", "\\1", .)%>%
gsub("(.*?)Steindachner.*", "\\1", .)%>%
gsub("(.*?)Stimpson.*", "\\1", .)%>%
gsub("(.*?)Carvacho.*", "\\1", .)%>%
gsub("(.*?)Stimpson.*", "\\1", .)%>%
gsub("(.*?)Milne.*", "\\1", .)%>%
gsub("(.*?)Poey.*", "\\1", .)%>%
gsub("(.*?)Günther.*", "\\1", .)%>%
gsub("(.*?)Ordoway.*", "\\1", .)%>%
gsub("(.*?)Quoy.*", "\\1", .)%>%
gsub("(.*?)Streets.*", "\\1", .)%>%
gsub("(.*?)Holthuis.*", "\\1", .)%>%
gsub("(.*?)Squires.*", "\\1", .)%>%
gsub("(.*?)Bate.*", "\\1", .)%>%
gsub("(.*?)Hubbs.*", "\\1", .)%>%
gsub("(.*?)Houttuyn.*", "\\1", .)%>%
gsub("(.*?)Rosenblatt.*", "\\1", .)%>%
gsub("(.*?)Rafinesque.*", "\\1", .)%>%
gsub("(.*?)Leach.*", "\\1", .)%>%
gsub("(.*?)Richardson.*", "\\1", .)%>%
gsub("(.*?)Verrill.*", "\\1", .)%>%
gsub("(.*?)Dooley.*", "\\1", .)%>%
gsub("(.*?)Agassiz.*", "\\1", .)%>%
gsub("(.*?)Zahur.*", "\\1", .)%>%
gsub("(.*?)Bleeker.*", "\\1", .)%>%
gsub("(.*?)Lay.*", "\\1", .)%>%
gsub("(.*?)Thominot.*", "\\1", .)%>%
gsub("(.*?)Regan.*", "\\1", .)%>%
gsub("(.*?)Clark.*", "\\1", .)%>%
gsub("(.*?)Munroe.*", "\\1", .)%>%
gsub("(.*?)Mahadeva.*", "\\1", .)%>%
gsub("(.*?)Chabanaud.*", "\\1", .)%>%
gsub("(.*?)Starks.*", "\\1", .)%>%
gsub("(.*?)Jenkins.*", "\\1", .)%>%
gsub("(.*?)Eigenmann.*", "\\1", .)%>%
gsub("(.*?)Bloch.*", "\\1", .)%>%
gsub("(.*?)Cuvier.*", "\\1", .)%>%
gsub("(.*?)Woolman.*", "\\1", .)%>%
gsub("(.*?)Coull.*", "\\1", .)%>%
gsub("(.*?)Pilsbiry.*", "\\1", .)%>%
gsub("(.*?)Menke.*", "\\1", .)%>%
gsub("(.*?)Broderip.*", "\\1", .)%>%
gsub("(.*?)Ordway.*", "\\1", .)%>%
gsub("(.*?)Rathbun.*", "\\1", .)%>%
gsub("(.*?)Randall.*", "\\1", .)%>%
gsub("(.*?)Pilsbry.*", "\\1", .)%>%
gsub("(.*?)Swainson.*", "\\1", .)%>% 
  str_trim %>% data.frame

new.species = cbind(species.list$NOMBRE_COMUN,species.clean)

write.csv(new.species, file="Corrected_species.csv")
