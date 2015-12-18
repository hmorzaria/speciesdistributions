#' This is our external R script called species_analysis_chunks.R
#' Hem Nalini Morzaria Luna hmorzarialuna@gmail.com
#' October 2015
#' This file retrieves data for the Eastern Pacific and Atlantic
#' species records can then be used for SDM models

#' chunks in this file
#'  setpreferences: library prefs
#' getbiodiversity: retrieve records using API and downloaded files
#' organize biodiversity: Check data and break up by species

## @knitr setpreferences

x = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
      "PBSmapping", "rgdal", "fields","data.table","rgbif","raster", "rasterVis",
      "sp","sperich","spocc","dplyr","SDMTools","ggplot2","ggmap", "ecoengine", 
      "rvertnet", "httr","wesanderson","tidyr","cowplot","rbison","rebird","taxize","readr")
lapply(x, require, character.only = TRUE)
cleanbib()
options("citation_format" = "pandoc")


## @knitr getbiodiversity

datafiles="E:/Archivos/1Archivos/Articulos/En preparacion/Climate_change_species_distribution/Data/Ocurrencia_especies"
datapath="E:/Archivos/1Archivos/Articulos/En preparacion/Climate_change_species_distribution/Analysis" #put path
datapath2="E:/Archivos/1Archivos/Articulos/En preparacion/Climate_change_species_distribution/Analysis/Raw_data" #put path
shapepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Species_records"

setwd(shapepath)

#read in Easter Pacific shapefile
goc.shape <- readOGR(".", "bounding_areas_pacific_buff") 

setwd(datapath)

#read in polygons
#this is a grid of 9km polygons
#polygons.region = source("polygons_pacific.R")
#polygons = polygons.region$value
# projections
#Lambert
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

#' NOTE ECOBIRDS CURRENTLY COMMENTED OUT BECAUSE I WAS LOOKING FOR ONLY FISH AND INVERTS
#' ULLOA files also commented because they are only seagrass and sharks
#' 
#' species to query instead of all records
#' each line has first the accepted names followed by synonyms
species.list = c("Hexaplex nigritus","Murex nigritus","Muricanthus nigritus",
                 "Rhinobatos productus","Rhinobatis producta",
                 "Squatina californica","Rhina philippi",
                 "Bothus constellatus","Platophrys constellatus",
                 "Citharichthys gilberti",
                 "Cyclopsetta panamensis","Citharichthys panamensis","Azevia panamensis",
                 "Cyclopsetta querna","Azevia querna","Dorsopsetta norma",
                 "Etropus ciadi",
                 "Etropus crossotus","Etropus crossotus crossotus",
                 "Paralichthys californicus","Hippoglossus californicus",
                 "Paralichthys woolmani","Paralichthys sinaloae","Paralichthys woolmanni",
                 "Hippoglossina tetrophthalma","Hippoglossina tetrophthalmus","Lioglossina tetrophthalmus",
                 "Paralichthys aestuarius","Paralichthys magdalenae",
                 "Syacium ovale","Hemirhombus ovalis","Citharichthys ovalis",
                 "Achirus mazatlanus","Solea mazatlana",
                 "Phyllonotus erythrostomus","Hexaplex erythrostomus","Chicoreus (Phyllonotus) erythrostomus",
                 "Callinectes bellicosus","Callinectes ochoterenai",
                 "Zapteryx exasperata","Platyrhina exasperata","Trigonorhina alveata")

#' this is the list of species without the synonyms, gbif needs it this way
#' because it takes species and retrieves key numbers which include synonyms

species.list2 = c("Hexaplex nigritus",
                  "Rhinobatos productus",
                  "Squatina californica",
                  "Bothus constellatus",
                  "Citharichthys gilberti",
                  "Cyclopsetta panamensis",
                  "Cyclopsetta querna",
                  "Etropus ciadi",
                  "Etropus crossotus",
                  "Paralichthys californicus",
                  "Paralichthys woolmani",
                  "Hippoglossina tetrophthalma",
                  "Paralichthys aestuarius",
                  "Syacium ovale",
                  "Achirus mazatlanus",
                  "Phyllonotus erythrostomus",
                  "Chicoreus (Phyllonotus) erythrostomus",
                  "Callinectes bellicosus",
                  "Zapteryx exasperata")

this.source = 'bison'
biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))

#not using by polygon because bison doesn't have too many records and its slow
#for(eachpolygon in 1:length(polygons)){
  
 # this.polygon = polygons[eachpolygon]#subset bounding box

  for(eachspecies in 1:length(species.list)){
    
    this.species = species.list[eachspecies]
  
    #spocc.data = bison(species = this.species, type = "scientific_name",aoi=this.polygon,count=10000, config=verbose())
  
    spocc.data = bison(species = this.species, type = "scientific_name",count=10000, config=verbose())
    
  spocc.data.source = spocc.data$points %>% 
    data.frame %>% 
    tbl_df 
  
  if(nrow(spocc.data.source)!=0) {
    
    spocc.data.source = spocc.data$points %>%  
      tbl_df 
    
    
    spocc.data.source$source_data = this.source
    
    setwd(datapath2)
    #write table
    write.csv(spocc.data.source, file=paste(this.source,"_",this.species,"_biodiver_full.csv",sep=""))
    
    
    spocc.data.source2 = spocc.data.source %>% 
      dplyr::select(name,decimalLongitude,decimalLatitude,source_data)
    
    
    spocc.data.source3 = spocc.data.source2[complete.cases(spocc.data.source2),]  #eliminate rows with NA
    # only needed columns
    
    setnames(spocc.data.source3, c("name","long","lat","source_data"))
    
    
    biodiversity.clean = spocc.data.source3[!duplicated(spocc.data.source3[,c('name', 'long', 'lat')]),]
    
    #section the file so it can be subset for the Gulf of California
    
    biodiv.rows = nrow(biodiversity.clean)
    
    iterations = round(biodiv.rows/1000,0)
    
    if(biodiv.rows>1000)
    {
      last.row = 1001
      first.row = 1
      
      for (each.iteration in 1:iterations)
        
      {
        new.last.row = ((last.row-1)*each.iteration)
        
        section.biodiv = biodiversity.clean[first.row:new.last.row,] %>% 
          na.omit %>% 
          mutate_each(funs(as.numeric),lat,long) %>% 
          as.data.frame
        
        coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
        proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
        print(summary(section.biodiv)) # print summary
        # subset ocurrence points within GOC
        stations_subset <- section.biodiv[goc.shape, ]
        #get table from shapefile
        biodiversity.goc <- as(stations_subset, "data.frame")
        
        test.bio = nrow(biodiversity.goc)==0
        if (test.bio==FALSE)
        {
          biodiversity = rbind(biodiversity,biodiversity.goc)
          biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
          print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records for species",eachspecies,":",this.species))
          
        }
        
        first.row = new.last.row+1
      }
    }
    
    if(biodiv.rows<1000)
    {
      last.row = biodiv.rows
      first.row = 1
      section.biodiv = biodiversity.clean[first.row:last.row,] %>% 
        na.omit %>% 
        mutate_each(funs(as.numeric),lat,long) %>% 
        as.data.frame
      
      coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
      proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
      print(summary(section.biodiv)) # print summary
      # subset ocurrence points within GOC
      stations_subset <- section.biodiv[goc.shape, ]
      #get table from shapefile
      biodiversity.goc <- as(stations_subset, "data.frame")
      
      test.bio = nrow(biodiversity.goc)==0
      if (test.bio==FALSE)
      {
        biodiversity = rbind(biodiversity,biodiversity.goc)
        biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
        print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
        
      }
    }
  } else {
  
  print(paste("No records found for species",eachspecies,":",this.species))
}
  } #end species

  #} #end polygon


setwd(datapath)
write.csv(biodiversity, file="bison_biodiver_full.csv")

#extract data gbif using their API

#uses same polygons as bison
biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))


#gbif finds taxon by key, synonyms are included in each key

keys <- sapply(species.list2, function(x) name_backbone(name=x, kingdom='animals')$speciesKey,
               USE.NAMES=FALSE) 

#for(eachpolygon in 1:length(polygons)){
  
 # this.polygon = polygons[eachpolygon]#subset bounding box

for(eachkey in 1:length(keys)){  
    #call GBIF API, max records are 200000
  this.key = keys[eachkey]
  
  #gbif.goc = occ_search(taxonKey=this.key, geometry=this.polygon,return='data',fields=c("name","decimalLatitude","decimalLongitude","datasetName","collectionCode"),limit=200000, callopts=verbose()) 
  
  gbif.goc = occ_search(taxonKey=this.key, return='data',fields=c("name","decimalLatitude","decimalLongitude","datasetName","collectionCode"),limit=200000, callopts=verbose()) 
  
  if(gbif.goc[1]=="no data found, try a different search")
  {
    print("no records found")
  } else
  
  {
    gbif.goc = gbif.goc %>% tbl_df
  this.class = class(gbif.goc)
    
  gbif.goc$source_data = "gbif"
  #add source
  this.source = "gbif"
  
  setwd(datapath2)
  write.csv(gbif.goc, file=paste(this.source,"_",this.species,"_biodiver_full.csv",sep=""))
  
  
  gbif.goc = gbif.goc %>% 
    dplyr::select(name, decimalLongitude, decimalLatitude,source_data) %>% 
    setnames(c("name","long","lat","source_data"))
  
  gbif.goc2 =  gbif.goc[complete.cases(gbif.goc),]#eliminate rows with NA
  
  
  #remove duplicates based on the combination of latitude, longitude and species
  
  biodiversity.clean = gbif.goc2[!duplicated(gbif.goc2[,c('name', 'lat', 'long')]),]
  
  #section the file so it can be subset for the Gulf of California
  
  biodiv.rows = nrow(biodiversity.clean)
  
  iterations = round(biodiv.rows/1000,0)
  
  if(biodiv.rows>1000)
  {
    last.row = 1001
    first.row = 1
    
    for (each.iteration in 1:iterations)
      
    {
      new.last.row = ((last.row-1)*each.iteration)
      
      section.biodiv = biodiversity.clean[first.row:new.last.row,] %>% 
        na.omit %>% 
        mutate_each(funs(as.numeric),lat,long) %>% 
        as.data.frame
      
      coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
      proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
      print(summary(section.biodiv)) # print summary
      # subset ocurrence points within GOC
      stations_subset <- section.biodiv[goc.shape, ]
      #get table from shapefile
      biodiversity.goc <- as(stations_subset, "data.frame")
      
      test.bio = nrow(biodiversity.goc)==0
      if (test.bio==FALSE)
      {
        biodiversity = rbind(biodiversity,biodiversity.goc)
        biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
        print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
        
      }
      
      first.row = new.last.row+1
    }
  }
  
  if(biodiv.rows<1000)
  {
    last.row = biodiv.rows
    first.row = 1
    section.biodiv = biodiversity.clean[first.row:last.row,] %>% 
      na.omit %>% 
      mutate_each(funs(as.numeric),lat,long) %>% 
      as.data.frame
    
    coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
    proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
    print(summary(section.biodiv)) # print summary
    # subset ocurrence points within GOC
    stations_subset <- section.biodiv[goc.shape, ]
    #get table from shapefile
    biodiversity.goc <- as(stations_subset, "data.frame")
    
    test.bio = nrow(biodiversity.goc)==0
    if (test.bio==FALSE)
    {
      biodiversity = rbind(biodiversity,biodiversity.goc)
      biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
      print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
      
    }
  }
  
  }
}
  
  #}
#end file

setwd(datapath)
#write table
write.csv(biodiversity, file="gbif_biodiver_full_all.csv")

#make new frame for ecoengine
biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))

#extract data from Berkley ecoengine

#read in polygons
#this is a grid of 9km polygons in CSV format
#polygons.region2 = source("polygons2.R")
#polygons2 = polygons.region2$value

#add source
this.source = "ecoengine"

#for(eachpolygon in 1:length(polygons2)){
  
 # this.polygon = polygons2[eachpolygon]#subset bounding box
  
  for(eachspecies in 1:length(species.list)){
    
    this.species = species.list[eachspecies]
  #call ecoengine API
 # ee.data.frame = ee_observations(page_size=10000,scientific_name=this.species, georeferenced = TRUE,bbox = this.polygon) %>% 
#    .$data %>% tbl_df()
  
  ee.data.frame = tryCatch(ee_observations(page_size=10000,scientific_name=this.species, georeferenced = TRUE),error=function(e) as.character()) 
  
  if(length(ee.data.frame)!=0){
    
    ee.data.frame = ee.data.frame %>% 
    .$data %>% tbl_df()
  
  setwd(datapath2)
  
  write.csv(ee.data.frame, file=paste(this.source,"_",this.species,"_biodiver_full.csv",sep=""))
  
  ee.data.goc = ee.data.frame %>% 
    dplyr::select(scientific_name, longitude, latitude) %>% 
    setnames(c("name","long","lat"))
  
  ee.data.goc$source_data = "ecoengine"
  
  
  ee.data.goc2 = ee.data.goc[complete.cases(ee.data.goc),]#eliminate rows with NA
  
  #remove duplicates based on the combination of latitude, longitude and species
  
  biodiversity.clean = ee.data.goc2[!duplicated(ee.data.goc2[,c('name', 'lat', 'lon')]),]
  
  #section the file so it can be subset for the Gulf of California
  
  biodiv.rows = nrow(biodiversity.clean)
  
  iterations = round(biodiv.rows/1000,0)
  
  if(biodiv.rows>1000)
  {
    last.row = 1001
    first.row = 1
    
    for (each.iteration in 1:iterations)
      
    {
      new.last.row = ((last.row-1)*each.iteration)
      
      section.biodiv = biodiversity.clean[first.row:new.last.row,] %>% 
        na.omit %>% 
        mutate_each(funs(as.numeric),lat,long) %>% 
        as.data.frame
      
      coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
      proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
      print(summary(section.biodiv)) # print summary
      # subset ocurrence points within GOC
      stations_subset <- section.biodiv[goc.shape, ]
      #get table from shapefile
      biodiversity.goc <- as(stations_subset, "data.frame")
      
      test.bio = nrow(biodiversity.goc)==0
      if (test.bio==FALSE)
      {
        biodiversity = rbind(biodiversity,biodiversity.goc)
        biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
        print(paste(this.source, "biodiversity has ",nrow(biodiversity)," records"))
        
      }
      
      first.row = new.last.row+1
    }
  }
  
  if(biodiv.rows<1000)
  {
    last.row = biodiv.rows
    first.row = 1
    section.biodiv = biodiversity.clean[first.row:last.row,] %>% 
      na.omit %>% 
      mutate_each(funs(as.numeric),lat,long) %>% 
      as.data.frame
    
    coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
    proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
    print(summary(section.biodiv)) # print summary
    # subset ocurrence points within GOC
    stations_subset <- section.biodiv[goc.shape, ]
    #get table from shapefile
    biodiversity.goc <- as(stations_subset, "data.frame")
    
    test.bio = nrow(biodiversity.goc)==0
    if (test.bio==FALSE)
    {
      biodiversity = rbind(biodiversity,biodiversity.goc)
      biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
      print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
      
    }
  }
  
} 
} # end species
  #}# end polygon

setwd(datapath)
#write table
write.csv(biodiversity, file="ecoengine__biodiver_full.csv")

#get OBIS, downloaded from website

setwd(datafiles)#switch directory
biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))

csv.files <- list.files(pattern = "\\.csv$")#list csv files

this.source= "obis"
#loop to read in data and obtain points
for(eachfile in 1:length(csv.files)){
  
  print(paste("Analyzing file ", eachfile, "of ",length(csv.files)))
  Biom  = fread(csv.files[eachfile],header=T, sep=",",select=c(1:11)) %>%
    tbl_df %>% 
    dplyr::select(sname, longitude, latitude) %>% #subset needed variables
    setnames(c("name","long","lat"))
  
  Biom$source_data = "obis" # set source
  
  
  Biom2 = Biom[complete.cases(Biom),]#eliminate rows with NA
  
  
  #remove duplicates based on the combination of latitude, longitude and species
  
  biodiversity.clean = Biom2[!duplicated(Biom2[,c('name', 'lat', 'lon')]),]
  # remove negative latitude values
  
  biodiversity.clean2 = biodiversity.clean %>% 
    filter(name %in% species.list)
    
  #section the file so it can be subset for the Gulf of California
  
  biodiv.rows = nrow(biodiversity.clean2)
  
  if(biodiv.rows!=0){
    
  iterations = round(biodiv.rows/1000,0)
  
  if(biodiv.rows>1000)
  {
    last.row = 1001
    first.row = 1
    
    for (each.iteration in 1:iterations)
      
    {
      new.last.row = ((last.row-1)*each.iteration)
      
      section.biodiv = biodiversity.clean2[first.row:new.last.row,] %>% 
        na.omit %>% 
        mutate_each(funs(as.numeric),lat,long) %>% 
        as.data.frame
      
      coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
      proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
      print(summary(section.biodiv)) # print summary
      # subset ocurrence points within GOC
      stations_subset <- section.biodiv[goc.shape, ]
      #get table from shapefile
      biodiversity.goc <- as(stations_subset, "data.frame")
      
      test.bio = nrow(biodiversity.goc)==0
      if (test.bio==FALSE)
      {
        biodiversity = rbind(biodiversity,biodiversity.goc)
        biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
        print(paste(this.source,nrow(biodiversity)," records"))
        
      }
      
      first.row = new.last.row+1
    }
  }
  
  if(biodiv.rows<1000)
  {
    last.row = biodiv.rows
    first.row = 1
    section.biodiv = biodiversity.clean2[first.row:last.row,] %>% 
      na.omit %>% 
      mutate_each(funs(as.numeric),lat,long) %>% 
      as.data.frame
    
    coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
    proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
    print(summary(section.biodiv)) # print summary
    # subset ocurrence points within GOC
    stations_subset <- section.biodiv[goc.shape, ]
    #get table from shapefile
    biodiversity.goc <- as(stations_subset, "data.frame")
    
    test.bio = nrow(biodiversity.goc)==0
    if (test.bio==FALSE)
    {
      biodiversity = rbind(biodiversity,biodiversity.goc)
      biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
      print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
      
    }
  }
  
}
  } # end files

setwd(datapath)
#write table
write.csv(biodiversity, file="obis_biodiver_full.csv")

biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))


# for excel files from UABCS
setwd(datafiles)
xls.files <- list.files(pattern = "\\.xlsx$")# list files
this.source="cobi_uabcs"
#loop to read in data and obtain GOC data
for(eachfile in 1:length(xls.files))
{
  
  print(paste("Analyzing"," file",eachfile,"_",xls.files[eachfile]))
  
  df = read_excel(xls.files[eachfile], sheet = 1, col_names = TRUE, col_types = NULL, na = "",skip = 0)
  indx.sp= grep("Especie|Nombre|especie|nombre|Species",colnames(df))
  indx.fuen= grep('Fuente|fuente|informacion|Base',colnames(df))
  indx.lon= grep('Longitud|longitud|Lon',colnames(df))
  indx.lat= grep('Latitud|latitud|Latutud|Lat',colnames(df))
  
  df2 = df[,c(indx.sp,indx.lon,indx.lat,indx.fuen)]
  
  setnames(df2, c("name","long","lat","source_data"))
  
  
  df2$long = as.numeric(df2$long) #make sure lon and lat are numeric
  df2$lat = as.numeric(df2$lat)
  
  df4 = df2[complete.cases(df2),]#eliminate rows with NA
  
  #remove duplicates based on the combination of latitude, longitude and species
  
  biodiversity.clean = df4[!duplicated(df4[,c('name', 'lat', 'long')]),]
  
  biodiversity.clean2 = biodiversity.clean %>% 
    filter(name %in% species.list)
  
  #section the file so it can be subset for the Gulf of California
  
  if (nrow(biodiversity.clean2)!=0) {
    
  biodiv.rows = nrow(biodiversity.clean2)
  
  iterations = round(biodiv.rows/1000,0)
  
  if(biodiv.rows>1000)
  {
    last.row = 1001
    first.row = 1
    
    for (each.iteration in 1:iterations)
      
    {
      new.last.row = ((last.row-1)*each.iteration)
      
      section.biodiv = biodiversity.clean2[first.row:new.last.row,] %>% 
        na.omit %>% 
        mutate_each(funs(as.numeric),lat,long) %>% 
        as.data.frame
      
      coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
      proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
      print(summary(section.biodiv)) # print summary
      # subset ocurrence points within GOC
      stations_subset <- section.biodiv[goc.shape, ]
      #get table from shapefile
      biodiversity.goc <- as(stations_subset, "data.frame")
      
      test.bio = nrow(biodiversity.goc)==0
      if (test.bio==FALSE)
      {
        biodiversity = rbind(biodiversity,biodiversity.goc)
        biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
        print(paste(this.source,nrow(biodiversity)," records"))
        
      }
      
      first.row = new.last.row+1
    }
  }
  
  if(biodiv.rows<1000)
  {
    last.row = biodiv.rows
    first.row = 1
    section.biodiv = biodiversity.clean2[first.row:last.row,] %>% 
      na.omit %>% 
      mutate_each(funs(as.numeric),lat,long) %>% 
      as.data.frame
    
    coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
    proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
    print(summary(section.biodiv)) # print summary
    # subset ocurrence points within GOC
    stations_subset <- section.biodiv[goc.shape, ]
    #get table from shapefile
    biodiversity.goc <- as(stations_subset, "data.frame")
    
    test.bio = nrow(biodiversity.goc)==0
    if (test.bio==FALSE)
    {
      biodiversity = rbind(biodiversity,biodiversity.goc)
      biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
      print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
      
    }
  }
  }
} # end file

setwd(datapath)
#write table
write.csv(biodiversity, file="cobi_biodiver_full.csv")


#retrieve vertnet files

setwd(datapath)


biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))

#points.goc = fread("polygon_grid.csv",header = T)

#loop to read in data and obtain points in Gulf of California

this.source = "vertnet"

for(eachspecies in 1:length(species.list))
{
  
  this.species = species.list[eachspecies]#species
  
  genus =  this.species %>% 
    strsplit(" ") %>% 
    unlist %>% 
    .[1]
  
  species =  this.species %>% 
    strsplit(" ") %>% 
    unlist %>% 
    .[2]
  
  continents = c("North America", "South America")
  for(eachcontinent in 1:length(continents)){
  
    this.continent = continents[eachcontinent]
    
  res <- searchbyterm(genus = genus, specificepithet = species, continent=this.continent,limit=1000)
  
  test.res = is.null(res)
  if(test.res==FALSE)
  {
    
    Biom = as.data.frame(res$data)
    
    setwd(datapath2)
    #write table
    write.csv(Biom, file=paste(this.source,"_biodiver_full.csv",sep=""))
    
    
    indx.sp= grep("scientificname",colnames(Biom))
    
    test.sp.name = !length(indx.sp)
    
    if ( !length(indx.sp)==FALSE )
    {
      biom.names = colnames(Biom)
      
      this.name = biom.names[ which(biom.names=="decimallongitude")]
      name.length = length(this.name)
      if(name.length == 0){
        
        print("No lat long given for record")
      } else {
      Biom = Biom[,c("scientificname","decimallongitude","decimallatitude")]
      
      setnames(Biom, c('name', 'long', 'lat')) # rename columns
      Biom$source_data = this.source # set source
      
      Biom$long = as.numeric(Biom$long) #make sure lon and lat are numeric
      Biom$lat = as.numeric(Biom$lat)
      Biom2 = Biom[complete.cases(Biom),]#eliminate rows with NA
      
      biodiversity.clean = Biom2[!duplicated(Biom2[,c('name', 'lat', 'long')]),]
      
      
      #section the file so it can be subset for the Gulf of California
      
      biodiv.rows = nrow(biodiversity.clean)
      
      iterations = round(biodiv.rows/1000,0)
      
      if(biodiv.rows>1000)
      {
        last.row = 1001
        first.row = 1
        
        for (each.iteration in 1:iterations)
          
        {
          new.last.row = ((last.row-1)*each.iteration)
          
          section.biodiv = biodiversity.clean[first.row:new.last.row,] %>% 
            na.omit %>% 
            mutate_each(funs(as.numeric),lat,long) %>% 
            as.data.frame
          
          coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
          proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
          print(summary(section.biodiv)) # print summary
          # subset ocurrence points within GOC
          stations_subset <- section.biodiv[goc.shape, ]
          #get table from shapefile
          biodiversity.goc <- as(stations_subset, "data.frame")
          
          test.bio = nrow(biodiversity.goc)==0
          if (test.bio==FALSE)
          {
            biodiversity = rbind(biodiversity,biodiversity.goc)
            biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
            print(paste(this.source,nrow(biodiversity)," records"))
            
          }
          
          first.row = new.last.row+1
        }
      }
      
      if(biodiv.rows<1000)
      {
        last.row = biodiv.rows
        first.row = 1
        section.biodiv = biodiversity.clean[first.row:last.row,] %>% 
          na.omit %>% 
          mutate_each(funs(as.numeric),lat,long) %>% 
          as.data.frame
        
        coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
        proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
        print(summary(section.biodiv)) # print summary
        # subset ocurrence points within GOC
        stations_subset <- section.biodiv[goc.shape, ]
        #get table from shapefile
        biodiversity.goc <- as(stations_subset, "data.frame")
        
        test.bio = nrow(biodiversity.goc)==0
        if (test.bio==FALSE)
        {
          biodiversity = rbind(biodiversity,biodiversity.goc)
          biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
          print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
}
        }
      }
    }
  }
  
} # end points
}
setwd(datapath)
#write table
write.csv(biodiversity, file="vertnet_biodiver_full.csv",row.names=FALSE)


# #get records from ebird
# setwd(datapath)
# 
# 
# biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))
# 
# #loop to read in data and obtain points in Gulf of California
# 
# this.source = "ebird"
# regions.ebird = c('MX-SON','MX-BCN','MX-SIN','MX-BCS','MX-NAY','MX-JAL')
# for(eachregion in 1:length(regions.ebird))
# {
#   
#   this.region = regions.ebird[eachregion]#region
#   
#   res <- ebirdregion(this.region, max=10000)
#   
#   if(nrow(res)!=
#   {
#     
#     setwd(datapath2)
#     #write table
#     write.csv(res, file=paste(this.source,"_",this.region,"_biodiver_full.csv",sep=""))
#     
#     Biom = res %>% 
#       select(sciName,lng,lat) %>% 
#       setnames(c('name', 'long', 'lat')) %>% 
#       mutate(source_data = this.source) %>% 
#       mutate_each(funs(as.numeric),long:lat)#make sure lon and lat are numeric
#     
#     Biom4 = Biom[complete.cases(Biom),]#eliminate rows with NA
#     
#     biodiversity.clean = Biom4[!duplicated(Biom4[,c('name', 'lat', 'long')]),]
#     
#     
#     #section the file so it can be subset for the Gulf of California
#     
#     biodiv.rows = nrow(biodiversity.clean)
#     
#     iterations = round(biodiv.rows/1000,0)
#     
#     if(biodiv.rows>1000)
#     {
#       last.row = 1001
#       first.row = 1
#       
#       for (each.iteration in 1:iterations)
#         
#       {
#         new.last.row = ((last.row-1)*each.iteration)
#         
#         section.biodiv = biodiversity.clean[first.row:new.last.row,] %>% 
#           na.omit %>% 
#           mutate_each(funs(as.numeric),lat,long) %>% 
#           as.data.frame
#         
#         coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
#         proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
#         print(summary(section.biodiv)) # print summary
#         # subset ocurrence points within GOC
#         stations_subset <- section.biodiv[goc.shape, ]
#         #get table from shapefile
#         biodiversity.goc <- as(stations_subset, "data.frame")
#         
#         test.bio = nrow(biodiversity.goc)==0
#         if (test.bio==FALSE)
#         {
#           biodiversity = rbind(biodiversity,biodiversity.goc)
#           biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
#           print(paste(this.source,nrow(biodiversity)," records"))
#           
#         }
#         
#         first.row = new.last.row+1
#       }
#     }
#     
#     if(biodiv.rows<1000)
#     {
#       last.row = biodiv.rows
#       first.row = 1
#       section.biodiv = biodiversity.clean[first.row:last.row,] %>% 
#         na.omit %>% 
#         mutate_each(funs(as.numeric),lat,long) %>% 
#         as.data.frame
#       
#       coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
#       proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
#       print(summary(section.biodiv)) # print summary
#       # subset ocurrence points within GOC
#       stations_subset <- section.biodiv[goc.shape, ]
#       #get table from shapefile
#       biodiversity.goc <- as(stations_subset, "data.frame")
#       
#       test.bio = nrow(biodiversity.goc)==0
#       if (test.bio==FALSE)
#       {
#         biodiversity = rbind(biodiversity,biodiversity.goc)
#         biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
#         print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
#         
#       }
#     }
#   }
# }
# 
# 
# 
# setwd(datapath)
# #write table
# write.csv(biodiversity, file="ebird_biodiver_full.csv",row.names=FALSE)

#get shark ad seagrass data files from Ulloa et al. 2006
#these were the only groups with species-level data

# setwd(ulloafiles)
# csv.files <- list.files(pattern = "\\.csv$")# list files
# biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))
# 
# #loop to read in data and obtain points in Gulf of California
# 
# this.source = "ulloa_et_al_2006"
# 
# #loop to read in data and obtain GOC data
# for(eachfile in 1:length(csv.files))
# {
#   
#   print(paste("Analyzing"," file",eachfile,"_",csv.files[eachfile]))
#   
#   df2 = fread(csv.files[eachfile], header=TRUE) %>% 
#     tbl_df %>% 
#     select(NOM_CIEN, LONGITUD, LATITUD) %>% 
#     mutate(source_data=this.source) %>% 
#     setnames(c("name","long","lat","source_data")) %>% 
#     mutate_each(funs(as.numeric),long:lat) %>% 
#     filter(name %in% species.list) %>% 
#     data.frame
#   
#   df4 = df2[complete.cases(df2),]#eliminate rows with NA
#   
#   biodiversity.clean = df4[!duplicated(df4[,c('name', 'lat', 'long')]),]
#   # remove negative latitude values
#   
#   #section the file so it can be subset for the Gulf of California
#   
#   biodiv.rows = nrow(biodiversity.clean)
#   
#   iterations = round(biodiv.rows/1000,0)
#   
#   if(biodiv.rows>1000)
#   {
#     last.row = 1001
#     first.row = 1
#     
#     for (each.iteration in 1:iterations)
#       
#     {
#       new.last.row = ((last.row-1)*each.iteration)
#       
#       section.biodiv = biodiversity.clean[first.row:new.last.row,] %>% 
#         na.omit %>% 
#         mutate_each(funs(as.numeric),lat,long) %>% 
#         as.data.frame
#       
#       coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
#       proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
#       print(summary(section.biodiv)) # print summary
#       # subset ocurrence points within GOC
#       stations_subset <- section.biodiv[goc.shape, ]
#       #get table from shapefile
#       biodiversity.goc <- as(stations_subset, "data.frame")
#       
#       test.bio = nrow(biodiversity.goc)==0
#       if (test.bio==FALSE)
#       {
#         biodiversity = rbind(biodiversity,biodiversity.goc)
#         biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
#         print(paste(this.source,nrow(biodiversity)," records"))
#         
#       }
#       
#       first.row = new.last.row+1
#     }
#   }
#   
#   if(biodiv.rows<1000)
#   {
#     last.row = biodiv.rows
#     first.row = 1
#     section.biodiv = biodiversity.clean[first.row:last.row,] %>% 
#       na.omit %>% 
#       mutate_each(funs(as.numeric),lat,long) %>% 
#       as.data.frame
#     
#     coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
#     proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
#     print(summary(section.biodiv)) # print summary
#     # subset ocurrence points within GOC
#     stations_subset <- section.biodiv[goc.shape, ]
#     #get table from shapefile
#     biodiversity.goc <- as(stations_subset, "data.frame")
#     
#     test.bio = nrow(biodiversity.goc)==0
#     if (test.bio==FALSE)
#     {
#       biodiversity = rbind(biodiversity,biodiversity.goc)
#       biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
#       print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
#       
#     }
#   }
#   
# } # end file
# setwd(datapath)
# #write table
# write.csv(biodiversity, file="ulloa_biodiver_full.csv",row.names=FALSE)


## @knitr organizebiodiversity
rm(list=ls(all=TRUE))

datafiles="E:/Archivos/1Archivos/Articulos/En preparacion/Climate_change_species_distribution/Data/Ocurrencia_especies"
datapath="E:/Archivos/1Archivos/Articulos/En preparacion/Climate_change_species_distribution/Analysis" #put path
speciespath="E:/Archivos/1Archivos/Articulos/En preparacion/Climate_change_species_distribution/Analysis/Species_files" #put path
shapepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Species_records"

setwd(shapepath)
#read in Easter Pacific/ Gulf of Mexico shapefile
goc.shape <- readOGR(".", "bounding_polygon_sea")

# projections
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

setwd(datapath)

biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4)) %>% 
  tbl_df %>% 
  setnames(c("name","long","lat","source"))

# get list of files

csv.files <- list.files(pattern = "\\_full.csv$")#list csv files

for(eachfile in 1:length(csv.files))
{
  
  biodiversity.goc  = fread(csv.files[eachfile],header=T) %>% 
    tbl_df %>% 
    dplyr::select(starts_with('name'),contains('lon'),contains('lat'),source_data) %>% 
    setnames(c("name","long","lat","source_data"))
  
  #read each file
  biodiversity = rbind(biodiversity, biodiversity.goc) #bind it to a empty frame
  biodiversity = biodiversity[!duplicated(biodiversity[,c("name","long","lat")]),] #eliminate duplicates
  
  print(paste("biodiversity has ",nrow(biodiversity)," records")) #print records
}

setwd(datapath)

#eliminate points on land

geo.biodiv = biodiversity %>% data.frame
coordinates(geo.biodiv) = c("long", "lat")

proj4string(geo.biodiv) <- crs.geo.wgs  # define projection system of our data
print(summary(geo.biodiv)) # print summary
# subset ocurrence points within GOC
stations_subset <- geo.biodiv[goc.shape, ]
biodiversity.2 <- as(stations_subset, "data.frame")

write.csv(biodiversity.2,"biodiversity_all_raw.csv")

setwd(datapath)
biodiversity.2= read.csv("biodiversity_all_raw.csv")

caracol1 = c("Hexaplex nigritus","Murex nigritus","Muricanthus nigritus")
caracol2 = c("Phyllonotus erythrostomus","Hexaplex erythrostomus","Chicoreus (Phyllonotus) erythrostomus")
guitarra = c("Rhinobatos productus","Rhinobatis producta")
angelito = c("Squatina californica","Rhina philippi")
lenguado1 = c("Bothus constellatus","Platophrys constellatus")
lenguado2 = c("Citharichthys gilberti")
lenguado3 = c("Cyclopsetta panamensis","Citharichthys panamensis","Azevia panamensis")
lenguado4 = c("Cyclopsetta querna","Azevia querna","Dorsopsetta norma")
lenguado5 = c("Etropus ciadi")
lenguado6 = c("Etropus crossotus","Etropus crossotus crossotus")
lenguado7 = c("Paralichthys californicus","Hippoglossus californicus")
lenguado8 = c("Paralichthys woolmani","Paralichthys sinaloae","Paralichthys woolmanni")
lenguado9 = c("Hippoglossina tetrophthalma","Hippoglossina tetrophthalmus","Lioglossina tetrophthalmus")
lenguado10 = c("Paralichthys aestuarius","Paralichthys magdalenae")
lenguado11 = c("Syacium ovale","Hemirhombus ovalis","Citharichthys ovalis")
lenguado12 = c("Achirus mazatlanus","Solea mazatlana")
jaiba = c("Callinectes bellicosus","Callinectes ochoterenai")
species.goc = list(caracol1,caracol2,guitarra,angelito,lenguado1,lenguado2,lenguado3,lenguado4,lenguado5,
                   lenguado6,lenguado7,lenguado8,lenguado9,lenguado10,lenguado11,lenguado12,jaiba)

for(eachspecies in 1:length(species.goc)){
  
  spp.list = species.goc[eachspecies] %>% 
    unlist
  
  valid.sp = species.goc[eachspecies] %>% 
    unlist %>% 
    .[1]
  
  biodiversity.sp = biodiversity.2 %>% 
    filter(name %in% spp.list) %>% 
    mutate(name=valid.sp)
  
  genus = valid.sp %>% strsplit(" ") %>% unlist %>% .[1]
  species = valid.sp %>% strsplit(" ") %>% unlist %>% .[2]
  
  setwd(speciespath)
  write.csv(biodiversity.sp,file=paste(genus,species,"data",".csv",sep="_"))
  
}

mapbiodiv <- get_map(location = c(lon = mean(geo.biodiv$long), lat = mean(geo.biodiv$lat)), zoom = 4,
                     source='stamen',maptype = "terrain", scale = 2) 

ggmap(mapbiodiv) +
  geom_point(data = biodiversity.2, aes(x = long, y = lat, alpha = 0.8), colour = 'darkred', fill= 'darkred', size = 2, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  labs(x = 'Longitude', y = 'Latitude')

ggsave ("map_goc_points.png", dpi = 300)


