rm(list = ls())
month="01"
year="2010"
fmonth="12"
fyear="2020"
path="f:/datos_temperatura"
setwd(path)
Files=list.files (pattern = ".nc")
numfiles=length(Files)
datos=matrix(1:(numfiles*5), ncol=5) #matriz donde 1=nom_archivo,2=año_inicio,3=mes_inicio,4=año_fin, 5=mes_fin
#guarda los datos de año de inicio y año final en la matriz datos, algunos archivos indica mes, otros solo el año
for(i in 1:length(Files))
{
tam=unlist(strsplit(Files[i], "_"))
periodo=unlist(strsplit(Files[i], "_"))[length(tam)]
years=unlist(strsplit(periodo, "-"))
years[2]=strtrim(years[2],nchar(years[2])-3)
datos[i,1]=Files[i]
if(nchar(years[1])>4)
{
  datos[i,2]=strtrim(years[1],4)
  datos[i,3]=unlist(strsplit(years[1],strtrim(years[1],4)))[2]
}
else if(nchar(years[1])==4)
{
  datos[i,2]=years[1]
  datos[i,3]="01"
}


if(nchar(years[2])>4)
{
  datos[i,4]=strtrim(years[2],4)
  datos[i,5]=unlist(strsplit(years[2],strtrim(years[2],4)))[2]
}
else if(nchar(years[2])==4)
{
  datos[i,4]=years[2]
  datos[i,5]="12"
}

}
#convierte los datos a tipo fecha, luego compara las fechas del rango solicitado por el usuario, si el archvi
#cumple con este rango de fechas, extrae max, min y mean, los archivos resultados los guarda en el mismo folder con terminación 
#{max|min|mean}_añosolicitadoinicio-messolicitadoinicio-añosolicitadofinal,messolicitadofinal.nc

for (i in 1:length(Files))
{
  mesui=as.Date(paste(year,month,"01",sep="-"))
  mesuf=as.Date(paste(fyear,fmonth,"28",sep="-"))
  mesi=as.Date(paste(datos[i,2],datos[i,3],"01",sep="-"))
  mesf=as.Date(paste(datos[i,4],datos[i,5],"28",sep="-"))
  if(mesi<=mesui && mesf>=mesuf)
  {
    inicapa=length(seq(from=mesi, to=mesui, by='month')) 
    fincapa=inicapa+length(seq(from=mesui, to=mesuf, by='month')) 
    arch=tam=unlist(strsplit(Files[i], ".nc"))[1]
  #getmax
    shell(paste("ncra -y max -d time,",inicapa,",",fincapa," ",datos[i,1]," ",arch[1],"_max_",year,"-",month,"-",fyear,"-",fmonth,".nc",sep=""),"C:/Windows/System32/cmd.exe",wait=TRUE)
  #getmin
  shell(paste("ncra -y min -d time,",inicapa,",",fincapa," ",datos[i,1]," ",arch[1],"_min_",year,"-",month,"-",fyear,"-",fmonth,".nc",sep=""),"C:/Windows/System32/cmd.exe",wait=TRUE)
   #getmean
  shell(paste("ncra -d time,",inicapa,",",fincapa," ",datos[i,1]," ",arch[1],"_mean_",year,"-",month,"-",fyear,"-",fmonth,".nc",sep=""),"C:/Windows/System32/cmd.exe",wait=TRUE)
   
    
  }
}

