#PAQUETES
library(readxl)
library(stringr)
library(reshape2)
library(tidyr)
library(dplyr)
library(lubridate)
library(MASS)
library(moments)
library(car)
library(nortest)
library(Amelia)

#ORGANIZAR TABLAS DE ONAMET SEGUN LO REQUERIDO POR RHTest
##Tomado de: https://gist.github.com/geofis/1dbac27f612eef8a40ed599d4181ac1c
##Funcion "Extraer numeros"
numextract <- function(string){
  str_extract(string, "\\-*\\d+\\.*\\d*")
}
##Datos de ONAMET. Descargar y abrir
directorio <- tempdir()
setwd(directorio) #FIJAR UN DIRECTORIO DE TRABAJO TEMPORAL
download.file("http://geografiafisica.org/r/onamet/tstonamet.xlsx", 'tstonamet.xlsx') #DESCARGA EL ARCHIVO ZIP Y LO NOMBRA COMO TEMPORAL
d <- read_xlsx("tstonamet.xlsx", sheet = 1, col_names = F)
d <- as.data.frame(d)
##Anios
anos <- sort(
  numextract(
    melt(
      sapply(
        d,
        function(x) grep('DATOS DIARIOS', x, value = T)
        )
      )$value
    )
  )
##Rangos de datos
Sys.setlocale(locale="en_GB.UTF-8")
inicioano <- grep('^DIA.*$', d[,1])
finano <- grep('^TOTAL$', d[,1])
rdat <- data.frame(anos, inicioano, finano)
seqdias <- seq.Date(as.Date(paste0(rdat$anos[1],'/1/1')), as.Date(paste0(rdat$anos[nrow(rdat)],'/12/31')), 'days')
seqtable <- data.frame(year=year(seqdias), month=months(seqdias), day=day(seqdias))
seqtable <- as.data.frame(sapply(seqtable, as.character), stringsAsFactors = F)
datos <- sapply(levels(rdat$anos), function(x) d[(rdat[rdat$anos==x,'inicioano']+1):(rdat[rdat$anos==x,'finano']-1), 1:13], simplify = F)
datos <- melt(datos)
colnames(datos) <- c('day',month.name, 'year')
datos #Visualizacion de resultado
##Reorganizacion segun requerido por RHTest. "INAP" a "0", "-" a NA
datos <- datos %>% gather(month, value, -day, -year)
datoscalendario <- merge(seqtable, datos, all.x=T, sort = F)
datoscalendario$value <- gsub('INAP','0', datoscalendario$value)
datoscalendario$value <- gsub('-','NA', datoscalendario$value)
datoscalendario$value <- as.numeric(datoscalendario$value)
datoscalendario$fecha <- as.Date(with(datoscalendario, paste0(day,'/',month,'/', year)), format = '%d/%B/%Y')
datoscalendario
View(datoscalendario)
#Guardar resultado en disco. Descomentar si fuese necesario
# write.csv(datoscalendario, 'datoscalendario.csv')


#INICIO HOMOGENEIZACION
datosR<-read.csv("D:/Documentos Tesis/tablas_bani_ocoa_depuradas/TABLAS_ESTACIONES_DEPURADAS/Rancho_Arriba_rhtest.csv", header=T)
head(datosR)
Ra<-datosR
Ra$V4
Ra<-Ra$v4
Ra
shapiro.Test(Ra)
shapiro.Test(Ra$V4)
Ra<-Ra[Ra>0]
Ra
shapiro.test(Ra)
hist(Ra)
qqnorm(Ra)
skewness(Ra)
kurtosis(Ra)
shapiro.test(Ra)
powerTransform(Ra) 
Ranorm<-bcPower(Ra,0.2758367)
Ranorm
hist(Ranorm)
qqnorm(Ranorm)
qqline(Ranorm)
shapiro.test(Ranorm)
ks.test(Ranorm, pnorm, mean(Ranorm), sd(Ranorm))
skewness(Ranorm)
Ra<-na.omit(Ra)
Ra
ks.test(Ranorm, pnorm, mean(Ranorm), sd(Ranorm))
Ranorm<-na.omit(Ranorm)
Ranorm
ks.test(Ranorm, pnorm, mean(Ranorm), sd(Ranorm))
skewness(Ranorm)
kurtosis(Ranorm)
getwd()
setwd("C:/Users/admin/Documents")
https://github.com/geofis/homogenization/blob/master/sampledata/datos_ocoa_para_RHT.csv NOTA: LOCALIZACION TABLA DATOS DE OCOA PARA RHTEST
source("D:\\Documentos Tesis\\Rhtes4\\RHtestsV4.r")
StartGUI()
source("RHtestsV4_20130719.r") #http://etccdi.pacificclimate.org/software.shtml
tdir <- tempdir()
setwd(tdir)
ghsource <- 'https://raw.githubusercontent.com/geofis/homogenization/master/'
fname <- 'datos_ocoa_para_RHT.csv'
download.file(paste0(ghsource, 'sampledata/', fname), fname)
d <- read.csv(fname, header = F)
d
head(d)
lam <- powerTransform(d$V4+0.01)$lambda
lam
d$trans <- bcPower(d$V4+0.01, lam)
d$trans 
dev.new();hist(d$trans)
dev.new();qqnorm(d$trans)
dev.new();qqnorm(d$trans)
shapiro.test(d$trans)
dprht <-d[,-4]
dprht 
write.table(dprht, 'dprht.csv', row.names = F, col.names = F, sep = ',')
getwd() 
setwd( "C:/Users/admin/Documents")
write.csv(dprht, 'dprht.csv')
source("D:\\Documentos Tesis\\Rhtes4\\RHtestsV4.r")NOTA: SE HACE EL PROCESO EN EL RHTEST
StartGUI()
FindU(InSeries = 'dprht.csv', MissingValueCode = "NA", output = 'homogenized')
datosO<-read.csv("D:/Documentos Tesis/tablas_bani_ocoa_depuradas/TABLAS_ESTACIONES_DEPURADAS/rhtest_tablas/dprht_U.csv", header=T)
hom<-read.csv("D:/Documentos Tesis/tablas_bani_ocoa_depuradas/TABLAS_ESTACIONES_DEPURADAS/rhtest_tablas/dprht_U.csv", header=T)
head(hom)
hom
d$inverse <- (hom$V9*lam + 1)^(1/lam) - 0.01
hom<-read.csv("D:/Documentos Tesis/tablas_bani_ocoa_depuradas/TABLAS_ESTACIONES_DEPURADAS/rhtest_tablas/dprht_U.csv", header=T)
head(hom)
lam
d$inverse <- (hom$V9*lam + 1)^(1/lam) - 0.01
d$inverse
summary(d$V4-d$inverse)
hom <- read.table('d$inverse')
hom <- read.table(d$inverse)
as.data.frame(d$inverse)
write.csv(d$inverse, 'transinversa.csv')
NOTA: SE CREO UN TABLA LLAMADA ocoa_para_amelia_final.csv
ocoa<-read.table(file="clipboard",sep="\t",head=TRUE) NOTA: AL ARCHIVO ocoa_para_amelia_final.csv SE LE ASIGNO EL NOMBRE DE ocoa
ocoa
hist(ocoa$P)
system.time(
  a.out <- amelia(
    x = ocoa, 
    m = 1000, 
    ts = "Year", 
    polytime = 1))
system.time(
  a.out.m <- Reduce(
    "+",
    lapply(
      a.out$imputations,
      function(ocoa) ocoa[,'Precipitacion']
    )
  )/length(a.out$imputations)
)
head(a.out.m)
a.out.m
hist(a.out.m)
qqnorm(a.out.m)
shapiro.test(a.out.m)
getwd()
setwd("C:/Users/admin/Documents")
write.csv(a.out.m, 'Ocoa_amelia_rellenado.csv')
