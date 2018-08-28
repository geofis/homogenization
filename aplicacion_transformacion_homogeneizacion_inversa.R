d <- read.csv('/home/jr/Documentos/tendenciashomogeneizacion/datos_ocoa_para_RHT.csv', header = F)
lam <- powerTransform(d$V4+0.01)$lambda
d$trans <- bcPower(d$V4+0.01, lam)
dev.new();hist(d$trans)
dev.new();qqnorm(d$trans)
shapiro.test(d$trans)
#homogeneizar
dprht <-d[,-4]
write.table(dprht, '/home/jr/Descargas/b2/out2/dprht.csv', row.names = F, col.names = F, sep = ',')
source('/home/jr/Documentos/proyecto_FONDOCyT/bibliografia/homogeneizacion_relleno_datos_series_temporales_climaticas_MICHELA/Estadistica/R/RHtestsV4.r/RHtestsV4.r')
FindU(InSeries = '/home/jr/Descargas/b2/out2/dprht.csv', MissingValueCode = "NA", output = "/home/jr/Descargas/b2/out2/")
#No hay changepoints, por lo tanto, la serie homogeneizada mostrara los mismos valores que la transformada que se uso como insumo en FindU
hom <- read.table('/home/jr/Descargas/b2/out2/_U.dat')
#La transformacion inversa obtiene los valores originales
d$inversa <- (hom$V9*lam + 1)^(1/lam) - 0.01
summary(d$V4-d$inversa)
