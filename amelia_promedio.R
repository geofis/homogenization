#Calcular imputaciones
system.time(
  a.out <- amelia(
    x = africa, #Matriz de datos
    m = 1000, #Numero de imputaciones
    cs = "country", #Nombre de la columna de agrupamiento (cross section variable)
    ts = "year", #Nombre de la columna temporal
    logs = "gdp_pc" #La variable gdp_pc se transforma logaritmicamente, pero esto no aplica en datos ya transformados
  )
)
#   user  system elapsed 
#  4.924   0.060   4.936 

#Promedio de imputaciones
system.time(
  a.out.m <- Reduce(
    "+",
    lapply(
      a.out$imputations,
      function(x) x[,c('gdp_pc','trade')]
      )
    )/length(a.out$imputations)
)
#  user  system elapsed 
# 0.284   0.000   0.286 

#Equipo: lshw -short
# ruta H/W       Dispositivo  Clase          Descripción
# =======================================================
#                             system         Computer
# /0                          bus            Motherboard
# /0/0                        memory         7937MiB Memoria de sistema
# /0/1                        processor      Intel(R) Core(TM) i7-3610QM CPU @ 2.30GHz
# /0/100                      bridge         3rd Gen Core processor DRAM Controller
# /0/100/1                    bridge         Xeon E3-1200 v2/3rd Gen Core processor PCI Express Root Port