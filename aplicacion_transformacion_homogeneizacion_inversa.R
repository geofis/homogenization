library(car)
## Not run:
source("RHtestsV4_20130719.r") #http://etccdi.pacificclimate.org/software.shtml
## End(Not run)
tdir <- tempdir()
setwd(tdir)
ghsource <- 'https://raw.githubusercontent.com/geofis/homogenization/master/'
fname <- 'datos_ocoa_para_RHT.csv'
download.file(paste0(ghsource, 'sampledata/', fname), fname)
d <- read.csv(fname, header = F)
head(d)
lam <- powerTransform(d$V4+0.01)$lambda
d$trans <- bcPower(d$V4+0.01, lam)
dev.new();hist(d$trans)
dev.new();qqnorm(d$trans)
shapiro.test(d$trans)
#Find changepoints
dprht <-d[,-4]#Prepare the data
write.table(dprht, 'dprht.csv', row.names = F, col.names = F, sep = ',')
FindU(InSeries = 'dprht.csv', MissingValueCode = "NA", output = 'homogenized')
#In this case there are no changepoints, so in the homogenized series will show the same values as the input series, which is also the transformed one
hom <- read.table('homogenized_U.dat')
#The inverse transformation shows the same values as the original series
d$inverse <- (hom$V9*lam + 1)^(1/lam) - 0.01
summary(d$V4-d$inverse)
