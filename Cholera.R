#install.packages("sp")
#install.packages("rgeos")
#install.packages("maptools")
#install.packages("spatstat")
#install.packages("maps")
#install.packages("mapproj")
#install.packages("spc")
#install.packages("epicalc")
#install.packages("splancs")
#install.packages("RColorBrewer")
#install.packages("classInt")

library(sp)
library(maptools)
library(spatstat)
library(maps)
library(MASS)
library(mapproj)
library(spc)
library(epicalc)
library(splancs)
library(gpclib)
library(RColorBrewer) # creates nice color schemes
library(classInt)     # finds class intervals for continuous variables



#####################################
#######  Reading databases  #########
#####################################

cholera_incidence<-read.csv("IncidenceRateYear.csv")
social_determinant<-read.csv("predictorsmapsPeru.csv")

########### Legends  ##############

legend=c("1.Amazonas","2.Ancash","3.Apurimac","4.Arequipa","5.Ayacucho","6.Cajamarca","7.Cusco","8.Huancavelica","9.Huanuco","10.Ica","11.Junin","12.La Libertad","13.Lambayeque","14.Lima","15.Loreto","16.Madre De Dios","17.Moquegua","18.Pasco","19.Piura","20.Puno","21.San Martin","22.Tacna","23.Tumbes","24.Ucayali")


#####################################
#######  Reading Maps ###############
#####################################

mapperu<-readShapeSpatial("perucholera.shp")
summary(mapperu)

quartz(width=10, height=6, pointsize=10)
plot(mapperu)
str(mapperu)
mapa = mapperu[11]




# Cholera 1991
plotvar <- mapperu@data$Incidenc_1
nclr <- 5
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)
quartz(width=6, height=6, pointsize=10)
plot(mapperu, col=colcode)
plot(mapperu, col=colcode, add=T)
title(main="Cholera incidence rates 1991",
      sub="Quantile (Equal-Frequency) Class Intervals")
text(cholera_incidence$coord_x, cholera_incidence$coord_y,1:24)
legend(locator(1), legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), cex=0.7, bty="n")
legend(locator(1), legend=legend, cex=0.7, bty="n")


# Cholera 1992
plotvar <- mapperu@data$Incidenc_2
nclr <- 5
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)
quartz(width=6, height=6, pointsize=10)
plot(mapperu, col=colcode)
plot(mapperu, col=colcode, add=T)
title(main="Cholera incidence rates 1992",
      sub="Quantile (Equal-Frequency) Class Intervals")
text(cholera_incidence$coord_x, cholera_incidence$coord_y,1:24)
legend(locator(1), legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), cex=0.7, bty="n")
legend(locator(1), legend=legend, cex=0.7, bty="n")



# Cholera 1993
plotvar <- mapperu@data$Incidenc_3
nclr <- 5
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)
quartz(width=6, height=6, pointsize=10)
plot(mapperu, col=colcode)
plot(mapperu, col=colcode, add=T)
title(main="Cholera incidence rates 1993",
      sub="Quantile (Equal-Frequency) Class Intervals")
text(cholera_incidence$coord_x, cholera_incidence$coord_y,1:24)
legend(locator(1), legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), cex=0.7, bty="n")
legend(locator(1), legend=legend, cex=0.7, bty="n")



# Cholera 1994
plotvar <- mapperu@data$Incidenc_4
nclr <- 5
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)
quartz(width=6, height=6, pointsize=10)
plot(mapperu, col=colcode)
plot(mapperu, col=colcode, add=T)
title(main="Cholera incidence rates 1994",
      sub="Quantile (Equal-Frequency) Class Intervals")
text(cholera_incidence$coord_x, cholera_incidence$coord_y,1:24)
legend(locator(1), legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), cex=0.7, bty="n")
legend(locator(1), legend=legend, cex=0.7, bty="n")


# Cholera 1995
plotvar <- mapperu@data$Incidenc_5
nclr <- 5
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)
quartz(width=6, height=6, pointsize=10)
plot(mapperu, col=colcode)
plot(mapperu, col=colcode, add=T)
title(main="Cholera incidence rates 1995",
      sub="Quantile (Equal-Frequency) Class Intervals")
text(cholera_incidence$coord_x, cholera_incidence$coord_y,1:24)
legend(locator(1), legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), cex=0.7, bty="n")
legend(locator(1), legend=legend, cex=0.7, bty="n")



# Cholera 1996
plotvar <- mapperu@data$Incidenc_6
nclr <- 5
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)
quartz(width=6, height=6, pointsize=10)
plot(mapperu, col=colcode)
plot(mapperu, col=colcode, add=T)
title(main="Cholera incidence rates 1996",
      sub="Quantile (Equal-Frequency) Class Intervals")
text(cholera_incidence$coord_x, cholera_incidence$coord_y,1:24)
legend(locator(1), legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), cex=0.7, bty="n")
legend(locator(1), legend=legend, cex=0.7, bty="n")


# Cholera 1997
plotvar <- mapperu@data$Incidenc_7
nclr <- 5
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)
quartz(width=6, height=6, pointsize=10)
plot(mapperu, col=colcode)
plot(mapperu, col=colcode, add=T)
title(main="Cholera incidence rates 1997",
      sub="Quantile (Equal-Frequency) Class Intervals")
text(cholera_incidence$coord_x, cholera_incidence$coord_y,1:24)
legend(locator(1), legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), cex=0.7, bty="n")
legend(locator(1), legend=legend, cex=0.7, bty="n")





#########################################################
#######  Reading Maps Social determinants ###############
#########################################################

socdetmapperu<-readShapeSpatial("socialdetperu.shp")
summary(socdetmapperu)

########### Legends  ##############

legend=c("1.Amazonas","2.Ancash","3.Apurimac","4.Arequipa","5.Ayacucho","6.Cajamarca","7.Cusco","8.Huancavelica","9.Huanuco","10.Ica","11.Junin","12.La Libertad","13.Lambayeque","14.Lima","15.Loreto","16.Madre De Dios","17.Moquegua","18.Pasco","19.Piura","20.Puno","21.San Martin","22.Tacna","23.Tumbes","24.Ucayali")


# Water supply
plotvar <- socdetmapperu@data$socialde_1
nclr <- 5
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)
quartz(width=6, height=6, pointsize=10)
plot(socdetmapperu, col=colcode)
plot(socdetmapperu, col=colcode, add=T)
title(main="Water supply",
      sub="Quantile (Equal-Frequency) Class Intervals")
text(social_determinant$coord_x, social_determinant$coord_y,1:24)
legend(locator(1), legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), cex=0.7, bty="n")
legend(locator(1), legend=legend, cex=0.7, bty="n")


# Access to sanitary facilities
plotvar <- 100- socdetmapperu@data$socialde_2
nclr <- 5
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)
quartz(width=6, height=6, pointsize=10)
plot(socdetmapperu, col=colcode)
plot(socdetmapperu, col=colcode, add=T)
title(main="Access to sanitary facilities",
      sub="Quantile (Equal-Frequency) Class Intervals")
text(social_determinant$coord_x, social_determinant$coord_y,1:24)
legend(locator(1), legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), cex=0.7, bty="n")
legend(locator(1), legend=legend, cex=0.7, bty="n")







plotvar <- cholera_incidence$y1991
nclr <- 1
plotclr <- brewer.pal(nclr,"PuOr")
plotclr <- plotclr[nclr:1] # reorder colors
class <- classIntervals(plotvar, nclr, style="equal")
colcode <- findColours(class, plotclr)

quartz(width=10, height=6, pointsize=10)
plot(mapperu)
points(cholera_incidence$coord_x, cholera_incidence$coord_y, pch=16, col=colcode, cex=2)
points(cholera_incidence$coord_x, cholera_incidence$coord_y, cex=2)
title("Oregon Climate Station Data -- Annual Temperature",
      sub="Equal-Interval Class Intervals")
legend(-117, 44, legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), cex=0.6, bty="n")


