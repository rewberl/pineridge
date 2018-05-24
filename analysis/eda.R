library(raster)
library(rgdal)
library(rgeos)
library(car)
library(corrplot)
library(qicharts2)
library(randtests)
library(ggplot2)
library(reshape2)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(rasterVis)
library(extrafont)

datapath = readLines("datapath.txt")

# Run once to import fonts
# font_import()

# Load fonts
loadfonts()
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.23/bin/gswin64c.exe")


# Load prepared predictor data
predictorsRM = stack(paste0(datapath, "predictors_RM.grd"))
predictorsSD = stack(paste0(datapath, "predictors_SD.grd"))
predictors4RM = stack(paste0(datapath, "predictors4_RM.grd"))
predictors4SD = stack(paste0(datapath, "predictors4_SD.grd"))
predictors8RM = stack(paste0(datapath, "predictors8_RM.grd"))
predictors8SD = stack(paste0(datapath, "predictors8_SD.grd"))

# Load extracted data
data = read.csv("./analysis/data.csv", header=T)
dataTWP = read.csv("./analysis/dataTWP.csv", header=T)
databurn = read.csv("./analysis/databurn.csv", header=T)

# Load TIGER/Line state boundaries, subset to SD and scopulorum var. states
states = readOGR(dsn=paste0(datapath, "TIGER Line/tl_2017_us_state"),
                 layer="tl_2017_us_state", verbose=F)
states = spTransform(states, CRSobj=proj4string(predictorsRM))
sd = states[states$NAME == "South Dakota",]
rm = states[states$NAME == "North Dakota" |
              states$NAME == "South Dakota" |
              states$NAME == "Montana" |
              states$NAME == "Wyoming" |
              states$NAME == "Nebraska" |
              states$NAME == "Colorado" |
              states$NAME == "Utah",]
## Crop range extent at western edge of Wyoming, just beyond eastern edge of South Dakota,
##  southern edge of Nebraska, northern border of US
rm = crop(rm, extent(-111, -96.43, 40, 49))

# Load TIGER/Line AI areas, subset Pine Ridge
aiannh = readOGR(dsn=paste0(datapath, "TIGER Line/tl_2017_us_aiannh"),
                 layer="tl_2017_us_aiannh", verbose=F)
aiannh = spTransform(aiannh, CRSobj=proj4string(predictorsRM))
aisd = crop(aiannh, sd)
aisd = aisd[!grepl("Trust Land", aisd$NAMELSAD),]
pr = aiannh[aiannh$NAME == "Pine Ridge",]




# Run charts by elevation
q = qic(ID, elev, data=data[data$PA == 1,])
qchart = ggplot() +
  geom_point(aes(x=ID, y=elev), data=data[data$PA == 1,], size=0.1, alpha=0.5) +
  geom_line(aes(x=ID, y=elev), data=data[data$PA == 1,], size=0.1, alpha=0.5) +
  geom_segment(aes(x=min(ID)-20, xend=max(ID)+20, y=summary(q)$CL, yend=summary(q)$CL),
               data=data[data$PA == 1,], linetype="longdash", color="#88CCEE") +
  annotate("text", x=max(data[data$PA == 1,]$ID)+80, y=summary(q)$CL,
            label=round(summary(q)$CL, 1), size=3, family="Open Sans") +
  labs(x="Index", y="Elevation (m)", title="Run Chart of Elevation in Positive Occurrence Data") +
  theme_minimal() +
  theme(text=element_text(family="Open Sans", color="#22211D"))
ggsave("./results/plots/runchart.pdf", qchart, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/runchart.pdf", outfile="./results/plots/runchart_embed.pdf")
ggsave("./results/plots/runchart.jpg", qchart, "jpeg", width=11, height=5, units="in")

summary(q)
runs.test(data[data$PA == 1,]$elev, plot=F)


qTWP = qic(ID, elev, data=dataTWP[dataTWP$PA == 1,])
qTWPchart = ggplot() +
  geom_point(aes(x=ID, y=elev), data=dataTWP[dataTWP$PA == 1,], size=0.1, alpha=0.5) +
  geom_line(aes(x=ID, y=elev), data=dataTWP[dataTWP$PA == 1,], size=0.1, alpha=0.5) +
  geom_segment(aes(x=min(ID)-2, xend=max(ID)+2, y=summary(qTWP)$CL, yend=summary(qTWP)$CL),
               data=dataTWP[dataTWP$PA == 1,], linetype="longdash", color="#88CCEE") +
  annotate("text", x=max(dataTWP[dataTWP$PA == 1,]$ID)+5, y=summary(qTWP)$CL,
           label=round(summary(qTWP)$CL, 1), size=3, family="Open Sans") +
  labs(x="Index", y="Elevation (m)", title="Run Chart of Elevation in Survival Data on Pine Ridge") +
  theme_minimal() +
  theme(text=element_text(family="Open Sans", color="#22211D"))
ggsave("./results/plots/runchartTWP.pdf", qTWPchart, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/runchartTWP.pdf", outfile="./results/plots/runchartTWP_embed.pdf")
ggsave("./results/plots/runchartTWP.jpg", qTWPchart, "jpeg", width=11, height=5, units="in")

summary(qTWP)
runs.test(dataTWP[dataTWP$PA == 1,]$elev, plot=F)


# Test difference in presence by burn probability
table(databurn$PA)

burnlogit = glm(PA ~ bp, data=databurn, family="binomial")
burnlogit2 = glm(PA ~ bp + I(bp^2), data=databurn, family="binomial")
summary(burnlogit)
summary(burnlogit2)
# pchisq(burnlogit$deviance, burnlogit$df.residual)
# anova(burnlogit, test="Chisq")

burnplot = ggplot(aes(x=bp, y=PA), data=databurn) +
  geom_point(shape=16, alpha=0.05) +
  geom_smooth(method="glm", method.args=list(family="binomial"),
              formula=y ~ x + I(x^2), color="#117733") +
  labs(x="Burn Probability", y="Occurrence Probability of P. ponderosa var. scopulorum",
       title="Logistic Fit of Tree Occurrence by Burned Area")

gb = ggplot_build(burnplot)
gbx = gb$data[[2]]$x[which(diff(sign(diff(gb$data[[2]]$y)))==-2)+1]
gby = gb$data[[2]]$y[which(diff(sign(diff(gb$data[[2]]$y)))==-2)+1]

burnplot = burnplot +
  geom_segment(aes(x=gbx, xend=gbx, y=0, yend=gby),
               linetype="longdash", color="#117733") +
  annotate("text", x=gbx, y=gby+0.04, label=round(gbx, 3), family="Open Sans") +
  theme_minimal() +
  theme(text=element_text(family="Open Sans", color="#22211D"))
ggsave("./results/plots/burnplot.pdf", burnplot, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/burnplot.pdf", outfile="./results/plots/burnplot_embed.pdf")
ggsave("./results/plots/burnplot.jpg", burnplot, "jpeg", width=11, height=3, units="in")




# Exploratory analysis

## Correlations
corm = cor(data[,-c(2)])
cormlow = corm
cormlow[lower.tri(cormlow)] = NA
diag(cormlow) = NA

cormplot = ggplot() +
  geom_tile(aes(x=Var1, y=Var2, fill=value), data=melt(cormlow)) +
  ylim(rev(levels(melt(cormlow)$Var2))) + xlim(levels(melt(cormlow)$Var1)) +
  scale_fill_distiller(type="div", palette="PuOr", direction=1, na.value="white",
                       limit=c(-1,1), name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(text=element_text(family="Open Sans", color="#22211D", size=11),
        axis.text.x=element_text(angle=-90, vjust=0.5, hjust=0),
        legend.position=c(0.8, 0.6)) +
  labs(x=NULL, y=NULL) +
  coord_fixed()

ggsave("./results/plots/corr.pdf", cormplot, "pdf", width=8.5, height=8.5, units="in")
embed_fonts("./results/plots/corr.pdf", outfile="./results/plots/corr_embed.pdf")
ggsave("./results/plots/corr.jpg", cormplot, "jpeg", width=8.5, height=8.5, units="in")


cortab = as.data.frame(as.table(cor(data[,-2])))
cortab = cortab[order(abs(cortab[,3]), decreasing=T),]
cortab = cortab[cortab$Var1 != cortab$Var2 & !duplicated(cortab$Freq),]
head(cortab, 30)
tail(cortab, 30)


pacor = cortab[cortab$Var1 == "PA" | cortab$Var2 == "PA",]
pacor = pacor[,c(1,3)]
names(pacor) = c("Variable", "Correlation")
pacor
write.table(pacor, "./results/pacor.txt", sep=",", quote=FALSE, row.names=F)



# Dimensionality reduction through PCA
pca = prcomp(data[,-c(1:2)], scale.=T)
pcadata = as.data.frame(prcomp(data[,-c(1:2)], scale.=T)$x)

windows()
plot(pca)

windows()
plot(PC1 ~ PC2, pcadata, type="n")
text(PC1 ~ PC2, pcadata,
     labels=data$PA, cex=0.5, col=rgb(0, 0, 0, 0.25))



# Climate change in South Dakota and Pine Ridge

presclimSD = mask(dropLayer(predictorsSD, 28:41), sd)
presclimPR = crop(presclimSD, pr)
presclimPR = mask(presclimPR, pr)
pr4climSD = mask(dropLayer(predictors4SD, 28:41), sd)
pr4climPR = crop(pr4climSD, pr)
pr4climPR = mask(pr4climPR, pr)
pr8climSD = mask(dropLayer(predictors8SD, 28:41), sd)
pr8climPR = crop(pr8climSD, pr)
pr8climPR = mask(pr8climPR, pr)

SDchange4 = (pr4climSD - presclimSD) / presclimSD
SDchange8 = (pr8climSD - presclimSD) / presclimSD
names(SDchange4) = names(predictorsSD)[1:27]
names(SDchange8) = names(predictorsSD)[1:27]

writeRaster(SDchange4, "./results/SDchange4.tif", "GTiff", overwrite=T, progress="text")
writeRaster(SDchange8, "./results/SDchange8.tif", "GTiff", overwrite=T, progress="text")

PRchange4 = (pr4climPR - presclimPR) / presclimPR
PRchange8 = (pr8climPR - presclimPR) / presclimPR
names(PRchange4) = names(predictorsSD)[1:27]
names(PRchange8) = names(predictorsSD)[1:27]

writeRaster(PRchange4, "./results/PRchange4.tif", "GTiff", overwrite=T, progress="text")
writeRaster(PRchange8, "./results/PRchange8.tif", "GTiff", overwrite=T, progress="text")


pdf("./results/plots/SDchange4.pdf", width=8.5, height=11, family="Open Sans")
levelplot(SDchange4, maxpixels=100000, layout=c(4, 7), at=seq(-1.2, 1.2, length.out=50),
          main="South Dakota\nRelative Change in Climatic Variables from Present Conditions to RCP4.5 in 2050",
          xlab=NULL, ylab=NULL, scales=list(draw=FALSE))
dev.off()
embed_fonts("./results/plots/SDchange4.pdf", outfile="./results/plots/SDchange4_embed.pdf")

pdf("./results/plots/SDchange8.pdf", width=8.5, height=11, family="Open Sans")
levelplot(SDchange8, maxpixels=100000, layout=c(4, 7), at=seq(-1.2, 1.2, length.out=50),
          main="South Dakota\nRelative Change in Climatic Variables from Present Conditions to RCP8.5 in 2050",
          xlab=NULL, ylab=NULL, scales=list(draw=FALSE))
dev.off()
embed_fonts("./results/plots/SDchange8.pdf", outfile="./results/plots/SDchange8_embed.pdf")


pdf("./results/plots/PRchange4.pdf", width=8.5, height=11, family="Open Sans")
levelplot(PRchange4, maxpixels=100000, layout=c(4, 7), at=seq(-1.2, 1.2, length.out=50),
          main="Pine Ridge Reservation and Trust Land\nRelative Change in Climatic Variables from Present Conditions to RCP4.5 in 2050",
          xlab=NULL, ylab=NULL, scales=list(draw=FALSE))
dev.off()
embed_fonts("./results/plots/PRchange4.pdf", outfile="./results/plots/PRchange4_embed.pdf")

pdf("./results/plots/PRchange8.pdf", width=8.5, height=11, family="Open Sans")
levelplot(PRchange8, maxpixels=100000, layout=c(4, 7), at=seq(-1.2, 1.2, length.out=50),
          main="Pine Ridge Reservation and Trust Land\nRelative Change in Climatic Variables from Present Conditions to RCP8.5 in 2050",
          xlab=NULL, ylab=NULL, scales=list(draw=FALSE))
dev.off()
embed_fonts("./results/plots/PRchange8.pdf", outfile="./results/plots/PRchange8_embed.pdf")




pdf("./results/plots/allchange.pdf", width=8.5, height=11, family="Open Sans")

dev.off()
embed_fonts("./results/plots/allchange.pdf", outfile="./results/plots/allchange_embed.pdf")
