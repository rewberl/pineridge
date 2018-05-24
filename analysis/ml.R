library(raster)
library(rgdal)
library(rgeos)
library(dismo)
library(randomForest)
# source("https://bioconductor.org/biocLite.R")
# biocLite("multtest")
library(multtest)
library(pRF)
library(e1071)
library(corrplot)
library(ggplot2)

datapath = readLines("datapath.txt")


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

# Load TIGER/Line state boundaries, subset to SD and scopulorum var. states
states = readOGR(dsn=paste0(datapath, "TIGER Line/tl_2017_us_state"),
                 layer="tl_2017_us_state", verbose=F)
states = spTransform(states, CRSobj = proj4string(predictorsRM))
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




# Dimensionality reduction through PCA
pca = prcomp(data[,-c(1:2)], scale.=T)
pcadata = as.data.frame(prcomp(data[,-c(1:2)], scale.=T)$x)

windows()
plot(pca)

windows()
plot(PC1 ~ PC2, pcadata, type="n")
text(PC1 ~ PC2, pcadata,
     labels=data$PA, cex=0.5, col=rgb(0, 0, 0, 0.25))


# Maxent
me = maxent(pcadata[,c("PC1", "PC2", "PC3")], data[,1])
me

windows()
plot(me)
response(me)




# Random Forest
rfall = tuneRF(data[,c(3:43)], data[,1], mtryStart=12,
            plot=F, doBest=T, importance=T)
print(rfall)

## Without potentially confounded or irrelevant elevation variables
rf = tuneRF(data[,c(3:29,34:43)], data[,1], mtryStart=12,
            plot=F, doBest=T, importance=T)

print(rf)
rfimp = importance(rf)[order(importance(rf)[,1], decreasing=T),]
rfimp
saveRDS(rf, "./results/rf.RDS")
write.table(rfimp, "./results/rfimp.txt", sep=",", quote=FALSE, row.names=T)

rfcrossval = rfcv(data[,c(3:29,34:43)], data[,1])
rfcrossval[1:2]

prf = pRF(data[,1], data[,c(3:29,34:43)], 50, mtry=rf$mtry, type=rf$type,
          ntree=rf$ntree)

# rfTWP = tuneRF(dataTWP[,c(3:29,34:43)], factor(dataTWP[,1]), plot=F, doBest=T, importance=T)
rfTWP = tuneRF(dataTWP[,c(3:29,34:43)], dataTWP[,1], plot=F, doBest=T, importance=T)

print(rfTWP)
importance(rfTWP)[order(importance(rfTWP)[,1], decreasing=T),]
saveRDS(rfTWP, "./results/rfTWP.RDS")

rfTWPcrossval = rfcv(dataTWP[,c(3:29,34:43)], dataTWP[,1])
rfTWPcrossval[1:2]

prfTWP = pRF(dataTWP[,1], dataTWP[,c(3:29,34:43)], 50, mtry=rfTWP$mtry, type=rfTWP$type,
          ntree=rfTWP$ntree)



# Support Vector Machines
## Without highly correlated variables
tsv1 = tune.svm(data[,c(3:31,33:36)], factor(data[,1]), kernel="linear",
                cost=2^(seq(-5,15,2)), gamma=2^(seq(-15,3,2)))
sv1 = svm(data[,c(3:31,33:36)], factor(data[,1]), kernel="linear",
          cost=tsv1$best.parameters$cost, gamma=tsv1$best.parameters$gamma)
# sv1 = best.svm(data[,c(3:5,7:10,14,17:18,21:22)], factor(data[,1]), kernel="linear",
#                cost=2^(seq(-5,15,2)), gamma=2^(seq(-15,3,2)))

sv1

tsv2 = tune.svm(data[,c(3:31,33:36)], factor(data[,1]), kernel="radial",
                cost=2^(seq(-5,15,2)), gamma=2^(seq(-15,3,2)))
sv2 = svm(data[,c(3:31,33:36)], factor(data[,1]), kernel="radial",
          cost=tsv2$best.parameters$cost, gamma=tsv2$best.parameters$gamma)
# sv2 = best.svm(data[,c(3:5,7:10,14,17:18,21:22)], factor(data[,1]), kernel="radial",
#                cost=2^(seq(-5,15,2)), gamma=2^(seq(-15,3,2)))

sv2





# Predictions from Random Forest
pres = predict(predictorsSD, rf, progress="text")
pres = mask(pres, sd)

pr4 = predict(predictors4SD, rf, progress="text")
pr4 = mask(pr4, sd)
pr8 = predict(predictors8SD, rf, progress="text")
pr8 = mask(pr8, sd)

presTWP = predict(predictorsSD, rfTWP, progress="text")
presTWP = mask(presTWP, sd)

pr4TWP = predict(predictors4SD, rfTWP, progress="text")
pr4TWP = mask(pr4TWP, sd)
pr8TWP = predict(predictors8SD, rfTWP, progress="text")
pr8TWP = mask(pr8TWP, sd)

writeRaster(pres, "./results/pres.tif", "GTiff", overwrite=T, progress="text")
writeRaster(pr4, "./results/pr4.tif", "GTiff", overwrite=T, progress="text")
writeRaster(pr8, "./results/pr8.tif", "GTiff", overwrite=T, progress="text")
writeRaster(presTWP, "./results/presTWP.tif", "GTiff", overwrite=T, progress="text")
writeRaster(pr4TWP, "./results/pr4TWP.tif", "GTiff", overwrite=T, progress="text")
writeRaster(pr8TWP, "./results/pr8TWP.tif", "GTiff", overwrite=T, progress="text")


# Predict occurrence at 2017 planting locations based on full Random Forest
plantTWP = read.csv(paste0(datapath, "TWP Trees.csv"), header=T)

plantpres = extract(pres, plantTWP[,c("X","Y")], method="simple", df=T)
mean(plantpres[,2])
sd(plantpres[,2])

plantpr4 = extract(pr4, plantTWP[,c("X","Y")], method="simple", df=T)
mean(plantpr4[,2])
sd(plantpr4[,2])

plantpr8 = extract(pr8, plantTWP[,c("X","Y")], method="simple", df=T)
mean(plantpr8[,2])
sd(plantpr8[,2])



plantTWP = cbind(plantTWP[,c("X","Y")], dataTWP[dataTWP$PA == 1,-c(1:2)])
names(plantTWP)[1:2] = c("x", "y")
plantTWP = rasterFromXYZ(plantTWP, crs=proj4string(predictorsRM))

plantTWP = SpatialPointsDataFrame(coords=plantTWP[,c("X","Y")], data=dataTWP[dataTWP$PA == 1,-c(1:2)],
                                  proj4string = CRS(proj4string(predictorsRM)))
prplant = predict(rasterize(plantTWP, x=coordinates(plantTWP)[,1], y=coordinates(plantTWP)[,2],
                            field=colnames(plantTWP)),
                  rf, progress="text")

