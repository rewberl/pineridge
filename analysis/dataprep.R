library(raster)
library(rgdal)
library(rgeos)
library(ncdf4)
library(spatialEco)
library(sf)

datapath = readLines("datapath.txt")


# Load POLARIS data for soils
plist = list.files(paste0(datapath, "POLARIS/"), pattern="*_mean_0_5.tif$", full.names=T)
pol = stack(plist)
rm(plist)

# Load TIGER/Line state boundaries, subset to SD and scopulorum var. states
states = readOGR(dsn=paste0(datapath, "TIGER Line/tl_2017_us_state"),
                 layer="tl_2017_us_state", verbose=F)
states = spTransform(states, CRSobj = proj4string(pol))
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

# Crop POLARIS data to RM
pol = crop(pol, rm, progress="text")
writeRaster(pol, paste0(datapath, "POLARIS/POLARIS_resample.grd"), "raster", overwrite=T, progress="text")


# Load Normals data
rlist = list.files(paste0(datapath, "AdaptWest/NA_NORM_8110_Bioclim_netCDF/"), pattern="*.nc$", full.names=T)
aw = stack(rlist)
proj4string(aw) = "+proj=lcc +lat_1=49.0 +lat_2=77.0 +lat_0=0.0 +lon_0=-95.0 +x_0=0.0 +y_0=0.0 +ellps=WGS84 +units=m +no_defs"
rm(rlist)

# Reproject Normals data to POLARIS
aw = projectRaster(aw, pol, method="bilinear", progress="text")
writeRaster(aw, paste0(datapath, "AdaptWest/norm_resample.grd"), "raster", overwrite=T, progress="text")


# Load RCP4.5 data
r4list = list.files(paste0(datapath, "AdaptWest/NA_ENSEMBLE_rcp45_2050s_Bioclim_netCDF/"), pattern="*.nc$", full.names=T)
rcp45 = stack(r4list)
proj4string(rcp45) = "+proj=lcc +lat_1=49.0 +lat_2=77.0 +lat_0=0.0 +lon_0=-95.0 +x_0=0.0 +y_0=0.0 +ellps=WGS84 +units=m +no_defs"
rm(r4list)

# Reproject RCP4.5 data to POLARIS
rcp45 = projectRaster(rcp45, pol, method="bilinear", progress="text")
writeRaster(rcp45, paste0(datapath, "AdaptWest/rcp45_resample.grd"), "raster", overwrite=T, progress="text")


# Load RCP8.5 data
r8list = list.files(paste0(datapath, "AdaptWest/NA_ENSEMBLE_rcp85_2050s_Bioclim_netCDF/"), pattern="*.nc$", full.names=T)
rcp85 = stack(r8list)
proj4string(rcp85) = "+proj=lcc +lat_1=49.0 +lat_2=77.0 +lat_0=0.0 +lon_0=-95.0 +x_0=0.0 +y_0=0.0 +ellps=WGS84 +units=m +no_defs"
rm(r8list)

# Reproject RCP8.5 data to POLARIS
rcp85 = projectRaster(rcp85, pol, method="bilinear", progress="text")
writeRaster(rcp85, paste0(datapath, "AdaptWest/rcp85_resample.grd"), "raster", overwrite=T, progress="text")


# Load SRTM data for elevation
elist = list.files(paste0(datapath, "SRTM/"), pattern="*.tif$", full.names=T)
elist = lapply(1:length(elist), function(x) {raster(elist[x])})
elist$fun = mean
elist$progress = "text"
srtm = do.call(mosaic, elist)
names(srtm) = "elev"
rm(elist)

# Reproject SRTM data to POLARIS
rasterOptions(chunksize = 1e+04, maxmemory = 1e+06)
srtm = projectRaster(srtm, pol, method="bilinear", progress="text")

# Calculate slope and aspect from elevation
terr = terrain(srtm$elev, c("slope", "aspect"), unit="radians", progress="text")
srtm = stack(c(srtm, terr))
rm(terr)
names(srtm) = c("elev", "slope", "aspect")
## Recode circular variable aspect as sine and cosine and remove
srtm$aspect.cos = cos(srtm$aspect)
srtm$aspect.sin = sin(srtm$aspect)
srtm = dropLayer(srtm, 3)

# Calculate HLI from elevation and combine
hli = hli(srtm$elev)
names(hli) = "hli"
srtm = stack(c(srtm, hli))
rm(hli)
writeRaster(srtm, paste0(datapath, "SRTM/SRTM_resample.grd"), "raster", overwrite=T, progress="text")


# Load WorldClim data for wind speed and solar radiation
wlist = list.files("Q:/Research/Data/WorldClim 2/", pattern="wind", full.names=T)
wind = stack(wlist)
slist = list.files("Q:/Research/Data/WorldClim 2/", pattern="srad", full.names=T)
srad = stack(slist)
rm(wlist, slist)

# Create mean WorldClim annuals and combine
wind = mean(wind, na.rm=T)
srad = mean(srad, na.rm=T)
wclim = stack(wind, srad)
names(wclim) = c("wind", "srad")
rm(wind, srad)

# Reproject WorldClim data to POLARIS
wclim = crop(wclim, extent(-180,0,0,100), progress="text")
wclim = projectRaster(wclim, pol, method="bilinear", progress="text")
writeRaster(wclim, paste0(datapath, "WorldClim 2/wclim_resample.grd"), "raster", overwrite=T, progress="text")


# Stack raster layers for training
predictorsRM = stack(aw, srtm, pol, wclim)
predictorsSD = crop(predictorsRM, sd, progress="text")
writeRaster(predictorsRM, paste0(datapath, "predictors_RM.grd"), "raster", overwrite=T, progress="text")
writeRaster(predictorsSD, paste0(datapath, "predictors_SD.grd"), "raster", overwrite=T, progress="text")

# Stack raster layers for prediction
predictors4RM = stack(rcp45, srtm, pol, wclim)
predictors4SD = crop(predictors4RM, sd, progress="text")
predictors8RM = stack(rcp85, srtm, pol, wclim)
predictors8SD = crop(predictors8RM, sd, progress="text")
writeRaster(predictors4RM, paste0(datapath, "predictors4_RM.grd"), "raster", overwrite=T, progress="text")
writeRaster(predictors4SD, paste0(datapath, "predictors4_SD.grd"), "raster", overwrite=T, progress="text")
writeRaster(predictors8RM, paste0(datapath, "predictors8_RM.grd"), "raster", overwrite=T, progress="text")
writeRaster(predictors8SD, paste0(datapath, "predictors8_SD.grd"), "raster", overwrite=T, progress="text")


# Remove all constituent layers
rm(aw, rcp45, rcp85, srtm, pol, wclim)


# Load occurrence data, subset to scopulorum var. range
occur = read.csv(paste0(datapath, "var.scopulorum.csv"), header=T, row.names=1)
occurRM = SpatialPointsDataFrame(coords=occur[,c("x","y")], data=occur,
                                 proj4string = CRS(proj4string(predictorsRM)))
occurRM = occurRM[!is.na(over(occurRM, as(rm, "SpatialPolygons"))),]
occurSD = occurRM[!is.na(over(occurRM, as(sd, "SpatialPolygons"))),]

# Load TWP survival data
occurTWP = read.csv(paste0(datapath, "TWP Trees.csv"), header=T)
occurTWP = SpatialPointsDataFrame(coords=occurTWP[,c("X","Y")], data=occurTWP,
                                  proj4string = CRS(proj4string(predictorsRM)))

# Extract data at occurrence points for each variable
data = cbind(occurRM$PA,
             extract(predictorsRM, occurRM, buffer=80, fun=mean, df=T))
names(data)[1] = "PA"
write.csv(data, "./analysis/data.csv", row.names=F)

dataTWP = rbind(cbind(PA = 1,
                      extract(predictorsSD, occurTWP, method="simple", df=T)),
                cbind(PA = 0,
                      extract(predictorsSD, occurSD[occurSD$PA == 0,], buffer=80, fun=mean, df=T)))
write.csv(dataTWP, "./analysis/dataTWP.csv", row.names=F)


# Load and reproject shaded relief map
relief = raster(paste0(datapath, "GTOPO30/srgrayi1kml.tif"))
relief = projectRaster(relief, pol, method="bilinear", progress="text")
writeRaster(relief, paste0(datapath, "GTOPO30/relief_resample.grd"), "raster", overwrite=T, progress="text")


# Load and reproject 2001 burn area data
blist = list.files(paste0(datapath, "BAECV/"), pattern="*.tif$", full.names=T)
burn = stack(blist)
rm(blist)
names(burn) = c("bc", "bd", "bp")
burn = projectRaster(burn, pol, method="bilinear", progress="text")
writeRaster(burn, paste0(datapath, "BAECV/BAECV_resample.grd"), "raster", overwrite=T, progress="text")


# Extract data at occurrence points for burn data
databurn = cbind(occurRM$PA,
                 extract(burn$bp, occurRM, buffer=80, fun=mean, df=T))
names(databurn)[1] = "PA"
databurn$PA = factor(databurn$PA)
databurn$bp = databurn$bp / 100
write.csv(databurn, "./analysis/databurn.csv", row.names=F)


