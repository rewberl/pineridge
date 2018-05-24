library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(rasterVis)
library(extrafont)
# library(maps)
# library(mapproj)
# library(ggalt)

datapath = readLines("datapath.txt")


# Map theme function
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Open Sans", color="#22211d"),
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      # panel.grid.major=element_line(color="#ebebe5", size=0.2),
      panel.grid.major=element_blank(),
      # panel.grid.minor=element_line(color = "#ebebe5", size=0.2),
      panel.grid.minor=element_blank(),
      panel.background=element_rect(fill="transparent", color=NA), 
      panel.border=element_blank(),
      panel.spacing=unit(c(-.1,0.2,.2,0.2), "cm"),
      plot.background=element_rect(fill="transparent", color=NA),
      plot.title=element_text(hjust=0.5, color="#4e4d47", face="bold"),
      plot.subtitle=element_text(hjust=0.5, color="#4e4d47", 
                                 margin=margin(b=-0.1, t=-0.1, l=2, unit="cm"), 
                                 debug=F),
      plot.margin=unit(c(.5,.5,.2,.5), "cm"),
      plot.caption=element_text(size=6, 
                                hjust=0.92, 
                                margin=margin(t=0.2, b=0, unit = "cm"), 
                                color = "#939184"),
      legend.background=element_rect(fill="transparent", color=NA),
      legend.position=c(0.5, 0.03),
      legend.text.align=0,
      legend.text=element_text(size=7, hjust=0, color="#4e4d47"),
      legend.title=element_text(size=8),
      ...
    )
}

# Run once to import fonts
# font_import()

# Load fonts
loadfonts()
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.23/bin/gswin64c.exe")


# Load RM predictor layer
predictorsRM = stack(paste0(datapath, "predictors_RM.grd"))

# Load ML output layers
pres = raster("./results/pres.tif")
pr4 = raster("./results/pr4.tif")
pr8 = raster("./results/pr8.tif")
presTWP = raster("./results/presTWP.tif")
pr4TWP = raster("./results/pr4TWP.tif")
pr8TWP = raster("./results/pr8TWP.tif")


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

# Load TIGER/Line AI areas, subset Pine Ridge and Rosebud
aiannh = readOGR(dsn=paste0(datapath, "TIGER Line/tl_2017_us_aiannh"),
                 layer="tl_2017_us_aiannh", verbose=F)
aiannh = spTransform(aiannh, CRSobj=proj4string(predictorsRM))
aisd = crop(aiannh, sd)
aisd = aisd[!grepl("Trust Land", aisd$NAMELSAD),]
pr = aiannh[aiannh$NAME == "Pine Ridge",]
rose = aiannh[aiannh$NAMELSAD == "Rosebud Indian Reservation",]
rosetr = aiannh[aiannh$NAMELSAD == "Rosebud Off-Reservation Trust Land",]
prr = aiannh[aiannh$NAME == "Pine Ridge" | aiannh$NAME == "Rosebud",]
## Designate planting area bounds
oglala = extent(c(-103.001, -102.55, 42.99939, 43.3))
porcupine = extent(c(-102.55, -102.1108, 43.138, 43.43861))

# Load TIGER/Line county data, merge to create extent of reservations and trust lands
counties = readOGR(dsn=paste0(datapath, "TIGER Line/tl_2017_us_county"),
                   layer="tl_2017_us_county", verbose=F)
counties = spTransform(counties, CRSobj=proj4string(predictorsRM))
counties = counties[counties$STATEFP == 46,]
countiesR = counties[counties$NAME == "Todd" | counties$NAME == "Mellette" |
                     counties$NAME == "Tripp" | counties$NAME == "Gregory" |
                     counties$NAME == "Lyman",]
prrbounds = gUnaryUnion(raster::bind(prr, countiesR))

# Load state GIS data on parks, subset Bear Butte
parks = readOGR(dsn=paste0(datapath, "South Dakota"),
                layer="SouthDakota_ParksAndRecreationAreas", verbose=F)
parks = spTransform(parks, CRSobj=proj4string(predictorsRM))
bb = parks[parks$ParkName == "Bear Butte" | parks$ParkName == "Bear Butte Lake",]
bbbounds = extent(-103.5528, -103.3010, 44.3860, 44.5657)

# Load National Park and National Forest boundaries
nps = readOGR(dsn=paste0(datapath, "NPS"), layer="nps_boundary", verbose=F)
nps = spTransform(nps, CRSobj=proj4string(predictorsRM))
nps = nps[nps$STATE == "SD",]
nps = crop(nps, sd)

usfs = readOGR(dsn=paste0(datapath, "USFS"), layer="S_USA.AdministrativeForest", verbose=F)
usfs = spTransform(usfs, CRSobj=proj4string(predictorsRM))
usfs = crop(usfs, sd)

nparks = bind(nps, usfs)

prbadl = nps[nps$PARKNAME == "Badlands",]
prbadl = crop(prbadl, pr)

# Load TIGER/Line road data, subset counties within Pine Ridge and within Meade
#   County to vicinity of Bear Butte
roads = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46_prisecroads"),
                layer="tl_2017_46_prisecroads", verbose=F)
roads = spTransform(roads, CRSobj=proj4string(predictorsRM))
# roadsNeb = readOGR(dsn=paste0(datapath, "TIGER Line/Nebraska/tl_2017_31_prisecroads"),
#                    layer="tl_2017_31_prisecroads", verbose=F)
# roadsNeb = spTransform(roadsNeb, CRSobj=proj4string(predictorsRM))
# roadsNeb = crop(roadsNeb, prrbounds)
roadsOL = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46102_roads"),
                  layer="tl_2017_46102_roads", verbose=F)
roadsOL = spTransform(roadsOL, CRSobj=proj4string(predictorsRM))
roadsJ = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46071_roads"),
                 layer="tl_2017_46071_roads", verbose=F)
roadsJ = spTransform(roadsJ, CRSobj=proj4string(predictorsRM))
roadsJ = crop(roadsJ, prrbounds)
roadsB = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46007_roads"),
                 layer="tl_2017_46007_roads", verbose=F)
roadsB = spTransform(roadsB, CRSobj=proj4string(predictorsRM))
roadsB = crop(roadsB, prrbounds)
# roadsS = readOGR(dsn=paste0(datapath, "TIGER Line/Nebraska/tl_2017_31161_roads"),
#                  layer="tl_2017_31161_roads", verbose=F)
# roadsS = spTransform(roadsS, CRSobj=proj4string(predictorsRM))
# roadsS = crop(roadsS, prrbounds)
roadsTo = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46121_roads"),
                 layer="tl_2017_46121_roads", verbose=F)
roadsTo = spTransform(roadsTo, CRSobj=proj4string(predictorsRM))
roadsMel = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46095_roads"),
                 layer="tl_2017_46095_roads", verbose=F)
roadsMel = spTransform(roadsMel, CRSobj=proj4string(predictorsRM))
roadsTr = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46123_roads"),
                 layer="tl_2017_46123_roads", verbose=F)
roadsTr = spTransform(roadsTr, CRSobj=proj4string(predictorsRM))
roadsG = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46053_roads"),
                 layer="tl_2017_46053_roads", verbose=F)
roadsG = spTransform(roadsG, CRSobj=proj4string(predictorsRM))
roadsL = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46085_roads"),
                 layer="tl_2017_46085_roads", verbose=F)
roadsL = spTransform(roadsL, CRSobj=proj4string(predictorsRM))
roadsMea = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46093_roads"),
                 layer="tl_2017_46093_roads", verbose=F)
roadsMea = spTransform(roadsMea, CRSobj=proj4string(predictorsRM))
roadsMea = crop(roadsMea, bbbounds)

# Load TIGER/Line water features, subset counties within Pine Ridge and within
#   Meade County to vicinity of Bear Butte
areawaterOL = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46102_areawater"),
                      layer="tl_2017_46102_areawater", verbose=F)
areawaterOL = spTransform(areawaterOL, CRSobj=proj4string(predictorsRM))
areawaterOL = crop(areawaterOL, prrbounds)
linearwaterOL = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46102_linearwater"),
                        layer="tl_2017_46102_linearwater", verbose=F)
linearwaterOL = spTransform(linearwaterOL, CRSobj=proj4string(predictorsRM))
linearwaterOL = crop(linearwaterOL, prrbounds)
areawaterJ = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46071_areawater"),
                     layer="tl_2017_46071_areawater", verbose=F)
areawaterJ = spTransform(areawaterJ, CRSobj=proj4string(predictorsRM))
areawaterJ = crop(areawaterJ, prrbounds)
linearwaterJ = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46071_linearwater"),
                       layer="tl_2017_46071_linearwater", verbose=F)
linearwaterJ = spTransform(linearwaterJ, CRSobj=proj4string(predictorsRM))
linearwaterJ = crop(linearwaterJ, prrbounds)
areawaterB = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46007_areawater"),
                     layer="tl_2017_46007_areawater", verbose=F)
areawaterB = spTransform(areawaterB, CRSobj=proj4string(predictorsRM))
areawaterB = crop(areawaterB, prrbounds)
linearwaterB = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46007_linearwater"),
                       layer="tl_2017_46007_linearwater", verbose=F)
linearwaterB = spTransform(linearwaterB, CRSobj=proj4string(predictorsRM))
linearwaterB = crop(linearwaterB, prrbounds)
# areawaterS = readOGR(dsn=paste0(datapath, "TIGER Line/Nebraska/tl_2017_31161_areawater"),
#                      layer="tl_2017_31161_areawater", verbose=F)
# areawaterS = spTransform(areawaterS, CRSobj=proj4string(predictorsRM))
# areawaterS = crop(areawaterS, prrbounds)
# linearwaterS = readOGR(dsn=paste0(datapath, "TIGER Line/Nebraska/tl_2017_31161_linearwater"),
#                        layer="tl_2017_31161_linearwater", verbose=F)
# linearwaterS = spTransform(linearwaterS, CRSobj=proj4string(predictorsRM))
# linearwaterS = crop(linearwaterS, prrbounds)
areawaterTo = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46121_areawater"),
                       layer="tl_2017_46121_areawater", verbose=F)
areawaterTo = spTransform(areawaterTo, CRSobj=proj4string(predictorsRM))
linearwaterTo = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46121_linearwater"),
                         layer="tl_2017_46121_linearwater", verbose=F)
linearwaterTo = spTransform(linearwaterTo, CRSobj=proj4string(predictorsRM))
areawaterMel = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46095_areawater"),
                       layer="tl_2017_46095_areawater", verbose=F)
areawaterMel = spTransform(areawaterMel, CRSobj=proj4string(predictorsRM))
linearwaterMel = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46095_linearwater"),
                         layer="tl_2017_46095_linearwater", verbose=F)
linearwaterMel = spTransform(linearwaterMel, CRSobj=proj4string(predictorsRM))
areawaterTr = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46123_areawater"),
                       layer="tl_2017_46123_areawater", verbose=F)
areawaterTr = spTransform(areawaterTr, CRSobj=proj4string(predictorsRM))
linearwaterTr = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46123_linearwater"),
                         layer="tl_2017_46123_linearwater", verbose=F)
linearwaterTr = spTransform(linearwaterTr, CRSobj=proj4string(predictorsRM))
areawaterG = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46053_areawater"),
                       layer="tl_2017_46053_areawater", verbose=F)
areawaterG = spTransform(areawaterG, CRSobj=proj4string(predictorsRM))
linearwaterG = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46053_linearwater"),
                         layer="tl_2017_46053_linearwater", verbose=F)
linearwaterG = spTransform(linearwaterG, CRSobj=proj4string(predictorsRM))
areawaterL = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46085_areawater"),
                       layer="tl_2017_46085_areawater", verbose=F)
areawaterL = spTransform(areawaterL, CRSobj=proj4string(predictorsRM))
linearwaterL = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46085_linearwater"),
                         layer="tl_2017_46085_linearwater", verbose=F)
linearwaterL = spTransform(linearwaterL, CRSobj=proj4string(predictorsRM))
areawaterMea = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46093_areawater"),
                     layer="tl_2017_46093_areawater", verbose=F)
areawaterMea = spTransform(areawaterMea, CRSobj=proj4string(predictorsRM))
areawaterMea = crop(areawaterMea, bbbounds)
linearwaterMea = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46093_linearwater"),
                       layer="tl_2017_46093_linearwater", verbose=F)
linearwaterMea = spTransform(linearwaterMea, CRSobj=proj4string(predictorsRM))
linearwaterMea = crop(linearwaterMea, bbbounds)

# Other TIGER Line layers
places = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46_place"), layer="tl_2017_46_place", verbose=F)
places = spTransform(places, CRSobj=proj4string(predictorsRM))

sdplaces = data.frame(places$NAME, as.data.frame(coordinates(places)))
sdplaces = sdplaces[sdplaces[,1] == "Sioux Falls" | sdplaces[,1] == "Rapid City" |
                    sdplaces[,1] == "Aberdeen" | sdplaces[,1] == "Brookings" |
                    sdplaces[,1] == "Watertown" | sdplaces[,1] == "Mitchell" |
                    sdplaces[,1] == "Yankton" | sdplaces[,1] == "Pierre" |
                    sdplaces[,1] == "Huron" | sdplaces[,1] == "Vermillion" |
                    sdplaces[,1] == "Spearfish" | sdplaces[,1] == "Sturgis" |
                    sdplaces[,1] == "Pine Ridge" | sdplaces[,1] == "Rosebud",]
sdplaces[,1] = as.character(sdplaces[,1])
sdplaces = rbind(sdplaces, c("McLaughlin", -100.811389, 45.813056),
                 c("North Eagle Butte", -101.231389, 45.006389),
                 c("Sisseton", -97.049167, 45.663333),
                 c("Fort\nThompson", -99.429167+0.075, 44.060833),
                 c("Lower\nBrule", -99.580833-0.075, 44.073889),
                 c("Wagner", -98.294722, 43.080278),
                 c("Flandreau", -96.596389-0.075, 44.047778))
sdplaces = data.frame(label = as.character(sdplaces[,1]), long = as.numeric(sdplaces[,2]), lat=as.numeric(sdplaces[,3]))
names(sdplaces) = c("label", "long", "lat")

prplaces = crop(places, pr)
prplaces = data.frame(prplaces$NAME, as.data.frame(coordinates(prplaces)))
prplaces[,1] = as.character(prplaces[,1])
prplaces = rbind(prplaces, c("Red Cloud Renewable\nEnergy Center", -102.6558043, 43.1267002),
                 c("Prairie Wind\nCasino &\nHotel", -103.015034+0.1, 43.1788703))
prplaces[5,1] = "Manderson-\nWhite Horse Creek"
prplaces = data.frame(label = as.character(prplaces[,1]), long = as.numeric(prplaces[,2]), lat=as.numeric(prplaces[,3]))

ogplaces = crop(places, oglala)
ogplaces = data.frame(ogplaces$NAME, as.data.frame(coordinates(ogplaces)))
ogplaces[,1] = as.character(ogplaces[,1])
ogplaces = rbind(ogplaces, c("Red Cloud Renewable\nEnergy Center", -102.6558043, 43.1267002),
                 c("Prairie Wind\nCasino &\nHotel", -103.015034+0.045, 43.1788703))
ogplaces = data.frame(label = as.character(ogplaces[,1]), long = as.numeric(ogplaces[,2]), lat=as.numeric(ogplaces[,3]))
ogplaces[2,2] = ogplaces[2,2]-0.025

porplaces = crop(places, porcupine)
porplaces = data.frame(porplaces$NAME, as.data.frame(coordinates(porplaces)))
porplaces[,1] = as.character(porplaces[,1])
porplaces[2,1] = "Manderson-\nWhite Horse Creek"
porplaces = data.frame(label = as.character(porplaces[,1]), long = as.numeric(porplaces[,2]), lat=as.numeric(porplaces[,3]))

rplaces = crop(places, bind(rose, rosetr))
rplaces = data.frame(rplaces$NAME, as.data.frame(coordinates(rplaces)))
rplaces[4,2] = rplaces[4,2]+0.1
rplaces[5,2] = rplaces[5,2]+0.05
rplaces[7,3] = rplaces[7,3]-0.015
rplaces[9,3] = rplaces[9,3]+0.015
rplaces = rplaces[-c(3,12),]
names(rplaces) = c("label", "long", "lat")

# nebplaces = readOGR(dsn=paste0(datapath, "TIGER Line/Nebraska/tl_2017_31_place"), layer="tl_2017_31_place", verbose=F)
# nebplaces = spTransform(nebplaces, CRSobj=proj4string(predictorsRM))
# nebplaces = crop(nebplaces, pr)
# nebplaces = data.frame(nebplaces$NAME, as.data.frame(coordinates(nebplaces)))
# nebplaces = data.frame(label = as.character(nebplaces[,1]), long = as.numeric(nebplaces[,2]), lat=as.numeric(nebplaces[,3]))
# names(nebplaces) = c("label", "long", "lat")

bbplaces = crop(places, bbbounds)
bbplaces = data.frame(bbplaces$NAME, as.data.frame(coordinates(bbplaces)))
# bbplaces[,1] = as.character(bbplaces[,1])
# bbplaces[3,1] = "Blucksberg\nMountain"
# bbplaces = data.frame(label = as.character(bbplaces[,1]), long = as.numeric(bbplaces[,2]), lat=as.numeric(bbplaces[,3]))
names(bbplaces) = c("label", "long", "lat")

# lmarks = readOGR(dsn=paste0(datapath, "TIGER Line/South Dakota/tl_2017_46_pointlm"), layer="tl_2017_46_pointlm", verbose=F)


# Load occurrence data, subset to scopulorum var. range
occur = read.csv(paste0(datapath, "var.scopulorum.csv"), header=T, row.names=1)
occurRM = SpatialPointsDataFrame(coords=occur[,c("x","y")], data=occur,
                                 proj4string = CRS(proj4string(predictorsRM)))
occurRM = occurRM[!is.na(over(occurRM, as(rm, "SpatialPolygons"))),]
occurSD = occurRM[!is.na(over(occurRM, as(sd, "SpatialPolygons"))),]
occurPR = occurSD[!is.na(over(occurSD, as(pr, "SpatialPolygons"))),]

# Load TWP survival data
occurTWP = read.csv(paste0(datapath, "TWP Trees.csv"), header=T)
occurTWP = SpatialPointsDataFrame(coords=occurTWP[,c("X","Y")], data=occurTWP,
                                  proj4string = CRS(proj4string(predictorsRM)))

# Load elevation relief layer
relief = raster(paste0(datapath, "GTOPO30/relief_resample.grd"))
reliefSD = crop(relief, sd)
reliefSD = mask(reliefSD, sd)
reliefPR = crop(reliefSD, pr)
reliefPR = mask(reliefPR, pr)




# PLOTS

# RM occurrence points overlaid on shaded relief map
rmlabels = data.frame(rm$STUSPS, as.data.frame(coordinates(rm)))
names(rmlabels) = c("label", "long", "lat")
rmlabels[rmlabels$label == "CO", "long"] = -104

plotRMoccur = gplot(relief, maxpixels=10000000) +
  geom_raster(aes(alpha=value)) +
  scale_alpha(name="", range=c(0.6, 0), guide=F) + 
  geom_polygon(data=rm, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  geom_polygon(data=pr, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  geom_polygon(data=rose, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  geom_polygon(data=bb, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  geom_text(data=rmlabels, aes(x=long, y=lat, label=label), family="Open Sans",
            color="black", size=4) +
  geom_point(data=as.data.frame(occurRM[occurRM$PA == 1,]), aes(x=x, y=y),
             color="#117733", shape=16, size=1, alpha=0.5) +
  coord_quickmap() +
  theme_map() +
  labs(x=NULL, y=NULL, 
       title="Rocky Mountain Front & Northern Great Plains", 
       subtitle="Present Occurrence of Pinus ponderosa var. scopulorum", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, GTOPO30, TIGER/Line")

# windows()
# plotRMoccur
# saveRDS(plotRMoccur, "./results/plots/plotRMoccur.RDS")
ggsave("./results/plots/plotRMoccur.pdf", plotRMoccur, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotRMoccur.pdf", outfile="./results/plots/plotRMoccur_embed.pdf")


# PR occurrence points overlaid on shaded relief map
plotPRoccur = gplot(reliefPR, maxpixels=10000000) +
  geom_raster(aes(alpha=value)) +
  scale_alpha(name="", range=c(0.6, 0), na.value=0, guide=F) +
  geom_text(data=prplaces, aes(x=long, y=lat, label=label), family="Open Sans",
            color="black", size=3, lineheight=0.9) +
  geom_point(data=as.data.frame(occurPR[occurPR$PA == 1,]), aes(x=x, y=y),
             color="#117733", shape=16, size=2, alpha=0.5) +
  geom_point(data=as.data.frame(occurTWP), aes(x=mean(occurTWP$X), y=mean(occurTWP$Y)),
             color="#DDCC77", shape=16, size=2, alpha=0.5) +
  coord_quickmap() +
  theme_map() +
  labs(x=NULL, y=NULL, 
       title="Pine Ridge Reservation and Trust Land", 
       subtitle="Present Occurrence of Pinus ponderosa var. scopulorum\n(Gold Point Indicates 2017 Planting)", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, GTOPO30, TIGER/Line")

# windows()
# plotPRoccur
# saveRDS(plotPRoccur, "./results/plots/plotPRoccur.RDS")
ggsave("./results/plots/plotPRoccur.pdf", plotPRoccur, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotPRoccur.pdf", outfile="./results/plots/plotPRoccur_embed.pdf")




# PRESENT

# Present habitat suitability in South Dakota
plotSDpres = gplot(pres, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1", midpoint=0.5,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=pr, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  geom_polygon(data=rose, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  geom_polygon(data=bb, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  coord_quickmap() +
  theme_map() +
  labs(x=NULL, y=NULL, 
       title="South Dakota", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under Present Conditions", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotSDpres
# saveRDS(plotSDpres, "./results/plots/plotSDpres.RDS")
ggsave("./results/plots/plotSDpres.pdf", plotSDpres, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotSDpres.pdf", outfile="./results/plots/plotSDpres_embed.pdf")


# With roads, settlements, reservations
plotSDpres_lines = gplot(pres, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1", midpoint=0.5,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_path(data=roads, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  geom_polygon(data=nparks, aes(x=long, y=lat, group=group), fill=NA, color="#117733") +
  geom_polygon(data=aisd, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  geom_text(data=sdplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=2) +
  coord_quickmap() +
  theme_map() +
  labs(x=NULL, y=NULL, 
       title="South Dakota", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under Present Conditions", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line, NPS, USFS")

# windows()
# plotSDpres_lines
# saveRDS(plotSDpres_lines, "./results/plots/plotSDpres_lines.RDS")
ggsave("./results/plots/plotSDpres_lines.pdf", plotSDpres_lines, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotSDpres_lines.pdf", outfile="./results/plots/plotSDpres_lines_embed.pdf")


# Present habitat suitability in Pine Ridge
prespr = crop(pres, pr)
prespr = mask(prespr, pr)

plotPRpres = gplot(prespr, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(prespr), na.rm=T) + max(values(prespr), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=areawaterOL, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterJ, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterB, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  # geom_polygon(data=areawaterS, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=as(oglala, "SpatialPolygons"), aes(x=long, y=lat, group=group), fill=NA, color="#DDCC77") +
  geom_polygon(data=as(porcupine, "SpatialPolygons"), aes(x=long, y=lat, group=group), fill=NA, color="#DDCC77") +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Pine Ridge Reservation and Trust Land", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under Present Conditions", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotPRpres
# saveRDS(plotPRpres, "./results/plots/plotPRpres.RDS")
ggsave("./results/plots/plotPRpres.pdf", plotPRpres, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotPRpres.pdf", outfile="./results/plots/plotPRpres_embed.pdf")


## With roads, rivers, settlements, Badlands NP
plotPRpres_lines = gplot(prespr, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(prespr), na.rm=T) + max(values(prespr), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=areawaterOL, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterJ, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterB, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  # geom_polygon(data=areawaterS, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_path(data=linearwaterOL, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=linearwaterJ, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=linearwaterB, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  # geom_path(data=linearwaterS, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=roadsOL, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_path(data=roadsJ, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_path(data=roadsB, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  # geom_path(data=roadsS, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_polygon(data=prbadl, aes(x=long, y=lat, group=group), fill=NA, color="#117733") +
  geom_polygon(data=as(oglala, "SpatialPolygons"), aes(x=long, y=lat, group=group), fill=NA, color="#DDCC77") +
  geom_polygon(data=as(porcupine, "SpatialPolygons"), aes(x=long, y=lat, group=group), fill=NA, color="#DDCC77") +
  geom_text(data=prplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=3.5, lineheight=0.9) +
  # geom_text(data=nebplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=3.5, lineheight=0.9) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Pine Ridge Reservation and Trust Land", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under Present Conditions", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotPRpres_lines
# saveRDS(plotPRpres_lines, "./results/plots/plotPRpres_lines.RDS")
ggsave("./results/plots/plotPRpres_lines.pdf", plotPRpres_lines, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotPRpres_lines.pdf", outfile="./results/plots/plotPRpres_lines_embed.pdf")


# Present habitat suitability in Pine Ridge planting areas
presog = crop(prespr, oglala)
prespor = crop(prespr, porcupine)

plotOgpres = gplot(presog, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(presog), na.rm=T) + max(values(presog), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=crop(areawaterOL, oglala), aes(x=long, y=lat, group=group), fill="white", color=NA) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Oglala Planting Area in Pine Ridge Reservation", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under Present Conditions", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotOgpres
# saveRDS(plotOgpres, "./results/plots/plotOgpres.RDS")
ggsave("./results/plots/plotOgpres.pdf", plotOgpres, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotOgpres.pdf", outfile="./results/plots/plotOgpres_embed.pdf")

plotPorpres = gplot(prespor, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(prespor), na.rm=T) + max(values(prespor), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=crop(areawaterOL, porcupine), aes(x=long, y=lat, group=group), fill="white", color=NA) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Porcupine Planting Area in Pine Ridge Reservation", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under Present Conditions", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotPorpres
# saveRDS(plotPorpres, "./results/plots/plotPorpres.RDS")
ggsave("./results/plots/plotPorpres.pdf", plotPorpres, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotPorpres.pdf", outfile="./results/plots/plotPorpres_embed.pdf")


## With roads, rivers, settlements
plotOgpres_lines = gplot(presog, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(presog), na.rm=T) + max(values(presog), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=crop(areawaterOL, oglala), aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_path(data=crop(linearwaterOL, oglala), aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=crop(roadsOL, oglala), aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_text(data=ogplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=3.5, lineheight=0.9) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Oglala Planting Area in Pine Ridge Reservation", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under Present Conditions", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotOgpres_lines
# saveRDS(plotOgpres_lines, "./results/plots/plotOgpres_lines.RDS")
ggsave("./results/plots/plotOgpres_lines.pdf", plotOgpres_lines, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotOgpres_lines.pdf", outfile="./results/plots/plotOgpres_lines_embed.pdf")

plotPorpres_lines = gplot(prespor, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(prespor), na.rm=T) + max(values(prespor), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=crop(areawaterOL, porcupine), aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_path(data=crop(linearwaterOL, porcupine), aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=crop(roadsOL, porcupine), aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_text(data=porplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=3.5, lineheight=0.9) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Porcupine Planting Area in Pine Ridge Reservation", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under Present Conditions", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotPorpres_lines
# saveRDS(plotPorpres_lines, "./results/plots/plotPorpres_lines.RDS")
ggsave("./results/plots/plotPorpres_lines.pdf", plotPorpres_lines, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotPorpres_lines.pdf", outfile="./results/plots/plotPorpres_lines_embed.pdf")



# Present habitat suitability in Rosebud
presr = crop(pres, countiesR)
presr = mask(presr, countiesR)

plotRpres = gplot(presr, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(presr), na.rm=T) + max(values(presr), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=areawaterTo, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterMel, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterTr, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterG, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterL, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=rose, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  # geom_polygon(data=rosetr, aes(x=long, y=lat, group=group), fill="white", color=NA, alpha=0.25) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Rosebud Reservation and Trust Land", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under Present Conditions", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotRpres
# saveRDS(plotRpres, "./results/plots/plotRpres.RDS")
ggsave("./results/plots/plotRpres.pdf", plotRpres, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotRpres.pdf", outfile="./results/plots/plotRpres_embed.pdf")


## With roads, rivers, settlements
plotRpres_lines = gplot(presr, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(presr), na.rm=T) + max(values(presr), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=areawaterTo, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterMel, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterTr, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterG, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterL, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_path(data=linearwaterTo, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=linearwaterMel, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=linearwaterTr, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=linearwaterG, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=linearwaterL, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=roadsTo, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_path(data=roadsMel, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_path(data=roadsTr, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_path(data=roadsG, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_path(data=roadsL, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_polygon(data=rose, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  # geom_polygon(data=rosetr, aes(x=long, y=lat, group=group), fill="white", color=NA, alpha=0.4) +
  geom_text(data=rplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=3.5, lineheight=0.9) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Rosebud Reservation and Trust Land", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under Present Conditions", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotRpres_lines
# saveRDS(plotRpres_lines, "./results/plots/plotRpres_lines.RDS")
ggsave("./results/plots/plotRpres_lines.pdf", plotRpres_lines, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotRpres_lines.pdf", outfile="./results/plots/plotRpres_lines_embed.pdf")


# Present habitat suitability in Bear Butte
presbb = crop(pres, bbbounds)

plotBBpres = gplot(presbb, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(presbb), na.rm=T) + max(values(presbb), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=areawaterMea, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=bb, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Bear Butte", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under Present Conditions", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotBBpres
# saveRDS(plotBBpres, "./results/plots/plotBBpres.RDS")
ggsave("./results/plots/plotBBpres.pdf", plotBBpres, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotBBpres.pdf", outfile="./results/plots/plotBBpres_embed.pdf")


## With roads, rivers, settlements
plotBBpres_lines = gplot(presbb, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(presbb), na.rm=T) + max(values(presbb), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=areawaterMea, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_path(data=linearwaterMea, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=roadsMea, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_polygon(data=bb, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  geom_text(data=bbplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=3.5, lineheight=0.9) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Bear Butte", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under Present Conditions", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotBBpres_lines
# saveRDS(plotBBpres_lines, "./results/plots/plotBBpres_lines.RDS")
ggsave("./results/plots/plotBBpres_lines.pdf", plotBBpres_lines, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotBBpres_lines.pdf", outfile="./results/plots/plotBBpres_lines_embed.pdf")






# RCP4.5

# RCP4.5 habitat suitability in South Dakota
plotSD4 = gplot(pr4, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1", midpoint=0.5,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=pr, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  geom_polygon(data=rose, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  geom_polygon(data=bb, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  coord_quickmap() +
  theme_map() +
  labs(x=NULL, y=NULL, 
       title="South Dakota", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability in 2050 under RCP4.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest, SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotSD4
# saveRDS(plotSD4, "./results/plots/plotSD4.RDS")
ggsave("./results/plots/plotSD4.pdf", plotSD4, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotSD4.pdf", outfile="./results/plots/plotSD4_embed.pdf")


# With roads, settlements, reservations
plotSD4_lines = gplot(pr4, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1", midpoint=0.5,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_path(data=roads, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  geom_polygon(data=nparks, aes(x=long, y=lat, group=group), fill=NA, color="#117733") +
  geom_polygon(data=aisd, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  geom_text(data=sdplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=2) +
  coord_quickmap() +
  theme_map() +
  labs(x=NULL, y=NULL, 
       title="South Dakota", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability in 2050 under RCP4.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest, SRTM, POLARIS, WorldClim2, TIGER/Line, NPS, USFS")

# windows()
# plotSD4_lines
# saveRDS(plotSD4_lines, "./results/plots/plotSD4_lines.RDS")
ggsave("./results/plots/plotSD4_lines.pdf", plotSD4_lines, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotSD4_lines.pdf", outfile="./results/plots/plotSD4_lines_embed.pdf")


# RCP4.5 habitat suitability in Pine Ridge
pr4pr = crop(pr4, pr)
pr4pr = mask(pr4pr, pr)

plotPR4 = gplot(pr4pr, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr4pr), na.rm=T) + max(values(pr4pr), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=areawaterOL, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterJ, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterB, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=as(oglala, "SpatialPolygons"), aes(x=long, y=lat, group=group), fill=NA, color="#DDCC77") +
  geom_polygon(data=as(porcupine, "SpatialPolygons"), aes(x=long, y=lat, group=group), fill=NA, color="#DDCC77") +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Pine Ridge Reservation and Trust Land", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability in 2050 under RCP4.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest, SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotPR4
# saveRDS(plotPR4, "./results/plots/plotPR4.RDS")
ggsave("./results/plots/plotPR4.pdf", plotPR4, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotPR4.pdf", outfile="./results/plots/plotPR4_embed.pdf")


## With roads, rivers, settlements, Badlands NP
plotPR4_lines = gplot(pr4pr, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr4pr), na.rm=T) + max(values(pr4pr), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=areawaterOL, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterJ, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterB, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_path(data=linearwaterOL, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=linearwaterJ, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=linearwaterB, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=roadsOL, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_path(data=roadsJ, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_path(data=roadsB, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_polygon(data=prbadl, aes(x=long, y=lat, group=group), fill=NA, color="#117733") +
  geom_polygon(data=as(oglala, "SpatialPolygons"), aes(x=long, y=lat, group=group), fill=NA, color="#DDCC77") +
  geom_polygon(data=as(porcupine, "SpatialPolygons"), aes(x=long, y=lat, group=group), fill=NA, color="#DDCC77") +
  geom_text(data=prplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=3.5, lineheight=0.9) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Pine Ridge Reservation and Trust Land", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability in 2050 under RCP4.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest, SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotPR4_lines
# saveRDS(plotPR4_lines, "./results/plots/plotPR4_lines.RDS")
ggsave("./results/plots/plotPR4_lines.pdf", plotPR4_lines, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotPR4_lines.pdf", outfile="./results/plots/plotPR4_lines_embed.pdf")


# RCP4.5 habitat suitability in Pine Ridge planting areas
pr4og = crop(pr4pr, oglala)
pr4por = crop(pr4pr, porcupine)

plotOg4 = gplot(pr4og, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr4og), na.rm=T) + max(values(pr4og), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=crop(areawaterOL, oglala), aes(x=long, y=lat, group=group), fill="white", color=NA) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Oglala Planting Area in Pine Ridge Reservation", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under RCP4.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotOg4
# saveRDS(plotOg4, "./results/plots/plotOg4.RDS")
ggsave("./results/plots/plotOg4.pdf", plotOg4, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotOg4.pdf", outfile="./results/plots/plotOg4_embed.pdf")

plotPor4 = gplot(pr4por, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr4por), na.rm=T) + max(values(pr4por), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=crop(areawaterOL, porcupine), aes(x=long, y=lat, group=group), fill="white", color=NA) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Porcupine Planting Area in Pine Ridge Reservation", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under RCP4.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotPor4
# saveRDS(plotPor4, "./results/plots/plotPor4.RDS")
ggsave("./results/plots/plotPor4.pdf", plotPor4, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotPor4.pdf", outfile="./results/plots/plotPor4_embed.pdf")


## With roads, rivers, settlements
plotOg4_lines = gplot(pr4og, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr4og), na.rm=T) + max(values(pr4og), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=crop(areawaterOL, oglala), aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_path(data=crop(linearwaterOL, oglala), aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=crop(roadsOL, oglala), aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_text(data=ogplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=3.5, lineheight=0.9) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Oglala Planting Area in Pine Ridge Reservation", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under RCP4.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotOg4_lines
# saveRDS(plotOg4_lines, "./results/plots/plotOg4_lines.RDS")
ggsave("./results/plots/plotOg4_lines.pdf", plotOg4_lines, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotOg4_lines.pdf", outfile="./results/plots/plotOg4_lines_embed.pdf")

plotPor4_lines = gplot(pr4por, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr4por), na.rm=T) + max(values(pr4por), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=crop(areawaterOL, porcupine), aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_path(data=crop(linearwaterOL, porcupine), aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=crop(roadsOL, porcupine), aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_text(data=porplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=3.5, lineheight=0.9) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Porcupine Planting Area in Pine Ridge Reservation", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under RCP4.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotPor4_lines
# saveRDS(plotPor4_lines, "./results/plots/plotPor4_lines.RDS")
ggsave("./results/plots/plotPor4_lines.pdf", plotPor4_lines, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotPor4_lines.pdf", outfile="./results/plots/plotPor4_lines_embed.pdf")



# RCP4.5 habitat suitability in Rosebud
pr4r = crop(pr4, countiesR)
pr4r = mask(pr4r, countiesR)

plotR4 = gplot(pr4r, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr4r), na.rm=T) + max(values(pr4r), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=areawaterTo, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterMel, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterTr, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterG, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterL, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=rose, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  # geom_polygon(data=rosetr, aes(x=long, y=lat, group=group), fill="white", color=NA, alpha=0.25) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Rosebud Reservation and Trust Land", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability in 2050 under RCP4.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest, SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotR4
# saveRDS(plotR4, "./results/plots/plotR4.RDS")
ggsave("./results/plots/plotR4.pdf", plotR4, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotR4.pdf", outfile="./results/plots/plotR4_embed.pdf")


## With roads, rivers, settlements
plotR4_lines = gplot(pr4r, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr4r), na.rm=T) + max(values(pr4r), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=areawaterTo, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterMel, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterTr, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterG, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterL, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_path(data=linearwaterTo, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=linearwaterMel, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=linearwaterTr, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=linearwaterG, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=linearwaterL, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=roadsTo, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_path(data=roadsMel, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_path(data=roadsTr, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_path(data=roadsG, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_path(data=roadsL, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_polygon(data=rose, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  # geom_polygon(data=rosetr, aes(x=long, y=lat, group=group), fill="white", color=NA, alpha=0.4) +
  geom_text(data=rplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=3.5, lineheight=0.9) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Rosebud Reservation and Trust Land", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability in 2050 under RCP4.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest, SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotR4_lines
# saveRDS(plotR4_lines, "./results/plots/plotR4_lines.RDS")
ggsave("./results/plots/plotR4_lines.pdf", plotR4_lines, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotR4_lines.pdf", outfile="./results/plots/plotR4_lines_embed.pdf")


# RCP4.5 habitat suitability in Bear Butte
pr4bb = crop(pr4, bbbounds)

plotBB4 = gplot(pr4bb, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr4bb), na.rm=T) + max(values(pr4bb), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=areawaterMea, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=bb, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Bear Butte", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability in 2050 under RCP4.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest, SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotBB4
# saveRDS(plotBB4, "./results/plots/plotBB4.RDS")
ggsave("./results/plots/plotBB4.pdf", plotBB4, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotBB4.pdf", outfile="./results/plots/plotBB4_embed.pdf")


## With roads, rivers, settlements
plotBB4_lines = gplot(pr4bb, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr4bb), na.rm=T) + max(values(pr4bb), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=areawaterMea, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_path(data=linearwaterMea, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=roadsMea, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_polygon(data=bb, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  geom_text(data=bbplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=3.5, lineheight=0.9) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Bear Butte", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability in 2050 under RCP4.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest, SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotBB4_lines
# saveRDS(plotBB4_lines, "./results/plots/plotBB4_lines.RDS")
ggsave("./results/plots/plotBB4_lines.pdf", plotBB4_lines, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotBB4_lines.pdf", outfile="./results/plots/plotBB4_lines_embed.pdf")






# RCP8.5

# RCP8.5 habitat suitability in South Dakota
plotSD8 = gplot(pr8, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1", midpoint=0.5,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=pr, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  geom_polygon(data=rose, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  geom_polygon(data=bb, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  coord_quickmap() +
  theme_map() +
  labs(x=NULL, y=NULL, 
       title="South Dakota", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability in 2050 under RCP8.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest, SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotSD8
# saveRDS(plotSD8, "./results/plots/plotSD8.RDS")
ggsave("./results/plots/plotSD8.pdf", plotSD8, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotSD8.pdf", outfile="./results/plots/plotSD8_embed.pdf")


# With roads, settlements, reservations
plotSD8_lines = gplot(pr8, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1", midpoint=0.5,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_path(data=roads, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  geom_polygon(data=nparks, aes(x=long, y=lat, group=group), fill=NA, color="#117733") +
  geom_polygon(data=aisd, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  geom_text(data=sdplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=2) +
  coord_quickmap() +
  theme_map() +
  labs(x=NULL, y=NULL, 
       title="South Dakota", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability in 2050 under RCP8.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest, SRTM, POLARIS, WorldClim2, TIGER/Line, NPS, USFS")

# windows()
# plotSD8_lines
# saveRDS(plotSD8_lines, "./results/plots/plotSD8_lines.RDS")
ggsave("./results/plots/plotSD8_lines.pdf", plotSD8_lines, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotSD8_lines.pdf", outfile="./results/plots/plotSD8_lines_embed.pdf")


# RCP8.5 habitat suitability in Pine Ridge
pr8pr = crop(pr8, pr)
pr8pr = mask(pr8pr, pr)

plotPR8 = gplot(pr8pr, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr8pr), na.rm=T) + max(values(pr8pr), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=areawaterOL, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterJ, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterB, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=as(oglala, "SpatialPolygons"), aes(x=long, y=lat, group=group), fill=NA, color="#DDCC77") +
  geom_polygon(data=as(porcupine, "SpatialPolygons"), aes(x=long, y=lat, group=group), fill=NA, color="#DDCC77") +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Pine Ridge Reservation and Trust Land", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability in 2050 under RCP8.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest, SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotPR8
# saveRDS(plotPR8, "./results/plots/plotPR8.RDS")
ggsave("./results/plots/plotPR8.pdf", plotPR8, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotPR8.pdf", outfile="./results/plots/plotPR8_embed.pdf")


## With roads, rivers, settlements, Badlands NP
plotPR8_lines = gplot(pr8pr, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr8pr), na.rm=T) + max(values(pr8pr), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=areawaterOL, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterJ, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterB, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_path(data=linearwaterOL, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=linearwaterJ, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=linearwaterB, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=roadsOL, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_path(data=roadsJ, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_path(data=roadsB, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_polygon(data=prbadl, aes(x=long, y=lat, group=group), fill=NA, color="#117733") +
  geom_polygon(data=as(oglala, "SpatialPolygons"), aes(x=long, y=lat, group=group), fill=NA, color="#DDCC77") +
  geom_polygon(data=as(porcupine, "SpatialPolygons"), aes(x=long, y=lat, group=group), fill=NA, color="#DDCC77") +
  geom_text(data=prplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=3.5, lineheight=0.9) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Pine Ridge Reservation and Trust Land", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability in 2050 under RCP8.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest, SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotPR8_lines
# saveRDS(plotPR8_lines, "./results/plots/plotPR8_lines.RDS")
ggsave("./results/plots/plotPR8_lines.pdf", plotPR8_lines, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotPR8_lines.pdf", outfile="./results/plots/plotPR8_lines_embed.pdf")


# RCP8.5 habitat suitability in Pine Ridge planting areas
pr8og = crop(pr8pr, oglala)
pr8por = crop(pr8pr, porcupine)

plotOg8 = gplot(pr8og, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr8og), na.rm=T) + max(values(pr8og), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=crop(areawaterOL, oglala), aes(x=long, y=lat, group=group), fill="white", color=NA) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Oglala Planting Area in Pine Ridge Reservation", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under RCP8.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotOg8
# saveRDS(plotOg8, "./results/plots/plotOg8.RDS")
ggsave("./results/plots/plotOg8.pdf", plotOg8, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotOg8.pdf", outfile="./results/plots/plotOg8_embed.pdf")

plotPor8 = gplot(pr8por, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr8por), na.rm=T) + max(values(pr8por), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=crop(areawaterOL, porcupine), aes(x=long, y=lat, group=group), fill="white", color=NA) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Porcupine Planting Area in Pine Ridge Reservation", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under RCP8.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotPor8
# saveRDS(plotPor8, "./results/plots/plotPor8.RDS")
ggsave("./results/plots/plotPor8.pdf", plotPor8, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotPor8.pdf", outfile="./results/plots/plotPor8_embed.pdf")


## With roads, rivers, settlements
plotOg8_lines = gplot(pr8og, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr8og), na.rm=T) + max(values(pr8og), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=crop(areawaterOL, oglala), aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_path(data=crop(linearwaterOL, oglala), aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=crop(roadsOL, oglala), aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_text(data=ogplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=3.5, lineheight=0.9) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Oglala Planting Area in Pine Ridge Reservation", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under RCP8.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotOg8_lines
# saveRDS(plotOg8_lines, "./results/plots/plotOg8_lines.RDS")
ggsave("./results/plots/plotOg8_lines.pdf", plotOg8_lines, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotOg8_lines.pdf", outfile="./results/plots/plotOg8_lines_embed.pdf")

plotPor8_lines = gplot(pr8por, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr8por), na.rm=T) + max(values(pr8por), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=crop(areawaterOL, porcupine), aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_path(data=crop(linearwaterOL, porcupine), aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=crop(roadsOL, porcupine), aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_text(data=porplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=3.5, lineheight=0.9) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Porcupine Planting Area in Pine Ridge Reservation", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability under RCP8.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest (PRISM), SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotPor8_lines
# saveRDS(plotPor8_lines, "./results/plots/plotPor8_lines.RDS")
ggsave("./results/plots/plotPor8_lines.pdf", plotPor8_lines, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotPor8_lines.pdf", outfile="./results/plots/plotPor8_lines_embed.pdf")



# RCP8.5 habitat suitability in Rosebud
pr8r = crop(pr8, countiesR)
pr8r = mask(pr8r, countiesR)

plotR8 = gplot(pr8r, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr8r), na.rm=T) + max(values(pr8r), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=areawaterTo, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterMel, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterTr, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterG, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterL, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=rose, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  # geom_polygon(data=rosetr, aes(x=long, y=lat, group=group), fill="white", color=NA, alpha=0.25) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Rosebud Reservation and Trust Land", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability in 2050 under RCP8.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest, SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotR8
# saveRDS(plotR8, "./results/plots/plotR8.RDS")
ggsave("./results/plots/plotR8.pdf", plotR8, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotR8.pdf", outfile="./results/plots/plotR8_embed.pdf")


## With roads, rivers, settlements
plotR8_lines = gplot(pr8r, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr8r), na.rm=T) + max(values(pr8r), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=areawaterTo, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterMel, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterTr, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterG, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=areawaterL, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_path(data=linearwaterTo, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=linearwaterMel, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=linearwaterTr, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=linearwaterG, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=linearwaterL, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=roadsTo, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_path(data=roadsMel, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_path(data=roadsTr, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_path(data=roadsG, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_path(data=roadsL, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_polygon(data=rose, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  # geom_polygon(data=rosetr, aes(x=long, y=lat, group=group), fill="white", color=NA, alpha=0.4) +
  geom_text(data=rplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=3.5, lineheight=0.9) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Rosebud Reservation and Trust Land", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability in 2050 under RCP8.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest, SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotR8_lines
# saveRDS(plotR8_lines, "./results/plots/plotR8_lines.RDS")
ggsave("./results/plots/plotR8_lines.pdf", plotR8_lines, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotR8_lines.pdf", outfile="./results/plots/plotR8_lines_embed.pdf")


# RCP8.5 habitat suitability in Bear Butte
pr8bb = crop(pr8, bbbounds)

plotBB8 = gplot(pr8bb, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr8bb), na.rm=T) + max(values(pr8bb), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=areawaterMea, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_polygon(data=bb, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Bear Butte", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability in 2050 under RCP8.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest, SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotBB8
# saveRDS(plotBB8, "./results/plots/plotBB8.RDS")
ggsave("./results/plots/plotBB8.pdf", plotBB8, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotBB8.pdf", outfile="./results/plots/plotBB8_embed.pdf")


## With roads, rivers, settlements
plotBB8_lines = gplot(pr8bb, maxpixels=10000000) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="#AE1C3E", mid="#FFFAD2", high="#3D52A1",
                       midpoint=(min(values(pr8bb), na.rm=T) + max(values(pr8bb), na.rm=T))/2,
                       name="Probability of Occurrence", na.value="transparent",
                       guide=guide_legend(
                         direction="horizontal",
                         keyheight=unit(0.1, units="in"),
                         keywidth=unit(1.25/length(labels), units="in"),
                         title.position="top",
                         title.hjust=0.5,
                         label.hjust=1,
                         nrow=1,
                         byrow=T,
                         reverse=T,
                         label.position="bottom")) +
  geom_polygon(data=areawaterMea, aes(x=long, y=lat, group=group), fill="white", color=NA) +
  geom_path(data=linearwaterMea, aes(x=long, y=lat, group=group), color="white", size=0.25, alpha=0.5) +
  geom_path(data=roadsMea, aes(x=long, y=lat, group=group), color="black", size=0.25, alpha=0.75) +
  geom_polygon(data=bb, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  geom_text(data=bbplaces, aes(x=long, y=lat, label=label), family="Open Sans", fontface="bold", color="white", size=3.5, lineheight=0.9) +
  coord_quickmap() +
  theme_map() +
  theme(legend.position=c(0.5, -0.03),
        plot.caption=element_text(margin=margin(t=1, b=0, unit="cm"))) +
  labs(x=NULL, y=NULL, 
       title="Bear Butte", 
       subtitle="Random Forest Prediction of Pinus ponderosa var. scopulorum Habitat Suitability in 2050 under RCP8.5 Climate Scenario", 
       caption="Author: Richard E.W. Berl; Map Theme: Timo Grossenbacher; Font: Open Sans; Data Sources: Maguire et al. 2018, AdaptWest, SRTM, POLARIS, WorldClim2, TIGER/Line")

# windows()
# plotBB8_lines
# saveRDS(plotBB8_lines, "./results/plots/plotBB8_lines.RDS")
ggsave("./results/plots/plotBB8_lines.pdf", plotBB8_lines, "pdf", width=11, height=8.5, units="in")
embed_fonts("./results/plots/plotBB8_lines.pdf", outfile="./results/plots/plotBB8_lines_embed.pdf")










# Create and embed fonts in final PDF
allplots = ls()[substr(ls(),1,4) == "plot"]
allplots = mget(allplots)
pdf("./results/plots/allplots.pdf", width=11, height=8.5, family="Open Sans")
invisible(lapply(allplots, print))
dev.off()
embed_fonts("./results/plots/allplots.pdf", outfile="./results/plots/allplots_embed.pdf")
