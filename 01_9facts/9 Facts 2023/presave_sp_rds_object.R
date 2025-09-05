# presave the sp object
# 2022 Yang Liu

library("sp")
library("RColorBrewer")
library("ggplot2")
library("rgdal")
library("scales")

map.dir <- "C:/Users/lyhel/Dropbox/UNICEF Work/unmap/" # location of the shapefiles
map.sp.dir <- "C:/Users/lyhel/Dropbox/UNICEF Work/unmap/sp" # location to save the sp object
stopifnot(dir.exists(map.dir))
stopifnot(dir.exists(map.sp.dir))

# Read in the UN cartography polygon shapefile (no antarctica)
# 10pct
world.un <- readOGR(dsn = map.dir, layer = "un-world-2012-no-antartica-10pct")
# convert to Robinson projection (the projection preferred by UN Cartography)
proj4string(world.un) <- CRS("+proj=longlat +ellps=WGS84") # (requires sp package)
world.robin <- spTransform(world.un, CRS("+proj=robin")) # (requires rgdal package)

saveRDS(world.robin, file.path(map.sp.dir, "sp.world.robin.rds"))

# 35pct (if need higher resolution)
# Read in the UN cartography polygon shapefile (no antarctica)
world.un <- readOGR(dsn = map.dir, layer = "un-world-2012-no-antartica-35pct")
# convert to Robinson projection (the projection preferred by UN Cartography)
proj4string(world.un) <- CRS("+proj=longlat +ellps=WGS84") # (requires sp package)
world.robin <- spTransform(world.un, CRS("+proj=robin")) # (requires rgdal package)

saveRDS(world.robin, file.path(map.sp.dir, "sp.world.robin.35pt.rds"))


# Read in the Un Cartography shapefile with country/area boundaries
bnd.un <- readOGR(map.dir, "2012_UNGIWG_bnd_ln_01") 
# convert to Robinson projection 
proj4string(bnd.un) <- CRS("+proj=longlat +ellps=WGS84")
bnd <- spTransform(bnd.un, CRS("+proj=robin"))

saveRDS(bnd, file.path(map.sp.dir, "sp.bnd.rds"))

# Read in the Un Cartography shapefile with coastlines
cst.un <- readOGR(map.dir, "2012_UNGIWG_cst_ln_01")  
# convert to Robinson projection 
proj4string(cst.un) <- CRS("+proj=longlat +ellps=WGS84")
cst <- spTransform(cst.un, CRS("+proj=robin"))
saveRDS(cst, file.path(map.sp.dir, "sp.cst.not.fortified.rds"))


# there is a color setting step 
# remove Antarctica -- this is a bit clunky, but it works
cst.df <- fortify(cst)
cst.df <- cst.df[cst.df$lat>=-6285430,]
boundary.color <- "white" # color for country boundaries
cst.df$colorcode <- boundary.color
cst.df <- cst.df[,c("id","colorcode")]
cst.df$id <- as.numeric(cst.df$id)
cst.df <- unique(cst.df)
names(cst.df) <- c("OBJECTID","colorcode")
cst <- merge(cst,cst.df,by="OBJECTID",all.x=FALSE,all.y=TRUE)

saveRDS(cst, file.path(map.sp.dir, "sp.cst.rds"))

# Read in the Un Cartography shapefile with lakes
lks.un <- readOGR(map.dir, "2012_UNGIWG_lks_ply_01")  
# convert to Robinson projection 
proj4string(lks.un) <- CRS("+proj=longlat +ellps=WGS84")
lks <- spTransform(lks.un, CRS("+proj=robin"))

saveRDS(lks, file.path(map.sp.dir, "sp.lks.rds"))

# Read in the Un Cartography shapefile with Antarctica (no need actually)
wld.un <- readOGR(map.dir, "un-world-2012-65pct")
ant.un <- wld.un[wld.un$TERR_NAME=="Antarctica",]
rm(wld.un)

saveRDS(ant.un, file.path(map.sp.dir, "sp.ant.rds"))

# convert to Robinson projection 
proj4string(ant.un) <- CRS("+proj=longlat +ellps=WGS84")
ant <- spTransform(ant.un, CRS("+proj=robin"))


# Later in the map making script, can load directly ----------------------------------

# much faster 

world.robin <- readRDS(file.path(map.sp.dir, "sp.world.robin.rds"))
bnd <- readRDS(file.path(map.sp.dir, "sp.bnd.rds"))
cst <- readRDS(file.path(map.sp.dir, "sp.cst.rds"))
lks <- readRDS(file.path(map.sp.dir, "sp.lks.rds"))
lks.df <- fortify(lks)
ant <- readRDS(file.path(map.sp.dir, "sp.ant.rds"))
ant.df <- fortify(ant)
ant.df$color.code <- NA
if (plot.coastlines==TRUE){ ant.df$color.code <- boundary.color }

