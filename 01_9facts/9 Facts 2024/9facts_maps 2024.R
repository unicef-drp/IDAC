# Project: 9 Facts 2024 Update
# Author: Sebastian Palmas

# PROFILE ----
rm(list=ls())

USERNAME    <- Sys.getenv("USERNAME")
USERPROFILE <- Sys.getenv("USERPROFILE")
USER        <- Sys.getenv("USER")

#file paths for each user of the repository
if (USERNAME == "palma"){
  projectFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "OneDrive - UNICEF/IDAC/9 facts/Version_2024/output")) #Output files
  repoFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "code/Dem-Analytics/08_IDAC_9facts")) #repository files
  rawdataFolder <- file.path(file.path(Sys.getenv("USERPROFILE"), "OneDrive - UNICEF/Migration and Displacement/Data/"))  #raw data folder
} 

# confirm that the main directory is correct
# check if the folders exist
stopifnot(dir.exists(projectFolder))
stopifnot(dir.exists(repoFolder))

# PACKAGES ----

library(dplyr) # filter tibble left_join select mutate pull
library(sp) # plot coordinates
library(ggplot2) # fortify
library(plotrix) #draw.circle
library(tidyr) 
library(caroline) #for pie charts over a scatterplot

source(file.path(repoFolder, "9 Facts 2024/pies_overplot.R"))

# LOAD DATA ----

#path to Migration and Displacement shared folder
path.map <- file.path(rawdataFolder, "Data/unmap/sp") # location of the spatial polgons


# MAP SETTINGS ----
# colors for polygons with data
NoDataColor <- "darkgray" # color for polygons with no data
boundary.color <- "grey" # color for country boundaries
background.color <- "white" # color for oceans, seas and lakes
coastline.color <- "grey"

plot.coastlines <- TRUE

# Read in the UN cartography polygon shapefile (no antarctica) --------------
#The .rds files are created in presave_sp_rds_object.R
world.robin <- readRDS(file.path(rawdataFolder, "unmap/sp/sp.world.robin.rds"))  #country polygons
bnd <- readRDS(file.path(rawdataFolder, "unmap/sp/sp.bnd.rds"))  #Boundaries lines
cst <- readRDS(file.path(rawdataFolder, "unmap/sp/sp.cst.rds"))  #Coastal polygons
lks <- readRDS(file.path(rawdataFolder, "unmap/sp/sp.lks.rds"))  #Lakes polygons
lks.df <- fortify(lks)  #converting to a dataframe
# ant <- readRDS(file.path(path.map, "sp.ant.rds"))  #Antarctica polygon
# ant.df <- fortify(ant)  #converting to a dataframe
# ant.df$color.code <- NA  #to not plot Antarctica


# FACT 3 ----
load(file.path(path.basic, "Data/UNPD/UNMigrantStock2020/UN_MigrantStockAge0to17.Rdata"))
country_metadata <- read.csv("input_misc/country_metadata_master.csv") #downloaded 2025-02-05 from DAPM github (https://github.com/unicef-drp/DW-Production/blob/master-dw-2024/00_master/010_metadata/country_metadata_master.csv)
load(file.path(path.basic, 'Data/WB/WB_classifications_2020.Rdata'))  #World bank regions

data3 <- mig.stock.0to17 |> filter(year==2020, sex=="both") #This results in one value per iso3

sdg.color.code <- tibble(color=c("#EDA877",
                                        "#8C789E",
                                        "#7FBA70",
                                        "#EAE8FF",
                                        "#62ABC3",
                                        "#C978CF",
                                        "#E6738E",
                                        "#FEE07F",
                                        "#FFFFFF"),
                         sdgregion=c("Eastern Asia and South-eastern Asia",
                                     "Central Asia and Southern Asia",
                                     "Latin America and the Caribbean",
                                     "Western Asia and Northern Africa",
                                     "Northern America and Europe",
                                     "Oceania excluding Australia and New Zealand",
                                     "Sub-Saharan Africa",
                                     "Australia and New Zealand",
                                     "World"))

world.robin.table <- tibble(ISO3_CODE=world.robin$ISO3_CODE)
world.robin.table <- world.robin.table |>
  left_join(country_metadata |> select(iso3, sdgregion), by=c('ISO3_CODE'='iso3')) |> 
  left_join(sdg.color.code, by="sdgregion") 

#adding the color column to the polygon object
world.robin$color <- world.robin.table$color 
world.robin$color[is.na(world.robin$color) ] <- NoDataColor
world.robin$color[world.robin$TERR_NAME == "Taiwan province of China"] <- world.robin$color[world.robin$TERR_NAME == "China"]   #color Taiwan the same as China
world.robin$color[world.robin$TERR_NAME == "Aksai Chin"] <- "#8C789E" #Background stripes color (color of India)
world.robin$color[world.robin$TERR_NAME == "Arunachal Pradesh"] <- world.robin$color[world.robin$TERR_NAME == "India"] #Same color as India
world.robin$color[world.robin$TERR_NAME == "Jammu and Kashmir"] <- "#8C789E" # #Pakistan and India are colored the same, so no need for stripes
world.robin$color[world.robin$TERR_NAME == "Western Sahara"] <- NoDataColor
world.robin$color[world.robin$TERR_NAME == "Ilemi triangle"] <- world.robin$color[world.robin$TERR_NAME == "South Sudan"] # Triangle South Sudan and Kenya 
world.robin$color[world.robin$TERR_NAME == "Abyei"] <- world.robin$color[world.robin$TERR_NAME == "Sudan"]
world.robin$color[world.robin$TERR_NAME %in% c("Jersey", "Falkland Islands (Malvinas)")] <- world.robin$color[world.robin$TERR_NAME == "U.K. of Great Britain and Northern Ireland"]
world.robin$color[world.robin$TERR_NAME == "French Guiana"] <- world.robin$color[world.robin$TERR_NAME == "France"]


# UN Cartography requires that the Askai Chin region be striped half in the color of China and half in the color of India
# to do that, we create a separate polygon file that contains only the single region Aksai Chin and assign it the color of China
# it will be layered on top of the map in a separate step
ac <- world.robin[world.robin$TERR_NAME=="Aksai Chin",]
acf <- fortify(ac) # transform to data frame for more plotting options
acf$color <- world.robin$color[which(world.robin$TERR_NAME=="China")]  #foreground stripes color

# three styles of boundaries should be mapped: standard solid line, dashed line for undetermined boundaries, dotted for selected disputed boundaries
# to do that, we create a separate dataframe with each containing boundaries of the same type
bnd.line <- bnd[bnd$CARTOGRAPH=="International boundary line",]
bnd.dash <- bnd[bnd$CARTOGRAPH=="Dashed boundary line" | bnd$CARTOGRAPH=="Undetermined international dashed boundary line",]
bnd.dot <- bnd[bnd$CARTOGRAPH=="Dotted boundary line" | bnd$CARTOGRAPH=="Dotted boundary line (Abyei)",]
bnd.ssd <- bnd[bnd$BDY_CNT01=="SDN" & bnd$BDY_CNT02=="SSD",] # Specify SSD-SDN boundaries and plot later to resolve issue of not showing in the original script 

#preparing circles layer
#this is a little tricky... The ID of the polygons of world.robin are the territory names, not iso3
#This means that some values of iso3 are duplicated. For example: PRT (Portugal and Azores), PSE (West Bank, Gaza)
#This means that we need to choose which polygon to use for each iso3
#to avoid removing polygons that are in the mig.stock data, we first add the migstock values to the table and then check for duplicates, 
world.robin@data <- world.robin@data |> left_join(data3 |> select(iso3, pop.mig.0to17), by=c("ISO3_CODE"="iso3"))

data3$iso3[!data3$iso3 %in% world.robin$ISO3_CODE]

#mig.stock values for "CHI" (Channel islands) "BLM" (Saint Barthélemy) "MAF" (Saint Martin (French part)) are not joined because these iso3 codes are not in world.robin
# We will plot CHI in Jersey
#BLM and MAF do not have miigration population
world.robin$pop.mig.0to17[world.robin$TERR_NAME == "Jersey"] <- data3$pop.mig.0to17[data3$area == "Channel Islands"]

#removing rows without values
world.robin.with.pop.mig.data <- world.robin[!is.na(world.robin$pop.mig.0to17),]

#removing duplicates (some territories have the same iso3, so they have duplicated mig data)
#manually choosing which to eliminate because it is complicated to make some rules to eliminate
#
world.robin.with.pop.mig.data <- world.robin.with.pop.mig.data[world.robin.with.pop.mig.data$pop.mig.0to17>0,] 
world.robin.with.pop.mig.data <- world.robin.with.pop.mig.data[!world.robin.with.pop.mig.data$TERR_NAME %in% 
                                                                 c("Guernsey",
                                                                   "Kuril islands",
                                                                   "Senkaku Islands",
                                                                   "Madeira Islands",
                                                                   "Azores Islands",
                                                                   "West Bank" #Althoug mig population is probably larger in WB, migstock for PSE is plotted in in Gaza because there are two WB polygons and it is easier to eliminate these.  
                                                                   ),] 
world.robin.with.pop.mig.data <- world.robin.with.pop.mig.data[!world.robin.with.pop.mig.data$CAPITAL %in% 
                                                                 c("Klien"),] 
#to check if there are duplicated numbers, which most likely means that there are duplicated mig.stock values
#any(duplicated(world.robin.with.pop.mig.data$pop.mig.0to17))

#preparing table with centroids and data
pop.mig.in.centroids <- coordinates(world.robin.with.pop.mig.data) 
pop.mig.in.centroids <- tibble(x=pop.mig.in.centroids[,1], y=pop.mig.in.centroids[,2], pop.mig.0to17 = world.robin.with.pop.mig.data$pop.mig.0to17)

# map function
unpd.map <- function(){
  plot(world.robin,border=NA,col=world.robin$color,bg=background.color) # plot country/area polygons
  #points(x=fact6_7$long, y=fact6_7$lat) #to double check that points are inside plotting area
  polygon(acf$long,acf$lat,col=acf$color[1],border=NA, density=130,angle=45,lwd=0.4) # plot Aksai Chin as striped region per UN Cartography requirements
  
  lines(bnd.line, col=boundary.color, lwd=0.2, lty=1) # plot solid boundaries
  lines(bnd.dash, col=boundary.color, lwd=0.2, lty=2) # plot dashed boundaries
  lines(bnd.dot, col=boundary.color, lwd=0.2, lty=3) # plot dotted boundaries
  lines(bnd.ssd, col=boundary.color, lwd=0.2, lty=2) # plot SSD-SDN boundary
  
  lks.grp <- unique(lks.df$group)
  for (gp in lks.grp) {
    lk <- lks.df[lks.df$group==gp,]
    polygon(lk$long,lk$lat,col=background.color,border=NA,lty=1,lwd=0.2) # plot lakes as background color
  }
  
  #if (plot.coastlines==TRUE) {
    #lines(cst, col=coastline.color, lwd=0.2, lty=1) # plot coastlines as solid lines
  #}
  
  for (i in 1:nrow(pop.mig.in.centroids))
  {
    draw.circle(x=pop.mig.in.centroids$x[i], y=pop.mig.in.centroids$y[i],
                radius=0.3*pop.mig.in.centroids$pop.mig.0to17[i], 
                border="#52525290", col="#52525280")}
  
}

#unpd.map() #to plot to Rstudio

#png(file = "output/fact3.png", width = 8, height = 4, units = "in", res = 200)
pdf(file = "output/fact3.pdf", width = 8, height = 4)
par(mfrow = c(1,1), omi = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), mgp = c(2, 0.5, 0), 
    las = 0, mex = 1, cex = 1, cex.main = 1, cex.lab = 1, cex.axis = 1)
unpd.map()
dev.off() # close the pdf


# FACT 6 and 7 ----
load(file.path(rawdataFolder,"IDMC/IDMC2024/idmc_2024.RData"))
country_regions <- readr::read_csv(file.path(rawdataFolder,"UNICEF/country_regions_master.csv")) 

idmc.stock.2008.2023 <- idmc.stock.2008.2023 |> left_join(country_regions |> select(iso3, sdgregion), by='iso3')
idmc.new.2008.2023 <- idmc.new.2008.2023 |> left_join(country_regions |> select(iso3, sdgregion), by='iso3') 
  

## Key numbers Fact 6 ----
idmc.stock.summary <-  idmc.stock.2008.2023 |> 
  filter(sex == "Both sexes") |> 
  filter(year==2023) |> 
  group_by(cause, year)|> 
  summarise(pop.0to17 = sum(idp.stock.0to17, na.rm = T),
            .groups = 'drop') |> 
  mutate(perc=round(100*pop.0to17/sum(pop.0to17), digits = 1),
         pop.0to17.millions = round(pop.0to17 / 1000000, digits = 1))
View(idmc.stock.summary)

idmc.stock.summary <-  idmc.stock.2008.2023 |> 
  filter(sex == "Both sexes") |> 
  filter(year==2023) |> 
  group_by(cause, year)|> 
  summarise(pop.0to17 = sum(idp.stock.0to17, na.rm = T),
            .groups = 'drop')


idmc.stock.summary.region <-  idmc.stock.2008.2023 |> 
  filter(sex == "Both sexes") |> 
  filter(year==2023) |> 
  filter(cause=="Conflict") |> 
  group_by(sdgregion)|> 
  summarise(pop.0to17 = sum(idp.stock.0to17, na.rm = T),
            .groups = 'drop') |> 
  mutate(perc=round(100*pop.0to17/sum(pop.0to17), digits = 1),
         pop.0to17.millions = round(pop.0to17 / 1000000, digits = 1))
View(idmc.stock.summary.region)


## Key numbers Fact 7 ----
idmc.new.summary <- idmc.new.2008.2023 |> 
  filter(year==2023) |> 
  group_by(cause) |> 
  summarise(idp.new.0to17 = sum(idp.new.0to17, na.rm = T),
            .groups = 'drop') |> 
  mutate(idp.new.0to17.millions = round(idp.new.0to17 / 1000000, digits = 1))
View(idmc.new.summary)

idmc.new.summary.region.conf <- idmc.new.2008.2023|> 
  filter(year==2023) |> 
  filter(cause=="Conflict") |> 
  group_by(sdgregion)|> 
  summarise(idp.new.0to17 = sum(idp.new.0to17, na.rm = T),
            .groups = 'drop') |> 
  mutate(idp.new.0to17.millions = round(idp.new.0to17 / 1000000, digits = 1))
View(idmc.new.summary.region.conf)

idmc.new.summary.region.dis <- idmc.new.2008.2023|> 
  filter(year==2023) |> 
  filter(cause=="Disaster") |> 
  group_by(, sdgregion)|> 
  summarise(idp.new.0to17 = sum(idp.new.0to17, na.rm = T),
            .groups = 'drop') |> 
  mutate(idp.new.0to17.millions = round(idp.new.0to17 / 1000000, digits = 1))
View(idmc.new.summary.region.dis)


## Prepare IDMC data for pie charts----
places <- data.frame(matrix(ncol = 5, nrow = length(world.robin)))
colnames(places) <- c("ISO3", "TERR_NAME", "long", "lat", "STATUS")

places$ISO3 <- as.character(world.robin$ISO3_CODE)
places$TERR_NAME <- world.robin$TERR_NAME
places$long <- coordinates(world.robin)[, 1]
places$lat <- coordinates(world.robin)[, 2]
places$STATUS <- world.robin$STATUS
places$ISO3[places$TERR_NAME == 'Abyei'] <- 'AB9'

fact6 <-idmc.stock.2008.2023 |>  
  filter(year == 2023) |> 
  filter(sex=="Both sexes") |> 
  select(iso3, cause, idp.stock.0to17) |> 
  pivot_wider(names_from=cause, values_from = idp.stock.0to17) |> 
  mutate(Disaster.stock = ifelse(is.na(Disaster), 0, Disaster),
         Conflict.stock = ifelse(is.na(Conflict), 0, Conflict))  |> 
  left_join(places, by = c('iso3' = 'ISO3')) |> 
  filter(STATUS != 'PT Territory', !(TERR_NAME %in% c('Guernsey','Senkaku Islands','Gaza Strip','Kuril islands')) )|> 
  mutate(fact6 = Conflict.stock + Disaster.stock) |> 
  mutate(fact6_color = fact6>0)

fact7 <-idmc.new.2008.2023 |>  
  filter(year == 2023) |> 
  select(iso3, cause, idp.new.0to17) |> 
  pivot_wider(names_from=cause, values_from = idp.new.0to17) |> 
  mutate(Disaster.new = ifelse(is.na(Disaster), 0, Disaster),
         Conflict.new = ifelse(is.na(Conflict), 0, Conflict))  |> 
  left_join(places, by = c('iso3' = 'ISO3')) |> 
  filter(STATUS != 'PT Territory', !(TERR_NAME %in% c('Guernsey','Senkaku Islands','Gaza Strip','Kuril islands')) )|> 
  mutate(fact7 = Conflict.new + Disaster.new) |> 
  mutate(fact7_color = fact7>0)


#add column to specify if polygons are present in the data, to color them differently
world.robin$fact6_color <- world.robin$ISO3_CODE %in% (fact6 |> filter(fact6_color) |> pull(iso3))
world.robin$fact6_color[world.robin$TERR_NAME == "Taiwan province of China"] <- world.robin$fact6_color[world.robin$TERR_NAME == "China"]   #color Taiwan the same as China
world.robin$fact6_color[world.robin$TERR_NAME == "Aksai Chin"] <- TRUE #China and India are colored, so no need for stripes
world.robin$fact6_color[world.robin$TERR_NAME == "Arunachal Pradesh"] <- world.robin$fact6_color[world.robin$TERR_NAME == "India"] #Same color as India
world.robin$fact6_color[world.robin$TERR_NAME == "Jammu and Kashmir"] <- TRUE # #Pakistan and India are colored, so no need for stripes

world.robin$fact7_color <- world.robin$ISO3_CODE %in% (fact7 |> filter(fact7_color) |> pull(iso3))
world.robin$fact7_color[world.robin$TERR_NAME == "Taiwan province of China"] <- world.robin$fact7_color[world.robin$TERR_NAME == "China"]   #color Taiwan the same as China
world.robin$fact7_color[world.robin$TERR_NAME == "Aksai Chin"] <- TRUE #China and India are colored, so no need for stripes
world.robin$fact7_color[world.robin$TERR_NAME == "Arunachal Pradesh"] <- world.robin$fact7_color[world.robin$TERR_NAME == "India"] #Same color as India
world.robin$fact7_color[world.robin$TERR_NAME == "Jammu and Kashmir"] <- TRUE # #Pakistan and India are colored, so no need for stripes


make.world.map <- function(ind0){
  
  library(mapplots) # No used functions found
  library(reshape2) # No used functions found
  
  #color countries only if they have data
  world.robin$colorcode <- NA
  
  
  # UN Cartography requires that the Askai Chin region be striped half in the color of China and half in the color of
  # Jammu-Kashmir (no data for most of UNPD purposes)
  # to do that, we create a separate polygon file that contains only the single region Aksai Chin and assign it the color of China for now
  # it will be layered on top of the map in a separate step
  #ac <- world.robin[world.robin$TERR_NAME=="Aksai Chin",]
  #acf <- fortify(ac) # transform to data frame for more plotting options
  #acf$colorcode <- world.robin$colorcode[which(world.robin$TERR_NAME=="China")]

  # three styles of boundaries should be mapped: standard solid line, dashed line for undetermined boundaries, dotted for selected disputed boundaries
  # to do that, we create a separate dataframe with each containing boundaries of the same type
  bnd.line <- bnd[bnd$CARTOGRAPH=="International boundary line",]
  bnd.dash <- bnd[bnd$CARTOGRAPH=="Dashed boundary line" | bnd$CARTOGRAPH=="Undetermined international dashed boundary line",]
  bnd.dot <- bnd[bnd$CARTOGRAPH=="Dotted boundary line" | bnd$CARTOGRAPH=="Dotted boundary line (Abyei)",]
  bnd.ssd <- bnd[bnd$BDY_CNT01=="SDN" & bnd$BDY_CNT02=="SSD",] # Specify SSD-SDN boundaries and plot later to resolve issue of not showing in the original script 
  
  #data of the indicator
  #ind0 <- 'fact6'
  if(ind0=="fact6"){
    filter_column <- "fact6_color"
    spec.nms <- c("Conflict.stock", "Disaster.stock")
    
    radii_multiply <- 0.003
    world.robin$colorcode[world.robin$fact6_color] <- "#d3f5ef"
    world.robin$colorcode[!world.robin$fact6_color] <- "grey97"
    fact6_7 <-  fact6 |> filter(get(filter_column)) #filtering only data for chosen indicator
    
    #scale legend specs
    legend.scales <- tibble(scale = c(250000, 500000, 1000000, 2000000),
                            long = rep(x = -12000000, times=4),
                            lat = seq(from=-4000000, to=-2000000, length.out=4),
                            scale.label=c("250 K", "500 K", "1 M", "2 M"))
  } else {
    filter_column <- "fact7_color"
    spec.nms <- c("Conflict.new", "Disaster.new")
    radii_multiply <- 0.003
    world.robin$colorcode[world.robin$fact7_color] <- "#d3f5ef"
    world.robin$colorcode[!world.robin$fact7_color] <- "grey97"
    
    fact6_7 <-  fact7 |> filter(get(filter_column)) #filtering only data for chosen indicator
    
    #scale legend specs
    legend.scales <- tibble(scale = c(250000, 500000, 1000000, 2000000),
                            long = rep(x = -12000000, times=4),
                            lat = seq(from=-4000000, to=-2000000, length.out=4),
                            scale.label=c("250 K", "500 K", "1 M", "2 M"))
  }
  
  #Pie specs Data
  color.table <- c(adjustcolor("#ED7D30", alpha.f=0.7), adjustcolor("#774C9E", alpha.f=0.7))
  names(color.table) <- spec.nms
  pie.list <- lapply(1:nrow(fact6_7),
                     function(i) as.table(nv(as.vector(as.matrix(fact6_7[i,spec.nms])),spec.nms))) #list of vectors with datas
  
  #Pie specs legend
  pie.list.legend <- lapply(1:nrow(legend.scales),
                            function(i) as.table(nv(as.vector(as.matrix(legend.scales[i,"scale"])),"scale"))) #list of vectors with datas
  color.table.legend <- c(adjustcolor("#808080", alpha.f=0.7))
  names(color.table.legend) <- "scale"
  
  
  # write the map function
  unpd.map <- function(){
    plot(world.robin,border=NA,col=world.robin$colorcode,bg=background.color) # plot country/area polygons
    #points(x=fact6_7$long, y=fact6_7$lat) #to double check that points are inside plotting area
    #polygon(acf$long,acf$lat,col=acf$colorcode[1],border=NA, density=130,angle=45,lwd=0.4) # plot Aksai Chin as striped region per UN Cartography requirements
    
    lines(bnd.line, col=boundary.color, lwd=0.2, lty=1) # plot solid boundaries
    lines(bnd.dash, col=boundary.color, lwd=0.2, lty=2) # plot dashed boundaries
    lines(bnd.dot, col=boundary.color, lwd=0.2, lty=3) # plot dotted boundaries
    lines(bnd.ssd, col=boundary.color, lwd=0.2, lty=2) # plot SSD-SDN boundary
    
    lks.grp <- unique(lks.df$group)
    for (gp in lks.grp) {
      lk <- lks.df[lks.df$group==gp,]
      polygon(lk$long,lk$lat,col=background.color,border=boundary.color,lty=1,lwd=0.2) # plot lakes as background color
    }
    
    if (plot.coastlines==TRUE) {
      lines(cst, col=coastline.color, lwd=0.2, lty=1) # plot coastlines as solid lines
    }
    
    #plotting pie charts in scatterplot
    pies_overplot(x=pie.list, x0=fact6_7$long, y0=fact6_7$lat, radii=sqrt(fact6_7 |> pull(ind0))*radii_multiply,
                  color.table = color.table, lty=0)
    
    #plotting legend pie charts in scatterplot
    pies_overplot(x=pie.list.legend, x0=legend.scales$long, y0=legend.scales$lat, radii=sqrt(legend.scales |> pull('scale'))*radii_multiply,
                  color.table = color.table.legend, lty=0)
    
    #Legend labels
    text(x = legend.scales$long+1800000, y = legend.scales$lat, labels = legend.scales$scale.label, cex=0.8, col=adjustcolor("#808080", alpha.f=0.7))

  }
  
  #png(file = paste0("output/", ind0, ".png"), width = 8, height = 4, units = "in", res = 200)
  pdf(file = file.path(projectFolder, paste0(ind0, ".pdf")), width = 8, height = 4)
  par(mfrow = c(1,1), omi = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), mgp = c(2, 0.5, 0), 
      las = 0, mex = 1, cex = 1, cex.main = 1, cex.lab = 1, cex.axis = 1)
  unpd.map()
  dev.off() # close the pdf
}

#run function to build maps
make.world.map(ind0 = "fact6")
make.world.map(ind0 = "fact7")

