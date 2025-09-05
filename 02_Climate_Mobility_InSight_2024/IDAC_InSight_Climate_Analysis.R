
# Project: Analysis and charts for IDAC Data InSight #2 Climate Mobility and Childhood
# Author: Sebastian Palmas
# Last modified: 06 May 2024



# PROFILE ----

# set working directories and all directories 
# this is the profile for the PROD-SDG_report_2023 project
# this profile should be loaded before running any other script

USERNAME    <- Sys.getenv("USERNAME")
USERPROFILE <- Sys.getenv("USERPROFILE")
USER        <- Sys.getenv("USER")

#file paths for each user of the repository
if (USERNAME == "palma"){
  projectFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "OneDrive - UNICEF/IDAC/Climate Mobility Data InSight/output")) #Output files
  repoFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "code/Dem-Analytics/09_IDAC_Climate_Mobility_InSight_2024")) #repository files
  rawdataFolder <- file.path(file.path(Sys.getenv("USERPROFILE"), "OneDrive - UNICEF/Migration and Displacement/Data/"))  #raw data folder
} 

# confirm that the main directory is correct
# check if the folders exist
stopifnot(dir.exists(projectFolder))
stopifnot(dir.exists(repoFolder))

# PACKAGES ----

library(dplyr) # filter summarise group_by left_join select mutate rename bind_rows tibble
library(ggplot2) # fortify ggplot aes geom_bar position_dodge labs theme_minimal theme element_blank scale_y_continuous
library(scales) 
library(tidyr) # tibble
library(sp) # plot


source(file.path(repoFolder, "addUnits.R"))


# MAP SETTINGS ----
#path to Migration and Displacement shared folder
# colors for polygons with data
NoDataColor <- "darkgray" # color for polygons with no data
boundary.color <- "grey" # color for country boundaries
background.color <- "white" # color for oceans, seas and lakes
coastline.color <- "grey"

plot.coastlines <- FALSE

## Read in the UN cartography polygon shapefile (no antarctica) --------------
#The .rds files are created in presave_sp_rds_object.R
world.robin <- readRDS(file.path(rawdataFolder, "unmap/sp/sp.world.robin.rds"))  #country polygons
bnd <- readRDS(file.path(rawdataFolder, "unmap/sp/sp.bnd.rds"))  #Boundaries lines
cst <- readRDS(file.path(rawdataFolder, "unmap/sp/sp.cst.rds"))  #Coastal polygons
lks <- readRDS(file.path(rawdataFolder, "unmap/sp/sp.lks.rds"))  #Lakes polygons
lks.df <- fortify(lks)  #converting to a dataframe
ant <- readRDS(file.path(rawdataFolder, "unmap/sp/sp.ant.rds"))  #Antarctica polygon
ant.df <- fortify(ant)  #converting to a dataframe
ant.df$color.code <- NA  #to not plot Antarctica


# FACT 3: CCRI map ----
ccri <- readxl::read_xlsx(file.path(rawdataFolder, "UNICEF/CCRI/FOR SHARING CCRI_V3_1_Model_.xlsx"),range="CCRI!B1:AF164")

world.robin.table <- tibble(ISO3_CODE=world.robin$ISO3_CODE)
world.robin.table <- world.robin.table |>
  left_join(ccri |> select(ISO, "Children's Climate and Environment Risk Index"), by=c('ISO3_CODE'='ISO')) 


ccri.severity  <- tibble(interval=c(1:5),
                         interval.min=c(0, 2.1, 3.8, 5.5, 7.1),
                         severity=c("Very Low", "Low", "Medium", "High", "Extremely High"),
                         severity.color=c("#EFE5F7", "#D8BEEC", "#B07AD8", "#5B2682", "#321547"))
world.robin$color <- ccri.severity$severity.color[findInterval(world.robin.table$`Children's Climate and Environment Risk Index`,
                                       vec=ccri.severity$interval.min)]

legend.title <- "Severity"
legend.labels <- c("≥ 7.1", "5.5 - 7", "3.8 - 5.4", "2.1 - 3.7", "≤ 2", "No data")

# UN Cartography requires that the Askai Chin region be striped half in the color of China and half in the color of India
# Jammu-Kashmir (no data for most of UNPD purposes)
# to do that, we create a separate polygon file that contains only the single region Aksai Chin and assign it the color of China for now
# it will be layered on top of the map in a separate step
ac <- world.robin[world.robin$TERR_NAME=="Aksai Chin",]
acf <- fortify(ac) # transform to data frame for more plotting options
acf$color <- world.robin$color[which(world.robin$TERR_NAME=="China")]  #foreground color

# three styles of boundaries should be mapped: standard solid line, dashed line for undetermined boundaries, dotted for selected disputed boundaries
# to do that, we create a separate dataframe with each containing boundaries of the same type
bnd.line <- bnd[bnd$CARTOGRAPH=="International boundary line",]
bnd.dash <- bnd[bnd$CARTOGRAPH=="Dashed boundary line" | bnd$CARTOGRAPH=="Undetermined international dashed boundary line",]
bnd.dot <- bnd[bnd$CARTOGRAPH=="Dotted boundary line" | bnd$CARTOGRAPH=="Dotted boundary line (Abyei)",]
bnd.ssd <- bnd[bnd$BDY_CNT01=="SDN" & bnd$BDY_CNT02=="SSD",] # Specify SSD-SDN boundaries and plot later to resolve issue of not showing in the original script 

#fixing individual regions
world.robin$color[is.na(world.robin$color) ] <- NoDataColor
world.robin$color[world.robin$TERR_NAME %in% c("Taiwan province of China", "Hong Kong", "Macau")] <- world.robin$color[world.robin$TERR_NAME == "China"]   #color Taiwan the same as China
world.robin$color[world.robin$TERR_NAME == "Abyei"] <- NoDataColor 
world.robin$color[world.robin$TERR_NAME == "Jammu and Kashmir"] <- world.robin$color[world.robin$TERR_NAME == "India"] # #Pakistan and India have same color, so no need for stripes
world.robin$color[world.robin$TERR_NAME == "Ilemi triangle"] <- world.robin$color[world.robin$TERR_NAME == "South Sudan"] # Triangle South Sudan and Kenya 
world.robin$color[world.robin$TERR_NAME == "Arunachal Pradesh"] <- world.robin$color[world.robin$TERR_NAME == "India"] #Same color as India
world.robin$color[world.robin$TERR_NAME == "Aksai Chin"] <- world.robin$color[world.robin$TERR_NAME == "India"] #background color. Striped color is above

# world.robin$color[world.robin$TERR_NAME %in% c("French Guiana", "Martinique", "Guadeloupe", "Mayotte", "R\xe9union" )] <- world.robin$color[world.robin$TERR_NAME == "France"]
# world.robin$color[world.robin$TERR_NAME == "Anguilla"] <- world.robin$color[world.robin$TERR_NAME == "U.K. of Great Britain and Northern Ireland"]
# world.robin$color[world.robin$TERR_NAME == "Sint Maarten (Dutch part)\n"] <- "#F26A21"

# map function
unpd.map <- function(){
  plot(world.robin,border=NA,col=world.robin$color,bg=background.color) # plot country/area polygons
  #points(x=data6c_ind$long, y=data6c_ind$lat) #to double check that points are inside plotting area
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
  
  if (plot.coastlines==TRUE) {
    #lines(cst, col=coastline.color, lwd=0.2, lty=1) # plot coastlines as solid lines
  }
  
  legend(-16820000, -1000000, 
         col=c(rev(ccri.severity$severity.color), NoDataColor),
         pt.bg = c(ccri.severity$severity.color, NoDataColor),
         pch = 15, pt.cex = 2, cex = 0.7, 
         legend = legend.labels,
         title = legend.title, box.lty = 0, box.col = "white",
         bty = 'o', bg = 'white',y.intersp	=1.2)

}

unpd.map()

#png(file = file.path(projectFolder, "fact3.png"), width = 8, height = 4, units = "in", res = 200)
pdf(file = file.path(projectFolder, "fact3.pdf"), width = 8, height = 4)
par(mfrow = c(1,1), omi = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), mgp = c(2, 0.5, 0), 
    las = 0, mex = 1, cex = 1, cex.main = 1, cex.lab = 1, cex.axis = 1)
unpd.map()
dev.off() # close the pdf


# FACT 5: New IDP continent map ----


load(file.path(rawdataFolder, 'WB/WB_classifications_2020.Rdata'))

## Load IDMC data ----
load(file.path(rawdataFolder, 'IDMC/IDMC2024/IDMC_2024.RData'))

#load(file.path(input.wpp, "wpp_pop.Rdata"))

#changing codes to assign world bank region
idmc.new.disaster.events.2008.2023$iso3[idmc.new.disaster.events.2008.2023$iso3 == 'AB9'] <- 'SDN' #abyei area to SDN just for assigning region
idmc.new.disaster.events.2008.2023$iso3[idmc.new.disaster.events.2008.2023$iso3 == 'COK'] <- 'NZL'  #cook islands
idmc.new.disaster.events.2008.2023$iso3[idmc.new.disaster.events.2008.2023$iso3 == 'AIA'] <- 'GBR'  #Anguilla
idmc.new.disaster.events.2008.2023$iso3[idmc.new.disaster.events.2008.2023$iso3 == 'JEY'] <- 'GBR'  #Jersey
idmc.new.disaster.events.2008.2023$iso3[idmc.new.disaster.events.2008.2023$iso3 %in% c('GLP', 'GUF', 'MTQ', 'MYT', 'REU')] <- 'FRA'  #guadeloupe


## Summary tables ----


#by hazard type
idmc.disaster.weather.byhazard <- idmc.new.disaster.events.2008.2023 |>
  filter(hazard.cat == 'Weather related') |> 
  filter(year >= 2016) |>
  group_by(hazard.type) |> 
  summarise(idp.dis.new.0to17=sum(idp.dis.new.0to17, na.rm=TRUE), .groups = 'drop')

print(paste0("Percentage of disaster new displacements by weather 2023: ", 
             label_percent(accuracy =1)((idmc.new.disaster.events.2008.2023 |>  filter(year == 2023) |> filter(hazard.cat=='Weather related') |> pull(idp.dis.new.0to17) |> sum(na.rm=T))/idmc.new.disaster.events.2008.2023 |>  filter(year == 2023) |> pull(idp.dis.new.0to17) |> sum(na.rm=T))))
print(paste0("Total new displacements by weather 2016-2023: ", 
             addUnits(sum(idmc.new.disaster.events.2008.2023 |>  filter(hazard.cat == 'Weather related') |> filter(year >= 2016) |> pull(idp.dis.new), na.rm=T))))
print(paste0("Total children new displacements by weather 2016-2023: ", 
             addUnits(sum(idmc.disaster.weather.byhazard$idp.dis.new.0to17))))
print(paste0("Total children new displacements by floods 2016-2023: ", 
             addUnits(sum(idmc.disaster.weather.byhazard |> filter(hazard.type == "Flood") |> pull (idp.dis.new.0to17)))))
print(paste0("Total children new displacements by storms 2016-2023: ", 
             addUnits(sum(idmc.disaster.weather.byhazard |> filter(hazard.type == "Storm") |> pull (idp.dis.new.0to17)))))
print(paste0("Proportion of children new displacements by storms and floods with respect to all weather, 2016-2023: ", 
             label_percent(accuracy=0.1)(sum(idmc.disaster.weather.byhazard |> 
                                               filter(hazard.type %in% c("Flood", "Storm")) |> 
                                               pull (idp.dis.new.0to17))/
                                           sum(idmc.disaster.weather.byhazard$idp.dis.new.0to17))))

#All weather by year
idmc.disaster.weather.year <- idmc.new.disaster.events.2008.2023 |>
  filter(hazard.cat == 'Weather related') |> 
  filter(year >= 2016) |>
  left_join(wb.income |> select(iso3, region.wb), by='iso3') |> 
  group_by(year) |> 
  summarise(idp.dis.new.0to17=sum(idp.dis.new.0to17, na.rm=TRUE), .groups = 'drop')|> 
  mutate(type='Weather') |> 
  rename('New displacements' = idp.dis.new.0to17)


#weather by region
idmc.disaster.weather.regions <- idmc.new.disaster.events.2008.2023 |>
  filter(hazard.cat == 'Weather related') |> 
  filter(year >= 2016) |>
  left_join(wb.income |> select(iso3, region.wb), by='iso3') |> 
  group_by(region.wb) |> 
  summarise(idp.dis.new.0to17=sum(idp.dis.new.0to17, na.rm=TRUE), .groups = 'drop') |> 
  mutate(idp.dis.new.0to17.units=addUnits(idp.dis.new.0to17))

#Drought displacements by region
idmc.disaster.drought.regions <- idmc.new.disaster.events.2008.2023 |>
  filter(hazard.type == 'Drought') |> 
  filter(year >= 2016) |>
  left_join(wb.income |> select(iso3, region.wb), by='iso3') |> 
  group_by(region.wb) |> 
  summarise(idp.dis.new.0to17=sum(idp.dis.new.0to17, na.rm=TRUE), .groups = 'drop') |> 
  mutate(idp.dis.new.0to17.units=addUnits(idp.dis.new.0to17))

#Wildfire displacements by region
idmc.disaster.wildfire.regions <- idmc.new.disaster.events.2008.2023 |>
  filter(hazard.type == 'Wildfire') |> 
  filter(year >= 2016) |>
  left_join(wb.income |> select(iso3, region.wb), by='iso3') |> 
  group_by(region.wb) |> 
  summarise(idp.dis.new.0to17=sum(idp.dis.new.0to17, na.rm=TRUE), .groups = 'drop') |> 
  mutate(idp.dis.new.0to17.units=addUnits(idp.dis.new.0to17))

#Storm displacements by region
idmc.disaster.storm.regions <- idmc.new.disaster.events.2008.2023 |>
  filter(hazard.type == 'Storm') |> 
  filter(year >= 2016) |>
  left_join(wb.income |> select(iso3, region.wb), by='iso3') |> 
  group_by(region.wb) |> 
  summarise(idp.dis.new.0to17=sum(idp.dis.new.0to17, na.rm=TRUE), .groups = 'drop') |> 
  mutate(idp.dis.new.0to17.units=addUnits(idp.dis.new.0to17))


print(paste0("Percentage of droughts-displaced in SSA: ",
             label_percent(accuracy=1)((idmc.disaster.drought.regions |> filter(region.wb=="Sub-Saharan Africa") |> pull(idp.dis.new.0to17))/sum(idmc.disaster.drought.regions$idp.dis.new.0to17))))
print(paste0("Percentage of wildfire-displaced in North America: ",
             label_percent(accuracy=1)((idmc.disaster.wildfire.regions |> filter(region.wb=="North America") |> pull(idp.dis.new.0to17))/sum(idmc.disaster.wildfire.regions$idp.dis.new.0to17))))
print(paste0("Percentage of storm-displaced in East Asia & Pacific: ",
             label_percent(accuracy=1)((idmc.disaster.storm.regions |> filter(region.wb=="East Asia & Pacific") |> pull(idp.dis.new.0to17))/sum(idmc.disaster.storm.regions$idp.dis.new.0to17))))


## Figure 3 displaced by regions ----


color.code <- tibble(region.wb=c("East Asia & Pacific",
                                 "Europe & Central Asia",
                                 "Latin America & Caribbean",
                                 "Middle East & North Africa",
                                 "North America",
                                 "South Asia",
                                 "Sub-Saharan Africa"  ),
                     color =c("#EB335F","#33A8D0", "#9270B1", "#FDCE33", "#3342AB", "#F19759", "#33C333"))

world.robin.table <- tibble(ISO3_CODE=world.robin$ISO3_CODE)
world.robin.table <- world.robin.table |>
  left_join(wb.income |> select(iso3, region.wb), by=c('ISO3_CODE'='iso3')) |> 
  left_join(color.code, by="region.wb") 


#add column to specify if polygons are present in the data, to color them differently
world.robin$color <- world.robin.table$color 


world.robin$color[world.robin$TERR_NAME == "Taiwan province of China"] <- world.robin$color[world.robin$TERR_NAME == "China"]   #color Taiwan the same as China
world.robin$color[world.robin$TERR_NAME == "Aksai Chin"] <- "#F19759" #China and India are colored, so no need for stripes
world.robin$color[world.robin$TERR_NAME == "Arunachal Pradesh"] <- world.robin$color[world.robin$TERR_NAME == "India"] #Same color as India
world.robin$color[world.robin$TERR_NAME == "Jammu and Kashmir"] <- "#F19759" # #Pakistan and India are colored, so no need for stripes
world.robin$color[world.robin$TERR_NAME == "Western Sahara"] <- NoDataColor
world.robin$color[world.robin$TERR_NAME == "Abyei"] <- world.robin$color[world.robin$TERR_NAME == "Sudan"]
world.robin$color[world.robin$TERR_NAME %in% c("French Guiana", "Martinique", "Guadeloupe", "Mayotte", "R\xe9union" )] <- world.robin$color[world.robin$TERR_NAME == "France"]
world.robin$color[world.robin$TERR_NAME == "Anguilla"] <- world.robin$color[world.robin$TERR_NAME == "U.K. of Great Britain and Northern Ireland"]
world.robin$color[world.robin$TERR_NAME == "Sint Maarten (Dutch part)\n"] <- "#9270B1"


# UN Cartography requires that the Askai Chin region be striped half in the color of China and half in the color of India
# Jammu-Kashmir (no data for most of UNPD purposes)
# to do that, we create a separate polygon file that contains only the single region Aksai Chin and assign it the color of China for now
# it will be layered on top of the map in a separate step
ac <- world.robin[world.robin$TERR_NAME=="Aksai Chin",]
acf <- fortify(ac) # transform to data frame for more plotting options
acf$color <- world.robin$color[which(world.robin$TERR_NAME=="China")]

# three styles of boundaries should be mapped: standard solid line, dashed line for undetermined boundaries, dotted for selected disputed boundaries
# to do that, we create a separate dataframe with each containing boundaries of the same type
bnd.line <- bnd[bnd$CARTOGRAPH=="International boundary line",]
bnd.dash <- bnd[bnd$CARTOGRAPH=="Dashed boundary line" | bnd$CARTOGRAPH=="Undetermined international dashed boundary line",]
bnd.dot <- bnd[bnd$CARTOGRAPH=="Dotted boundary line" | bnd$CARTOGRAPH=="Dotted boundary line (Abyei)",]
bnd.ssd <- bnd[bnd$BDY_CNT01=="SDN" & bnd$BDY_CNT02=="SSD",] # Specify SSD-SDN boundaries and plot later to resolve issue of not showing in the original script 

# map function
unpd.map <- function(){
  plot(world.robin,border=NA,col=world.robin$color,bg=background.color) # plot country/area polygons
  #points(x=data6c_ind$long, y=data6c_ind$lat) #to double check that points are inside plotting area
  polygon(acf$long,acf$lat,col=acf$color[1],border=NA, density=130,angle=45,lwd=0.4) # plot Aksai Chin as striped region per UN Cartography requirements
  
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
    #lines(cst, col=coastline.color, lwd=0.2, lty=1) # plot coastlines as solid lines
  }
  
  #c("East Asia & Pacific",
  #"Europe & Central Asia",
  #"Latin America & Caribbean",
  #"Middle East & North Africa",
  #"North America",
  #"South Asia",
  #"Sub-Saharan Africa"  )
  text(x=c(14000000, -2000000,-10000000, -3500000 , -13000000, 7500000, 3000000),
       y=c(3000000,  5000000,  -100000,  3000000,   5000000, -900000,   -4500000),
       labels=c("24.0 million","0.2 million","3.2 million","0.8 million","1.9 million","17.0 million","14.9 million"),
       col=c("#EB335F","#33A8D0", "#9270B1", "#FDCE33", "#3342AB", "#F19759", "#33C333"))
}

unpd.map()


#png(file = file.path(projectFolder, "fact5.png"), width = 8, height = 4, units = "in", res = 200)
pdf(file = file.path(projectFolder, "fact5.pdf"), width = 8, height = 4)
par(mfrow = c(1,1), omi = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), mgp = c(2, 0.5, 0), 
    las = 0, mex = 1, cex = 1, cex.main = 1, cex.lab = 1, cex.axis = 1)
unpd.map()
dev.off() # close the pdf

# FACT 10: New displacements by cause (Weather vs Conflict) ----

#New displacements by year, disaster vs conflict
idmc.new.conflict.2016.2023 <- idmc.new.2008.2023 |> 
  filter(year >= 2016) |>
  left_join(wb.income |> select(iso3, region.wb), by='iso3') |> 
  group_by( year, cause) |> 
  summarise('New displacements'=sum(idp.new.0to17, na.rm=TRUE), .groups = 'drop') |> 
  filter(cause == 'Conflict')

idmc.new.disaster.conflict.2016.2023 <- idmc.new.disaster.events.2008.2023 |>
  filter(year >= 2016) |> 
  mutate(cause=hazard.cat) |> 
  group_by(year, cause) |> 
  summarise(`New displacements`=sum(idp.dis.new.0to17, na.rm=TRUE),
            ,.groups='drop') |> 
  bind_rows(idmc.new.conflict.2016.2023)

idmc.new.weather.conflict.2016.2023 <- idmc.new.disaster.conflict.2016.2023 |> filter(cause %in% c("Weather related", "Conflict"))

idmc.new.disaster.conflict.2016.2023.wide <- idmc.new.disaster.conflict.2016.2023 |> 
  pivot_wider(names_from = cause, values_from = `New displacements`) |> 
  mutate(total = Geophysical + `Weather related` + Conflict,
         weather.perc = round(100*(`Weather related`/total)),
         conflict.perc = round(100*(Conflict/total)))


#2016-2023 conflict vs weather summary
idmc.new.weather.conflict.2016.2023 |> 
  group_by(cause) |> 
  summarise(sum(`New displacements`))

idmc.new.hazard.type.2023 <- idmc.new.disaster.events.2008.2023 |> 
  filter(year == 2023) |> 
  group_by(hazard.cat) |> 
  summarise(`New displacements`=sum(idp.dis.new.0to17, na.rm=TRUE)) |> 
  bind_rows(tibble(hazard.cat="Conflict", `New displacements` = 9235080)) |> 
  mutate(prop = round(100*`New displacements`/sum(`New displacements`)))
  


#2023 conflict vs disaster new displacements
idmc.new.disaster.conflict.2023 <- idmc.new.2008.2023 |> 
  filter(year==2023) |> 
  group_by(cause) |> 
  summarise(idp.new.0to17 = sum(idp.new.0to17, na.rm=T))

Conflict <- 92635080
Weather <- 6959942
Geophysical <- 8825992-Weather

print(paste0("Percentage of disaster new displacements by total new displacements 2023: ", 
             label_percent(accuracy =1)((idmc.new.disaster.conflict.2023 |> filter(cause== 'Disaster') |> pull(idp.new.0to17))/(idmc.new.disaster.conflict.2023 |> pull(idp.new.0to17) |> sum()))))

idmc.new.weather.conflict.2016.2023$cause[idmc.new.weather.conflict.2016.2023$cause=="Conflict"] <- "Conflict and violence"
idmc.new.weather.conflict.2016.2023$cause <- factor(idmc.new.weather.conflict.2016.2023$cause, levels=c("Weather","Conflict and violence"))
#plotting
g <- ggplot(data=idmc.new.weather.conflict.2016.2023, aes(x=factor(year), y=`New displacements`, fill=cause, group=cause))+
  geom_bar(stat='identity',position = position_dodge())+
  labs(x="Year", y="New internal child displacements (in millions)")+
  theme_minimal()+
  theme(legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())+
  scale_y_continuous(breaks=seq(2500000, 12500000, 2500000),
                     labels = c("2.5 M","5.0 M", "7.5 M","10.0 M","12.5 M"))+
  scale_fill_manual(values=c("#4DB3D6", "#EE4D73"))
ggsave(filename = file.path(projectFolder, "fact10.pdf"), plot = g, width = 18, height=10, units='cm')
print(g)


## 2023 conflict vs disaster IDP children stock ----
# 
idmc.stock.disaster.conflict.2023 <- idmc.stock.2008.2023 |> 
  filter(year==2023) |> 
  filter(sex=="Both sexes") |> 
  group_by(cause) |> 
  summarise(sum(idp.stock.0to17))







