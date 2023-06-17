################################################################################
# Visualize avalanche database
# 
# HiAVAL_Figures.R
#
# ReadMe: 
# Read and analyze avalanche events from https://github.com/fidelsteiner/HiAVAL
#
#
# Created:          2022/11/04
# Latest Revision:  2023/03/30
#
#
# Jakob F Steiner | x-hydrolab.org 
################################################################################
# clear entire workspace (excl. packages)
rm(list = ls())
gc()

# define &-sign for pasting string-elements
'&' <- function(...) UseMethod('&')
'&.default' <- .Primitive('&')
'&.character' <- function(...) paste(...,sep='')

library('geosphere')
library(leastcostpath)
library(raster)
library(rgdal)
library(sf)
library(terra)
library(vioplot)
library('scales')
library(lwgeom)
library(ggplot2)
library('tidyverse')

################ 
# paths and raw data (needs to be updated to local paths)
################

projec_utm <- '+proj=utm +datum=WGS84'

data_path <- 'C:\\Work\\Repositories\\HiAVAL'       # location of the database on local station
db_file <- 'HiAVALDB.csv'                                         # file name of database

figures_path <- 'C:\\Work\\Research\\Avalanches\\ManuscriptAnushilan\\Figures'

DEM_path <- 'C:\\Work\\GeospatialData\\HMA\\SRTM\\VoidFilled'           # location of DEM for topographic analysis
DEM_file <- 'SRTM_Corrected_Extended_HMA.tif'                           # DEM filename

HKH_path <- 'C:\\Work\\GeospatialData\\HMA\\HKH_Boundary'               # location of HKH Outline (http://rds.icimod.org/Home/DataDetail?metadataId=3924&searchlist=True)
HKH_file <- 'HKH_Outline.shp'
HKH_outline<-readOGR(dsn=HKH_path&'\\'&HKH_file)                        # read in HKH outline
HKH_outline <- spTransform(HKH_outline,CRSobj = "+proj=longlat +datum=WGS84 +no_defs")                          
HKH_outlinesf <- read_sf(HKH_path&'\\'&HKH_file)
HKH_outlinesf <- st_transform(HKH_outlinesf,crs="+proj=longlat +datum=WGS84 +no_defs")

DEM_HMA <- raster(DEM_path&'\\'&DEM_file)                       # Load DEM


################ 
# 1 Evaluate avalanche database
################

db_data <- read.csv(data_path&'\\'&db_file, fileEncoding="latin1")
missingYear <- length(which(is.na(db_data$Year))) / length(db_data$Year) * 100   # Events where we do not know the year
availableDay <- length(which(!is.na(db_data$Day))) / length(db_data$Year) * 100
availableMonth <- length(which(!is.na(db_data$Month))) / length(db_data$Year) * 100

avalImpact <- which(db_data$Impact=='Y')

# Time series of avalanches

db_data$Region <- db_data$Region_HiMAP # Name Regions for better plotting (HiMap)
db_data$Region[db_data$Region_HiMAP == '4'] <- 'Tian Shan'
db_data$Region[db_data$Region_HiMAP == '2'] <- 'Tian Shan'
db_data$Region[db_data$Region_HiMAP == '5'] <- 'Pamir'
db_data$Region[db_data$Region_HiMAP == '6'] <- 'Pamir'
db_data$Region[db_data$Region_HiMAP == '8'] <- 'Hindukush'
db_data$Region[db_data$Region_HiMAP == '9'] <- 'Karakoram'
db_data$Region[db_data$Region_HiMAP == '10'] <- 'Himalaya W'
db_data$Region[db_data$Region_HiMAP == '11'] <- 'Himalaya C'
db_data$Region[db_data$Region_HiMAP == '12'] <- 'Himalaya C'
db_data$Region[db_data$Region_HiMAP == '16'] <- 'Tibet'
db_data$Region[db_data$Region_HiMAP == '21'] <- 'Tibet'
db_data$Region[db_data$Region_HiMAP == '22'] <- 'Tibet'


db_data$numyear <- as.numeric(db_data$Year)
db_data$decade <- (c(db_data$numyear[]) %/% 10) * 10
db_dataImpact <- db_data[avalImpact,]

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

seasonalCol <- RColorBrewer::brewer.pal(9, "RdYlGn")
seasonalCol <- seasonalCol[-5]
coeff <- 50
pdecadal <- ggplot(subset(db_dataImpact, numyear>0), aes(x=factor(decade),fill = factor(Region)))+
  geom_bar(stat="count", width=1, position = position_dodge2(preserve = "single"))+
  #geom_bar(stat = 'identity',width=0.05,col='grey', aes(x = factor(decade), y = Fatalities/50,fill = factor(Region)), 
  #         inherit.aes = FALSE)+
  scale_fill_manual(values = seasonalCol)+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "grey"),
        legend.position = c(0.15, 0.75),
        text = element_text(size = 25))+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Events",
    
    # Add a second axis and specify its features
    #sec.axis = sec_axis(~.*coeff, name="Fatalities")
  ) +
  xlab("") + 
  ylab("Events")+
  labs(fill = "")

png(file=figures_path&'\\avalanchestemporal.png', res = 160,width=1800,height=900)
print(pdecadal)
dev.off()


pdecadal_fatalities <- ggplot(subset(db_dataImpact, numyear>0), aes(x=factor(decade),fill = factor(Region)))+
  #geom_bar(stat="count", width=1, position = position_dodge2(preserve = "single"))+
  geom_bar(stat = 'identity',width=0.5,col='grey', aes(x = factor(decade), y = Fatalities,fill = factor(Region)), 
                   inherit.aes = FALSE)+
  scale_fill_manual(values = seasonalCol)+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "grey"),
        legend.position = "none",
        text = element_text(size = 25))+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Fatalities"
    
    # Add a second axis and specify its features
    #sec.axis = sec_axis(~.*coeff, name="Fatalities")
  ) +
  xlab("") + 
  ylab("Fatalities")+
  labs(fill = "")

png(file=figures_path&'\\avalanchestemporal_fatalities.png', res = 160,width=1800,height=900)
print(pdecadal_fatalities)
dev.off()

# Seasonal distribution of avalanches (events per month divided by the number of years recorded)

# Calculate N month / N years for each region
seasonalM <- matrix(nrow = 12,ncol=8)
seasonalY <- matrix(nrow = 12,ncol=8)
for(mh in 1:12){
  seasonalM[mh,1] <- length(which(db_dataImpact$Region=='Himalaya C'&db_dataImpact$Month==mh))
  seasonalY[mh,1] <- length(unique(db_dataImpact$Year[which(db_dataImpact$Region=='Himalaya C'&db_dataImpact$Month==mh)]))
  seasonalM[mh,2] <- length(which(db_dataImpact$Region=='Himalaya W'&db_dataImpact$Month==mh))
  seasonalY[mh,2] <- length(unique(db_dataImpact$Year[which(db_dataImpact$Region=='Himalaya W'&db_dataImpact$Month==mh)]))
  seasonalM[mh,3] <- length(which(db_dataImpact$Region=='Hindukush'&db_dataImpact$Month==mh))
  seasonalY[mh,3] <- length(unique(db_dataImpact$Year[which(db_dataImpact$Region=='Hindukush'&db_dataImpact$Month==mh)]))
  seasonalM[mh,4] <- length(which(db_dataImpact$Region=='Karakoram'&db_dataImpact$Month==mh))
  seasonalY[mh,4] <- length(unique(db_dataImpact$Year[which(db_dataImpact$Region=='Karakoram'&db_dataImpact$Month==mh)]))
  seasonalM[mh,5] <- length(which(db_dataImpact$Region=='Pamir'&db_dataImpact$Month==mh))
  seasonalY[mh,5] <- length(unique(db_dataImpact$Year[which(db_dataImpact$Region=='Pamir'&db_dataImpact$Month==mh)]))
  seasonalM[mh,6] <- length(which(db_dataImpact$Region=='Tibet'&db_dataImpact$Month==mh))
  seasonalY[mh,6] <- length(unique(db_dataImpact$Year[which(db_dataImpact$Region=='Tibet'&db_dataImpact$Month==mh)]))
  seasonalM[mh,7] <- length(which(db_dataImpact$Region=='Tien Shan'&db_dataImpact$Month==mh))
  seasonalY[mh,7] <- length(unique(db_dataImpact$Year[which(db_dataImpact$Region=='Tien Shan'&db_dataImpact$Month==mh)]))
  seasonalM[mh,8] <- length(which(is.na(db_dataImpact$Region)&db_dataImpact$Month==mh))
  seasonalY[mh,8] <- length(unique(db_dataImpact$Year[which(is.na(db_dataImpact$Region)&db_dataImpact$Month==mh)]))
  
}


dbseasonal <- seasonalM/seasonalY
colnames(dbseasonal) <- c("Himalaya C","Himalaya W","Hindukush","Karakoram","Pamir","Tibet","Tien Shan","NA")
rownames(dbseasonal) <- c(1,2,3,4,5,6,7,8,9,10,11,12)


dbseasonal[is.infinite(dbseasonal)] <- 0
dbseasonal[is.nan(dbseasonal)] <- 0
dbs <- as.data.frame(dbseasonal)
dbs2 <- dbs %>%
  rownames_to_column() %>%
  pivot_longer(cols = -rowname, names_to = "Region")
dbs2$rowname <- as.numeric(dbs2$rowname)

dbs2 <- within(dbs2, 
                  rowname <- factor(rowname, 
                                      levels=names(sort(table(as.numeric(dbs2$rowname)), 
                                                        increasing=TRUE))))

pseasonal2 <- ggplot(dbs2, aes(x = reorder(rowname,rowname,length), y = value, fill = Region)) + 
  geom_bar(width = 1, stat = "identity",position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = seasonalCol)+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "grey"),
        legend.position =  "none",
        text = element_text(size = 25))+
  scale_x_discrete("Month") +
  xlab("") + 
  ylab("Events [n month / n years]")+
  labs(fill = "")
pseasonal <- ggplot(subset(db_dataImpact, !is.na(Month)), aes(x=factor(Month),fill = factor(Region)))+
  geom_bar(stat="count", width=1, position = position_dodge2(preserve = "single"))+
  scale_fill_manual(values = seasonalCol)+
  theme(panel.background = element_rect(fill = "white"),
       panel.grid = element_line(color = "grey"),
        legend.position =  "none",
        text = element_text(size = 25))+
  scale_x_discrete("Month") +
  xlab("") + 
  ylab("Events")+
  labs(fill = "")

png(file=figures_path&'\\avalanchesseasonal.png', res = 160,width=900,height=1600)
print(pseasonal)
dev.off()

png(file=figures_path&'\\avalanchesseasonal_rev.png', res = 160,width=900,height=1600)
print(pseasonal2)
dev.off()

# Distribution by country and RGI region

cntrDistrib <- c(length(which(db_dataImpact$Country=='Afghanistan')),length(which(db_dataImpact$Country=='Kyrgyzstan')),length(which(db_dataImpact$Country=='Tajikistan')),
                 length(which(db_dataImpact$Country=='Kazakhstan')),length(which(db_dataImpact$Country=='China')),length(which(db_dataImpact$Country=='India')),length(which(db_dataImpact$Country=='Nepal')),
                 length(which(db_dataImpact$Country=='Bhutan')),length(which(db_dataImpact$Country=='Pakistan'))) / length(db_dataImpact$Year) * 100

regDistrib <- c(length(which(db_dataImpact$Region=='Tien Shan')),length(which(db_dataImpact$Region=='Pamir')),length(which(db_dataImpact$Region=='Hindukush')),
                 length(which(db_dataImpact$Region=='Himalaya W')),length(which(db_dataImpact$Region=='Himalaya C')),length(which(db_dataImpact$Region=='Karakoram')),length(which(db_dataImpact$Region=='Tibet'))) / length(db_dataImpact$Year) * 100


# Avalanche type
NAA <- length(which(!is.na(db_data$Type))) / length(db_data$Year) * 100
SLA <- length(which(db_data$Type=='slab avalanche')) / length(db_data$Year) * 100
SNA <- length(which(db_data$Type=='snow avalanche')) / length(db_data$Year) * 100
WSNA <- length(which(db_data$Type=='wet snow avalanche')) / length(db_data$Year) * 100
ISA <- length(which(db_data$Type=='ice and snow avalanche')) / length(db_data$Year) * 100
IRA <- length(which(db_data$Type=='ice and rock avalanche ')) / length(db_data$Year) * 100
PSA <- length(which(db_data$Type=='powder snow avalanche ')) / length(db_data$Year) * 100
GD <- length(which(db_data$Type=='glacier detachment')) / length(db_data$Year) * 100

summerAval <- length(which(db_data$Month>=4&db_data$Month<12)) / length(which(!is.na(db_data$Month))) * 100
winterAval <- length(which(db_data$Month<4|db_data$Month>=12)) / length(which(!is.na(db_data$Month))) * 100

