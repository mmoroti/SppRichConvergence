# Load and install needed package
needed_packages <- c("data.table", "dplyr", "ggplot2", "plyr", "raster",
                     "readxl", "sf", "SpatialPack", "viridis")
new.packages <- needed_packages[!(needed_packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(needed_packages, require, character.only = TRUE)

# Define the working directory:
setwd("D:/OneDrive/00 - COLARABORACOES/IGOR OKAWADA/2024 - Temporal trends in prioritization schemes") # Mario PC
setwd("D:/OneDrive/IGOR OKAWADA/2024 - Temporal trends in prioritization schemes") # Amazonia
setwd("D:/OneDrive/00 - COLARABORACOES/IGOR OKAWADA/2024 - Temporal trends in prioritization schemes")

# STEP 1 - COMPUTE AND MAP ASSEMBLAGE-LEVEL SPECIES RICHNESS FOR THE CURRENT TIME
##########################################################################################################################
# STEP 1 - COMPUTE AND MAP ASSEMBLAGE-LEVEL SPECIES RICHNESS FOR THE CURRENT TIME
rm(list=ls()); gc()
library(data.table)
library(ggplot2)
library(raster)
library(scales)
library(sf)
library(viridis)

# Load the TetrapodTraits database:
TetraData<-data.table::fread("Datasets/TetrapodTraits_1.0.0.csv")

# Load data grid cell intersections for tetrapods:
vert_assemblages <- data.table::fread("Datasets/Tetrapod_360.csv", stringsAsFactors=T)

# Merge assemblage-level data (grid cell intersections) with the species-level data informing year of discovery:
vert_assemblages <- merge(x = vert_assemblages,
                          y = TetraData[ ,c("Scientific.Name", "TreeTaxon", "YearOfDescription")],
                          by= 'Scientific.Name', 
                          all.x = TRUE)

# Compute assemblage-level metrics, including richness and average description year for species within grid cells:
grid_cells_data <- vert_assemblages[, .(
                                        SppRichness = .N,
                                        AvgYear = mean(YearOfDescription, na.rm=T)),
                                        by=.(Cell_Id110)]

# Remove unnecessary objects:
rm(TetraData, vert_assemblages)

# Load the 360 grid shapefile (spatial feature representing the 110x110km grid cells):
grid_cells_sf <- sf::st_read(dsn = "Shapefiles/", layer='gridcells_110km')
names(grid_cells_sf)<-c("Cell_Id110", "Long", "Lat", "WWF_Realm", "PropLandArea", "geometry")

# Merge assemblage-level metrics:
grid_cells_sf <- merge(x = grid_cells_sf, 
                       y = grid_cells_data, 
                       by = "Cell_Id110", 
                       all.y = T)

# Extract the equal area projection used in the grid_cells_sf shapefile:
equalareaproj <- raster::crs(grid_cells_sf)

# Load shapefile on the study area and convert to an equal-area projection:
wwf_realms <- sf::read_sf(dsn="Shapefiles/", layer='wwf_realms')  # change directory as needed
wwf_realms <- sf::st_transform(wwf_realms, crs=equalareaproj)

# To simplify visualization, omit the Antarctic boundaries (they are poorly visible using the equal area projection:
wwf_realms <- wwf_realms[wwf_realms$wwf_realm!="Antarctic",]

# For better mapping aesthetics, perform the spatial intersection between grid_cells_sf and wwf_realms:
grid_cells_sf <- sf::st_intersection(wwf_realms, grid_cells_sf) # it will take some time (not mandatory to run this)

# Filter cells with at least 1 species:
grid_cells_sf <- grid_cells_sf[grid_cells_sf$SppRichness >= 1, ]

# Build the plot:
MyMap <- ggplot2::ggplot() +
      
  # Add the shapefile layer with cell colour informed by 'SppRichness' variable:
  geom_sf(data=grid_cells_sf, aes(fill=SppRichness), colour=NA) +
      
  # Specify the color ramp:
  scale_fill_gradientn(colours= viridis::viridis(n = 100, option = "plasma", direction = 1), 
                       na.value='white', breaks=scales::extended_breaks(4)) +
      
  # Add polygon boundaries for the wwf realms:
  geom_sf(data=wwf_realms, fill=NA, colour="black", size=0.1)+
  
  # Specify other aesthetics:
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        panel.background=element_blank(), 
        plot.background=element_rect(fill="white"),
        plot.margin=unit(c(-0.5, 0, 0, 0), "cm"),  # top, right, bottom, left (force the plot on the top part of the panel)
        panel.spacing=unit(c(0, 0, 0, 0), "cm"),  # top, right, bottom, left
        panel.border=element_blank(),
        legend.justification=c(0.5, 1),
        legend.position=c(0.6, 0.15),
        legend.direction="horizontal",
        legend.title=element_text(size=12, hjust=0.5),
        legend.text=element_text(size=10, hjust=0.5),
        legend.background=element_blank()
  ) +
  
  # Aesthetics for legend colour bar:
  guides(fill=guide_colourbar(nbin=15, 
                              barwidth=15, 
                              barheight=1, 
                              draw.ulim=T, 
                              draw.llim=T, 
                              title=bquote('Species richess'),
                              label=T, 
                              title.position = "top", 
                              title.hjust = 0.5, 
                              label.position="bottom", 
                              label.hjust=0.5,
                              frame.colour="black",
                              frame.linewidth=0.1,
                              ticks.linewidth=0.2,
                              ticks=T,
                              ticks.colour="black"))

# Visualize the plot:
dir.create("Figures", showWarnings = FALSE)
ggsave(filename="Figures/Map.png", plot=MyMap, width=12, height=8, units="in", bg="white", limitsize=F)
ggsave(filename="Figures/Map.pdf", plot=MyMap, width=12, height=8, units="in", bg="white", limitsize=F)

#####

# STEP 2 - COMPUTE THE CORRELATION BETWEEN CURRENT SPECIES RICHNESS AND HISTORICAL RICHNESS
##########################################################################################################################
# STEP 2 - COMPUTE THE CORRELATION BETWEEN CURRENT SPECIES RICHNESS AND HISTORICAL RICHNESS
rm(list=ls()); gc()

# Load the TetrapodTraits database:
TetraData <- data.table::fread("Datasets/TetrapodTraits_1.0.0.csv")

# Load data grid cell intersections for tetrapods:
vert_assemblages <- data.table::fread("Datasets/Tetrapod_360.csv", stringsAsFactors=T)

# Merge assemblage-level data (grid cell intersections) with the species-level data informing year of discovery:
vert_assemblages <- merge(x = vert_assemblages, 
                          y = TetraData[,c("Scientific.Name", "TreeTaxon", "YearOfDescription")],
                          by = 'Scientific.Name',
                          all.x = TRUE)

# It is possible to filter one specific tetrapod group using levels of 'TreeTaxon' variable:
levels(as.factor(vert_assemblages$TreeTaxon))
vert_assemblages <- vert_assemblages[vert_assemblages$TreeTaxon=="Amphibians",]

# Create a vector with the year indicating the ending period of species descriptions:
LastYear <- seq(1800, 2015, by = 5)  

# Create an empty list to store the ouput of the for loop ahead:
SppRichPerDecade <- list()

# Compute the assemblage-level species richness using species described until different ending periods:
for(i in 1:length(LastYear)){
  
  # Subset the 'vert_assemblages' data:
  data_subset <- vert_assemblages[vert_assemblages$YearOfDescription <= LastYear[i],]
  
  # Compute assemblage-level metrics, including richness and average description year for species within grid cells:
  data_subset <- data_subset[, .(SppRichness = .N), by=.(Cell_Id110)]
  
  # Add a new column informing the ending period used of species descriptions:
  data_subset$LastYear <- LastYear[i]
  
  # Store the information in the list 'SppRichPerDecade':
  SppRichPerDecade[[i]] <- data_subset
  
  # Remove unnecessary object before next iteration:
  rm(data_subset)

} # end of i for loop

# Remove unnecessary objects:
rm(TetraData, vert_assemblages)

# Unlist in different data.frame:
SppRichPerDecade <- data.table::rbindlist(SppRichPerDecade)

# Export to disk:
data.table::fwrite(SppRichPerDecade, file="Datasets/SppRichPerDecade.csv")
SppRichPerDecade <- as.data.frame(SppRichPerDecade)

# Extract a separated data.frame holding the species richness values for the current period:
CurrentRich <- SppRichPerDecade[SppRichPerDecade$LastYear == 2015, c(1:2)]
names(CurrentRich) <- c("Cell_Id110", "CurrentRichness")

# Remove cells with zero richness:
CurrentRich <- CurrentRich[CurrentRich$CurrentRichness >= 1,]
head(CurrentRich)

# Compute the Pearson correlation between the current richness and the richness of different ending periods:
CorrOutput <- list()
for(i in 1:length(LastYear)){
  
  # Separate the richness values of the selected decade:
  SppRich_LastYear <- SppRichPerDecade[which(SppRichPerDecade$LastYear == LastYear[i]),]
  
  # Merge the per grid cell current richness with historical richness of the 'LastYear' i:
  CurrentRich_test <- merge(x = CurrentRich,
                            y = SppRich_LastYear,
                            by = "Cell_Id110",
                            all.x = TRUE)
  
  # Redefine rows with historical richness equal to NA as zero:
  if( nrow(CurrentRich_test[is.na(CurrentRich_test$SppRichness), ]) >= 1) {
    CurrentRich_test[is.na(CurrentRich_test$SppRichness), ]$SppRichness <- 0
    }
  
  # Compute the statistic metric:
  output <- cor.test(x = CurrentRich_test$CurrentRichness, y = CurrentRich_test$SppRichness, method = "pearson")
  
  # Store the statistic metrics in a data.frame, together with the LastYear assessed:
  CorrOutput[[i]] <- data.frame ( LastYear = LastYear[i],
                                  PearsonCorr = output$estimate,
                                  Pvalue = output$p.value )
  
  # Remove unnecessary objects before next iteration:
  rm(SppRich_LastYear, CurrentRich_test, output)
  
  } # end of i for loop

# Bind the data.frame in a single list:
CorrOutput <- rbindlist(CorrOutput)

# Scatterplot of Pearson correlation value against year:
MyPlot1 <- ggplot(CorrOutput, aes(x=LastYear, y=PearsonCorr)) + 
  
  # Add point symbols:
  geom_point(size = 3, color = "white", fill = "black", shape = 21) +
  
  # Add a line connecting the point symbols:
  geom_line() +
  
  # Specify the legend captions:
  ylab(bquote('Correlation between richness patterns')) + 
  xlab(bquote('Year')) +
  
  # Other aesthetics:
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0.2, 0, 0, 0), "cm"), # top right bottom left
        axis.line = element_line(colour="black"),
        axis.ticks = element_line(colour="black"),
        axis.text.y = element_text(size=10, colour="black"),
        axis.text.x = element_text(size=10, colour="black", hjust=0.5),
        axis.title = element_text(size=12, margin=ggplot2::margin(t=0, r=5, b=0, l=0) ,colour="black", face="bold"),
        legend.position="none"); MyPlot1

# Export to disk:
ggsave(filename="Figures/Scatterplot_RichnessCorrelationAcrossTime.png", plot=MyPlot1, width=6, height=5, units="in", bg="white", limitsize=F)
ggsave(filename="Figures/Scatterplot_RichnessCorrelationAcrossTime.pdf", plot=MyPlot1, width=6, height=5, units="in", bg="white", limitsize=F)

#####

# STEP 3 - COMPUTE THE AMOUNT OF CHANGES ACROSS TOP-RICHNESS ASSEMBLAGES
##########################################################################################################################
# STEP 3 - COMPUTE THE AMOUNT OF CHANGES ACROSS TOP-RICHNESS ASSEMBLAGES
rm(list=ls()); gc()

# Load the data table on species richness per grid cell:
SppRichPerDecade <- data.table::fread("Datasets/SppRichPerDecade.csv")

# Convert from data.table to data.frame (it will allow some filtering operations ahead):
SppRichPerDecade <- as.data.frame(SppRichPerDecade)

# Extract a separated data.frame holding the species richness values for the current period:
CurrentRich <- SppRichPerDecade[SppRichPerDecade$LastYear == 2015, c(1:2)]
names(CurrentRich) <- c("Cell_Id110", "CurrentRichness")

# Create a vector with the year indicating the ending period of species descriptions:
LastYear <- seq(1800, 2015, by = 5)  

# Identify top10% of cells with highest current species richness:
TopCells <- CurrentRich[CurrentRich$CurrentRichness >= quantile(CurrentRich$CurrentRichness, 0.7),]
TopCells <- TopCells$Cell_Id110

# Compute how many top10% grid cells are lost based on historical patterns of species richness:
ConstancyTopCells <- list()
for(i in 1:length(LastYear)){
  
  # Separate the richness values of the selected decade:
  SppRich_LastYear <- SppRichPerDecade[which(SppRichPerDecade$LastYear == LastYear[i]),]
  
  # Check the proportion of top10% cells that kept their status of top10% cells with highest richness:
  NCells <- length(TopCells[TopCells %in% SppRich_LastYear$Cell_Id110])
  
  # Store the statistic metrics in a data.frame, together with the LastYear assessed:
  ConstancyTopCells[[i]] <- data.frame ( LastYear = LastYear[i],
                                         NTopCells = NCells,
                                         PropTopCells = NCells / length(TopCells) )
  
  # Remove unnecessary objects before next iteration:
  rm(SppRich_LastYear, NCells)
  
} # end of i for loop

# Bind the data.frame in a single list:
ConstancyTopCells <- rbindlist(ConstancyTopCells)

# Scatterplot of Pearson correlation value against year:
MyPlot1 <- ggplot(ConstancyTopCells, aes(x=LastYear, y=PropTopCells)) + 
  
  # Add point symbols:
  geom_point(size = 3, color = "white", fill = "black", shape = 21) +
  
  # Add a line connecting the point symbols:
  geom_line() +
  
  # Specify the legend captions:
  ylab(bquote('Proportion of correctly classified top cells')) + 
  xlab(bquote('Year')) +
  
  # Other aesthetics:
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0.2, 0, 0, 0), "cm"), # top right bottom left
        axis.line = element_line(colour="black"),
        axis.ticks = element_line(colour="black"),
        axis.text.y = element_text(size=10, colour="black"),
        axis.text.x = element_text(size=10, colour="black", hjust=0.5),
        axis.title = element_text(size=12, margin=ggplot2::margin(t=0, r=5, b=0, l=0) ,colour="black", face="bold"),
        legend.position="none"); MyPlot1

# Export to disk:
ggsave(filename="Figures/Scatterplot_Accuracy_TopCells.png", plot=MyPlot1, width=6, height=5, units="in", bg="white", limitsize=F)
ggsave(filename="Figures/Scatterplot_Accuracy_TopCells.pdf", plot=MyPlot1, width=6, height=5, units="in", bg="white", limitsize=F)

#####

# STEP 4 - COMPUTE THE CORRELATION BETWEEN HISTORICAL RICHNESS AND LATITUDE ACROSS TIME
##########################################################################################################################
# STEP 4 - COMPUTE THE CORRELATION BETWEEN HISTORICAL RICHNESS AND LATITUDE ACROSS TIME
rm(list=ls()); gc()

# Load the data table on species richness per grid cell:
SppRichPerDecade <- data.table::fread("Datasets/SppRichPerDecade.csv")

# Convert from data.table to data.frame (it will allow some filtering operations ahead):
SppRichPerDecade <- as.data.frame(SppRichPerDecade)

# Load the data table on spatial features per grid cell:
grid_cells <- sf::st_read(dsn="Shapefiles/", layer='gridcells_110km')
grid_cells <- sf::st_drop_geometry(grid_cells)
names(grid_cells) <-c ("Cell_Id110", "Long", "Lat", "WWF_Realm", "PropLandArea")
#grid_cells <- data.table::fread("Datasets/SpatialAttributesPerCell.csv")

# Select only grid cells indicating terrestrial lands:
grid_cells <- grid_cells[grid_cells$PropLandArea > 0,]

# Create a vector with the year indicating the ending period of species descriptions:
LastYear <- seq(1800, 2015, by = 5)  

# Compute the Pearson correlation between the historical richness and the latitude:
CorrOutput <- list()
for(i in 1:length(LastYear)){
  
  # Separate the richness values of the selected decade:
  SppRich_LastYear <- SppRichPerDecade[ which(SppRichPerDecade$LastYear == LastYear[i]),]
  
  # Merge the per grid cell current richness with historical richness of the 'LastYear' i:
  grid_cells_test <- merge(x = grid_cells,
                           y = SppRich_LastYear,
                           by = "Cell_Id110",
                           all.x = TRUE)
  
  # Consider only cells with richness >= 1:
  grid_cells_test <- grid_cells_test[which(grid_cells_test$SppRichness>=1),]
  
  # Compute the statistic metric:
  output <- cor.test(x = sqrt(grid_cells_test$Lat^2), y = grid_cells_test$SppRichness, method = "pearson")
  
  # Store the statistic metrics in a data.frame, together with the LastYear assessed:
  CorrOutput[[i]] <- data.frame ( LastYear = LastYear[i],
                                  PearsonCorr = output$estimate,
                                  Pvalue = output$p.value )
  
  # Remove unnecessary objects before next iteration:
  rm(SppRich_LastYear, grid_cells_test, output)
  
} # end of i for loop

# Bind the data.frame in a single list:
CorrOutput <- rbindlist(CorrOutput)

# Repeat the same computation above, but now correcting degrees of freedom for spatial autocorrelation:
SpatialCorrOutput <- list()
for(i in 1:length(LastYear)){
  
  # Separate the richness values of the selected decade:
  SppRich_LastYear <- SppRichPerDecade[ which(SppRichPerDecade$LastYear == LastYear[i]),]
  
  # Merge the per grid cell current richness with historical richness of the 'LastYear' i:
  grid_cells_test <- merge(x = grid_cells,
                           y = SppRich_LastYear,
                           by = "Cell_Id110",
                           all.x = TRUE)
  
  # Consider only cells with richness >= 1:
  grid_cells_test <- grid_cells_test[which(grid_cells_test$SppRichness>=1),]
  
  # Separate a data.frame holding the geographical coordinates:
  coords <- grid_cells_test[, c("Long", "Lat")]
  
  # Compute correlation with spatially corrected degrees of freedom:
  output <- SpatialPack::modified.ttest(x = sqrt(grid_cells_test$Lat^2), 
                                        y = grid_cells_test$SppRichness, 
                                        coords = as.data.frame(coords),
                                        nclass = NULL)
 
  # Store the statistic metrics in a data.frame, together with the LastYear assessed:
  SpatialCorrOutput[[i]] <- data.frame ( LastYear = LastYear[i],
                                  PearsonCorr = output$corr,
                                  df = output$dof,
                                  Pvalue = output$p.value )
  
  # Remove unnecessary objects before next iteration:
  rm(SppRich_LastYear, coords, grid_cells_test, output)
  
} # end of i for loop
SpatialCorrOutput <- rbindlist(SpatialCorrOutput)

# The modified t-test only corrects degrees of freedom, and thus p-values (Pearson values are the same):
CorrOutput$PearsonCorr
SpatialCorrOutput$PearsonCorr
CorrOutput$Pvalue
SpatialCorrOutput$Pvalue

# Add the uncorrected p-values to the CorrOutput
data.table::fwrite(SpatialCorrOutput, file = "Datasets/SpatialCorrOutput.csv")
SpatialCorrOutput <- fread( "Datasets/SpatialCorrOutput.csv")

# Scatterplot of Pearson correlation value against year:
MyPlot1 <- ggplot(SpatialCorrOutput, aes(x=LastYear, y=PearsonCorr)) + 
  
  # Add a line connecting the point symbols:
  geom_line() +
  
  # Add point symbols:
  geom_point(data = SpatialCorrOutput[SpatialCorrOutput$Pvalue<=0.05], size = 3, color = "white", fill = "black", shape = 21) +
  geom_point(data = SpatialCorrOutput[SpatialCorrOutput$Pvalue>0.05], size = 3, color = "red", fill = "white", shape = 21) +
  
  # Specify the legend captions:
  ylab(bquote('Strength of Richness-Latitude Relationship')) + 
  xlab(bquote('Year')) +
  
  # Other aesthetics:
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0.2, 0, 0, 0), "cm"), # top right bottom left
        axis.line = element_line(colour="black"),
        axis.ticks = element_line(colour="black"),
        axis.text.y = element_text(size=10, colour="black"),
        axis.text.x = element_text(size=10, colour="black", hjust=0.5),
        axis.title = element_text(size=12, margin=ggplot2::margin(t=0, r=5, b=0, l=0) ,colour="black", face="bold"),
        legend.position="none"); MyPlot1

# Export to disk:
ggsave(filename="Figures/Scatterplot_RichnessLatitudeAcrossTime.png", plot=MyPlot1, width=6, height=5, units="in", bg="white", limitsize=F)
ggsave(filename="Figures/Scatterplot_RichnessLatitudeAcrossTime.pdf", plot=MyPlot1, width=6, height=5, units="in", bg="white", limitsize=F)

#####
