# Load and install needed package
needed_packages <- c("data.table", "dplyr", "ggplot2", "plyr", "raster",
                     "readxl", "sf", "SpatialPack", "viridis", "cowplot",
                     "rphylopic", "tidyverse")
new.packages <- needed_packages[!(needed_packages %in% 
                                    installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(needed_packages, require, character.only = TRUE)

# packages
rm(list=ls()); gc()
library(data.table)
library(ggplot2)
library(raster)
library(scales)
library(sf)
library(viridis)
library(cowplot)
library(rphylopic)
library(tidyverse)

# STEP 1 - COMPUTE AND MAP ASSEMBLAGE-LEVEL SPECIES RICHNESS FOR THE CURRENT TIME ----
# Load the TetrapodTraits database:
TetraData<-data.table::fread("Datasets/TetrapodTraits_1.0.0.csv")

# Load data grid cell intersections for tetrapods:
vert_assemblages <- data.table::fread("Datasets/Tetrapod_360.csv",
                                      stringsAsFactors=T)

# Merge assemblage-level data (grid cell intersections) with 
# the species-level data informing year of discovery:
vert_assemblages <- merge(x = vert_assemblages,
                          y = TetraData[ ,c("Scientific.Name", 
                                            "TreeTaxon",
                                            "YearOfDescription")],
                          by= 'Scientific.Name', 
                          all.x = TRUE)

# Compute assemblage-level metrics, including richness and average description 
# year for species within grid cells:
grid_cells_data <- vert_assemblages[, .(
                                        SppRichness = .N,
                                        AvgYear = mean(YearOfDescription,
                                                       na.rm=T)),
                                        by=.(Cell_Id110)]

# Remove unnecessary objects:
rm(TetraData, vert_assemblages)

# Load the 360 grid shapefile (spatial feature representing the 110x110km grid cells):
grid_cells_sf <- sf::st_read(dsn = "Shapefiles/", layer='gridcells_110km')
names(grid_cells_sf)<-c("Cell_Id110", "Long", "Lat", "WWF_Realm", 
                        "PropLandArea", "geometry")

# Merge assemblage-level metrics:
grid_cells_sf <- merge(x = grid_cells_sf, 
                       y = grid_cells_data, 
                       by = "Cell_Id110", 
                       all.y = T)

# Extract the equal area projection used in the grid_cells_sf shapefile:
equalareaproj <- raster::crs(grid_cells_sf)

# Load shapefile on the study area and convert to an equal-area projection:
wwf_realms <- sf::read_sf(dsn="Shapefiles/", layer='wwf_realms')  
wwf_realms <- sf::st_transform(wwf_realms, crs=equalareaproj)

# To simplify visualization, omit the Antarctic boundaries 
# (they are poorly visible using the equal area projection:
wwf_realms <- wwf_realms[wwf_realms$wwf_realm!="Antarctic",]

# For better mapping aesthetics, perform the spatial intersection 
# between grid_cells_sf and wwf_realms:
grid_cells_sf <- sf::st_intersection(wwf_realms, grid_cells_sf) 

# Filter cells with at least 1 species:
grid_cells_sf <- grid_cells_sf[grid_cells_sf$SppRichness >= 1, ]

# Build the plot:
MyMap <- ggplot2::ggplot() +
      
  # Add the shapefile layer with cell colour informed by 'SppRichness' variable:
  geom_sf(data=grid_cells_sf, aes(fill=SppRichness), colour=NA) +
      
  # Specify the color ramp:
  scale_fill_gradientn(colours= viridis::viridis(n = 100, option = "plasma",
                                                 direction = 1), 
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
ggsave(filename="Figures/Map.png", plot=MyMap, width=12, height=8, units="in",
       bg="white", limitsize=F)
ggsave(filename="Figures/Map.pdf", plot=MyMap, width=12, height=8, units="in", 
       bg="white", limitsize=F)


# STEP 2 - COMPUTE THE CORRELATION BETWEEN CURRENT SPECIES RICHNESS AND HISTORICAL RICHNESS ----
rm(list=ls()); gc()

# Load the TetrapodTraits database:
TetraData <- data.table::fread("Datasets/TetrapodTraits_1.0.0.csv")

# Load data grid cell intersections for tetrapods:
vert_assemblages <- data.table::fread("Datasets/Tetrapod_360.csv", stringsAsFactors=T)

# Merge assemblage-level data (grid cell intersections) with the species-level 
# data informing year of discovery:
vert_assemblages <- merge(x = vert_assemblages, 
                          y = TetraData[,c("Scientific.Name", 
                                           "TreeTaxon",
                                           "YearOfDescription")],
                          by = 'Scientific.Name',
                          all.x = TRUE)

# It is possible to filter one specific tetrapod group using 
# levels of 'TreeTaxon' variable:
levels(as.factor(vert_assemblages$TreeTaxon))

# Replace "Squamates" and "TurtCroc" to "Reptilia
vert_assemblages$TreeTaxon <- gsub("Squamates", "Reptilia",
                                   vert_assemblages$TreeTaxon)

vert_assemblages$TreeTaxon <- gsub("TurtCroc", "Reptilia",
                                   vert_assemblages$TreeTaxon)

# Tetrapod communities 
# vert_assemblages <- vert_assemblages[vert_assemblages$TreeTaxon=="Birds",]

# extinc tetrapods
#na_rows <- vert_assemblages[is.na(vert_assemblages$TreeTaxon),]
#levels(as.factor(na_rows$Scientific.Name))

# Create a vector with the year indicating the ending period of species descriptions:
LastYear <- seq(1800, 2015, by = 5)  

# Create vector contains tetrapods groups
groups <- levels(as.factor(vert_assemblages$TreeTaxon))

# Create an empty list to store the ouput of the for loop ahead:
SppRichPerDecade_Mammals <- list()
SppRichPerDecade_Birds <- list()
SppRichPerDecade_Reptiles <- list()
SppRichPerDecade_Amphibians <- list()

# Compute the assemblage-level species richness using species 
# described until different ending periods:
for(i in 1:length(LastYear)){
  
  # loop para adicionar a lista de cada grupo
  for(group in groups){
    
    # Subset the 'vert_assemblages' data:
    data_subset <- vert_assemblages[vert_assemblages$YearOfDescription <= LastYear[i] 
                                    & vert_assemblages$TreeTaxon == group,]
    
    # Compute assemblage-level metrics, including richness and 
    # average description year for species within grid cells:
    data_subset <- data_subset[, .(SppRichness = .N), by=.(Cell_Id110)]
    
    # Add a new column informing the ending period used of species descriptions:
    data_subset$LastYear <- LastYear[i]
    
    # Armazenar as informações na lista correspondente ao grupo
    if(group == "Mammals"){
      SppRichPerDecade_Mammals[[i]] <- data_subset
    } else if(group == "Birds"){
      SppRichPerDecade_Birds[[i]] <- data_subset
    } else if(group == "Reptilia"){
      SppRichPerDecade_Reptiles[[i]] <- data_subset
    } else if(group == "Amphibians"){
      SppRichPerDecade_Amphibians[[i]] <- data_subset
    }
    
    # Remove unnecessary object before next iteration:
    rm(data_subset)
    
  } # end of group loop
  
} # end of i for loop

# Remove unnecessary objects:
# rm(TetraData, vert_assemblages)

# Unlist in different data.frame:
#SppRichPerDecade_Mammals <- data.table::rbindlist(SppRichPerDecade_Mammals)
#SppRichPerDecade_Birds <- data.table::rbindlist(SppRichPerDecade_Birds)
#SppRichPerDecade_Reptiles <- data.table::rbindlist(SppRichPerDecade_Reptiles )
#SppRichPerDecade_Amphibians <- data.table::rbindlist(SppRichPerDecade_Amphibians)

# Save .RData
#SppRichPerDecade_Amphibians <- as.data.frame(SppRichPerDecade_Amphibians)
#SppRichPerDecade_Reptiles <- as.data.frame(SppRichPerDecade_Reptiles)
#SppRichPerDecade_Birds <- as.data.frame(SppRichPerDecade_Birds)
#SppRichPerDecade_Mammals <- as.data.frame(SppRichPerDecade_Mammals)

# Extract a separated data.frame holding the species richness values
# for the current period:
CurrentRichAmphibia <- SppRichPerDecade_Amphibians[
  SppRichPerDecade_Amphibians$LastYear == 2015,
  c(1:2)
  ]
CurrentRichReptiles <- SppRichPerDecade_Reptiles[
  SppRichPerDecade_Reptiles$LastYear == 2015,
  c(1:2)
  ]
CurrentRichBirds <- SppRichPerDecade_Birds[
  SppRichPerDecade_Birds$LastYear == 2015,
  c(1:2)
  ]
CurrentRichMammals <- SppRichPerDecade_Mammals[
  SppRichPerDecade_Mammals$LastYear == 2015,
  c(1:2)]

# criando vetor com as celulas preenchidas com celulas presentes na riqueza atual
# 2015
id_amp <- CurrentRichAmphibia$Cell_Id110
id_rep <- CurrentRichReptiles$Cell_Id110
id_birds <- CurrentRichBirds$Cell_Id110
id_mammals <- CurrentRichMammals$Cell_Id110

# gerar para cada Cell uma replica de cada um dos 44 anos (LastYear)
id_amp <- gerar_dataframe_anos(id_amp, LastYear)
id_rep <- gerar_dataframe_anos(id_rep, LastYear)
id_birds <- gerar_dataframe_anos(id_birds, LastYear)
id_mammals <- gerar_dataframe_anos(id_mammals, LastYear)

# o numero de linhas presentes (2015) vezes 44 tem que ser igual o numero de 
# linhas unicas em 2015 - 
# isso significa que para cada celula, teremos 44 time-frames associados
dim(CurrentRichAmphibia)[1] * 44 == nrow(id_amp)
dim(CurrentRichReptiles)[1] * 44 == nrow(id_rep)
dim(CurrentRichBirds)[1] * 44 == nrow(id_birds)
dim(CurrentRichMammals)[1] * 44 == nrow(id_mammals)

# rename cols
names(CurrentRichAmphibia) <- c("Cell_Id110", "CurrentRichness")
names(CurrentRichReptiles) <- c("Cell_Id110", "CurrentRichness")
names(CurrentRichBirds) <- c("Cell_Id110", "CurrentRichness")
names(CurrentRichMammals) <- c("Cell_Id110", "CurrentRichness")

# checando celulas duplicadas
any(duplicated(CurrentRichAmphibia$Cell_Id110))
any(duplicated(CurrentRichReptiles$Cell_Id110))
any(duplicated(CurrentRichBirds$Cell_Id110))
any(duplicated(CurrentRichMammals$Cell_Id110))

id_amp <- CurrentRichAmphibia$Cell_Id110
id_rep <- CurrentRichReptiles$Cell_Id110
id_birds <- CurrentRichBirds$Cell_Id110
id_mammals <- CurrentRichMammals$Cell_Id110

# criando as celulas dos anos que nao haviam especies descritas no passado
# por exemplo, o primeiro ano da celula 18173 eh em 1835, pq os anos anteriores
# nao haviam especies descritas nessa celula, so recebendo a primeira spp em 1835
# por isso elas nao foram computadas com riqueza 0 nos anos anteriores
sum(SppRichPerDecade_Amphibians$Cell_Id110 == 18173) # tem que ter 44 repeticoes
# pois sao 44 repeticoes em LastYear

# funcao criada para adicionar os anos que faltam nas celulas presentes
# para isso, precisa definir um vetor (LastYear) com os anos de interesse
id_amp <- gerar_anos_faltantes(id_amp, LastYear)
id_rep <- gerar_anos_faltantes(id_rep, LastYear)
id_birds <- gerar_anos_faltantes(id_birds, LastYear)
id_mammals <- gerar_anos_faltantes(id_mammals, LastYear)

SppRichPerDecade_Amphibians <- full_join(
  id_amp,
  SppRichPerDecade_Amphibians, 
  by = c("Cell_Id110", "LastYear")
)
SppRichPerDecade_Reptiles <- full_join(
  id_rep,
  SppRichPerDecade_Reptiles, 
  by = c("Cell_Id110", "LastYear")
)
SppRichPerDecade_Birds <- full_join(
  id_birds,
  SppRichPerDecade_Birds, 
  by = c("Cell_Id110", "LastYear")
)
SppRichPerDecade_Mammals <- full_join(
  id_mammals,
  SppRichPerDecade_Mammals, 
  by = c("Cell_Id110", "LastYear")
)

table(SppRichPerDecade_Mammals$Cell_Id110)
sum(SppRichPerDecade_Amphibians$Cell_Id110 == 18173) # 44 repeticoes

# Export to disk:
data.table::fwrite(SppRichPerDecade_Amphibians, 
                   file="Datasets/SppRichPerDecade_Amphibians.csv")
data.table::fwrite(SppRichPerDecade_Reptiles, 
                   file="Datasets/SppRichPerDecade_Reptiles.csv")
data.table::fwrite(SppRichPerDecade_Birds, 
                   file="Datasets/SppRichPerDecade_Birds.csv")
data.table::fwrite(SppRichPerDecade_Mammals, 
                   file="Datasets/SppRichPerDecade_Mammals.csv")

# Remove cells with zero richness:
#CurrentRich <- CurrentRich[CurrentRich$CurrentRichness >= 1,]
head(CurrentRichMammals)

# Criando listas para que seja possivel fazer apenas um unico for
# aqui as ordens precisam ser as mesmas na lista 
# amphibians > reptiles > birds > mammals
SppRichPerDecade_list <- list(SppRichPerDecade_Amphibians, 
                              SppRichPerDecade_Reptiles, 
                              SppRichPerDecade_Birds,
                              SppRichPerDecade_Mammals)

# riqueza presente
CurrentRich_list <- list(CurrentRichAmphibia,
                         CurrentRichReptiles,
                         CurrentRichBirds,
                         CurrentRichMammals)

# View(CurrentRich_list[[i]])

# Compute the Pearson correlation between the current richness and 
# the richness of different ending periods:
CorrOutput_list <- list()

for(j in 1:length(SppRichPerDecade_list)) {
  
  SppRichPerDecade <- SppRichPerDecade_list[[j]]
  CurrentRich <- CurrentRich_list[[j]]
  CorrOutput <- list()  # Lista para armazenar os resultados para o conjunto atual
  
  for(i in 1:length(LastYear)){
    
    # Separe os valores de riqueza da década selecionada:
    SppRich_LastYear <- SppRichPerDecade[which(SppRichPerDecade$LastYear == LastYear[i]),]
    
    # Mescle a riqueza atual por célula com a riqueza histórica da 'LastYear' i:
    CurrentRich_test <- merge(x = CurrentRich,
                              y = SppRich_LastYear,
                              by = "Cell_Id110",
                              all.x = TRUE)
    
    # Redefina linhas com riqueza histórica igual a NA como zero:
    if( nrow(CurrentRich_test[is.na(CurrentRich_test$SppRichness), ]) >= 1) {
      CurrentRich_test[is.na(CurrentRich_test$SppRichness), ]$SppRichness <- 0
    }
    
    # Compute the statistic metric:
    output <- cor.test(x = CurrentRich_test$CurrentRichness,
                       y = CurrentRich_test$SppRichness,
                       method = "pearson")
    
    # Armazene as métricas estatísticas em um data.frame, juntamente com o LastYear avaliado:
    CorrOutput[[i]] <- data.frame ( LastYear = LastYear[i],
                                    PearsonCorr = output$estimate,
                                    Pvalue = output$p.value )
    
    # Remova objetos desnecessários antes da próxima iteração:
    rm(SppRich_LastYear, CurrentRich_test, output)
    
  } # fim do loop i
  
  # Armazene os resultados para o conjunto atual na lista principal
  CorrOutput_list[[j]] <- CorrOutput
  
} # fim do loop j

# correlacoes de cada grupo
CorrOutput_Amphibia <- rbindlist(CorrOutput_list[[1]])
CorrOutput_Reptiles <- rbindlist(CorrOutput_list[[2]])
CorrOutput_Birds <- rbindlist(CorrOutput_list[[3]])
CorrOutput_Mammals <- rbindlist(CorrOutput_list[[4]])

# Combine os dados em um único dataframe
CorrOutput_Amphibia$Group <- "Amphibia"
CorrOutput_Reptiles$Group <- "Reptiles"
CorrOutput_Birds$Group <- "Birds"
CorrOutput_Mammals$Group <- "Mammals"

# Combinando os dados
combined_data <- rbind(CorrOutput_Amphibia, 
                       CorrOutput_Reptiles,
                       CorrOutput_Birds, 
                       CorrOutput_Mammals)

# Escolhendo a paleta
palette_colors <- RColorBrewer::brewer.pal(4, "Set2")

# Crie o gráfico
MyPlot1 <- ggplot(combined_data, aes(x=LastYear, y=PearsonCorr, color=Group)) + 
  
  # Adiciona símbolos de ponto
  geom_point(size = 3, aes(fill=Group), shape = 21) +
  
  # Adiciona uma linha conectando os símbolos de ponto
  geom_line(aes(group=Group)) +
  
  # Especifica as legendas
  ylab(bquote('Correlation between richness patterns')) + 
  xlab(bquote('Year')) +
  
  # Outras estéticas
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0.2, 0, 0, 0), "cm"), # top right bottom left
        axis.line = element_line(colour="black"),
        axis.ticks = element_line(colour="black"),
        axis.text.y = element_text(size=10, colour="black"),
        axis.text.x = element_text(size=10, colour="black", hjust=0.5),
        axis.title = element_text(size=12, 
                                  margin=ggplot2::margin(t=0, r=5, b=0, l=0),
                                  colour="black", face="bold"),
        legend.position="right") +
  scale_fill_manual(values = palette_colors) +
  scale_color_manual(values = palette_colors) +
  theme_minimal_grid(12)

MyPlot1

# Export to disk:
ggsave(filename="Figures/Scatterplot_RichnessCorrelationAcrossTime.png",
       plot=MyPlot1, width=6, height=5, units="in", bg="white", limitsize=F)
ggsave(filename="Figures/Scatterplot_RichnessCorrelationAcrossTime.pdf",
       plot=MyPlot1, width=6, height=5, units="in", bg="white", limitsize=F)

# save datasets for the next step
save(SppRichPerDecade_Amphibians,
     SppRichPerDecade_Reptiles,
     SppRichPerDecade_Birds,
     SppRichPerDecade_Mammals,
     file = "Datasets/SppRichPerDecade.RData")

# save current richness
save(CurrentRichAmphibia,
     CurrentRichReptiles,
     CurrentRichBirds,
     CurrentRichMammals,
     file = "Datasets/CurrentRichness.RData")

# STEP 3 - COMPUTE THE CORRELATION OF WWF REALMS ACROSS TETRAPODS ####
rm(list=ls()); gc()
load("Datasets/SppRichPerDecade.RData")
load("Datasets/CurrentRichness.RData")

# Load the 360 grid shapefile (spatial feature representing the 110x110km grid cells):
grid_cells_sf <- sf::st_read(dsn = "Shapefiles/", layer='gridcells_110km')
names(grid_cells_sf)<-c("Cell_Id110", "Long", "Lat", "WWF_Realm", 
                        "PropLandArea", "geometry")

# Extract the equal area projection used in the grid_cells_sf shapefile:
equalareaproj <- raster::crs(grid_cells_sf)

# Load shapefile on the study area and convert to an equal-area projection:
wwf_realms <- sf::read_sf(dsn="Shapefiles/", layer='wwf_realms')  
wwf_realms <- sf::st_transform(wwf_realms, crs=equalareaproj)

# To simplify visualization, omit the Antarctic boundaries 
# (they are poorly visible using the equal area projection:
wwf_realms <- wwf_realms[!is.na(wwf_realms$wwf_realm) &
                           wwf_realms$wwf_realm!="Antarctic",]
grid_cells_sf <- grid_cells_sf[!is.na(grid_cells_sf$WWF_Realm) &
                                 grid_cells_sf$WWF_Realm != "Antarctic", ]

# For better mapping aesthetics, perform the spatial intersection 
# between grid_cells_sf and wwf_realms:
grid_cells_sf <- sf::st_intersection(wwf_realms, grid_cells_sf) 
# add wwf_realm to id grids
grid_id <- sf::st_drop_geometry(grid_cells_sf)[,c("Cell_Id110", "WWF_Realm")]

#
# Criando listas para que seja possivel fazer apenas um unico for
# aqui as ordens precisam ser as mesmas na lista 
# amphibians > reptiles > birds > mammals
SppRichPerDecade_list <- list(SppRichPerDecade_Amphibians, 
                              SppRichPerDecade_Reptiles, 
                              SppRichPerDecade_Birds,
                              SppRichPerDecade_Mammals)

# riqueza presente
CurrentRich_list <- list(CurrentRichAmphibia,
                         CurrentRichReptiles,
                         CurrentRichBirds,
                         CurrentRichMammals)

# Create a vector with the year indicating the ending period of species descriptions:
LastYear <- seq(1800, 2015, by = 5)  

# adicionando informacao do reals wwf
for(i in 1:length(SppRichPerDecade_list)){
  SppRichPerDecade_list[[i]] <- merge(x = SppRichPerDecade_list[[i]],
                                      y = grid_id,
                                      by = "Cell_Id110")
}

for(i in 1:length(CurrentRich_list)){
  CurrentRich_list[[i]] <- merge(x = CurrentRich_list[[i]],
                                 y = grid_id,
                                 by = "Cell_Id110")
}

# correlation between groups and realms
CorrOutput_list <- list()

for (j in 1:length(SppRichPerDecade_list)) {
  
  SppRichPerDecade <- SppRichPerDecade_list[[j]]
  CurrentRich <- CurrentRich_list[[j]]
  CorrOutput <- list()  # Lista para armazenar os resultados para o conjunto atual
  
  unique_realms <- unique(SppRichPerDecade$WWF_Realm)
  
  for (realm in unique_realms) {
    
    SppRichPerDecade_realm <- SppRichPerDecade[SppRichPerDecade$WWF_Realm == realm, ]
    CurrentRich_realm <- CurrentRich[CurrentRich$WWF_Realm == realm, ]
    
    CorrOutput_realm <- list()
    
    for (i in 1:length(LastYear)) {
      
      # Separe os valores de riqueza da década selecionada:
      SppRich_LastYear <- SppRichPerDecade_realm[which(SppRichPerDecade_realm$LastYear == LastYear[i]), ]
      
      # Mescle a riqueza atual por célula com a riqueza histórica da 'LastYear' i:
      CurrentRich_test <- merge(x = CurrentRich_realm,
                                y = SppRich_LastYear,
                                by = "Cell_Id110",
                                all.x = TRUE)
      
      # Redefina linhas com riqueza histórica igual a NA como zero:
      if (nrow(CurrentRich_test[is.na(CurrentRich_test$SppRichness), ]) >= 1) {
        CurrentRich_test[is.na(CurrentRich_test$SppRichness), ]$SppRichness <- 0
      }
      
      # Compute the statistic metric:
      output <- cor.test(x = CurrentRich_test$CurrentRichness,
                         y = CurrentRich_test$SppRichness,
                         method = "pearson")
      
      # Armazene as métricas estatísticas em um data.frame, juntamente com o LastYear avaliado:
      CorrOutput_realm[[i]] <- data.frame ( LastYear = LastYear[i],
                                            Realm = realm,
                                            PearsonCorr = output$estimate,
                                            Pvalue = output$p.value )
      
      # Remova objetos desnecessários antes da próxima iteração:
      rm(SppRich_LastYear, CurrentRich_test, output)
      
    } # fim do loop i
    
    # Armazene os resultados para o realm atual na lista principal
    CorrOutput[[realm]] <- do.call(rbind, CorrOutput_realm)
    
  } # fim do loop realm
  
  # Armazene os resultados para o conjunto atual na lista principal
  CorrOutput_list[[j]] <- do.call(rbind, CorrOutput)
  
} # fim do loop j

# correlacoes de cada grupo
CorrOutput_Amphibia <- CorrOutput_list[[1]]
CorrOutput_Reptiles <- CorrOutput_list[[2]]
CorrOutput_Birds <- CorrOutput_list[[3]]
CorrOutput_Mammals <- CorrOutput_list[[4]]

# Combine os dados em um único dataframe
CorrOutput_Amphibia$Group <- "Amphibia"
CorrOutput_Reptiles$Group <- "Reptiles"
CorrOutput_Birds$Group <- "Birds"
CorrOutput_Mammals$Group <- "Mammals"

# Combinando os dados
combined_data <- rbind(CorrOutput_Amphibia, 
                       CorrOutput_Reptiles,
                       CorrOutput_Birds, 
                       CorrOutput_Mammals)

# Escolhendo a paleta
palette_colors <- RColorBrewer::brewer.pal(7, "Set2")

# correlation plots
ggplot(combined_data, aes(x=LastYear, y=PearsonCorr, color=Realm)) + 
  # Adiciona símbolos de ponto
  geom_point(size = 3, aes(fill=Realm), shape = 21) +
  
  # Adiciona uma linha conectando os símbolos de ponto
  geom_line(aes(group=Realm)) +
  
  # Especifica as legendas
  ylab(bquote('Correlation between richness patterns')) + 
  xlab(bquote('Year')) +
  
  # Outras estéticas
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0.2, 0, 0, 0), "cm"), # top right bottom left
        axis.line = element_line(colour="black"),
        axis.ticks = element_line(colour="black"),
        axis.text.y = element_text(size=10, colour="black"),
        axis.text.x = element_text(size=10, colour="black", hjust=0.5),
        axis.title = element_text(size=12, 
                                  margin=ggplot2::margin(t=0, r=5, b=0, l=0),
                                  colour="black", face="bold"),
        legend.position="right") +
  scale_fill_manual(values = palette_colors) +
  scale_color_manual(values = palette_colors) +
  theme_minimal_grid(12) +
  
  # Divide os gráficos por grupo
  facet_wrap(~ Group, ncol = 2)

# TODO Merge with spatial data
SppRich1800_Amphibians <- merge(x = grid_cells_sf, 
                       y = SppRichPerDecade_Amphibians[LastYear == 1850, ], 
                       by = "Cell_Id110", 
                       all.y = T)

# Build the spatial plot:
ggplot2::ggplot() +
  
  # Add the shapefile layer with cell colour informed by 'SppRichness' variable:
  geom_sf(data=SppRich1800_Amphibians, aes(fill=SppRichness), colour=NA) +
  
  # Specify the color ramp:
  scale_fill_gradientn(colours= viridis::viridis(n = 100, option = "plasma",
                                                 direction = 1), 
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
#dir.create("Figures", showWarnings = FALSE)
#ggsave(filename="Figures/Map.png", plot=MyMap, width=12, height=8, units="in",
#       bg="white", limitsize=F)
#ggsave(filename="Figures/Map.pdf", plot=MyMap, width=12, height=8, units="in", 
#       bg="white", limitsize=F)

# STEP 4 - COMPUTE GLOBAL SPECIES RICHNESS THROUGH THE TIME FOR EACH REALM ----
rm(list=ls()); gc()
# Load the TetrapodTraits database:
TetraData<-data.table::fread("Datasets/TetrapodTraits_1.0.0.csv")

TetraData <- TetraData %>% select(
  "Scientific.Name", "YearOfDescription", "Afrotropic", "Australasia", 
  "IndoMalay", "Nearctic", "Neotropic", "Oceania", "Palearctic", "Antarctic"
)

# STEP 5 - COMPUTE THE AMOUNT OF CHANGES ACROSS TOP-RICHNESS ASSEMBLAGES ----
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

# STEP 5 - COMPUTE THE CORRELATION BETWEEN HISTORICAL RICHNESS AND LATITUDE ACROSS TIME
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
