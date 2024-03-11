Migration_matrix <- function(working_folder, models_file, model_number, simulation_number, path_to_rasters, seq_demes, non_seq_demes, landscape_effect_file,dispersal_capacity,Resistance_script){
  
  library(raster)
  library(gtools)
  library(gdistance)
  
  options(scipen=999)
  options(warn=-1)
  
  quiet <- function(x) {
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  }
  
  scale_to_0_1 <- function(x){ 
    values <- getValues(x)
    x[which(values > as.numeric(quantile(x, 0.6)))] <- 1
    (x- cellStats(x,"min"))/(cellStats(x,"max") - cellStats(x,"min"))
  }
  
  scale_0_1 <- function(x){ 
    (x- cellStats(x,"min"))/(cellStats(x,"max") - cellStats(x,"min"))
  }
  
  Models <- read.table(models_file,h=T)
  Models_sim <- Models[model_number,]
  layers_sim <- colnames(Models_sim[which(Models_sim == 1)])
  
  layers_names <- list()
  
  for (x in layers_sim){
    setwd(file.path(path_to_rasters,x))
    n_files <- length(list.files(pattern=".tif"))
    layers_names <- c(layers_names, list(gsub(".tif","",list.files(pattern=".tif"))))
    for (i in 1:n_files){
      assign(gsub(".tif","",list.files(pattern="tif")[i]),raster(file.path(path_to_rasters,x,list.files(pattern="tif")[i])))
      if ((!layers_sim == "IBD")[1]){
        assign(gsub(".tif","",list.files(pattern="tif")[i]),scale_0_1(get(gsub(".tif","",list.files(pattern="tif")[i]))))
      }}}
  
  names(layers_names) <- layers_sim
  
  for (x in 1:length(layers_names)){
    layers_names[[x]] <- mixedsort(layers_names[[x]])
  }
  
  seq_demes_coords <- read.table(seq_demes,h=T)
  non_seq_demes_coords <- read.table(non_seq_demes,h=T)
  
  n_seq_demes <- length(seq_demes_coords[,1])
  n_non_seq_demes <- length(non_seq_demes_coords[,1])
  
  all_demes_coords <- rbind(seq_demes_coords, non_seq_demes_coords)
  
  colnames(all_demes_coords) <- c("long","lat")
  all_demes_coords <- SpatialPointsDataFrame(coords = all_demes_coords, data = all_demes_coords,
                                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) 
  if ((!layers_sim == "IBD")[1]){
    
    land_effect <- read.table(landscape_effect_file,h=T)
    
    scaled_layers <- list()
    list_climate <- list()
    resistance_values <- data.frame(land_effect[simulation_number,])
    colnames(resistance_values) <- colnames(land_effect)
    index <- which(Models_sim == 1)
    sum_resitance_surface <- numeric()
    
    for (i in 1:length(which(Models_sim == 1))){
      for (u in 1:length(layers_names[[i]])){
        if (length(layers_names[[i]]) == 1){
          scaled_layers[[i]] <- as.numeric(resistance_values[colnames(Models_sim)[index[i]]]) * scale_to_0_1(get(layers_names[[i]][u]))
        } else {
          list_climate <- c(list_climate,as.numeric(resistance_values[colnames(Models_sim)[index[i]]]) * scale_to_0_1(get(layers_names[[i]][u])))
          if (u == length(layers_names[[i]])){
            scaled_layers[[i]] <- list_climate
          }}}}
    
      true_resistance_surface <- list()
      if (length(unlist(scaled_layers)) < 7){
        for (x in 1:length(scaled_layers)){
          if (x == 1){
            resistance_surface <- Reduce("+", c(scaled_layers[[x]])) 
          } else {
            resistance_surface <- Reduce("+", c(resistance_surface,scaled_layers[[x]])) 
          }
          if (x == length(scaled_layers)){
            true_resistance_surface[[1]] <- resistance_surface
            for (x in 1:length(true_resistance_surface)){
              if ((!layers_sim == "IBD")[1]){
                true_resistance_surface[[x]] <- true_resistance_surface[[1]]
              } else {
                true_resistance_surface[[x]][] <- 0.1
              }}}
        }} else if (length(unlist(scaled_layers)) > 7) {
          if (!length(scaled_layers[[1]]) > 1000){
            true_resistance_surface <- unlist(scaled_layers)
          } else {
          for (x in 1:length(scaled_layers)){
            if (x == 1){
              resistance_surface <- Reduce("+", c(scaled_layers[[x]])) 
            } else {
              if (!(length(scaled_layers[[x]]) > 2 && length(scaled_layers[[x]]) < 100)){
                resistance_surface <- Reduce("+", c(resistance_surface,scaled_layers[[x]])) 
              } else {
                for (i in 1:length(scaled_layers[[x]])){
                  true_resistance_surface[[i]] <- Reduce("+", c(resistance_surface,scaled_layers[[x]][[i]])) 
                }}}}}}    

    if (length(land_effect) > 1){
      for (x in 1:length(true_resistance_surface)){
        true_resistance_surface[[x]] <- scale_0_1(true_resistance_surface[[x]] * mean(as.numeric(land_effect[simulation_number,])))
      }}
  }
  
  if ((!layers_sim == "IBD")[1]){
    for (x in 1:length(true_resistance_surface)){
      true_resistance_surface[[x]] <- scale_0_1(true_resistance_surface[[x]])
      values <- getValues(true_resistance_surface[[x]])
      index <- which(values < 0.1)
      true_resistance_surface[[x]][index] <- 0.1
    }} else {
      true_resistance_surface <- list()
      true_resistance_surface[[1]] <- IBD
      true_resistance_surface[[1]][] <- 0.1
    }
  
  migration_matrix <- list()
  d.cap <- runif(1,dispersal_capacity[1],dispersal_capacity[2])
  for (x in 1:length(true_resistance_surface)){
    tr <- transition(true_resistance_surface[[x]], function(x) 1/mean(x),8)
    resistance_model <- as.matrix(costDistance(tr, all_demes_coords))
    resistance_model[intersect(which(resistance_model < 1), which(resistance_model > 0))] <- 1
    migration_matrix[[x]] <- (1/resistance_model^3) * d.cap
    diag(migration_matrix[[x]]) <- 0
    migration_matrix[[x]] <- cbind(rep(0,length(all_demes_coords)), migration_matrix[[x]])
    migration_matrix[[x]] <- rbind(rep(0,length(all_demes_coords)+1), migration_matrix[[x]])
    migration_matrix[[x]] <- round(migration_matrix[[x]], digits=6)
    
    if(x == length(true_resistance_surface)){
      migration_matrix[[x+1]] <- matrix(0,dim(migration_matrix[[x]])[1],dim(migration_matrix[[x]])[2])
    }}
  
  setwd(working_folder)
  
  if(!dir.exists("Migration_matrices")){
    dir.create("Migration_matrices")
  }
  
  setwd("Migration_matrices")
  
  save(migration_matrix, file=paste0("Simulation_",simulation_number,".RData"))
  options(warn=0)
  
}




