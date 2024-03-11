landscape_effect <- function(path_to_folder,models_file,model_number,nsim){
  
  setwd(path_to_folder)
  
  Models <- read.table(models_file,h=T)
  Models_sim <- Models[model_number,]
  layers_sim <- colnames(Models_sim[which(Models_sim == 1)])
  
  land_effect <- data.frame(matrix(NA,nsim,length(layers_sim)))
  colnames(land_effect) <- layers_sim
  
  for (x in 1:nsim){
    for (i in 1:length(layers_sim)){
      if (layers_sim[i] == "Habitat_shift"){
        land_effect[x,i] <- round(runif(1,-5,-2),digits=2)
      } else if (layers_sim[i] == "Niche_suitability"){
        land_effect[x,i] <- round(runif(1,2,5),digits=2)
        }else {
        land_effect[x,i] <- round(runif(1,2,5),digits=2)
      }}}
  
  write.table(land_effect,"Landscape_effect.txt",row.names = F, quote = F)
  
}

