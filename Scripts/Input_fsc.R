input_fastsimcoal <- function(working_folder,
                              models_file,
                              model_number,
                              simulation_number,
                              PopSizeAnc,
                              TDIV,
                              Number_ind_loci,
                              number_of_demes,
                              pop_size_range,
                              number_of_individuals_sampled,
                              path_to_migration_matrix){
  setwd(working_folder)
  
  pop_size_range <- sample(pop_size_range[1]:pop_size_range[2],number_of_demes,r=T)
  
  if (!dir.exists("Simulations")){
    dir.create("Simulations")
  }
  
  setwd("Simulations")
  
  if (!dir.exists(paste0("Simulation_",simulation_number))){
    dir.create(paste0("Simulation_",simulation_number))
  }
  
  setwd(paste0("Simulation_",simulation_number))
  
  Models <- read.table(models_file,h=T)
  Models_sim <- Models[model_number,]
  layers_sim <- colnames(Models_sim[which(Models_sim == 1)])
  
  PopSizeAnc_input <- ceiling(runif(1,PopSizeAnc[1], PopSizeAnc[2]))
  
  load(paste0(path_to_migration_matrix,"/Simulation_",simulation_number,".RData"))
  
  cat("", file="./input.par", append=FALSE, sep = "")
  
  part1 <- "//Parameters for the coalescence simulation program : simcoal.exe"
  cat(part1, file="./input.par", append=TRUE, sep = "\n")
  
  part2 <- paste(number_of_demes+1, "samples to simulate :")
  cat(part2, file="./input.par", append=TRUE, sep = "\n")
  
  part3 <- "//Population effective sizes (number of genes)"
  cat(part3, file="./input.par", append=TRUE, sep = "\n")
  
  for (x in 1:(number_of_demes+1)){
    if (x == 1){
      cat(PopSizeAnc_input, file="./input.par", append=TRUE, sep = "\n")
    } else {
      cat(pop_size_range[x-1], file="./input.par", append=TRUE, sep = "\n")
    }}
  
  part4 <- "//Samples sizes and samples age"
  cat(part4, file="./input.par", append=TRUE, sep = "\n")
  
  for (x in 1:(length(number_of_individuals_sampled)+1)){
    if (x == 1){
      cat("0", file="./input.par", append=TRUE, sep = "\n")
    } else {
      cat(number_of_individuals_sampled[x-1], file="./input.par", append=TRUE, sep = "\n")
    }}
  
  part5 <- "//Growth rates: negative growth implies population expansion"
  cat(part5, file="./input.par", append=TRUE, sep = "\n")
  
  for (x in 1:(number_of_demes+1)){
    cat("0", file="./input.par", append=TRUE, sep = "\n")
  }
  
  part6 <- "//Number of migration matrices : 0 implies no migration between demes"
  cat(part6, file="./input.par", append=TRUE, sep = "\n")
  cat(length(migration_matrix), file="./input.par", append=TRUE, sep = "\n")
  
  for (x in 1:length(migration_matrix)){
    cat("//Migration matrix", file="./input.par", append=TRUE, sep = "\n")
    write.table(migration_matrix[[x]], file="./input.par", append=TRUE,col.names = F, row.names = F)
  }
  
  part7 <- "//historical event: time, source, sink, migrants, new deme size, growth rate, migr mat index"
  cat(part7, file="./input.par", append=TRUE, sep = "\n")
  
  if (length(migration_matrix) > 1 && length(migration_matrix) < 5){
    part8 <- paste(number_of_demes, "historical event")
    cat(part8, file="./input.par", append=TRUE, sep = "\n")
  } else if (length(migration_matrix) > 5 && length(migration_matrix) < 12) {
    part8 <- paste(number_of_demes+length(migration_matrix)-1, "historical event")
    cat(part8, file="./input.par", append=TRUE, sep = "\n")
  } else if (length(migration_matrix) > 15 && length(migration_matrix) < 30){
    part8 <- paste(number_of_demes+length(migration_matrix)-2, "historical event")
    cat(part8, file="./input.par", append=TRUE, sep = "\n")
  }
  
    if (length(migration_matrix) > 1 && length(migration_matrix) < 5){
	migration_times <- seq(21000,1000,-1000)
  } else if (length(migration_matrix) > 5 && length(migration_matrix) < 12) {
    Current <- round(runif(1,50,200))
    Meghalayan <- round(runif(1,300,4199))
    Northgrippian  <- round(runif(1,4200,8325))
    Greenlandian  <- round(runif(1,8326,11669))
    Younger_Dryas_Stadial <- round(runif(1,11700,12899))
    Bolling_Allerod <- round(runif(1,12900,14699))
    Heinrich_Stadial <- round(runif(1,14700,17000))
    LGM <- 21000
    migration_times <- rev(c(Current,Meghalayan, Northgrippian, Greenlandian, Younger_Dryas_Stadial, Bolling_Allerod, Heinrich_Stadial, Heinrich_Stadial, LGM))
  } else if (length(migration_matrix) > 15 && length(migration_matrix) < 30){
    migration_times <- seq(21000,1000,-1000)
  }
  
  
  index <- length(migration_matrix)-1
  if (length(migration_matrix) == 2){
    for (x in 1:(number_of_demes)){
      if (x == 1000){
        cat(paste(1000000, "0 0 1 1 0 0"), file="./input.par", append=TRUE, sep = "\n")
      } else {
        cat(paste(TDIV, (x), "0", "1 1 0 1"), file="./input.par", append=TRUE, sep = "\n")
      }}} else if (length(migration_matrix) > 9) {
        for (x in 1:(number_of_demes+length(migration_matrix)-2)){
          if (x == 1000){
            cat(paste(1000000, "0 0 1 1 0", index), file="./input.par", append=TRUE, sep = "\n")
          } else if (x >= 1 && x <= number_of_demes) {
            cat(paste(TDIV, x, "0", "1 1 0", index), file="./input.par", append=TRUE, sep = "\n")
          } else {
            index <- index - 1
            cat(paste(migration_times[x-(number_of_demes)+1], "0 0 1 1 0", index), file="./input.par", append=TRUE, sep = "\n")
          }}} else if (length(migration_matrix) == 9){
            for (x in 1:(number_of_demes+length(migration_matrix)-1)){
              if (x == 1000){
                cat(paste(1000000, "0 0 1 1 0", index), file="./input.par", append=TRUE, sep = "\n")
              } else if (x >= 1 && x <= number_of_demes) {
                cat(paste(TDIV, x, "0", "1 1 0", index), file="./input.par", append=TRUE, sep = "\n")
              } else {
                index <- index - 1
                cat(paste(migration_times[x-(number_of_demes)+1], "0 0 1 1 0", index), file="./input.par", append=TRUE, sep = "\n")
              }}
          }
  
  part9 <- "//Number of independent loci [chromosome] "
  cat(part9, file="./input.par", append=TRUE, sep = "\n")
  
  cat(Number_ind_loci, file="./input.par", append=TRUE, sep = "\n")
  
  part10 <- "//Per chromosome: Number of contiguous linkage Block: a block is a set of contiguous loci
1"
  cat(part10, file="./input.par", append=TRUE, sep = "\n")
  
  part11 <- "//per Block:data type, number of loci, per gen recomb and mut rates"
  cat(part11, file="./input.par", append=TRUE, sep = "\n")
  
  part12 <- "SNP 1 0"
  cat(part12, file="./input.par", append=TRUE, sep = "\n")
  
}


