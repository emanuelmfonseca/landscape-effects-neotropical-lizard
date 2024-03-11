create_images <- function(working_folder, sampled_ind, folder_save_simulations, model_number, simulation_number, nsim){
  
  quiet <- function(x) {
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  }
  
  setwd(working_folder)
  
  if (!dir.exists("Simulations")){
    dir.create("Simulations")
  }
  
  setwd(file.path(working_folder,"Simulations"))
  setwd(paste0("Simulation_",simulation_number,"/input"))
  
  out <- readLines(list.files(pattern="arp"))
  lines <- which(grepl("^[[:digit:]]", out) == TRUE)
  SNPs <- out[lines]
  SNPs <- sub(".*\\s+(.*)", "\\1", SNPs)
  SNPs <- strsplit(SNPs, "")
  length.snps <- length(SNPs[[1]])
  n_seq <- length(lines)
  SNPs <- data.frame(matrix(unlist(SNPs), ncol = length.snps, nrow=n_seq, byrow = T))
  
  freq <- numeric()
  for (r in 1:length.snps){
    freq <- c(freq, length(which(as.numeric(SNPs[,r])==1))/n_seq)
  }
  
  index <- sort(freq, index.return=TRUE, decreasing=T)
  SNPs <- SNPs[,index$ix]
  
  img <- raster(nrow=length(lines), ncol=length.snps)
  seq <- c(t(SNPs))
  img <- setValues(img,as.numeric(seq))
  
  setwd(folder_save_simulations)
  
  if (!dir.exists("Simulated_datasets")){
    dir.create("Simulated_datasets")
  }
  
  setwd("Simulated_datasets")
  
  if (!dir.exists("training_dataset")){
    dir.create("training_dataset")
  }
  
  if (!dir.exists("test_dataset")){
    dir.create("test_dataset")
  }
  
  if (simulation_number <= nsim*0.8){
    setwd(folder_save_simulations)
    setwd("Simulated_datasets")
    setwd("training_dataset")
    
    if (!dir.exists(paste0("Model",model_number))){
      dir.create(paste0("Model",model_number))
    }
    
    setwd(paste0("Model",model_number))
    
    png(filename=paste0("Image_",simulation_number,"_Model",model_number, ".png"),height=length(lines), width=length.snps)
    dat <- matrix(getValues(img), ncol = length(lines), nrow = length.snps)
    par(mai=c(0,0,0,0))
    image(dat, col=c("black", "white"), axes=F, frame.plot=T)
    quiet(dev.off())
    
  } else {
    
    setwd(folder_save_simulations)
    setwd("Simulated_datasets")
    setwd("test_dataset")
    
    if (!dir.exists(paste0("Model",model_number))){
      dir.create(paste0("Model",model_number))
    }
    
    setwd(paste0("Model",model_number))
    
    png(filename=paste0("Image_",simulation_number,"_Model",model_number, ".png"),height=length(lines), width=length.snps)
    dat <- matrix(getValues(img), ncol = length(lines), nrow = length.snps)
    par(mai=c(0,0,0,0))
    image(dat, col=c("black", "white"), axes=F, frame.plot=T)
    quiet(dev.off())
    
  }}
