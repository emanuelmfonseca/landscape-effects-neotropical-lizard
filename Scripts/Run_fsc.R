run_fsc <- function(working_folder,simulation_number,path_to_fastsimcoal){
  
  quiet <- function(x) {
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  }
  
  setwd(working_folder) 
  setwd("Simulations")
  setwd(paste0("Simulation_",simulation_number))
  
  input <- file.path(sub(" ", "\\\\ ",getwd()), list.files(pattern="par"))
  
  quiet(system(paste(path_to_fastsimcoal, "-i", input, "-n1"),intern=T))
  
}
