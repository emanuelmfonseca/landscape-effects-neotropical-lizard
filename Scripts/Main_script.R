library(parallel)

source("~/Scripts/Landscape_effect.R")
source("~/Scripts/Migration_matrices.R")
source("~/Scripts/Input_fsc.R")
source("~/Scripts/Run_fsc.R")
source("~/Scripts/Create_images.R")

Model_number <- 1
n_sim <- 2500
sampled_ind <- c(2,4,12,12,2,20,12,4,36)

if (Model_number != 1 | Model_number != 2){
  landscape_effect(path_to_folder = paste0("~/Models/Model",Model_number),
                   models_file = "~/Models/@Models.txt",
                   model_number = Model_number,
                   nsim = n_sim)
}

Simulations <- function(sim){
  
  Migration_matrix(working_folder = paste0("~/Models/Model",Model_number),
                   models_file = "~/Models/@Models.txt", 
                   model_number = Model_number,
                   simulation_number = sim,
                   path_to_rasters = "~/Rasters/Working_rasters",
                   seq_demes = "~/Datasets/sequenced_demes.txt", 
                   non_seq_demes = "~/Datasets/non_sequenced_demes.txt", 
                   landscape_effect_file = paste0("~/Models/Model",Model_number,"/Landscape_effect.txt"),
                   dispersal_capacity = c(.01,.01),
                   Resistance_script = "~/Scripts/ResistanceSurface.R")
  
  input_fastsimcoal(working_folder = paste0("~/Model",Model_number),
                    models_file = "~/Models/@Models.txt",
                    model_number = Model_number,
                    simulation_number = sim,
                    PopSizeAnc = 20000,
                    TDIV = 21000,
                    Number_ind_loci = 2000,
                    number_of_demes = 39,
                    pop_size_range = c(20,50),
                    number_of_individuals_sampled = c(sampled_ind, rep(0,30)),
                    path_to_migration_matrix = paste0("~/Model",Model_number,"/Migration_matrices"),
                    migration_times = seq(21000,1000,-1000))
  
  run_fsc(working_folder = paste0("~/Model",Model_number),
          simulation_number = sim,
          path_to_fastsimcoal = "~/fsc26")
  
  create_images(working_folder = paste0("~/Model",Model_number),
                folder_save_simulations = "~",
                model_number = Model_number,
                sampled_ind = sampled_ind,
                simulation_number = sim,
                nsim = n_sim)
  
  print(sim)
  
}

mclapply(1:n_sim, function(sim) Simulations(sim), mc.cores=detectCores())

unlink(paste0("~/Models/Model",Model_number,"/Simulations"),recursive = T)
unlink(paste0("~/Models/Model",Model_number,"/Migration_matrices"),recursive = T)




