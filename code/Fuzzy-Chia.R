############################################################################################
## Implementation of functions that help us to evaluate fuzzy inference systems 
## when predicting mass yield in the context of extraction of chia extract from chia cake
############################################################################################

# preparing the dataset and returning its solvent-oriented parts
prepare_dataset <- function(tbl) {
  # requiring the use of dplyr package
  require(dplyr)
  
  # guaranteeing the correct order of the columns/attributes
  tbl <- select(tbl, solvent, extraction_time, temperature, mass_yield)
  
  tbl$temperature <- gsub("In natura", "30", tbl$temperature)
  tbl$temperature <- as.numeric(tbl$temperature)
  
  tbl_water <- filter(tbl, solvent == "Water")
  tbl_methanol <- filter(tbl, solvent == "Methanol")
  tbl_hexane <- filter(tbl, solvent == "Hexane")
  
  list(water = tbl_water, methanol = tbl_methanol, hexane = tbl_hexane)
}

# preparing the two folds for the hold-out validation (training and test)
# each time that this function is executed, a different configuration of folds is processed
prepare_hold_out <- function(tbl, training_perc) {
  # randomly reordering the observations
  tbl_mixed <- tbl[sample(1:nrow(tbl)), ]
  nrow <- nrow(tbl_mixed)
  # number of observations to the training set
  nrow_train <- ceiling(training_perc * nrow)
  data_trn <- tbl_mixed[1:nrow_train, ]
  data_tst <- tbl_mixed[(1+nrow_train):(nrow), ]
  # returning the pair as list
  list(training = data_trn, test = data_tst)
}

# assigning the components of a fuzzy inference system: fuzzy sets, linguistic variables, linguistic values, and fuzzy rules
# fis is an fuzzy inference system created by FuzzyR
# each solvent has its own fuzzy rules set
assign_vars_to_fis <- function(fis, solvent) {
  # adding the variables
  fis <- addvar(fis, "input", "Extraction Time", c(15, 60), "singleton.fuzzification")
  fis <- addvar(fis, "input", "Temperature", c(30, 80), "singleton.fuzzification")
  fis <- addvar(fis, "output", "Mass Yield", c(1.376, 1.484))
  
  # adding the fuzzy sets (with their linguistic values) for each variable
  
  # extraction time
  fis <- addmf(fis, "input", 1, "low", "gbellmf", c(15, 2, 10, 1))
  fis <- addmf(fis, "input", 1, "medium", "gbellmf", c(10, 2, 40, 1))
  fis <- addmf(fis, "input", 1, "high", "gbellmf", c(40, 5, 80, 1))
  
  # temperature
  fis <- addmf(fis, "input", 2, "in natura", "gbellmf", c(25, 10, 10, 1))
  fis <- addmf(fis, "input", 2, "low", "gbellmf", c(2, 1.5, 40, 1))
  fis <- addmf(fis, "input", 2, "low medium", "gbellmf", c(2, 1.5, 50, 1))
  fis <- addmf(fis, "input", 2, "medium", "gbellmf", c(2, 1.5, 60, 1))
  fis <- addmf(fis, "input", 2, "low high", "gbellmf", c(2, 1.5, 70, 1))
  fis <- addmf(fis, "input", 2, "high", "gbellmf", c(2, 2, 80, 1))
  
  if(!(fis$type %in% c("mamdani", "tsk"))) {
    stop("Invalid fuzzy inference system.", call. = FALSE)
  }
  
  # mass yield (which can vary according to solvent and type of fuzzy inference system)
  
  if(tolower(solvent) == "water") {
    
    if(fis$type == "mamdani") {
      fis <- addmf(fis, "output", 1, "very low", "trimf", c(1.376, 1.376, 1.396, 1))
      fis <- addmf(fis, "output", 1, "low ", "trimf", c(1.376, 1.396, 1.416, 1))
      fis <- addmf(fis, "output", 1, "low medium", "trimf", c(1.396, 1.416, 1.436,1))
      fis <- addmf(fis, "output", 1, "medium", "trimf", c(1.416, 1.436, 1.456, 1))
      fis <- addmf(fis, "output", 1, "low high", "trimf", c(1.436, 1.456, 1.476, 1))
      fis <- addmf(fis, "output", 1, "medium high", "trimf", c(1.456, 1.480, 1.480, 1))
    } else {
      fis <- addmf(fis, "output", 1, "very low", "linearmf", c(1.376, 1.43, 1.43))  
      fis <- addmf(fis, "output", 1, "low ", "linearmf", c(1.465561, (1.480-1.465561)/2,  (1.480-1.465561)/2))
      fis <- addmf(fis, "output", 1, "low medium", "linearmf", c(1.447122,  (1.480-1.447122)/2, (1.480-1.447122)/2))
      fis <- addmf(fis, "output", 1, "medium", "linearmf", c(1.410244, (1.480-1.410244)/2, (1.480-1.410244)/2))
      fis <- addmf(fis, "output", 1, "low high", "linearmf", c(1.391805, (1.480-1.391805)/2, (1.480-1.391805)/2))
      fis <- addmf(fis, "output", 1, " medium high", "linearmf", c(1.480, 1.376, 1.376))
    }
    
  } else if (tolower(solvent) == "methanol") {
    
    if(fis$type == "mamdani") {
      fis <- addmf(fis, "output", 1, "very low", "trimf", c(1.376, 1.376, 1.394, 1))
      fis <- addmf(fis, "output", 1, "low ", "trimf", c(1.376, 1.394, 1.412, 1))
      fis <- addmf(fis, "output", 1, "low medium", "trimf", c(1.394, 1.412, 1.430,1))
      fis <- addmf(fis, "output", 1, "medium", "trimf", c(1.412, 1.430, 1.448, 1))
      fis <- addmf(fis, "output", 1, "low high", "trimf", c(1.430, 1.448, 1.466, 1))
      fis <- addmf(fis, "output", 1, "medium high", "trimf", c(1.448, 1.484, 1.484, 1))
    } else {
      fis <- addmf(fis, "output", 1, "very low", "linearmf", c(1.376, 1.43, 1.43))  
      fis <- addmf(fis, "output", 1, "low ", "linearmf", c(1.394, (1.484-1.394)/2,  (1.484-1.394)/2))
      fis <- addmf(fis, "output", 1, "low medium", "linearmf", c(1.43,  (1.484-1.43)/2, (1.484-1.43)/2))
      fis <- addmf(fis, "output", 1, "medium", "linearmf", c(1.448, (1.484-1.448)/2, (1.484-1.448)/2))
      fis <- addmf(fis, "output", 1, "low high", "linearmf", c(1.466, (1.484-1.466/2), (1.484-1.466)/2))
      fis <- addmf(fis, "output", 1, " medium high", "linearmf", c(1.484, 1.376, 1.376))
    }
    
  } else if (tolower(solvent) == "hexane") {
    
    if(fis$type == "mamdani") {
      fis <- addmf(fis, "output", 1, "very low", "trimf", c(1.376, 1.376, 1.383, 1))
      fis <- addmf(fis, "output", 1, "low ", "trimf", c(1.376, 1.383, 1.390, 1))
      fis <- addmf(fis, "output", 1, "low medium", "trimf", c(1.383, 1.390, 1.397,1))
      fis <- addmf(fis, "output", 1, "medium", "trimf", c(1.390, 1.397, 1.404, 1))
      fis <- addmf(fis, "output", 1, "low high", "trimf", c(1.397, 1.404, 1.411, 1))
      fis <- addmf(fis, "output", 1, "medium high", "trimf", c(1.404, 1.417, 1.417, 1))
    } else {
      fis <- addmf(fis, "output", 1, "very low", "linearmf", c(1.376, 1.3965, 1.3965))  
      fis <- addmf(fis, "output", 1, "low ", "linearmf", c(1.383, (1.417-1.383)/2,  (1.417-1.383)/2))
      fis <- addmf(fis, "output", 1, "low medium", "linearmf", c(1.39, (1.417-1.39)/2, (1.417-1.39)/2))
      fis <- addmf(fis, "output", 1, "medium", "linearmf", c(1.404, (1.417-1.404)/2, (1-1.404)/2))
      fis <- addmf(fis, "output", 1, "low high", "linearmf", c(1.411, (1.417-1.411)/2, (1.417-1.411)/2))
      fis <- addmf(fis, "output", 1, " medium high", "linearmf", c(1.417, 1.376, 1.376))
    }
    
  } else {
    stop("Invalid Solvent.", call. = FALSE)
  }
  
  # specifying the fuzzy rules (which can vary according to the solvent)
  if(tolower(solvent) == "water") {
    water_rules <- rbind(
                      c(0, 2, 2, 1, 1),
                      c(0, 2, 3, 1, 1),
                      c(0, 3, 3, 1, 1),
                      c(0, 3, 4, 1, 1),
                      c(0, 4, 3, 1, 1),
                      c(0, 4, 4, 1, 1),
                      c(0, 5, 3, 1, 1),
                      c(0, 5, 4, 1, 1),
                      c(0, 1, 1, 1, 1),
                      c(0, 1, 2, 1, 1),
                      c(0, 5, 1, 1, 1),
                      c(3, 6, 5, 1, 1))
    # adding the rules
    fis <- addrule(fis, water_rules)
  } else if (tolower(solvent) == "methanol") {
    methanol_rules <- rbind(
                      c(0, 1, 1, 1, 1),
                      c(0, 1, 2, 1, 1),
                      c(0, 2, 2, 1, 1),
                      c(0, 3, 4, 1, 1),
                      c(0, 3, 5, 1, 1),
                      c(0, 4, 5, 1, 1),
                      c(0, 5, 5, 1, 1),
                      c(0, 6, 6, 1, 1),
                      c(1, 4, 4, 1, 1),
                      c(1, 4, 6, 1, 1),
                      c(1, 5, 6, 1, 1),
                      c(2, 2, 3, 1, 1),
                      c(2, 3, 3, 1, 1),
                      c(2, 4, 6, 1, 1),
                      c(2, 5, 6, 1, 1),
                      c(3, 2, 3, 1, 1),
                      c(3, 2, 4, 1, 1),
                      c(3, 4, 4, 1, 1),
                      c(3, 5, 4, 1, 1))
    # adding the rules
    fis <- addrule(fis, methanol_rules)
  } else if (tolower(solvent) == "hexane") {
    hexane_rules <- rbind(
                      c(0, 1, 1, 1, 1),
                      c(0, 1, 2, 1, 1),
                      c(0, 2, 6, 1, 1),
                      c(0, 6, 5, 1, 1),
                      c(0, 6, 6, 1, 1),
                      c(0, 4, 6, 1, 1),
                      c(0, 5, 5, 1, 1),
                      c(0, 5, 6, 1, 1),
                      c(0, 3, 6, 1, 1),
                      c(1, 3, 5, 1, 1),
                      c(1, 4, 5, 1, 1),
                      c(2, 3, 5, 1, 1),
                      c(2, 4, 5, 1, 1),
                      c(3, 2, 5, 1, 1))
    # adding the rules
    fis <- addrule(fis, hexane_rules)
  } 
  
  # returning the fis
  fis
}

# function to build fuzzy inference systems for each solvent type
build_fis <- function(tbl, solvent) {
  # Mamdani
  fis_mamdani <- newfis("Mamdani", fisType = "mamdani", mfType = "t1", defuzzMethod = "centroid", 
                        andMethod = "min", orMethod = "max",
                        aggMethod = "max", impMethod = "min")
  fis_mamdani <- assign_vars_to_fis(fis_mamdani, solvent)
  
  # Larsen
  fis_larsen <- newfis("Larsen", fisType = "mamdani", mfType = "t1", defuzzMethod = "centroid",
                       andMethod = "min", orMethod = "max", 
                       aggMethod = "max", impMethod = "prod")
  fis_larsen <- assign_vars_to_fis(fis_larsen, solvent)
  
  # TSK with prod
  fis_tsk_prod = newfis("ANFIS 1 (TSK Prod)", fisType = "tsk", andMethod = "prod")
  fis_tsk_prod <- assign_vars_to_fis(fis_tsk_prod, solvent)
  
  # TSK with min
  fis_tsk_min = newfis("ANFIS 2 (TSK Min)", fisType = "tsk", andMethod = "min")
  fis_tsk_min <- assign_vars_to_fis(fis_tsk_min, solvent)
  
  all <- list(mamdani = fis_mamdani, larsen = fis_larsen, 
              tsk_prod = fis_tsk_prod, tsk_min = fis_tsk_min)
  
  list(dataset = tbl, solvent = solvent, fis_list = all)
}

# function that calculates statistical measures for evaluating the fuzzy inference systems
accuracy_measures <- function(predicted, observed) {
  e <- observed - predicted  
  mae <- mean(abs(e), na.rm = TRUE)
  mse <- mean(e^2, na.rm = TRUE)
  rmse <- sqrt(mse)
  
  rss <- sum(e ^ 2)  # residual sum of squares
  tss <- sum((observed - mean(observed)) ^ 2)  # total sum of squares
  r2 <- 1 - rss/tss
  
  pe <- e/observed * 100
  mape <- mean(abs(pe), na.rm = TRUE)
  
  list(MAE = mae, RMSE = rmse, MAPE = mape, R2 = r2)
}

# Function for executing the repeated hold-out validation to evaluate the fuzzy inference systems
execute_repeated_holdout <- function(chia_dataset, k) {
  
  if(k <= 0) {
    stop("Invalid k; it should be greater than 0.", call. = FALSE)
  }
  
  # requiring the use of some specific packages
  require(tibble)
  require(FuzzyR) # which is also required by build_fis... therefore, we only invoke this function here
  
  datasets <- prepare_dataset(chia_dataset)
  
  # specifying and creating the fuzzy inference systems for all types of solvents
  fis_water <- build_fis(datasets$water, "water")
  fis_hexane <- build_fis(datasets$hexane, "hexane")
  fis_methanol <- build_fis(datasets$methanol, "methanol")
  
  list_all_solvents <- list(fis_water, fis_hexane, fis_methanol)
  
  # the resulting table of the experiments (which is later returned by this function)
  rho_results <- tibble(holdout_execution = integer(), 
                        solvent = character(), 
                        fis_name = character(),
                        MAE = numeric(), 
                        RMSE = numeric(), 
                        MAPE = numeric(),
                        R2 = numeric())
  
  # parameters employed to create ANFIS (based on the TSK)
  algorithm <- c("gradient", "lse")
  epochs <- 100
  stepsize <- 0.01
  online_flag <- 0
  err_log <- FALSE
  
  # it is used to guarantee reproducible results/experiments
  set.seed(456)
  
  # for each iteration of the repeated hold-out validation
  for(i in 1:k) {
    
    # for each type of solvent, we execute the hold-out (each type of solvent has its own dataset)
    for(ds_solvent in list_all_solvents) {
      # First: we separate training and test sets for each solvent (80% training and 20% test)
      ho_folds <- prepare_hold_out(ds_solvent$dataset, 0.8)
      training <- as.matrix(ho_folds$training[,2:4])
      test <- as.matrix(ho_folds$test[,2:3])
      real <- as.matrix(ho_folds$test[,4])
      
      # Second: for each fuzzy inference system of this dataset, we execute the hold-out
      for(fis in ds_solvent$fis_list) {
        
        # verifying if we need to build an ANFIS or not
        if(fis$type == "tsk") {
          
          # yes, in this case, we use an ANFIS built on the FIS and optimize it using the training set
          anfis <- anfis.builder(fis)
          anfis <- anfis.optimise(anfis, training, 
                                  epoch.total = epochs, stepsize = stepsize,
                                  rate.inc = 1.1, rate.dec = 0.9, method = algorithm,
                                  err.log = err_log, online = online_flag, 
                                  lambda = 1, opt.by = "err.trn")
          result_anfis <- anfis.eval(anfis, test)
          
          # Third: evaluating the accuracy of this execution
          acc_measures <- accuracy_measures(result_anfis, real)
        } else {
          # no, it is a classical Mamdani/Larsen system so that we simply execute the test set in its evaluation
          result_fis <- evalfis(test, fis)
          # it is possible because some rules may not fired (thus, we assume the minimum value of our domain)
          result_fis[is.nan(result_fis)] <- 1.376 
          
          # Third: evaluating the accuracy of this execution
          acc_measures <- accuracy_measures(result_fis, real)
        }
        
        # Fourth: adding the results in our table so that we can lately calculate the average of accuracy measures
        rho_results <- add_row(rho_results, holdout_execution = i, 
                               solvent = ds_solvent$solvent, 
                               fis_name = fis$name,
                               MAE = acc_measures$MAE, 
                               RMSE = acc_measures$RMSE, 
                               MAPE = acc_measures$MAPE,
                               R2 = acc_measures$R2)
      } # end of the evaluation of the models of one particular solvent
    } # end of the evaluation of the solvents
  } # end of the interaction of the repeated hold-out
  
  rho_results
}
