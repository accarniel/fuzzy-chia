load_required_packages <- function(x) {
  for(i in x){
    if(!require(i, character.only = TRUE)){
      install.packages(i, dependencies = TRUE)
      require(i, character.only = TRUE)
    }
  }
}

# first, we need to install/load all required packages
load_required_packages(c("this.path", "readr", "tibble", "dplyr", "FuzzyR"))

current_dir <- this.path::this.dir()

# loading needed underlying functions
source(file.path(current_dir, "FIS-ANFIS-Chia.R"))


# reading the dataset from its standard location and using its standard name
chia_dataset <- read_csv(file.path(current_dir, "../data/dataset.csv"))
k <- 2

results <- execute_repeated_holdout(chia_dataset, k)

# writing the raw results as a csv file
write_csv(results, file.path(current_dir, "../results/raw_repeated_holdout_results.csv"))

# producing the table with the average results
summarized_results <- group_by(results, solvent, fis_name) %>%
  summarise(MAE_avg = mean(MAE),
            RMSE_avg = mean(RMSE),
            MAPE_avg = mean(MAPE),
            R2_avg = mean(R2),
            MAE_stddev = sd(MAE),
            RMSE_stddev = sd(RMSE),
            MAPE_stddev = sd(MAPE),
            R2_stddev = sd(R2),
            MAE_var = var(MAE),
            RMSE_var = var(RMSE),
            MAPE_var = var(MAPE),
            R2_var = var(R2)) %>% arrange(solvent, MAE_avg, desc(R2_avg))

# writing the summarized results as a csv file
write_csv(summarized_results, file.path(current_dir, "../results/summarized_repeated_holdout_results.csv"))