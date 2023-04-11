library(dplyr)

prepare_population <- function(input_path, output_path){
  #' Prepare UN population data
  #' Download the file `Population on 01 January, by 5-year age groups`
  #' from `https://population.un.org/wpp/Download/Standard/CSV/`

  population_df <- read.csv(input_path)
  
  prepared_df <- population_df %>%
    select(ISO3_code, Location, Time, PopTotal) %>%
    rename(year = Time, population_thousands = PopTotal, country = Location) %>%
    filter(year < 2022) %>%
    group_by(country, year) %>%
    summarise(ISO3_code = unique(ISO3_code),
              population_thousands = sum(population_thousands))
  
  write.csv(prepared_df, output_path)
  
  return(prepared_df)
}

prepare_all_data <- function(){ 
  population_file_input  <- "data/WPP2022_Population1JanuaryByAge5GroupSex_Medium.csv"
  population_file_output <- "data/population.csv"
  
  prepare_population(population_file_input, population_file_output)
}


