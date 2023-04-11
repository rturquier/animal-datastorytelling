library(tidyverse)

prepare_population <- function(input_path){
  #' Prepare UN population data
  #' Download the file "Population on 01 January, by 5-year age groups"
  #' from <https://population.un.org/wpp/Download/Standard/CSV/>

  population_df <- read.csv(input_path)
  
  prepared_df <- population_df %>%
    select(ISO3_code, Location, Time, PopTotal) %>%
    rename(year = Time, population_thousands = PopTotal, country = Location) %>%
    filter(year < 2022) %>%
    group_by(country, year) %>%
    summarise(ISO3_code = unique(ISO3_code),
              population_thousands = sum(population_thousands))
  
  write.csv(prepared_df, "population.csv")
  
  return(prepared_df)
}

prepare_cages <- function(input_path){
  #' Prepare data on number of animal caged in the EU
  #' 
  #' To create the input file:
  #' - Download the Compassion in World Farming report "End the Cage Age"
  #' from https://www.europarl.europa.eu/cmsdata/231961/%27End%20the%20Cage%20Age%27%20report,%20October%202020.pdf
  #' - Upload page 28 and 29 at <https://www.docsumo.com/free-tools/extract-tables-from-pdf-images>
  #' - Adjust as needed
  #' - Download the file in Excel format
  
  sheet_1 <- readxl::read_excel(input_path,
                                sheet = 1,
                                range = "A1:E29")
  sheet_2 <- readxl::read_excel(input_path,
                                sheet = 2,
                                range = "A1:D29")
  cages_df <- bind_cols(sheet_1, sheet_2)
  
  column_names <- c(
    "country",
    "laying_hens_farmed", "laying_hen_caged",
    "rabbits_farmed", "rabbits_caged",
    "sows_farmed", "sows_in_stalls", "sows_in_crates",
    "total_caged"
  )
  names(cages_df) <- column_names
  
  cages_df <- cages_df %>%
    mutate_all(~ str_replace_all(., r"{[,\*\s]|\([\d.]+%\)}", "")) %>%
    type_convert() %>%
    mutate(
      laying_hen_caged_share = laying_hen_caged  / laying_hens_farmed,
      rabbits_caged_share    = rabbits_caged     / rabbits_farmed,
      sows_caged_share       = sows_in_crates    / sows_farmed
    )
  
  write_csv(cages_df, "data/cages.csv")
  return(cages_df)
}


prepare_all_data <- function(){
  population_input  <- "data/WPP2022_Population1JanuaryByAge5GroupSex_Medium.csv"
  prepare_population(population_input)
  
  cages_input <- "data/docsumo_cages.xlsx"
  prepare_cages(cages_input)
}
