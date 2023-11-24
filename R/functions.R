#Read an excel data file
read_excel_file <- function(file){
  excel_data <- read_excel(file) 
}

#Read a csv data file
read_csv_file <- function(file){
  csv_data <- read_csv(file) |>
    clean_names()
}

