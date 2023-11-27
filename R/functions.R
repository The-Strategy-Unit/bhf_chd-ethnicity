#Read an excel data file
read_excel_file <- function(file){
  excel_data <- read_excel(file) 
}

#Read a csv data file
read_csv_file <- function(file){
  csv_data <- read_csv(file) |>
    clean_names()
}


#Get Fingertips data
get_my_fingertips_gp_data <- function(ind,year){
  Metric <- fingertips_data(IndicatorID = ind,AreaTypeID = "All") |>
    filter(AreaType=="GPs", Timeperiod==year) |>
    select(AreaCode,Value)
}

#Get QOF via Excel file
read_qof_excel_file <- function(file){
  excel_qof_data <- read_xlsx(file,skip=11,.name_repair = "unique_quiet") 
}


