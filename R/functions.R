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

# 
# 
# 
# colnames(Metric01)[colnames(Metric01) == 'Value'] <- 'Metric01Prev'
# #View(Metric01)