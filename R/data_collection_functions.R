################################################################################

# Functions for various data collection methods

################################################################################


#Read an excel data file
read_excel_file <- function(file){
  excel_data <- read_excel(file) 
}

#Read a csv data file
read_csv_file <- function(file){
  csv_data <- read_csv(file) |>
    clean_names()
}



#Get Fingertips data Value
get_my_fingertips_gp_data <- function(ind,year){
  Metric <- fingertips_data(IndicatorID = ind,AreaTypeID = "All") |>
    filter(AreaType=="GPs", Timeperiod==year) |>
    select(AreaCode,Value)
}

#Get Fingertips data count
get_my_fingertips_gp_data_count <- function(ind,year){
  Metric <- fingertips_data(IndicatorID = ind,AreaTypeID = "All") |>
    filter(AreaType=="GPs", Timeperiod==year) |>
    select(AreaCode,Count)
}

#Get QOF via Excel file
read_qof_excel_file <- function(file){
  excel_qof_data <- read_xlsx(file,skip=11,.name_repair = "unique_quiet") 
}



#Connect to Server and get data
get_data_via_server <- function(){
  StrategicWorking_DB <- dbConnect(odbc(), 
                                   driver = "SQL Server",
                                   server="MLCSU-BI-SQL",
                                   database="StrategicWorking")
  server_metric<- dbFetch(dbSendQuery(StrategicWorking_DB,"SELECT [PRACTICE_CODE]
,[VALUE]
FROM [StrategicWorking].[adhoc].[QOF_ACHIEVEMENT_2021_v2]
where
indicator_code = 'CHD007'
and
measure = 'PCAS'
order by PRACTICE_CODE")) |>
    clean_names()
  #colnames(server_metric)[colnames(server_metric) == 'VALUE'] <- 'Metric32'
}