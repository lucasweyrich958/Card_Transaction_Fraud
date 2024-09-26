#install.packages("arrow")

library(arrow)
path = "path" #Set path of Parquet File downloaded from Github repo: https://github.com/lucasweyrich958/DATA622/blob/main/sales.parquet

tx_raw = read_parquet(path)
