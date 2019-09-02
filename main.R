library(tidyverse)
library(measurements)
source("R/coordinate_conv.R")

# Read data
dms_example<-read_csv("data/data_degree_minute_second.csv",locale = readr::locale(encoding = "latin1") )

# Convert Coordinate: Degree, Minute, Second (DMS) to Decimal Degree (DD)
dms_to_dec_deg(dms_example)



