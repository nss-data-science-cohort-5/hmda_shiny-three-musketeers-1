library(tidyverse)
library(sparklyr)
library(httr)

sc <- spark_connect(master = "local")

queries <- list(states = "WA",
                years = "2020")

base_url <- "https://ffiec.cfpb.gov/v2/data-browser-api/view/csv"
request <- GET(url = base_url,
               query = queries)
WA_table <- copy_to(sc, read_csv(request$content))
spark_disconnect(sc = sc)
