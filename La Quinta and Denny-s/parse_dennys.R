# this script reads all of the saved xml files in data/dennys/ and constructs a data frame where each row is a restaurant with columns for the relevant restaurant characteristics. This data frame is saved as dennys.Rdata in the data/ directory.

library(methods)
library(rvest)
library(stringr)
library(tibble)
library(dplyr)

files = dir("data/dennys/", "xml", full.names = TRUE)

latitude = c()
longitude = c()
country = c()
address = c()
phone = c()
state = c()

for(i in seq_along(files))
{
  file = files[i]
  page = read_html(file)
  
  # extract out the latitude of each Denny's
  lat = page %>%
    html_nodes("latitude") %>%
    html_text() %>%
    as.numeric()
  # extract out the longitude of each Denny's
  lon = page %>%
    html_nodes("longitude") %>%
    html_text() %>%
    as.numeric()
  
  # extract out the country each Denny's is located in
  coun = page %>%
    html_nodes("country") %>%
    html_text()
  
  addr = page %>%
    html_nodes("address1") %>%
    html_text()
  
  phone_num = page %>%
    html_nodes("phone") %>%
    html_text()
  
  sta = page %>%
    html_nodes("state") %>%
    html_text()
  
  latitude = c(latitude, lat)
  longitude = c(longitude, lon)
  country = c(country, coun)
  address = c(address, addr)
  phone = c(phone, phone_num)
  state = c(state, sta)
}

dennys = cbind.data.frame(address, state, country, latitude, longitude, phone) %>%
  filter(country == "US") %>%
  distinct()

dir.create("data/",showWarnings = FALSE)
save(dennys,file="data/dennys.Rdata")