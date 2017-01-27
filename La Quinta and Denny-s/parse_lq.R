# This script reads all of the saved hotel pages in data/lq/ and constructs an appropriate data frame where each row is a hotel and the columns reflect hotel characteristics (e.g. lat, long, state, amenitities). This data frame is saved as lq.Rdata in the data/ directory.

library(rvest)
library(stringr)
library(tibble)
library(dplyr)
library(datasets)

files = dir("data/lq/", "html", full.names = TRUE)
res = list()
for(i in seq_along(files))
{
  file = files[i]
  page = read_html(file)
  # extract out the address, phone number and fax number
  hotel_info = page %>% 
    html_nodes(".hotelDetailsBasicInfoTitle p") %>%
    html_text() %>% 
    str_split("\n") %>% 
    .[[1]] %>% 
    str_trim() %>%
    .[. != ""]
  
  # extract out the state each hotel is located in from the second part of the second line of address
  # it is the only word with two characters in the second part of the second line
  state = hotel_info[2] %>%
    str_split(",") %>%
    unlist() %>%
    .[2] %>%
    str_split(" ") %>%
    unlist(.) %>%
    .[nchar(.)==2]
  
  # extract out the number of rooms in each hotel
  n_rooms = page %>% 
    html_nodes(".hotelFeatureList li:nth-child(2)") %>%
    html_text() %>%
    str_trim() %>%
    str_replace("Rooms: ", "") %>%
    as.integer()
  
  # extract out the number of floors in each hotel
  n_floors = page %>%
    html_nodes(".hotelFeatureList li:nth-child(1)")  %>%
    html_text() %>%
    str_trim() %>%
    str_replace("Floors: ", "") %>%
    as.integer()
  
  # extract out the internet service in each hotel from the list of amenities
  internet_service = page %>%
    html_nodes(".section:nth-child(1) .pptab_contentL li") %>%
    html_text() %>% 
    .[grep("Internet",.)] %>%
    str_trim() %>%
    paste(., collapse="/ ")
  
  # extract out information about swimming pool from the list of amenities
  swimming_pool = page %>%
    html_nodes(".section:nth-child(1) .pptab_contentL li") %>%
    html_text() %>% 
    .[grep("Swimming Pool",.)] %>%
    str_trim() %>%
    paste(., collapse="/ ")
  
  # Google link includes latitude first then longitude
  lat_long = page %>%
    html_nodes(".minimap") %>%
    html_attr("src") %>%
    str_match("\\|(-?[0-9]{1,2}\\.[0-9]+),(-?[0-9]{1,3}\\.[0-9]+)&") 
  
  # include in the hotel list only if it is in the US 
  # this is done by checking if the hotel is located in one of the 50 US states
  # state.abb provides the abbreviation of the 50 US states
  if(length(state) != 0 && state %in% state.abb)
  {
    res[[i]] = data_frame(
      address = paste(hotel_info[1:2],collapse=" "),
      state = state,
      phone = hotel_info[3] %>% str_replace("Phone: ", ""), 
      fax   = hotel_info[4] %>% str_replace("Fax: ", ""),
      n_rooms = n_rooms,
      n_floors = n_floors,
      lat   = lat_long[,2] %>% as.numeric(),
      long  = lat_long[,3] %>% as.numeric(),
      internet_service = internet_service,
      swimming_pool = swimming_pool
    )
  }
}

hotels = bind_rows(res)

dir.create("data/",showWarnings = FALSE)
save(hotels, file="data/lq.Rdata")