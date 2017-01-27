spark_home = "/data/spark/spark-2.0.1-bin-hadoop2.7/"
Sys.setenv(SPARK_HOME=spark_home)

library(sparklyr)
library(dplyr)
library(ggplot2)
library(tidyr)

sc = spark_connect(master = "local")

# reading in the data
green  = spark_read_csv(sc, "green", "/data/nyc-taxi-data/data/green_tripdata_2014-09.csv")
yellow = spark_read_csv(sc, "yellow", "/data/nyc-taxi-data/data/yellow_tripdata_2014-09.csv")
uber = spark_read_csv(sc, "uber", "/data/nyc-taxi-data/data/uber-raw-data-sep14.csv")

# a function to clean the column names of the data frame
fix_names = function(df)
{
  df %>%
    setNames(
      colnames(df) %>% 
        tolower() %>% 
        sub("[tl]pep_","",.) 
    )
}

green  = green  %>% fix_names()
yellow = yellow %>% fix_names()

## TASK 1

# plot 1 - yellow and green cab pickups
# a function to get the data frame for yellow and green cab pickups for both task 1 and task 2
heat_pickup = function(df, HOUR = FALSE)
{
  taxi_type = deparse(substitute(df))
  df = df %>%
    mutate(long = as.integer(pickup_longitude * 1e3), 
           lat  = as.integer(pickup_latitude  * 1e3),
           hour = hour(pickup_datetime),
           rush_hour = ifelse(hour %in% 7:9, TRUE, FALSE)) %>% 
    filter(lat > 40477, lat < 40917, long > -74259, long < -73700)
  
  # perform task 1 when HOUR == FALSE
  if(HOUR == FALSE) 
  {
    df  %>% 
      group_by(long, lat) %>%
      count() %>%
      mutate(taxi = taxi_type) %>%
      collect()
  }
  
  # perform task 2 when HOUR == TRUE, add an extra column indicating whether it is rush hour or not
  else
  {
    df %>%
      group_by(long, lat, rush_hour) %>%
      count() %>%
      mutate(taxi = taxi_type) %>%
      collect()
  }
}

# combine the data frames to get the data for all the yellow and green cab pickups together
green_pickup = heat_pickup(green, HOUR = FALSE)
yellow_pickup = heat_pickup(yellow, HOUR = FALSE)
taxi_pickups = rbind(green_pickup, yellow_pickup)

# plot 2 - yellow and green cab dropoffs
# a function to get the data frame for yellow and green cab dropoffs for both task 1 and task 2
heat_dropoff = function(df, HOUR = FALSE)
{
  taxi_type = deparse(substitute(df))
  df = df %>%
    mutate(long = as.integer(dropoff_longitude * 1e3), 
           lat  = as.integer(dropoff_latitude  * 1e3),
           hour = hour(dropoff_datetime),
           rush_hour = ifelse(hour %in% 7:9, TRUE, FALSE)) %>% 
    filter(lat > 40477, lat < 40917, long > -74259, long < -73700)
  if(HOUR == FALSE) 
  {
    df  %>% 
      group_by(long, lat) %>%
      count() %>%
      mutate(taxi = taxi_type) %>%
      collect()
  }
  else
  {
    df %>%
      group_by(long, lat, rush_hour) %>%
      count() %>%
      mutate(taxi = taxi_type) %>%
      collect()
  }
}

green_dropoff = heat_dropoff(green, HOUR = FALSE)
yellow_dropoff = heat_dropoff(yellow, HOUR = FALSE)
taxi_dropoffs = rbind(green_dropoff, yellow_dropoff)

# plot 3 - Uber pickup
# a function to get the data frame for uber pickups for both task 1 and task 2
uber_pickup = function(df, HOUR = FALSE)
{
  df = df %>%
    mutate(longitude = as.integer(Lon * 1e3), 
           latitude  = as.integer(Lat  * 1e3),
           hour=as.numeric(substr(DateTime,nchar(DateTime)-7,2)),
           rush_hour = ifelse(hour %in% 7:9, TRUE, FALSE)) %>% 
    filter(latitude > 40477, latitude < 40917, longitude > -74259, longitude < -73700)
  if(HOUR == FALSE)
  {
    df %>% 
      group_by(longitude, latitude) %>%
      count() %>%
      collect()
  }
  else
  {
    df %>% 
      group_by(longitude, latitude, rush_hour) %>%
      count() %>%
      collect()
  }
}

uber_pickups = uber_pickup(uber, HOUR = FALSE)

## TASK 2

# Taxi pickups
green_pickup2 = heat_pickup(green, HOUR = TRUE)
yellow_pickup2 = heat_pickup(yellow, HOUR = TRUE)
taxi_pickups2 = rbind(green_pickup2, yellow_pickup2)
taxi_pickups_rushhour = taxi_pickups2 %>% filter(rush_hour == TRUE)
taxi_pickups_nonrush = taxi_pickups2 %>% filter(rush_hour == FALSE)

# Taxi dropoffs
green_dropoff2 = heat_dropoff(green, HOUR = TRUE)
yellow_dropoff2 = heat_dropoff(yellow, HOUR = TRUE)
taxi_dropoffs2 = rbind(green_dropoff2, yellow_dropoff2)
taxi_dropoffs_rushhour = taxi_dropoffs2 %>% filter(rush_hour == TRUE)
taxi_dropoffs_nonrush = taxi_dropoffs2 %>% filter(rush_hour == FALSE)

# Uber pickups
uber_pickups2 = uber_pickup(uber, HOUR = TRUE)
uber_pickups_rushhour = uber_pickups2 %>% filter(rush_hour == TRUE)
uber_pickups_nonrush  = uber_pickups2 %>% filter(rush_hour == FALSE)

# combine all the data frames to an Rdata file
combined = list(taxi_pickups = taxi_pickups, taxi_dropoffs = taxi_dropoffs, uber_pickups = uber_pickups,
                taxi_pickups_rushhour = taxi_pickups_rushhour, taxi_pickups_nonrush = taxi_pickups_nonrush,
                taxi_dropoffs_rushhour = taxi_dropoffs_rushhour, taxi_dropoffs_nonrush = taxi_dropoffs_nonrush,
                uber_pickups_rushhour = uber_pickups_rushhour, uber_pickups_nonrush = uber_pickups_nonrush)
save(combined, file = 'combined.Rdata')
