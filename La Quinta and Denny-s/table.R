load("data/dennys.Rdata")
load("data/lq.Rdata")

suppressMessages(library(knitr))
suppressMessages(library(dplyr))

#calculate the distance
res = list()
for (i in 1:nrow(hotels)){
  lat1 = rep(hotels$lat[i],nrow(dennys))
  lon1 = rep(hotels$long[i],nrow(dennys))
  lat2 = dennys$latitude
  lon2 = dennys$longitude
  
  # Sphere Distance in meters
  res[[i]] = acos(sin(lat1*pi/180)*sin(lat2*pi/180) +
                    cos(lat1*pi/180)*cos(lat2*pi/180) *
                    cos(lon2*pi/180 - lon1*pi/180)) * 6378137
}


# Counting number of Denny's from one La Quinta Hotel within radius 100 meters
counts_100 = c()
for (i in 1:nrow(hotels)){
  counts_100 = c(counts_100, sum(res[[i]] < 100))
}

# Counting number of Denny's from one La Quinta Hotel within radius 500 meters
counts_500 = c()
for (i in 1:nrow(hotels)){
  counts_500 = c(counts_500, sum(res[[i]] < 500))
}

# Counting number of Denny's from one La Quinta Hotel within radius 500 meters
counts_1000 = c()
for (i in 1:nrow(hotels)){
  counts_1000 = c(counts_1000, sum(res[[i]] < 1000))
}

# Visualization (Table)
## column names:
##      n: Total number of La Quinta Hotels in each state.
##      n_lq_100: Number of hotels which has Denny's within a 100-meter radius
##      p_100: Proportion of hotels with Denny's in close proximity (100 meters)
##      n_lq_500: Number of hotels which has Denny's within a 500-meter radius
##      p_500: Proportion of hotels with Denny's in close proximity (500 meters)
##      n_lq_1000: Number of hotels which has Denny's within a 1000-meter radius
##      p_1000: Proportion of hotels with Denny's in close proximity (1000 meters)
t = data_frame(state=hotels$state, counts_100 = counts_100,
               counts_500 = counts_500, counts_1000 = counts_1000) %>%
  group_by(state) %>%
  summarize(n = n() , n_lq_100 = sum(counts_100 != 0) , p_100 = n_lq_100/n(),
            n_lq_500 = sum(counts_500 != 0), p_500 = n_lq_500/n(),
            n_lq_1000 = sum(counts_1000 != 0), p_1000 = n_lq_1000/n()) %>%
  arrange(desc(n))


kable(t %>% 
        add_row(state = "TOTAL", n = sum(t$n), 
                n_lq_100 = sum(t$n_lq_100), p_100 = sum(t$n_lq_100)/sum(t$n),
                n_lq_500 = sum(t$n_lq_500), p_500 = sum(t$n_lq_500)/sum(t$n),
                n_lq_1000 = sum(t$n_lq_1000), p_1000 = sum(t$n_lq_1000)/sum(t$n)) )