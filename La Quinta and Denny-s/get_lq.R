# This script goes to the La Quinta hotel listing page and download each individual hotel page. All of these hotel pages are saved into the data/lq/ directory. If these folders do not exist they would be created.

library(rvest)

site = "http://www2.stat.duke.edu/~cr173/lq_test/www.lq.com/en/findandbook/"
url = paste0(site, "hotel-listings.html")
page = read_html(url)
# extract out all the hotel listing
hotel_pages = page %>% 
  html_nodes("#hotelListing .col-sm-12 a") %>% 
  html_attr("href") %>% 
  na.omit()

dir.create("data/lq", recursive = TRUE, showWarnings = FALSE)

for(hotel_page in hotel_pages)
{
  hotel_url = paste0(site, hotel_page)
  download.file(url = hotel_url, 
                destfile = file.path("data/lq", hotel_page),
                quiet = TRUE)
  # output something so that wercker will not get time out after no response
  cat(hotel_page, "\n")
}
