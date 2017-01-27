# this script downloads xml data from the Where2GetIt API and saves the results to data/dennys/. If these folders do not exist they would be created.

# create a function to extract xml from the Where2GetIt API
get_url = function(limit, zip_code, radius)
{
  paste0(
    "https://hosted.where2getit.com/dennys/responsive/ajax?&xml_request=%3Crequest%3E%3Cappkey%3E6B962D40-03BA-11E5-BC31-9A51842CA48B%3C%2Fappkey%3E%3Cformdata+id%3D%22locatorsearch%22%3E%3Cdataview%3Estore_default%3C%2Fdataview%3E%3Climit%3E",
    limit,"%3C%2Flimit%3E%3Corder%3Erank%2C_distance%3C%2Forder%3E%3Cgeolocs%3E%3Cgeoloc%3E%3Caddressline%3E",
    zip_code,
    "%3C%2Faddressline%3E%3Clongitude%3E%3C%2Flongitude%3E%3Clatitude%3E%3C%2Flatitude%3E%3Ccountry%3E%3C%2Fcountry%3E%3C%2Fgeoloc%3E%3C%2Fgeolocs%3E%3Cstateonly%3E1%3C%2Fstateonly%3E%3Csearchradius%3E",
    radius,
    "%3C%2Fsearchradius%3E%3C%2Fformdata%3E%3C%2Frequest%3E"
  )
}

# Using four locations to obtain all Denny's locations
LA = get_url(limit = 1000, zip_code = 90210, radius = 5000)
Durham = get_url(limit = 1000, zip_code = 27705, radius = 5000)
Alaska = get_url(limit = 1000, zip_code = 99709, radius = 5000)
Hawaii = get_url(limit = 1000, zip_code = 96701, radius = 5000)

dir.create("data/dennys",recursive = TRUE, showWarnings = FALSE)

download.file(LA, dest="data/dennys/LA.xml")
download.file(Durham, dest="data/dennys/Durham.xml")
download.file(Alaska, dest="data/dennys/Alaska.xml")
download.file(Hawaii, dest="data/dennys/Hawaii.xml")