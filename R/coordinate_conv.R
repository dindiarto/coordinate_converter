library(measurements)
library(stringr)
library(magrittr)
library(dplyr)
library(readr)
# Geographic coordinates converter

#
#' DMS to decimal degree
#'
#' @param df df is a dataframe with latitude and longitude columns in gms format
#' @param lat latitude in gms format
#' @param lon longitude in gms format
#'
#' @return
#' @export
#'
#' @examples
dms_to_dec_deg <- function(df, lon=lon,lat=lat) {
  
  remove_separators <- function(x) { 
    stringr::str_remove_all(x, " ") %>% 
      stringr::str_replace_all('[°\'\"]', " ")
  }
  
  dms_dd<-function(x3) {
    measurements::conv_unit(x3, from = 'deg_min_sec', to = 'dec_deg')
  }
  
  remove_cardinal<-function(x4){
    gsub('.{1}$' , "", x4)
  }
  
  
  
  df %>% 
    dplyr::mutate(Latitude = lat, Longitude = lon) %>%
    mutate_at(vars(Latitude, Longitude), remove_separators) %>% #remove whitespaces,'," ymbols
    mutate(Hem_Lat = str_sub(Latitude, start = -1),Hem_Lon = str_sub(Longitude, start = -1)) %>% # extract the latest character ~ N/S and E/W
    mutate(Hem_Lat = case_when(Hem_Lat == "N" ~ 1 , Hem_Lat == "S" ~ -1),Hem_Lon = case_when(Hem_Lon == "E" ~ 1 , Hem_Lon == "W" ~ -1)) %>% # determine N/S and E/W and convert it to 1 or -1
    mutate_at(vars(Latitude, Longitude), remove_cardinal)%>% #remove latest character ~ N/S and E/W
    mutate_at(vars(Latitude, Longitude), dms_dd) %>% # convert dms to dec deg
    mutate(   Latitude = as.numeric(Latitude) * Hem_Lat,    Longitude = as.numeric(Longitude) * Hem_Lon) %>% # assign  N/S and E/W to dec deg format
    dplyr::select(-Hem_Lat,-Hem_Lon)
}
