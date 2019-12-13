library(readr)
library(tidyr)
library(tidyverse)
library(dplyr)
library(readxl)
library(geojsonio)
library(htmltools)
library(knitr)
library(kableExtra)


pvr <- read_excel("/Users/dearm/Documents/police-violencereport-2017.xlsx")
#removing unwanted columns
pvr1 <- pvr[,-c(1,5,11,15,30)]
install.packages("kableExtra")

#glance at region
ggplot(pvr1, aes(x = pvr1$State))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#combine columns for address
pvr1<- unite(pvr1, Address, c('Street Address of Incident', 'City', 'State', 'Zipcode'), sep = ' ', remove = TRUE)
pvr1 <-pvr1[,-c(14)]

#renaming cols
colnames(pvr1) <- c("Vage","Vgen", "Vrace","date", "Address", "Agency","COD","ChargesO","Mental","Unarmed","Allwep","Allthreat","Fleeing","Allcrime", "Suspoff","Pname","Prace",  "Years", "Prevsht", "Time", "Video")

#manipulating variables for possible comparision
pvr1$fleeing <- ifelse(pvr1$'Fleeing'%in% c("Not fleeing"),
                       "Not fleeing",
                       "Fleeing")



pvr1$Suspoff<- tolower(pvr1$Suspoff)
pvr1$Vrace<- tolower(pvr1$Vrace)

pvr1$Vage <- ifelse(pvr1$Vage == "Unknown", NA, pvr1$Vage)
pvr1$Vage <- ifelse(pvr1$Vage == "40s", 45, pvr1$Vage)
pvr1$Vage<- as.numeric(pvr1$Vage)

library(ggplot2)
ggplot(pvr1, aes(x = fleeing  ))+ geom_bar()+
  facet_wrap(~Vrace)



#looking  at race
ggplot(pvr1, aes(x = Vrace))+
   geom_bar()+
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#is gender a factor
ggplot(pvr1, aes(x = Vrace, fill = Vgen))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# start looking at a multitude of factors
ggplot(pvr1, aes(x = Suspoff, fill = Vrace))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#better look at info
ggplot(pvr1, aes(x = Suspoff, fill = Vrace))+
  geom_bar(position = 'fill')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(pvr1, aes(x = Vage))+
   geom_histogram(binwidth = 5, color = 'black', fill = 'purple' )+
  scale_x_continuous(breaks=seq(0,100, 5))

ggplot(pvr1, aes(x = Suspoff))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#heat map prep
#install.packages(c("Rtools", "ggmap", "leaflet"))
library(leaflet)
nominatim_osm <- function(address = NULL){
  out <- tryCatch({
    d <- jsonlite::fromJSON(
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address),
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    )
    if(length(d) == 0){
      return(data.frame(longitude = NA, latitude = NA))
    }else{
      return(data.frame(longitude = as.numeric(d$lon), latitude = as.numeric(d$lat)))
    }
  },
  error = function(e){
    return(data.frame(longitude = NA, latitude = NA))
  }
  )
  return(out)
}


coords <-
  lapply(pvr1$Address, function(address) {
    print(address)
    #calling the nominatim OSM API
    api_output <- nominatim_osm(address)
    #return data.frame with the input address, output of the nominatim_osm function and elapsed time
    return(data.frame(local = address, api_output))
  }) %>%
  #stack the list output into data.frame
  bind_rows() %>% data.frame() %>% distinct()


pvr1 <- 
  pvr1 %>% 
  left_join(coords, c("Address" = "local"))

pvr1 %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers()

write.csv(pvr1, "/Users/dearm/Documents/police-violence-data.csv")

#creating a better map at a quick glance
library(tigris)

#creating a visual representation/comparision of the US
df <-pvr 
states <- geojsonio::geojson_read("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")

#laying the foundation-creataing a base map
map<-
  leaflet() %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("CartoDB")

#state variable
summary_data <-
  pvr1 %>%
  mutate(state = str_extract(Address, "\\b[A-Z]{2}(?=\\s+\\d{5}$)"),
         state = ifelse(is.na(state), str_extract(Address, "\\b[A-Z]{2}(?=\\s+\\d{4}$)"), state)) %>%
  # add state names
  mutate(name = state.name[match(state, state.abb)]) %>%
  count(name) %>%
  ungroup()

states <- geo_join(data_frame = summary_data, spatial_data = states, by = "name")


#color palate
bins <- c(0, 5, 10, 15, 20, 25, 30, 35, Inf)
pal <- colorBin("YlOrRd", domain = states$n, bins = bins)

# create popup labels
labels <- sprintf(
  "<strong>%s</strong><br/>%g Incidents",
  states$name, states$n) %>%
  lapply(htmltools::HTML)

# add state colored polygons and labels to map
map <-
  map %>%
  addPolygons(
    data = states,
    fillColor = ~pal(n),weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"))

map














#saving cleaned data




save.image(file = "thestriggle.RData")

