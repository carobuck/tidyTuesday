## 1/19/21 
## Kenya census data for Tidy Tuesday!

# idea: make map of things in ggmap? Use icons for labels??
# things I'd like to figure out with the font-awesome icons (not sure it's actually doable in Rstudio...maybe only markdown/I think I saw something about window()???)
# https://cran.r-project.org/web/packages/emojifont/vignettes/emojifont.html#ggplot2
# https://fontawesome.com/cheatsheet?from=io

## HOW FEASIBLE IS A SHAPEFILE/GGMAP?? Doable. :)----
## Trying this:
# https://rpubs.com/spoonerf/countrymapggplot2
library(maptools)
library(raster)
library(plyr)
library(ggplot2)
library(rgdal)
library(tidyverse)

Kenya_counties <- getData("GADM", country="KE", level=1)

# Need to transform the shape data to the right UTM (Universal Transverse Mercator; a plane coord grid sys named for map projection it is based on, the Transverse Mercator )
Kenya_counties_UTM <- spTransform(Kenya_counties, CRS("+init=EPSG:32737"))  

# This fortify is key step for getting the lat/long coordinates!!!
kenya_fortify <- fortify(Kenya_counties_UTM)

# If want to add on other data, there are some joins needed: 
NAME_1<-Kenya1_UTM@data$NAME_1
count<-sample(1:1000,47)     #or any other data you can associate with admin level here
count_df<-data.frame(NAME_1, count)
Kenya1_UTM@data$id <- rownames(Kenya1_UTM@data)
Kenya1_UTM@data <- join(Kenya1_UTM@data, count_df, by="NAME_1")
Kenya1_df <- fortify(Kenya1_UTM)
Kenya1_df <- join(Kenya1_df,Kenya1_UTM@data, by="id")

# Make map w/ fortify data
# NOTE: works!! but this takes a while, i think b/c so many things in fill (group isn't good e.g.)
kenya_fortify %>%
  ggplot(aes(x=long,y=lat,group=group)) +
  geom_polygon(aes(fill=group)) +
  # _path connects observations in order they appear in df, _line connects them by order of x axis value
  geom_path(color = 'purple') +
  theme_bw()
  



## GET THE TIDY TUES DATA ----
# Let's go w/ crop data
kenya_crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/crops.csv')
kenya_gender <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/gender.csv')

# JK don't do this (not scaled to overall pop size since can vary a ton by county): Group by crop type, get average amount, then calc diff from avg for each crop/county, take biggest crop diff as most over index for county and plot that
# Instead: join to kenya_gender to get total pop for each county, then divide crop pop by county pop to get % and take max of those for each county
# Exclude "Farming" crop b/c it's the max everywhere and isn't too exciting... 
kenya_crops %>%
  mutate(County = str_to_title(SubCounty)) %>%
  full_join(kenya_gender) %>%
  filter(SubCounty != 'KENYA') %>%
  filter(!is.na(SubCounty)) %>%
  select(c(1:11,16)) %>% 
  # Intersting! my which.max methodology doesn't work if the first col has NA...so I wonder what it's been doing in the background when it's had NA before...hmm. suspicious
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(max_crop = colnames(.[,3:11])[apply(.[,3:11],1,which.max)],
         NAME_1 = str_to_title(SubCounty),
         # NAIROBI county only has "farming" (all others are NA, so say this one is max Farming crops. Alternatively, could just exclude county)
         max_crop = ifelse(SubCounty == 'NAIROBI','Farming',max_crop)) -> kenya_crops_clean
  # TODO: Calc total # (or %??) of ppl who grow each crop?? interesting add on?
  
  
## Join Kenya data w/ mapping data to plot ----
# If want to add on other data, there are some joins needed: 
Kenya_counties_UTM@data$id <- rownames(Kenya_counties_UTM@data)
Kenya_counties_UTM@data <- join(Kenya_counties_UTM@data, kenya_crops_clean,by = 'NAME_1')
kenya_crops_df <- fortify(Kenya_counties_UTM)
kenya_crops_df <- join(kenya_crops_df,Kenya_counties_UTM@data, by = 'id')

## Now we can plot/map the kenya_crops_df
# https://rpubs.com/spoonerf/countrymapggplot2
# NOTE: this method w/ max_crop fill and geom_path takes a looooong time to load!!

kenya_crops_df %>% 
  ggplot(aes(x=long,y=lat,group=group)) +
  geom_polygon(aes(fill=max_crop),color = 'black',size = .25) +
  #scale_fill_manual(values=as.vector(pals::polychrome(26)),
                    # Let's get fancy w/ the labels and do icons!
                    # Very experimental: https://cran.r-project.org/web/packages/emojifont/vignettes/emojifont.html#ggplot2
                    # avocado, cashew nut, coconut, coffee, farming, khat, mango, tea, NA
                    #label = emojifont::fontawesome(c('fa-bread-slice','fa-moon','fa-bowling-ball','fa-coffee','fa-seedling','fa-pagelines','fa-lemon','fa-mug-hot','naa'))) +
  scale_fill_manual(values=as.vector(pals::polychrome(26))) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  labs(title = 'Top Kenyan Crop by County',fill = 'Top Crop',x='',y='',
       subtitle = 'Excluded "Farming Crop" as top crop, unless all other crops were NA for a county') +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
  #theme(legend.text=element_text(family='fontawesome-webfont'))
 

## TRYING ALTERNATIVE MAPPING (NONE REALLY WORK...) ----
# Try ggmap to see if there is a better way to map
kenya_crops_df %>%
  ggmap::ggmap() +
  geom_point(aes(x=long,y=lat,color = max_crop)) +
  theme_bw()

## other ideas... # this also takes FOREEEVVVVER... 
library(broom)
kenya_crops_df_2 <- tidy(Kenya_counties_UTM)
spdf_fortified <- tidy(spdf, region = "code")
ggplot() +
  geom_polygon(data = kenya_crops_df_2, aes( x = long, y = lat, group = group), fill="white", color="grey") +
  theme_void() +
  coord_map()


## Trying leaflet, just to see if it's more efficient
# Loads faster, but nothing shows up on map???
library(leaflet)
pal <-  colorFactor('Dark2',NULL)
leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(data = Kenya_counties_UTM,
              fillOpacity = 0.7,
              weight = 0.2,
              fillColor = ~pal(Kenya_counties_UTM$max_crop),
              color = 'black',
              popup = paste(Kenya_counties_UTM$NAME_1,paste('Max Crop: ',Kenya_counties_UTM$max_crop),sep = '</br>')) %>%
  addLegend(pal = pal,
            values = Kenya_counties_UTM$max_crop,
            #labFormat = scales::comma,
            position = 'bottomright',
            title = 'Top Crops by Kenyan County') #-> m1 #%>%
#mapview::mapshot(url = 'dma_zulily.html')


## DINK/FAIL ----
# Read in shapefile/see if I can make ggmap easily (if not, then do it some other viz...)
library('sf')
# install.packages('shapefiles')

# Load shapefile
shapename <- read_sf('~/path/to/file.shp')
kenya_counties <- read_sf('kenyan-counties/County.shp')

kenya_counties_shx <- shapefiles::read.shx('kenyan-counties/County.shx')

lat_lng <- gdal::coordi
rgdal::proj4string(kenya_counties$geometry)
sp::coordinates(kenya_counties$geometry)