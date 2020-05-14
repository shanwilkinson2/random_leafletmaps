# install.packages("readxl")
# install.packages("sf")
# install.packages("stringr")
# install.packages("dplyr")
# install.packages("leaflet")
# install.packages("janitor")
# install.packages("leaflet.extras")
# install.packages("htmlwidgets")
# install.packages("htmltools")
# install.packages("glue")
# install.packages("htmltools")
# install.packages("htmlwidgets")

library(readxl)
library(sf)
library(stringr)
library(dplyr)
library(leaflet)
library(janitor)
library(leaflet.extras)
library(htmlwidgets)
library(htmltools)
library(glue)
library(htmltools)
library(htmlwidgets)

# geographical data ##################################################################################

desired_areas <- "Bolton" 
#  wards are hard coded as no borough indicator on the clipped to 20m ward boundaries

# LSOA
  # LSOA boundaries 2011 (current)
  # https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2011-generalised-clipped-boundaries-in-england-and-wales  
  lsoas_2011 <- st_read("G:\\Mapping Data\\R\\map\\Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales/Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales.shp")
  
  # add boroughs variable from LSOA name
  lsoas_2011 <- lsoas_2011 %>%
    mutate(borough = str_sub(lsoa11nm, 1, nchar(as.character(lsoa11nm))-5)) %>%
    st_transform(crs = 4326) # transforms to lat/ long from OSGB36

  # filter lsoas 2011 Bolton only
  lsoas_bolton <- filter(lsoas_2011, borough %in% desired_areas)
  # plot(st_geometry(lsoas_bolton)) # check areas look right  
  rm(lsoas_2011) # remove whole country of lsoas
  
# # OA - no name!!! so can't filter to bolton only
#   # https://geoportal.statistics.gov.uk/datasets/output-areas-december-2001-generalised-clipped-boundaries-in-england-and-wales
#   oas_2011 <- st_read("G:\\Mapping Data\\R\\map\\OA/Output_Areas_December_2001_Generalised_Clipped_Boundaries_in_England_and_Wales.shp")
#   
#   # add boroughs variable from LSOA name - no name!!!!!!
#   oas_20112 <- oas_2011 %>%
#     mutate(borough = str_sub(lsoa11nm, 1, nchar(as.character(lsoa11nm))-5)) %>%
#     st_transform(crs = 4326) # transforms to lat/ long from OSGB36
#   
#   # filter lsoas 2011 Bolton only
#   lsoas_bolton <- filter(lsoas_2011, borough %in% desired_areas)
#   # plot(st_geometry(lsoas_bolton)) # check areas look right  
#   rm(lsoas_2011) # remove whole country of lsoas
  
# neighbourhoods
  
  # neighbourhoods<- c(st_union(lsoas_bolton[1:3,]),
  #                                st_union(lsoas_bolton[4:6,]))

  neighbourhoods <- st_read("G:\\Mapping Data\\neighbourhoods 9 areas for integ care\\boundaries/9 Areas 121216.TAB")
  new_names <- data.frame(AreaCode = c("Area 1A", "Area 1B", "Area 1C", "Area 2A", "Area 2B", "Area 2C", "Area 3A", "Area 3B", "Area 3C"),
                             newname = c("Horwich", "Chorley Roads", "Westhoughton", "Rumworth", "Farnworth/Kearsley", "Central/Great Lever", "Crompton/Halliwell", "Breightmet/Little Lever", "Turton")
                             )
  neighbourhoods <- left_join(neighbourhoods, new_names, by = "AreaCode")
  st_crs(neighbourhoods) <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs"
  neighbourhoods <- st_transform(neighbourhoods, crs = 4326)
  rm(new_names) # remove new names list as no longer needed
  #plot(st_geometry(neighbourhoods)) # check geometry loooks ok. 
  
# MSOA  
  # https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-boundaries-bgc
  msoas_2011 <- st_read("G:\\Mapping Data\\R\\map\\MSOA/Middle_Layer_Super_Output_Areas_December_2011_Boundaries_BGC.shp")
  
  # add borough variable from MSOA name
  msoas_2011 <- msoas_2011 %>%
    mutate(borough = str_sub(msoa11nm, 1, nchar(as.character(msoa11nm))-4)) %>%
    st_transform(crs = 4326) # transforms to lat/ long from OSGB36
  
  # filter msoas 2011 Bolton only
  msoas_bolton <- filter(msoas_2011, borough %in% desired_areas)
  # plot(st_geometry(msoas_bolton)) # check areas look right  
  rm(msoas_2011) # remove whole country of lsoas
  
  # local names
  msoa_localnames <- data.frame(msoa11cd = c("E02000984","E02000985","E02000986","E02000987","E02000988",
                                                   "E02000989","E02000990","E02000991","E02000992","E02000993",
                                                   "E02000994","E02000995","E02000996","E02000997","E02000998",
                                                   "E02000999","E02001000","E02001001","E02001002","E02001003",
                                                   "E02001004","E02001005","E02001006","E02001007","E02001008",
                                                   "E02001009","E02001010","E02001011","E02001012","E02001013",
                                                   "E02001014","E02001015","E02001016","E02001017","E02001018"),
                                   local_name = c("Egerton & Dunscar","Turton","Sharples","Horwich Town","Sweetlove",
                                                  "Harwood","Horwich Loco","Smithills N&E","Blackrod","Tonge Moor & Hall i'th' Wood",
                                                  "Halliwell Rd","Johnson Fold & Doffcocker","Breightmet N & Withins","Middlebrook & Brazley","Victory",
                                                  "Town Centre","Tonge Fold","Heaton","Leverhulme & Darcy Lever","Lostock & Ladybridge",
                                                  "Lower Deane & The Willows","Burnden","Daubhill","Little Lever","Lever Edge",
                                                  "Deane & Middle Hulton","Moses Gate","Westhoughton East","Townleys","Over Hulton",
                                                  "Wingates & Washacre","Central Farnworth","Highfield & New Bury","Central Kearsley","Daisy Hill")
                                  )
  
  # merge in local names
  msoas_bolton <- left_join(msoas_bolton, msoa_localnames, by = "msoa11cd")
  rm(msoa_localnames) # remove localnames as no longer needed
  
# # wards
#   # https://www.ordnancesurvey.co.uk/business-government/products/boundaryline
#   wards2 <- st_read("G:\\Mapping Data\\R\\map\\OS boundary file\\Data\\GB\\district_borough_unitary_ward.TAB")
#   wards2 <- wards2 %>%
#     mutate(borough = str_replace_all(File_Name, "_", " "),
#            borough = str_replace_all(borough, "  ", " "),
#            borough = str_remove(borough, " \\(B\\)"), # () are special characters, need to escape them.
#            borough = str_remove(borough, " DISTRICT"),
#            borough = str_to_title(borough)) %>%
#     st_transform(crs = 4326) # transforms to lat/ long from OSGB36
# 
#   # filter wards bolton only
#   wards_bolton <- filter(wards2, borough %in% desired_areas)
#   # plot(st_geometry(wards_bolton)) # check areas look right
#   rm(wards2) # remove whole country of wards

# wards clipped to 20m 
  # https://geoportal.statistics.gov.uk/datasets/wards-december-2011-boundaries-ew-bgc  
  #G:\Mapping Data\R\map\wards BGC
  
  wards <- st_read("G:\\Mapping Data\\R\\map\\wards BGC/Wards_December_2011_Boundaries_EW_BGC.shp")
  # filter wards bolton only, no borough column on this file, so done on wardcode number range 
  # but welsh have same num so only 'E' codes 
  wards_bolton <- wards %>%
    mutate(wd11cd = as.character(wd11cd), 
           wardcode_num = str_sub(wd11cd, 2, nchar(wd11cd)),
           wardcode_num = as.numeric(wardcode_num),
           wardcode_letter = str_sub(wd11cd, 1, 1)) %>%
    filter(between(wardcode_num, 5000650, 5000669), 
           wardcode_letter == "E") %>%
    st_transform(crs = 4326) # transforms to lat/ long from OSGB36
  
  # filter wards bolton only
  wards_bolton <- filter(wards, borough %in% desired_areas)
  # plot(st_geometry(wards_bolton)) # check areas look right
  rm(wards) # remove whole country of wards
  
    
# boroughs
  boroughs <- st_read("G:\\Mapping Data\\R\\map\\OS boundary file\\Data\\GB\\district_borough_unitary.TAB")
  boroughs <- boroughs %>%
    mutate(borough = str_remove(Name, " District \\(B\\)")) %>%
    st_transform(crs = 4326) # transforms to lat/ long from OSGB36  
  
  # filter boroughs Bolton only
    boroughs_bolton <- filter(boroughs, borough %in% desired_areas)
    # plot(st_geometry(boroughs_bolton)) # check areas look right 
    rm(boroughs) # remove whole country of boroughs
   

# map ########################################################################################

my_bbox <- as.vector(st_bbox(boroughs_bolton)) # boundary box around selected areas for to get corners for default map view 

borough_labels = (glue("<b>Borough</b><br>
                       Local Authority code: {boroughs_bolton$Census_Code}<br>
                       CCG code: 00T<br>
                       Name: {boroughs_bolton$borough}<br><br>
                       <i>Bolton CCG and Bolton Council use the same boundaries. However the CCG must also consider the needs of people who live outside Bolton but are registered with a Bolton GP.</i>")) 

ward_labels = (glue("<b>Ward</b><br>
                    Code: {wards_bolton$Census_Code}<br>
                    Name: {str_sub(wards_bolton$Name, 1, nchar(as.character(wards_bolton$Name)) - 5)}"))

msoa_labels = (glue("<b>MSOA</b><br>
                    Code: {msoas_bolton$msoa11cd}<br>
                    Name: {msoas_bolton$msoa11nmw}<br>
                    Local name: {msoas_bolton$local_name}"))

lsoa_labels = (glue("<b>LSOA</b><br>
                    Code: {lsoas_bolton$lsoa11cd}<br>
                    Name: {lsoas_bolton$lsoa11nmw}"))

neighbourhood_labels = (glue("<b>Neighbourhood</b><br>
                            Name: {neighbourhoods$newname}<br><br>
                            <i>Neighbourhoods are local geographies for integrated health & social care, made up of LSOAs</i>"))

# oa_labels = (glue("<b>OA</b><br>
#                   Code: 
#                   Name: <br>
#                   Only census data is availale at this level as it's so small"))
  
my_title <- (glue("<h2>Bolton geographies</h2>
                  Click on an area to find out more | Turn layers on and off to compare<br>
                  (Boundaries from <a href=https://geoportal.statistics.gov.uk/ target=_blank>ONS Open Geographies Portal)</a>"))

# # make colour palatte 
# imd_decile_colours <- colorFactor(
#   palette = c("#B30000", "#418FDE"), 
#   levels = c(1, 10), 
#   na.color = "white")

geographies_map <-  
  leaflet() %>%
  addResetMapButton() %>%
  fitBounds(lng1 = my_bbox[1], lat1 = my_bbox[2], lng2 = my_bbox[3], lat2= my_bbox[4]) %>%
  addProviderTiles("Stamen.TonerLite") %>%
    # Borough boundary - bolton brand darker green  #009639
    addPolygons(data = boroughs_bolton, weight = 5, color = "#009639", 
              fillColor = "white", fillOpacity = 0, group = "Borough",
              highlight = highlightOptions(weight = 5, color = "#009639", bringToFront = FALSE),
              popup = ~borough_labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  # ward boundaries - bolton brand lighter blue
  addPolygons(data = wards_bolton, weight = 2, color = "#4FA8FF", 
              fillColor = "white", fillOpacity = 0, group = "Wards",
              highlight = highlightOptions(weight = 4, color = "#4FA8FF", bringToFront = TRUE),
              popup = ~ward_labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  # MSOA boundaries - bolton brand turquoise
  addPolygons(data = msoas_bolton, weight = 1.5, color = "#00C7B1", 
              fillColor = "white", fillOpacity = 0, group = "Middle Super Output Areas",
              highlight = highlightOptions(weight = 4, color = "#00C7B1", bringToFront = TRUE),
              popup = ~msoa_labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  # LSOA boundaries - bolton brand orange
  addPolygons(data = lsoas_bolton, weight = 0.75, color = "#ff6600", 
              fillColor = "white", fillOpacity = 0, group = "Lower Super Output Areas",
              highlight = highlightOptions(weight = 4, color = "#ff6600", bringToFront = TRUE),
              popup = ~lsoa_labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  # neighbourhood boundaries - bolton brand yellow
  addPolygons(data = neighbourhoods, weight = 0.8, color = "#FFB300", 
              fillColor = "white", fillOpacity = 0, group = "Neighbourhoods",
              highlight = highlightOptions(weight = 4, color = "#FFB300", bringToFront = TRUE),
              popup = ~neighbourhood_labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLayersControl(overlayGroups  = c("Borough", "Wards", "Middle Super Output Areas", "Lower Super Output Areas", "Neighbourhoods"), position = "topleft") %>%
  addControl(my_title, position = "topright")

# save map 
# htmltools::save_html(geographies_map, "geographies_map.html") # smaller file size but in a fixed window size
saveWidget(geographies_map, "geographies_map.html") # larger file size but window size adjusts properly

################################## working copy for making adjustments on below #############