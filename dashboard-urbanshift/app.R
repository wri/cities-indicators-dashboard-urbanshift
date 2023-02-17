library(shiny)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(plyr)
library(dplyr)
library(rgdal)
library(shinyWidgets)
library(rnaturalearth)
library(tidyverse)
library(sf)
library(rgeos)
library(httr)
library(jsonlite)
library(raster)
library(data.table)
library(DT)
library(leafem)
library(RColorBrewer)
library(shinydisconnect)
library(shinyjs)

# define aws s3 path

aws_s3_path = "https://cities-urbanshift.s3.eu-west-3.amazonaws.com/"

############### Load data

# read indicator definition ------------

indicators_definitions = read.csv(paste(aws_s3_path,
                                        "indicators/indicators_definition_v4.csv",
                                        sep = ""))

# get list of themes
indicators_themes = unique(indicators_definitions$theme)

# get list of indicators

indicators_list = unique(indicators_definitions$indicator_label)


# read boundaries georef -----

boundary_georef = read.csv(paste(aws_s3_path,
                                 "data/boundaries/v_0/boundary_georef.csv",
                                 sep = ""),
                           fileEncoding="UTF-8-BOM")

boundary_georef = boundary_georef %>%
  # recode empty strings "" by NAs
  na_if("") %>%
  # remove NAs
  drop_na(units_boundary_name)

cities = unique(boundary_georef$geo_name)


# read indicator ------------


# indicators_v4 = read.csv(paste(aws_s3_path,
#                                "indicators/urbanshift_indicators_v4.csv",
#                                sep = ""),
#                          encoding="UTF-8")

# indicators_v5 = read.csv(paste(aws_s3_path,
#                                "indicators/urbanshift_indicators_v5.csv",
#                                sep = ""),
#                          encoding="UTF-8")

indicators_v5 = read.csv(paste(aws_s3_path,
                               "indicators/urbanshift_indicators_v6.csv",
                               sep = ""),
                         encoding="UTF-8")

indicators = indicators_v5 %>%
  mutate(BIO.1 = 100 * BIO.1) %>% 
  mutate(BIO.2 = 1 * BIO.2) %>% 
  mutate(BIO.3 = na_if(BIO.3, -9999)) %>% 
  mutate(BIO.3 = 100 * BIO.3) %>% 
  mutate(BIO.4 = na_if(BIO.4, -9999)) %>% 
  mutate(BIO.5 = na_if(BIO.5, -9999)) %>% 
  mutate(BIO.6 = na_if(BIO.6, -9999)) %>% 
  mutate(LND.1 = na_if(LND.1, -9999)) %>% 
  mutate(LND.1 = 100 * LND.1) %>% 
  # mutate(LND.1 = na_if(LND.1, -9999)) %>% 
  # mutate(LND.1 = na_if(LND.1, -999900)) %>% 
  mutate(LND.2 = 100 * LND.2) %>% 
  mutate(LND.4 = 100 * LND.4) %>% 
  mutate(LND.5 = 100 * LND.5) %>% 
  mutate(LND.6 = 100 * LND.6) %>% 
  mutate(LND.7 = na_if(LND.7, -9999)) %>%
  mutate(LND.7 = 100 * LND.7) %>% 
  mutate(LND.8 = na_if(LND.8, -9999)) %>%
  mutate(LND.8 = 100 * LND.8) %>% 
  mutate(GRE.2 = 100 * GRE.2) %>% 
  mutate(GRE.3 = 100 * GRE.3) %>% 
  mutate(GRE.4 = 100 * GRE.4)


# read indicator GHG-1  ------------
indicators_GHG_1 = read.csv(paste(aws_s3_path,
                                  "indicators/urbanshift_GHG-1.csv",
                                  sep = ""),
                            encoding="UTF-8")

indicators_GHG_1_aoi = indicators_GHG_1 %>% 
  filter(!X %in% c("ARG-Mar_del_Plata_ADM-2_1",
                   "ARG-Ushuaia_ADM-3_1",
                   "BRA-Teresina_ADM-2-union_1",
                   "BRA-Florianopolis_ADM-2-union_1",
                   "BRA-Belem_ADM-2-union_1",
                   "IND-Chennai_ADM-6-union_1")) %>% 
  add_column(geo_parent_name = c("ARG-Mendoza",
                                 "ARG-Mar_del_Plata",
                                 "ARG-Ushuaia",
                                 "ARG-Salta",
                                 "ARG-Buenos_Aires",
                                 "BRA-Teresina",
                                 "BRA-Florianopolis",
                                 "BRA-Belem",
                                 "CRI-San_Jose",
                                 "RWA-Kigali",
                                 "SLE-Freetown_city",
                                 "SLE-Freetown_region",
                                 "MAR-Marrakech",
                                 "IND-Chennai",
                                 "IND-Pune",
                                 "IND-Surat",
                                 "CHN-Chengdu",
                                 "CHN-Chongqing",
                                 "CHN-Ningbo",
                                 "IDN-Jakarta",
                                 "IDN-Bitung",
                                 "IDN-Semarang",
                                 "IDN-Balikpapan",
                                 "IDN-Palembang")) %>%
  mutate(GHG.1 = 100 * total_change_co2e) %>% 
  dplyr::select(geo_parent_name,GHG.1) %>%
  mutate_if(is.numeric, round, 0)

indicators = indicators %>% 
  right_join(indicators_GHG_1_aoi,
             by = "geo_parent_name") 

indicators_GHG_1_unit = indicators_GHG_1 %>% 
  dplyr::select(1,
                c(205:268),
                c(329:418)) %>% 
  dplyr::select(-c("co_2000_co2e",
                   "ch4_2000_co2e",
                   "co2_2000_co2e",
                   "nox_2000_co2e",
                   "ch4_2020_co2e",
                   "co_2020_co2e" ,
                   "oc_2020_co2e" , 
                   "nmvoc_2020_co2e",
                   "co2_2020_co2e" ,
                   "nox_2020_co2e")) %>% 
  rename(geo_id = X)


indicators = indicators %>% 
  left_join(indicators_GHG_1_unit,
            by = "geo_id")


# label indicator map function -----

pal.indicator.fun = function(selected_indicator_values){
  if(sum(is.na(selected_indicator_values)) == length(selected_indicator_values))
  {
    print("NOT available")
    selected_indicator_values = 0
    
    pal_indicator<- colorNumeric(palette = "gray",
                                 domain = selected_indicator_values,
                                 na.color = "gray",
                                 revers = FALSE)
  } else {
    pal_indicator<- colorNumeric(palette = "Greens",
                                 domain = selected_indicator_values,
                                 na.color = "gray",
                                 revers = FALSE)
  }
  return(pal_indicator)
}


# cities comparison ----

cities_comparison_list = c("ARG-Mendoza_ADM-3-union_1",
                           "ARG-Mar_del_Plata_ADM-3_1",
                           "ARG-Ushuaia_ADM-4_1",
                           "ARG-Salta_ADM-2-union_1",
                           "ARG-Buenos_Aires_ADM-2-union_1",
                           "BRA-Teresina_ADM-4-union_1",
                           "BRA-Florianopolis_ADM-4-union_1",
                           "BRA-Belem_ADM-4-union_1",
                           "CRI-San_Jose_ADM-2-union_1",
                           "RWA-Kigali_ADM-4-union_1",
                           "SLE-Freetown_city_ADM-4-union_1",
                           "MAR-Marrakech_ADM-2_1",
                           "IND-Chennai_ADM-4-union_1",
                           "IND-Pune_ADM-4-union_1",
                           "IND-Surat_ADM-4-union_1",
                           "CHN-Chengdu_ADM-3-union_1",
                           "CHN-Chongqing_ADM-1_1",
                           "CHN-Ningbo_ADM-3-union_1",
                           "IDN-Jakarta_ADM-4-union_1",
                           "IDN-Bitung_ADM-2_1",
                           "IDN-Semarang_ADM-1_1",
                           "IDN-Balikpapan_ADM-4-union_1",
                           "IDN-Palembang_ADM-2-union_1")

indicators_comparison = indicators[indicators$geo_id %in% cities_comparison_list, ]

############### App

ui = tagList(
  useShinyjs(),
#   tags$script(src="https://cdn.weglot.com/weglot.min.js"),
#   tags$script(src="Weglot.initialize({
#         api_key: 'wg_af620c4f25dacaa6bc9fe25247f6be664'
#     })
# "),
  tags$script(type="text/javascript"),
  tags$script(src="https://cdn.weglot.com/weglot.min.js"),
  tags$script(src="Weglot.initialize({
        api_key: 'wg_af620c4f25dacaa6bc9fe25247f6be664'
    })
"),
  navbarPage(title = div("Indicators Dashboard",
                         img(src = "logo.png",
                             height = "30px",
                             style = "top: -3px;
                                    right: -900px;padding-right:100px;")),
             id = "active_tab",
             
             ### Indicators tab ----
             tabPanel("Indicators",
                      
                      ### Filters ----
                      fluidRow(
                        
                        
                        
                        column(3,
                               
                               
                               
                               ### Select city  ----
                               selectInput(inputId = "city",
                                           # label = "Select your city",
                                           label = tags$span(style="color: #242456;","Select your city"),
                                           choices = cities,
                                           # selected = "CRI-San_Jose",
                                           selected = "BRA-Belem",
                                           width = '100%'),
                               
                               # select theme ----
                               selectizeInput(inputId = "theme",
                                              # label = "Theme",
                                              label = tags$span(style="color: #242456;","Theme"),
                                              choices = indicators_themes,
                                              selected = indicators_themes[1],
                                              multiple = FALSE,
                                              width = '100%'),
                               
                               # select indicator ----
                               selectizeInput(inputId = "indicator",
                                              # label = "Select indicator",
                                              label = tags$span(style="color: #242456;","Select indicator"),
                                              choices = indicators_list,
                                              selected = "Natural Areas",
                                              multiple = FALSE,
                                              width = '100%'),
                               
                               # Main indicators
                               
                               # h4("City wide level: "),
                               tags$span(h4("City wide level: "),
                                         style="color: #242456;"),
                               htmlOutput("city_wide_indicator"),
                               
                               
                               
                        ),
                        ### Specify plots ----  
                        column(8,
                               div(style = "background-color: red; width: 100%; height: 100%;"),
                               tabsetPanel(type = "tabs",
                                           id = "tabs",
                                           ### Map plot
                                           tabPanel("Map", 
                                                    leafletOutput("indicator_map", 
                                                                  height = 500),
                                                    # disconnect message
                                                    disconnectMessage(
                                                      text = "An error occurred due to the data volumetry. Please refresh the page and try again with another city.",
                                                      refresh = "Refresh",
                                                      background = "#FFFFFF",
                                                      colour = "#077D29",
                                                      refreshColour = "#337AB7",
                                                      overlayColour = "#000000",
                                                      overlayOpacity = 0.6,
                                                      width = 450,
                                                      top = 50,
                                                      size = 22),
                                                    # # disconnect dashboard
                                                    # actionButton("disconnect", 
                                                    #              "Disconnect the dashboard",
                                                    #              width = '30%'
                                                    #              # class = "btn-warning",
                                                    #              # style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                                    # ),
                                                    # download geo data
                                                    downloadButton(outputId = "download_geo_data",
                                                                   label = "Download geospatial data")
                                                    ),
                                           ### Table plot
                                           tabPanel("Table", DT::dataTableOutput("indicator_table"),
                                                    downloadButton(outputId = "downloadData",
                                                                   label = "Download tabular data")),
                                           ### barchart 
                                           tabPanel("Chart", 
                                                    plotlyOutput("indicator_chart",
                                                                 height = 500)),
                                           
                                           ## Cities comparison
                                           tabPanel("Benchmark", plotlyOutput("cities_comparison_plot",
                                                                              height = 500),
                                                    downloadButton(outputId = "downloadDataBenchmark",
                                                                   label = "Download benchmark data")),
                                           ### Data description
                                           tabPanel("Definitions", htmlOutput("indicator_definition", 
                                                                              height = 500))
                               )
                        )
                      )
             )
  )
)

# Define server
server <- function(input, output, session) {
  
  # disconnect message
  observeEvent(input$disconnect, {
    session$close()
  })
  
  # Update indicators based on selected theme
  observeEvent(input$theme,{
    updateSelectInput(session,
                      'indicator',
                      choices=unique(indicators_definitions[indicators_definitions$theme==input$theme, "indicator_label"]),
                      selected = unique(indicators_definitions[indicators_definitions$theme==input$theme, "indicator_label"])[1],
    )
  })
  
  # hide tab based on selected indicator
  observeEvent(input$indicator, {
    if(input$indicator %in% c("Change in greenhouse gas emissions")){
      showTab(inputId = "tabs", target = "Chart")
      showTab(inputId = "tabs", target = "Definitions")
      showTab(inputId = "tabs", target = "Table")
      hideTab(inputId = "tabs", target = "Map")
      show("city_wide_indicator") 
    } else if(!input$indicator %in% c("Change in greenhouse gas emissions")){
      showTab(inputId = "tabs", target = "Table")
      showTab(inputId = "tabs", target = "Chart")
      showTab(inputId = "tabs", target = "Map")
      showTab(inputId = "tabs", target = "Benchmark") 
      show("city_wide_indicator") 
    }
  })
  
  
  observe({
    
    # update panel data when the panel is selected
    input$active_tab
    
    geo_name = input$city
    print(geo_name)
    
    # read boundaries -----
    aoi_boundary_name = boundary_georef[boundary_georef$geo_name == geo_name, "aoi_boundary_name"]
    units_boundary_name = boundary_georef[boundary_georef$geo_name == geo_name, "units_boundary_name"]
    
    aoi_boundary_name = aoi_boundary_name[1]
    units_boundary_name = units_boundary_name[1]
    
    print(aoi_boundary_name)
    print(units_boundary_name)
    
    boundary_aoi = st_read(paste(aws_s3_path,
                                 "data/boundaries/v_0/boundary-",
                                 geo_name,
                                 "-",
                                 aoi_boundary_name,
                                 ".geojson",
                                 sep = "")
    )
    
    boundary_unit = st_read(paste(aws_s3_path,
                                  "data/boundaries/v_0/boundary-",
                                  geo_name,
                                  "-",
                                  units_boundary_name,
                                  ".geojson",
                                  sep = "")
    )
    
    
    # join ----------------
    
    aoi_indicators = boundary_aoi %>%
      dplyr::select(geo_id) %>%
      left_join(indicators, by = "geo_id")
    
    unit_indicators = boundary_unit %>%
      dplyr::select(geo_id) %>%
      left_join(indicators, by = "geo_id")
    
    # get selected indicator
    
    selected_indicator_label = input$indicator
    
    selected_indicator_name = indicators_definitions %>% 
      filter(indicator_label %in% selected_indicator_label) %>% 
      pull(indicator_name)
    
    # print(selected_indicator_name)
    
    # get indicator legend  -----
    selected_indicator_legend = indicators_definitions %>% 
      filter(indicator_label %in% selected_indicator_label) %>% 
      pull(indicator_legend)
    
    # get indicator values  -----
    
    selected_indicator_values = unit_indicators %>% 
      as.data.frame() %>% 
      pull(selected_indicator_name)
    
    # print(selected_indicator_values)
    
    # indicator color values ----
    
    
    
    # pal_indicator<- colorNumeric(palette = "Greens", 
    #                              domain = selected_indicator_values,
    #                              na.color = "gray",
    #                              revers = FALSE)
    
    # pal.indicator.fun = function(selected_indicator_values){
    #   if(is.na(sum(is.na(selected_indicator_values)) == length(selected_indicator_values)))
    #   {
    #     print("NOT available")
    #     selected_indicator_values = 0
    #     
    #     pal_indicator<- colorNumeric(palette = "gray",
    #                                  domain = selected_indicator_values,
    #                                  na.color = "gray",
    #                                  revers = FALSE)
    #   } else {
    #     pal_indicator<- colorNumeric(palette = "Greens",
    #                                  domain = selected_indicator_values,
    #                                  na.color = "gray",
    #                                  revers = FALSE)
    #   }
    #   return(pal_indicator)
    # }
    
    pal_indicator = pal.indicator.fun(selected_indicator_values) 
    
    
    # indicator labels for map ----
    
    labels_indicator <- sprintf("<strong>%s</strong><br/>%s: %s",
                                unit_indicators$geo_name,
                                selected_indicator_label,
                                round(selected_indicator_values, 2)) %>% 
      lapply(htmltools::HTML)
    
    
    ########################
    # collect layers ----
    ########################
    
    # layers: esa world cover
    if(input$indicator %in% c("Natural Areas",
                              "Connectivity of ecological networks",
                              "Biodiversity in built-up areas (birds)",
                              "Built-up Key Biodiversity Areas")){
      
      # ESA land cover
      
      esa_worldcover_data_path = paste("/vsicurl/https://cities-urbanshift.s3.eu-west-3.amazonaws.com/",
                                       "data/land_use/esa_world_cover/v_0/",
                                       geo_name,
                                       "-",
                                       aoi_boundary_name,
                                       "-ESA-world_cover-2020-100m.tif",
                                       sep = "")
      
      
      # collect raster data
      esa_worldcover_data = raster(esa_worldcover_data_path)
      
      city_esa_worldcover = raster::mask(esa_worldcover_data,
                                         boundary_aoi)
      
      
      # define color palette for WOrld cover
      Trees_10_green = "#006400"
      Shrubland_20_orange = "#ffbb22"
      Grassland_30_yellow = "#ffff4c" 
      Cropland_40_mauve = "#f096ff"
      Built_up_50_red = "#fa0000"
      Barren_sparse_vegetation_60_gray = "#b4b4b4"
      Snow_ice_70_white = "#f0f0f0"
      Open_Water_80_blue = "#0064c8"
      Herbaceous_wetland_90_blue2 = "#0096a0"
      Mangroves_95_green2 = "#00cf75"
      Moss_lichen_100_beige = "#fae6a0"
      
      worldcover_col = c(Trees_10_green,
                         Shrubland_20_orange,
                         Grassland_30_yellow,
                         Cropland_40_mauve,
                         Built_up_50_red,
                         Barren_sparse_vegetation_60_gray,
                         Snow_ice_70_white,
                         Open_Water_80_blue,
                         Herbaceous_wetland_90_blue2,
                         Mangroves_95_green2,
                         Moss_lichen_100_beige)
      worldcover_labels = c('Trees','Shrubland','Grassland','Cropland','Built-up',
                            'Barren / sparse vegetation','Snow/ice','Open water','Herbaceous wetland',
                            'Mangroves','Moss/lichen')
      
      # define a color palette
      pal_worldcover <- colorFactor(palette = worldcover_col, 
                                    levels = c("10","20","30","40","50","60",
                                               "70","80","90","95","100"),
                                    na.color = "transparent")
      
      # ESA land cover - natural areas
      city_worldcover_natural_areas_mask = city_esa_worldcover
      
      city_worldcover_natural_areas_mask[!city_worldcover_natural_areas_mask%in% c("10","20","30","90","95","100")] <- NA
      
      city_worldcover_natural_areas = mask(x = city_esa_worldcover,
                                           mask = city_worldcover_natural_areas_mask)
      
    }
    
    # layers: GLAD land cover
    if(input$indicator %in% c("Proportion of natural areas restored",
                              "Number of habitat types restored")){
      
      
      # GLAD 2000 ----
      glad_2000_data_path = paste("/vsicurl/https://cities-urbanshift.s3.eu-west-3.amazonaws.com/",
                                  "data/land_use/GLAD-ARD/v_0/",
                                  geo_name,
                                  "-",
                                  aoi_boundary_name,
                                  "-GLADlandcover-2000-100m.tif",
                                  sep = "")
      
      # collect raster data
      city_glad_2000 = raster(glad_2000_data_path)
      
      city_glad_2000 = raster::mask(city_glad_2000,
                                    boundary_aoi)
      
      # GLAD 2020 ----
      
      glad_2020_data_path = paste("/vsicurl/https://cities-urbanshift.s3.eu-west-3.amazonaws.com/",
                                  "data/land_use/GLAD-ARD/v_0/",
                                  geo_name,
                                  "-",
                                  aoi_boundary_name,
                                  "-GLADlandcover-2020-100m.tif",
                                  sep = "")
      
      # collect raster data
      city_glad_2020 = raster(glad_2020_data_path)
      
      city_glad_2020 = raster::mask(city_glad_2020,
                                    boundary_aoi)
      
      
      # color ----
      # define color palette for WOrld cover
      bareGround_0 = "#FEFECC"
      shortVegetation_1 = "#B9B91E"
      forest_2 = "#347834"
      tallForest_3 = "#0D570D"
      wetlandShortVegetation_4 = "#88CAAD"
      wetlandForest_5 = "#589558"
      water_6 = "#6BAED6"
      snowIce_7 = "#ACD1E8"
      cropland_8 = "#FFF183"
      built_9 = "#E8765D"
      
      glad_col = c(bareGround_0,
                   shortVegetation_1,
                   forest_2,
                   tallForest_3,
                   wetlandShortVegetation_4,
                   wetlandForest_5,
                   water_6,
                   snowIce_7,
                   cropland_8,
                   built_9)
      glad_labels = c('bare ground',
                      'short vegetation',
                      'forest',
                      'tall forest',
                      'wetland - short vegetation',
                      'wetland - forest',
                      'water',
                      'snow/ice',
                      'cropland',
                      'built-up')
      
      
      # define a color palette
      pal_glad <- colorFactor(palette = glad_col, 
                              levels = c("0","1","2","3","4","5",
                                         "6","7","8","9"),
                              na.color = "transparent")
      
      
      # GLAD changes ----
      
      glad_change_data_path = paste("/vsicurl/https://cities-urbanshift.s3.eu-west-3.amazonaws.com/",
                                    "data/land_use/GLAD-ARD/v_0/",
                                    geo_name,
                                    "-",
                                    aoi_boundary_name,
                                    "-habitatchange-2000to2020-100m.tif",
                                    sep = "")
      
      # collect raster data
      city_glad_change = raster(glad_change_data_path)
      
      city_glad_change = raster::mask(city_glad_change,
                                      boundary_aoi)
      
      city_glad_change[city_glad_change==0] = NA
      
      # define color palette for WOrld cover
      habitat_loss_10 = "red"
      habitatr_gain_1 = "purple"
      
      
      glad_change_col = c(habitat_loss_10,
                          habitatr_gain_1)
      glad_change_labels = c('Habitat loss',
                             'Habitat gain')
      
      
      # define a color palette
      pal_glad_change <- colorFactor(palette = glad_change_col, 
                                     levels = c("10","1"),
                                     na.color = "transparent")
      
      # GLAD changes loss ----
      
      city_glad_change_loss = city_glad_change
      
      city_glad_change_loss[!city_glad_change_loss==10] = NA
      
      # GLAD changes gain ----
      
      city_glad_change_gain = city_glad_change
      
      city_glad_change_gain[!city_glad_change_gain==1] = NA
      
    }
    
    
    # layers: OSM open space ----
    if(input$indicator %in% c("Recreational space per capita",
                              "Urban open space for public use",
                              "Proximity to public open space")){
      osm_open_space = st_read(paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/open_space/openstreetmap/v_0/",
                                     geo_name,
                                     "-",
                                     aoi_boundary_name,
                                     "-OSM-open_space-2022.geojson",
                                     sep = "")
      )
      
    }
    
    
    # layers: population ----
    if(input$indicator %in% c("Recreational space per capita",
                              "Proximity to public open space",
                              "Proximity to tree cover")){
      pop_data_path = paste("/vsicurl/https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/population/worldpop/v_0/",
                            geo_name,
                            "-",
                            aoi_boundary_name,
                            "-WorldPop-population-2020.tif",
                            sep = "")
      
      # collect raster data
      city_pop = raster(pop_data_path)
      
      city_pop_boundary = raster::mask(city_pop,
                                       boundary_aoi)
      
      # color pop
      pop_values = values(city_pop_boundary)[!is.na(values(city_pop_boundary))]
      
      pal_pop <- colorNumeric("RdYlBu",
                              pop_values,
                              na.color = "transparent",
                              reverse = TRUE)
      
    }
    
    # layers: Population with access to open space within 400 meters ----
    if(input$indicator %in% c("Proximity to public open space")){
      pop_openspace_data_path = paste("/vsicurl/https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/population/worldpop/v_0/",
                                      geo_name,
                                      "-",
                                      aoi_boundary_name,
                                      "-population-wOpenSpace-2020.tif",
                                      sep = "")
      
      # collect raster data
      city_pop_openspace = raster(pop_openspace_data_path)
      
      city_pop_openspace_boundary = raster::mask(city_pop_openspace,
                                                 boundary_aoi)
      
      # color pop
      pop_openspace_values = values(city_pop_openspace_boundary)[!is.na(values(city_pop_openspace_boundary))]
      
      pal_pop_openspace <- colorNumeric("RdYlBu",
                                        pop_openspace_values,
                                        na.color = "transparent",
                                        reverse = TRUE)
      
    }
    
    # layers: tree cover----
    if(input$indicator %in% c("Proximity to tree cover",
                              "Tree cover")){
      tml_data_path = paste("/vsicurl/https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/tree_cover/tree_mosaic_land/v_0/",
                            geo_name,
                            "-",
                            aoi_boundary_name,
                            "-TML-tree_cover-2020-50m.tif",
                            sep = "")
      
      # collect raster data
      city_tml = raster(tml_data_path)
      
      city_tml_boundary = raster::mask(city_tml,
                                       boundary_aoi)
      
      city_tml_boundary[city_tml_boundary==0] = NA
      
      # define color for tree cover
      pal_tml <- colorNumeric(palette = "Greens",
                              domain = values(city_tml_boundary), 
                              na.color = "transparent")
      
    }
    
    # layers: Population with access to at least 10% mean tree cover within 400 meters (persons per hectare) ----
    if(input$indicator %in% c("Proximity to tree cover")){
      
      pop_tree_cover_data_path = paste("/vsicurl/https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/population/worldpop/v_0/",
                                       geo_name,
                                       "-",
                                       aoi_boundary_name,
                                       "-population-wTreeCover-2020.tif",
                                       sep = "")
      
      # collect raster data
      pop_tree_cover_data = raster(pop_tree_cover_data_path)
      
      city_pop_tree_cover = raster::mask(pop_tree_cover_data,
                                         boundary_aoi)
      
      # color pop
      pop_tree_cover_values = values(city_pop_tree_cover)[!is.na(values(city_pop_tree_cover))]
      
      pal_pop_tree_cover <- colorNumeric("RdYlBu",
                                         pop_tree_cover_values,
                                         na.color = "transparent",
                                         reverse = TRUE)
      
    }
    
    # layers: Flooding impervious surfaces  -----
    if(input$indicator == "Permeable areas"){
      
      impervious_data_path = paste("/vsicurl/https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/impervious/tsinghua/v_0/",
                                   geo_name,
                                   "-",
                                   aoi_boundary_name,
                                   "-impervious-areas-through-2018.tif",
                                   sep = "")
      
      # collect raster data
      city_impervious = raster(impervious_data_path)
      
      
      city_impervious_boundary = raster::mask(city_impervious,
                                              boundary_aoi)
      city_impervious_boundary[city_impervious_boundary==0] = NA
      
      # color  
      impervious_values = values(city_impervious_boundary)[!is.na(values(city_impervious_boundary))]
      
      pal_impervious <- colorNumeric("Greys", 
                                     impervious_values,
                                     na.color = "transparent",
                                     reverse = FALSE)
      
      
      
      
    }
    
    # layers: Protected areas ----
    if(input$indicator %in% c("Protected areas",
                              "Protection of Key Biodiversity Areas") & !input$city %in% c("BRA-Belem",
                                                                                           "ARG-Mar_del_Plata",
                                                                                           "ARG-Ushuaia",
                                                                                           "BRA-Teresina",
                                                                                           "CHN-Ningbo",
                                                                                           "IDN-Balikpapan",
                                                                                           "IDN-Semarang",
                                                                                           "IND-Chennai",
                                                                                           "IND-Pune",
                                                                                           "IND-Surat",
                                                                                           "MAR-Marrakech",
                                                                                           "RWA-Kigali")){
      wdpa = st_read(paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/biodiversity/WDPA/v_1/",
                           geo_name,
                           "-",
                           aoi_boundary_name,
                           "-WDPA-2022.geojson",
                           sep = "")
      )
      
    }
    
    # layers: Key Biodiversity Areas ----
    if(input$indicator %in% c("Protection of Key Biodiversity Areas",
                              "Built-up Key Biodiversity Areas") & !input$city %in% c("BRA-Belem",
                                                                                      "BRA-Teresina")){
      kba = st_read(paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/biodiversity/KBA/v_1/",
                          geo_name,
                          "-",
                          aoi_boundary_name,
                          "-KBA-2022.geojson",
                          sep = "")
      )
      
    }
    
    # layers: GBIF - Vascular plant species  ----
    if(input$indicator %in% c("Vascular plant species")){
      gbif_Tracheophyta = st_read(paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/biodiversity/GBIF/Tracheophyta-",
                                        geo_name,
                                        "-",
                                        aoi_boundary_name,
                                        ".geojson",
                                        sep = ""))
      
      gbif_Tracheophyta = gbif_Tracheophyta %>% 
        mutate(long = unlist(map(gbif_Tracheophyta$geometry,1)),
               lat = unlist(map(gbif_Tracheophyta$geometry,2))) %>% 
        as.data.frame()
      
    }
    
    # layers: GBIF - Bird species  ----
    if(input$indicator %in% c("Bird species")){
      gbif_Aves = st_read(paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/biodiversity/GBIF/Aves-",
                                geo_name,
                                "-",
                                aoi_boundary_name,
                                ".geojson",
                                sep = ""))
      
      gbif_Aves = gbif_Aves %>% 
        mutate(long = unlist(map(gbif_Aves$geometry,1)),
               lat = unlist(map(gbif_Aves$geometry,2))) %>% 
        as.data.frame()
      
    }
    
    # layers: GBIF - Arthropoda  ----
    if(input$indicator %in% c("Arthropod species")){
      gbif_Arthropod = st_read(paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/biodiversity/GBIF/Arthropoda-",
                                     geo_name,
                                     "-",
                                     aoi_boundary_name,
                                     ".geojson",
                                     sep = ""))
      
      gbif_Arthropod = gbif_Arthropod %>% 
        mutate(long = unlist(map(gbif_Arthropod$geometry,1)),
               lat = unlist(map(gbif_Arthropod$geometry,2))) %>% 
        as.data.frame()
      
    }
    
    # layers: carbon flux  -----
    if(input$indicator == "Climate change impact of trees"){
      
      carbonflux_data_path = paste("/vsicurl/https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/tree_cover/wri-forest-carbon-fluxes/v_0/",
                                   geo_name,
                                   "-",
                                   aoi_boundary_name,
                                   "-WRI-ForestCarbonFluxes-MgCO2eperHA2001-2021-100m.tif",
                                   sep = "")
      
      # collect raster data
      city_carbonflux = raster(carbonflux_data_path)
      
      
      city_carbonflux_boundary = raster::mask(city_carbonflux,
                                              boundary_aoi)
      
      # color  
      carbonflux_values = values(city_carbonflux_boundary)[!is.na(values(city_carbonflux_boundary))]
      
      pal_carbonflux <- colorNumeric("PRGn", 
                                     carbonflux_values,
                                     na.color = "transparent",
                                     reverse = TRUE)
      
      
      
    }
    
    
    ########################
    # map indicator ----
    ########################
    
    # indicator layer ----
    m = leaflet(boundary_aoi) %>%
      # addTiles
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addScaleBar() %>%
      fitBounds(~as.numeric(st_bbox(boundary_aoi)[1]),
                ~as.numeric(st_bbox(boundary_aoi)[2]),
                ~as.numeric(st_bbox(boundary_aoi)[3]),
                ~as.numeric(st_bbox(boundary_aoi)[4])) %>% 
      addPolygons(data = boundary_aoi,
                  group = "Administrative boundaries",
                  stroke = TRUE, color = "black", weight = 3,dashArray = "3",
                  smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.3,
                    bringToFront = TRUE),
                  label = boundary_aoi$geo_name,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      # indicator layer - value
      addPolygons(data = unit_indicators,
                  group = selected_indicator_legend,
                  fillColor = ~pal_indicator(selected_indicator_values),
                  weight = 1,
                  opacity = 1,
                  color = "grey",
                  fillOpacity = 0.8,
                  label = labels_indicator,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = FALSE),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 6px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal_indicator,
                values = selected_indicator_values,
                opacity = 0.9,
                title = selected_indicator_legend,
                group = selected_indicator_legend,
                position = "topright",
                labFormat = labelFormat(suffix = "")) %>%
      
      # Layers control
      addLayersControl(
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
      addFullscreenControl()
    
    
    # BIO-1: Natural Areas ----
    if(input$indicator  %in% c("Natural Areas")){
      m = m %>% 
        # plot layer: ESA world cover ----
      addRasterImage(city_esa_worldcover,
                     colors = pal_worldcover,
                     opacity = 1,
                     maxBytes = 100 * 1024 * 1024,
                     project=FALSE,
                     group = "Land cover classes (ESA World Cover)") %>%
        addLegend(colors = worldcover_col,
                  labels = worldcover_labels,
                  title = "Land cover classes (ESA World Cover)",
                  group = "Land cover classes (ESA World Cover)",
                  position = "bottomleft",
                  opacity = 1) %>%
        # Raster of natural areas
        addRasterImage(city_worldcover_natural_areas,
                       colors = "#65B96B",
                       opacity = 1,
                       maxBytes = 20 * 1024 * 1024,
                       project=FALSE,
                       group = "Natural areas (derived from ESA World Cover)",
                       layerId = "Natural areas (derived from ESA World Cover)") %>%
        # Layers control
        addLayersControl(
          baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Land cover classes (ESA World Cover)",
                            "Natural areas (derived from ESA World Cover)"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Land cover classes (ESA World Cover)",
                    "Natural areas (derived from ESA World Cover)")) 
    }
    
    # BIO-2: Connectivity of ecological networks ----
    if(input$indicator  %in% c("Connectivity of ecological networks")){
      m = m %>% 
        # Raster of natural areas
        addRasterImage(city_worldcover_natural_areas,
                       colors = "#65B96B",
                       opacity = 1,
                       maxBytes = 20 * 1024 * 1024,
                       project=FALSE,
                       group = "Natural areas (derived from ESA World Cover)",
                       layerId = "Natural areas (derived from ESA World Cover)") %>%
        # Layers control
        addLayersControl(
          baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Natural areas (derived from ESA World Cover)"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Natural areas (derived from ESA World Cover)")) 
    }
    
    
    # BIO-3: Biodiversity in built-up areas (birds) ----
    if(input$indicator  %in% c("Biodiversity in built-up areas (birds)")){
      m = m %>% 
        # plot layer: ESA world cover ----
      addRasterImage(city_esa_worldcover,
                     colors = pal_worldcover,
                     opacity = 1,
                     maxBytes = 100 * 1024 * 1024,
                     project=FALSE,
                     group = "Land cover classes (ESA World Cover)") %>%
        addLegend(colors = worldcover_col,
                  labels = worldcover_labels,
                  title = "Land cover classes (ESA World Cover)",
                  group = "Land cover classes (ESA World Cover)",
                  position = "bottomleft",
                  opacity = 1) %>%
        # Layers control
        addLayersControl(
          baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Land cover classes (ESA World Cover)"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Land cover classes (ESA World Cover)")) 
    }
    
    # BIO-4: Vascular plant species ----
    if(input$indicator  %in% c("Vascular plant species")){
      m = m %>% 
        # add gbif layer
        addCircleMarkers(lat = gbif_Tracheophyta$lat,
                         lng = gbif_Tracheophyta$long,
                         radius = 3,
                         fillColor = "green",
                         color  = "black",
                         stroke = TRUE,
                         weight = 0.8,
                         fillOpacity = 0.6,
                         popup = gbif_Tracheophyta$species,
                         group ="Vascular plant species") %>% 
        # add cluster markers
        addMarkers(lat = gbif_Tracheophyta$lat,
                   lng = gbif_Tracheophyta$long, 
                   clusterOptions = markerClusterOptions(),
                   group = "Vascular plant species clusters") %>% 
        # Layers control
        addLayersControl(
          baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Vascular plant species",
                            "Vascular plant species clusters"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Vascular plant species",
                    "Vascular plant species clusters")) 
    }
    
    # BIO-5: Bird species ----
    if(input$indicator  %in% c("Bird species")){
      m = m %>% 
        # add gbif layer
        addCircleMarkers(lat = gbif_Aves$lat,
                         lng = gbif_Aves$long,
                         radius = 3,
                         fillColor = "green",
                         color  = "black",
                         stroke = TRUE,
                         weight = 0.8,
                         fillOpacity = 0.6,
                         popup = gbif_Aves$species,
                         group ="Bird species") %>% 
        # add cluster markers
        addMarkers(lat = gbif_Aves$lat,
                   lng = gbif_Aves$long, 
                   clusterOptions = markerClusterOptions(),
                   group = "Bird species clusters") %>% 
        # Layers control
        addLayersControl(
          baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Bird species",
                            "Bird species clusters"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Bird species",
                    "Bird species clusters")) 
    }
    
    # BIO-6: Arthropod species ----
    if(input$indicator  %in% c("Arthropod species")){
      m = m %>% 
        # add gbif layer
        addCircleMarkers(lat = gbif_Arthropod$lat,
                         lng = gbif_Arthropod$long,
                         radius = 3,
                         fillColor = "green",
                         color  = "black",
                         stroke = TRUE,
                         weight = 0.8,
                         fillOpacity = 0.6,
                         popup = gbif_Arthropod$species,
                         group ="Arthropod species") %>% 
        # add cluster markers
        addMarkers(lat = gbif_Arthropod$lat,
                   lng = gbif_Arthropod$long, 
                   clusterOptions = markerClusterOptions(),
                   group = "Arthropod species clusters") %>% 
        # Layers control
        addLayersControl(
          baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Arthropod species",
                            "Arthropod species clusters"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Arthropod species",
                    "Arthropod species clusters")) 
    }
    
    # GRE-1: Recreational space per capita ----
    if(input$indicator %in% c("Recreational space per capita")){
      m = m %>% 
        # plot layer: OSM ----
      addPolygons(data = osm_open_space,
                  group = "Open spaces for public use (OpenStreetMap)",
                  stroke = TRUE, color = "black", weight = 1,dashArray = "1",
                  smoothFactor = 0.5, fill = TRUE, fillColor = "green",fillOpacity = 0.5,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.3,
                    bringToFront = TRUE)) %>% 
        # plot layer: POP
        addRasterImage(city_pop_boundary,
                       colors = pal_pop ,
                       opacity = 0.9,
                       group = "Population density (persons per hectare, WorldPop)",
                       project=FALSE,
                       maxBytes = 8 * 1024 * 1024,
                       layerId = "Population") %>% 
        # Legend for population 
        addLegend(pal = pal_pop ,
                  values = pop_values,
                  opacity = 0.9,
                  title = "Population density (persons per hectare, WorldPop)",
                  group = "Population density (persons per hectare, WorldPop)",
                  position = "bottomleft") %>% 
        # Layers control ----
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Open spaces for public use (OpenStreetMap)",
                          "Population density (persons per hectare, WorldPop)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("Open spaces for public use (OpenStreetMap)",
                    "Population density (persons per hectare, WorldPop)")) 
    }
    
    # GRE-2: Urban open space for public use----
    if(input$indicator %in% c("Urban open space for public use")){
      m = m %>% 
        # plot layer: OSM ----
      addPolygons(data = osm_open_space,
                  group = "Open spaces for public use (OpenStreetMap)",
                  stroke = TRUE, color = "black", weight = 1,dashArray = "1",
                  smoothFactor = 0.5, fill = TRUE, fillColor = "green",fillOpacity = 0.5,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.3,
                    bringToFront = TRUE)) %>% 
        #   # plot layer: ESA world cover ----
      # addRasterImage(city_esa_worldcover,
      #                colors = pal_worldcover,
      #                opacity = 1,
      #                maxBytes = 100 * 1024 * 1024,
      #                project=FALSE,
      #                group = "Land cover classes (ESA World Cover)") %>%
      #   addLegend(colors = worldcover_col,
      #             labels = worldcover_labels,
      #             title = "Land cover classes (ESA World Cover)",
      #             group = "Land cover classes (ESA World Cover)",
      #             position = "bottomleft",
      #             opacity = 1) %>% 
      # Layers control ----
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          # "Land cover classes (ESA World Cover)"
                          "Open spaces for public use (OpenStreetMap)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("Open spaces for public use (OpenStreetMap)")) 
    }
    
    
    # GRE-3: Recreational space per capita ----
    if(input$indicator %in% c("Proximity to public open space")){
      m = m %>% 
        # plot layer: OSM ----
      addPolygons(data = osm_open_space,
                  group = "Open spaces for public use (OpenStreetMap)",
                  stroke = TRUE, color = "black", weight = 1,dashArray = "1",
                  smoothFactor = 0.5, fill = TRUE, fillColor = "green",fillOpacity = 0.5,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.3,
                    bringToFront = TRUE)) %>% 
        # plot layer: POP
        addRasterImage(city_pop_boundary,
                       colors = pal_pop ,
                       opacity = 0.9,
                       group = "Population density (persons per hectare, WorldPop)",
                       project=FALSE,
                       maxBytes = 8 * 1024 * 1024,
                       layerId = "Population density (persons per hectare, WorldPop)") %>% 
        # Legend for population 
        addLegend(pal = pal_pop ,
                  values = pop_values,
                  opacity = 0.9,
                  title = "Population density <br> (persons per hectare, WorldPop)",
                  group = "Population density (persons per hectare, WorldPop)",
                  position = "bottomleft") %>% 
        # plot layer: POP openspace
        addRasterImage(city_pop_openspace_boundary,
                       colors = pal_pop_openspace ,
                       opacity = 0.9,
                       group = "Population with access to open space within 400 meters (persons per hectare)",
                       project=FALSE,
                       maxBytes = 8 * 1024 * 1024,
                       layerId = "Population with access to open space within 400 meters (persons per hectare)") %>% 
        # Legend for population openspace
        addLegend(pal = pal_pop_openspace ,
                  values = pop_openspace_values,
                  opacity = 0.9,
                  title = "Population with access to open space <br> within 400 meters <br> (persons per hectare)",
                  group = "Population with access to open space within 400 meters (persons per hectare)",
                  position = "bottomleft") %>% 
        # Layers control ----
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Open spaces for public use (OpenStreetMap)",
                          "Population density (persons per hectare, WorldPop)",
                          "Population with access to open space within 400 meters (persons per hectare)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("Open spaces for public use (OpenStreetMap)",
                    "Population density (persons per hectare, WorldPop)",
                    "Population with access to open space within 400 meters (persons per hectare)")) 
    }
    
    
    # GRE-4: Proximity to tree cover ----
    if(input$indicator %in% c("Proximity to tree cover")){
      m = m %>% 
        # Raster of tree cover
        addRasterImage(city_tml_boundary, 
                       colors = pal_tml,
                       opacity = 0.9,
                       maxBytes = 20 * 1024 * 1024,
                       project=FALSE,
                       group = "Tree cover (% of pixel with tree cover)") %>%
        addLegend(pal = pal_tml,
                  values = values(city_tml_boundary), #values(city_tml_aggregate),
                  title = "Tree cover <br> (% of pixel with tree cover)",
                  group = "Tree cover (% of pixel with tree cover)",
                  position = "bottomleft") %>%
        # plot layer: POP
        addRasterImage(city_pop_boundary,
                       colors = pal_pop ,
                       opacity = 0.9,
                       group = "Population density (persons per hectare, WorldPop)",
                       project=FALSE,
                       maxBytes = 8 * 1024 * 1024,
                       layerId = "Population density (persons per hectare, WorldPop)") %>% 
        # Legend for population 
        addLegend(pal = pal_pop ,
                  values = pop_values,
                  opacity = 0.9,
                  title = "Population density <br> (persons per hectare, WorldPop)",
                  group = "Population density (persons per hectare, WorldPop)",
                  position = "bottomleft") %>% 
        # plot layer: POP with access to tree cover ----
      addRasterImage(city_pop_tree_cover,
                     colors = pal_pop_tree_cover,
                     opacity = 0.9,
                     group = "Population with access to at least 10% mean tree cover <br> within 400 meters (persons per hectare)",
                     project=FALSE,
                     maxBytes = 8 * 1024 * 1024) %>%
        addLegend(pal = pal_pop_tree_cover ,
                  values = pop_tree_cover_values,
                  opacity = 0.9,
                  title = "Population with access to <br> at least 10% mean tree cover <br> within 400 meters (persons per hectare)",
                  group = "Population with access to at least 10% mean tree cover <br> within 400 meters (persons per hectare)",
                  position = "bottomleft") %>%
        # Layers control ----
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Tree cover (% of pixel with tree cover)",
                          "Population density (persons per hectare, WorldPop)",
                          "Population with access to at least 10% mean tree cover <br> within 400 meters (persons per hectare)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("Tree cover (% of pixel with tree cover)",
                    "Population density (persons per hectare, WorldPop)",
                    "Population with access to at least 10% mean tree cover <br> within 400 meters (persons per hectare)")) 
    }
    
    
    # LND-1: Permeable areas ----
    if(input$indicator %in% c("Permeable areas")){
      m = m %>% 
        # plot layer:Impervious surfaces ----
      addRasterImage(city_impervious_boundary,
                    colors = "black",
                    opacity = 1,
                    maxBytes = 100 * 1024 * 1024,
                    project=FALSE,
                    group = "Impervious surfaces (Tsinghua GAIA)") %>% 
      # addRasterImage(city_impervious_boundary,
      #                colors = pal_impervious,
      #                opacity = 0.7,
      #                maxBytes = 20 * 1024 * 1024,
      #                project=FALSE,
      #                group = "Impervious surfaces (Tsinghua GAIA)") %>%
      #   addLegend(pal = pal_impervious,
      #             values = impervious_values,
      #             title = "Impervious surfaces (Tsinghua GAIA)",
      #             group = "Impervious surfaces (Tsinghua GAIA)",
      #             position = "bottomleft") %>% 
        # Layers control ----
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Impervious surfaces (Tsinghua GAIA)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("Impervious surfaces (Tsinghua GAIA)")) 
    }
    
    # LND-2: Tree cover ----
    if(input$indicator %in% c("Tree cover")){
      m = m %>% 
        # Raster of tree cover
        addRasterImage(city_tml_boundary, 
                       colors = pal_tml,
                       opacity = 0.9,
                       maxBytes = 20 * 1024 * 1024,
                       project=FALSE,
                       group = "Tree cover <br> (% of pixel with tree cover)") %>%
        addLegend(pal = pal_tml,
                  values = values(city_tml_boundary), #values(city_tml_aggregate),
                  title = "Tree cover <br> (% of pixel with tree cover)",
                  group = "Tree cover <br> (% of pixel with tree cover)",
                  position = "bottomleft") %>%
        # Layers control ----
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Tree cover <br> (% of pixel with tree cover)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("Tree cover <br> (% of pixel with tree cover)")) 
    }
    
    
    # LND-4: Habitat areas restored + Habitat types restored ----
    if(input$indicator %in% c("Proportion of natural areas restored",
                              "Number of habitat types restored")){
      m = m %>% 
        # city_glad_2000
        addRasterImage(city_glad_2000,
                       colors = pal_glad,
                       opacity = 1,
                       maxBytes = 100 * 1024 * 1024,
                       project=FALSE,
                       group = "Land cover classes 2000 <br> (UDM GLAD)") %>% 
        addLegend(colors = glad_col,
                  labels = glad_labels,
                  title = "Land cover classes <br> (UDM GLAD)",
                  group = "Land cover classes 2000 <br> (UDM GLAD)",
                  position = "bottomleft",
                  opacity = 1) %>%
        # city_glad_2020
        addRasterImage(city_glad_2020,
                       colors = pal_glad,
                       opacity = 1,
                       maxBytes = 100 * 1024 * 1024,
                       project=FALSE,
                       group = "Land cover classes 2020 <br> (UDM GLAD)") %>% 
        addLegend(colors = glad_col,
                  labels = glad_labels,
                  title = "Land cover classes <br> (UDM GLAD)",
                  group = "Land cover classes 2020 <br> (UDM GLAD)",
                  position = "bottomleft",
                  opacity = 1) %>% 
        addRasterImage(city_glad_change,
                       colors = pal_glad_change,
                       opacity = 1,
                       maxBytes = 100 * 1024 * 1024,
                       project=FALSE,
                       group = "Habitat changes between 2000 and 2020 <br> (derived from UDM GLAD)") %>% 
        addLegend(colors = glad_change_col,
                  labels = glad_change_labels,
                  title = "Habitat changes between 2000 and 2020 <br> (derived from UDM GLAD)",
                  group = "Habitat changes between 2000 and 2020 <br> (derived from UDM GLAD)",
                  position = "bottomleft",
                  opacity = 1) %>% 
        # addRasterImage(city_glad_change_loss,
        #                colors = "red",
        #                opacity = 1,
        #                maxBytes = 100 * 1024 * 1024,
        #                project=FALSE,
        #                group = "Habitat loss") %>% 
        # addRasterImage(city_glad_change_gain,
        #                colors = "purple",
        #                opacity = 1,
        #                maxBytes = 100 * 1024 * 1024,
        #                project=FALSE,
        #                group = "Habitat gain") %>% 
        # Layers control ----
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Habitat changes between 2000 and 2020 <br> (derived from UDM GLAD)",
                          "Land cover classes 2000 <br> (UDM GLAD)",
                          "Land cover classes 2020 <br> (UDM GLAD)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("Land cover classes 2000 <br> (UDM GLAD)",
                    "Land cover classes 2020 <br> (UDM GLAD)",
                    "Habitat changes between 2000 and 2020 <br> (derived from UDM GLAD)")) 
    }
    
    # LND-6: Protected areas ----
    if(input$indicator %in% c("Protected areas") & !input$city %in% c("BRA-Belem",
                                                                      "ARG-Mar_del_Plata",
                                                                      "ARG-Ushuaia",
                                                                      "BRA-Teresina",
                                                                      "CHN-Ningbo",
                                                                      "IDN-Balikpapan",
                                                                      "IDN-Semarang",
                                                                      "IND-Chennai",
                                                                      "IND-Pune",
                                                                      "IND-Surat",
                                                                      "MAR-Marrakech",
                                                                      "RWA-Kigali")){
      m = m %>% 
        # plot layer: OSM 
      addPolygons(data = wdpa,
                  group = "Protected areas (WDPA)",
                  stroke = TRUE, color = "black", weight = 1,dashArray = "1",
                  smoothFactor = 0.5, fill = TRUE, fillColor = "green",fillOpacity = 0.5,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.3,
                    bringToFront = TRUE)) %>% 
        # Layers control 
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Protected areas (WDPA)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("Protected areas (WDPA)")) 
    }
    
    # LND-7: Protection of Key Biodiversity Areas ----
    if(input$indicator %in% c("Protection of Key Biodiversity Areas") & !input$city %in% c("BRA-Belem",
                                                                                           "ARG-Mar_del_Plata",
                                                                                           "ARG-Ushuaia",
                                                                                           "BRA-Teresina",
                                                                                           "CHN-Ningbo",
                                                                                           "IDN-Balikpapan",
                                                                                           "IDN-Semarang",
                                                                                           "IND-Chennai",
                                                                                           "IND-Pune",
                                                                                           "IND-Surat",
                                                                                           "MAR-Marrakech",
                                                                                           "RWA-Kigali")){
      m = m %>% 
        # plot layer: WDPA 
      addPolygons(data = wdpa,
                  group = "Protected areas (WDPA)",
                  stroke = TRUE, color = "black", weight = 1,dashArray = "1",
                  smoothFactor = 0.5, fill = TRUE, fillColor = "green",fillOpacity = 0.5,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.3,
                    bringToFront = TRUE)) %>% 
        # plot layer: KBA 
      addPolygons(data = kba,
                  group = "Key biodiversity areas <br> (KBA Partnership)",
                  stroke = TRUE, color = "black", weight = 1,dashArray = "1",
                  smoothFactor = 0.5, fill = TRUE, fillColor = "yellow",fillOpacity = 0.5,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.3,
                    bringToFront = TRUE)) %>% 
        # Layers control 
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Protected areas (WDPA)",
                          "Key biodiversity areas <br> (KBA Partnership)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("Protected areas (WDPA)",
                    "Key biodiversity areas <br> (KBA Partnership)")) 
    }
    
    
    # LND-8: Built-up Key Biodiversity Areas ----
    if(input$indicator %in% c("Built-up Key Biodiversity Areas") & !input$city %in% c("BRA-Belem",
                                                                                      "BRA-Teresina")){
      m = m %>% 
        # plot layer: KBA ----
      addPolygons(data = kba,
                  group = "Key biodiversity areas <br> (KBA Partnership)",
                  stroke = TRUE, color = "black", weight = 1,dashArray = "1",
                  smoothFactor = 0.5, fill = TRUE, fillColor = "yellow",fillOpacity = 0.5,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.3,
                    bringToFront = TRUE)) %>% 
        # plot layer: ESA world cover ----
      addRasterImage(city_esa_worldcover,
                     colors = pal_worldcover,
                     opacity = 1,
                     maxBytes = 100 * 1024 * 1024,
                     project=FALSE,
                     group = "Land cover classes (ESA World Cover)") %>%
        addLegend(colors = worldcover_col,
                  labels = worldcover_labels,
                  title = "Land cover classes (ESA World Cover)",
                  group = "Land cover classes (ESA World Cover)",
                  position = "bottomleft",
                  opacity = 1) %>%
        # Layers control ----
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Key biodiversity areas <br> (KBA Partnership)",
                          "Land cover classes (ESA World Cover)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("Key biodiversity areas <br> (KBA Partnership)",
                    "Land cover classes (ESA World Cover)")) 
    }
    
    # GHG-2: Climate change impact of trees ----
    if(input$indicator == "Climate change impact of trees"){
      m = m %>%
        # plot layer: carbon flux
        addRasterImage(city_carbonflux_boundary,
                       colors = pal_carbonflux ,
                       opacity = 0.9,
                       group = "Carbon flux from trees <br> (net, Mg CO2e/ha, 2001 to 2021)",
                       project=FALSE,
                       maxBytes = 8 * 1024 * 1024,
                       layerId = "Carbon flux from trees") %>%
        # Legend for population
        addLegend(pal = pal_carbonflux ,
                  values = carbonflux_values,
                  opacity = 0.9,
                  title = "Carbon flux from trees <br> (net, Mg CO2e/ha, 2001 to 2021)",
                  group = "Carbon flux from trees <br> (net, Mg CO2e/ha, 2001 to 2021)",
                  position = "bottomleft") %>% 
        # Layers control
        addLayersControl(
          baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Carbon flux from trees <br> (net, Mg CO2e/ha, 2001 to 2021)"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        hideGroup(c("Carbon flux from trees <br> (net, Mg CO2e/ha, 2001 to 2021)"))
    }
    
    # center map  
    output$indicator_map <- renderLeaflet({
      m
    })
    
    
    
    # download spatial data ----
    
    unit_indicators_download = unit_indicators %>% 
      dplyr::select(geo_id,
                    geo_level,
                    geo_name,
                    geo_parent_name,
                    selected_indicator_name)
    
    output$download_geo_data <- downloadHandler(
      filename = function() {
        paste("data-geo-", 
              geo_name,
              "-",
              selected_indicator_label,
              # Sys.Date(), 
              ".geojson", sep="")
      },
      content = function(file) {
        st_write(unit_indicators_download, file, driver = "GeoJSON")
      }
    )
    
    
    #########################################
    ### Main indicators ----
    
    # city wide  ----
    city_wide_indicator_value = aoi_indicators %>%
      as.data.frame() %>%
      pull(selected_indicator_name) %>% 
      round(2)
    
    # unit
    
    if(input$indicator %in% c("Natural Areas",
                              # "Connectivity of ecological networks",
                              "Biodiversity in built-up areas (birds)",
                              "Permeable areas",
                              "Tree cover",
                              "Proportion of natural areas restored",
                              "Number of habitat types restored",
                              "Protected areas",
                              "Protection of Key Biodiversity Areas",
                              "Built-up Key Biodiversity Areas",
                              "Urban open space for public use",
                              "Urban open space for public use",
                              "Proximity to public open space",
                              "Proximity to tree cover")){
      city_wide_indicator_value_unit = "%"
    } else if(input$indicator %in% c("Vascular plant species",
                                     "Bird species",
                                     "Arthropod species",
                                     "Connectivity of ecological networks")){
      city_wide_indicator_value_unit = ""
      
    } else if(input$indicator %in% c("Recreational space per capita")){
      city_wide_indicator_value_unit = "hectares"
      
    } else if(input$indicator %in% c("Change in greenhouse gas emissions")){
      city_wide_indicator_value_unit = "% change"
    }
    else if(input$indicator %in% c("Climate change impact of trees")){
      city_wide_indicator_value_unit = "Mg CO2 eq/hectare"
    }
    
    
    
    # City-wide inidcator
    output$city_wide_indicator <- renderText({
      paste("<center>","<font size=5px; weight=500; color=\"#2A553E\"><b>", 
            city_wide_indicator_value, 
            city_wide_indicator_value_unit,"<br>",
            # "<font size=2px; weight=500; color=\"#168A06\"><b>",
            "<font size=2px; weight=500; color=\"#A0D1B4\"><b>",
            selected_indicator_legend)
    })
    
    
    
    #########################################
    ### Table ----
    
    # Table plot
    
    if(input$indicator %in% c("Change in greenhouse gas emissions")){
      table_plot_years = aoi_indicators %>% 
        as.data.frame() %>%
        dplyr::select(-geometry) %>% 
        pivot_longer(
          cols = bc_agl_2000_co2e:nmvoc_tro_2020_co2e,
          names_to = c("gas", "sector","year","unit"),
          names_pattern = "(.*)_(.*)_(.*)_(.*)",
          values_to = "value") %>% 
        dplyr::select(geo_name,
                      gas,
                      sector, 
                      year,
                      value) %>% 
        arrange(desc(value)) %>% 
        mutate_at("gas", 
                  ~recode(.,
                          "bc"='Black carbon', 
                          'ch4'='Methane',
                          'co' = 'Carbon monoxide',
                          'co2' = 'Carbon dioxide',
                          'nox' = 'Nitrogen oxides',
                          'so2' = 'Sulfur dioxide',
                          'oc' = 'Organic carbon',
                          'nh3' = 'Ammonia',
                          'nmvoc' = 'Non-methane volatile organic compounds')) %>% 
        mutate_at("sector", 
                  ~recode(.,
                          "agl"='Agriculture livestock', 
                          'ags'='Agriculture soils',
                          'awb' = 'Agriculture waste burning',
                          'ene' = 'Power generation',
                          'fef' = 'Fugitives',
                          'ind' = 'Industry',
                          'res' = 'Residential, commercial, and other combustion',
                          'shp' = 'Ships',
                          'slv' = 'Solvents',
                          'sum' = 'All sources',
                          'swd' = 'Solid waste and wastewater',
                          'tnr' = 'Off-road transportation',
                          'tro' = 'Road transportation'))
      
      table_plot = table_plot_years %>% 
        dplyr::select("Sector" = "sector",
                      "Gas" = "gas",
                      "Year" = "year",
                      "Emissions (tonnes of CO2 equivalent)" = "value")
    } else {
      table_plot = unit_indicators %>% 
        drop_na(selected_indicator_name, geo_name) %>% 
        as.data.frame() %>%
        dplyr::select(-geometry) %>% 
        dplyr::select(geo_name,selected_indicator_name) %>% 
        mutate_if(is.numeric, round, 2) %>% 
        distinct(geo_name, .keep_all = T) %>% 
        arrange(desc(selected_indicator_name)) 
      
      selected_indicator_legend_table = str_remove(selected_indicator_legend, "<br> ")
      # names(table_plot) = c("City name",selected_indicator_legend_table)
      names(table_plot) = c("Name",selected_indicator_legend_table)
    }
    
    
    
    # plot table
    
    
    output$indicator_table <- DT::renderDataTable(
      if(input$indicator %in% c("Change in greenhouse gas emissions")){
        DT::datatable(table_plot,
                      options = list(pageLength = 10,order = list(list(2, 'desc')))) 
      } else {
        DT::datatable(table_plot, 
                      options = list(pageLength = 10,
                                     order = list(list(2, 'desc')))) %>% formatStyle(
                                       selected_indicator_legend_table,
                                       backgroundColor = styleInterval(seq(from = min(table_plot[,selected_indicator_legend_table]),
                                                                           to = max(table_plot[,selected_indicator_legend_table]),
                                                                           length.out = 8), 
                                                                       brewer.pal(9, "Greens")
                                       ),
                                       fontWeight = 'bold')
      }
    )
    
    
    # output data to download
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$city,"-", input$indicator,"-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(table_plot,
                  file,
                  row.names=FALSE,
                  fileEncoding = "latin1")
      }
    )
    
    
    
    
    #########################################
    ### Chart ----
    
    if(input$indicator %in% c("Change in greenhouse gas emissions")){
      
      table_plot_2000 = table_plot_years %>% 
        filter(year == 2000)
      
      table_plot_2020 = table_plot_years %>% 
        filter(year == 2020)
      
      fig_2000 <- plot_ly(table_plot_2000, 
                          x = ~sector, 
                          y = ~value, 
                          type = 'bar',
                          name = ~gas,
                          color = ~gas,
                          legendgroup= ~year,
                          showlegend = FALSE) %>% 
        layout(yaxis = list(title = 'Greenhouse gas emissions <br> (tonnes of CO2 equivalent)'), 
               xaxis = list(title = 'sectors',categoryorder = "total descending"),
               barmode = 'stack')
      
      
      fig_2020 <- plot_ly(table_plot_2020, 
                          x = ~sector, 
                          y = ~value, 
                          type = 'bar',
                          name = ~gas,
                          color = ~gas,
                          legendgroup= ~year,
                          showlegend = TRUE) %>% 
        layout(yaxis = list(title = 'Greenhouse gas emissions <br> (tonnes of CO2 equivalent)'), 
               xaxis = list(title = 'sectors',categoryorder = "total descending"),
               barmode = 'stack')
      
      annotations = list( 
        list( 
          x = 0.2,  
          y = 1.0,  
          text = "2000",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = FALSE 
        ),  
        list( 
          x = 0.8,  
          y = 1,  
          text = "2020",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = FALSE 
        ))
      
      fig <- subplot(fig_2000, fig_2020, shareY = TRUE, margin = 0.05) %>% 
        layout(legend=list(title=list(text='<b> Gases </b>')),
               annotations = annotations)
    } else {
      fig = plot_ly(x = table_plot$`Name`,
                    # x = table_plot$`City name`,
                    y = table_plot[[colnames(table_plot)[2]]],
                    type = "bar",
                    orientation = "v",
                    name = names(table_plot)[2],
                    # color = I("green4"),
                    color = I("#2A553E")) %>% 
        layout(yaxis = list(title = selected_indicator_legend),
               xaxis = list(categoryorder = "total descending"))
      
      fig
    }
    
    output$indicator_chart <- renderPlotly({
      fig
      
    })
    
    
    ########################################
    
    # cities comparison ----
    
    indicators_comparison = indicators_comparison %>% 
      as.data.frame() %>%
      dplyr::select(geo_name,selected_indicator_name) %>% 
      drop_na(selected_indicator_name, geo_name) %>% 
      mutate_if(is.numeric, round, 2) %>% 
      arrange(desc(selected_indicator_name)) 
    
    # change names
    names(indicators_comparison) = c("City name",selected_indicator_label)
    
    city_num = which(indicators_comparison$`City name` == geo_name)
    city_color = rep("#A0D1B4",nrow(indicators_comparison)) #, "grey"
    city_color[city_num] = "#2A553E" #"green"
    
    
    
    cities_indicator_avg = round(mean(indicators_comparison[[colnames(indicators_comparison)[2]]]),2)
    
    
    output$cities_comparison_plot <- renderPlotly({
      fig = plot_ly(x = indicators_comparison$`City name`,
                    y = indicators_comparison[[colnames(indicators_comparison)[2]]],
                    type = "bar",
                    orientation = "v",
                    name = names(indicators_comparison)[2],
                    marker = list(color = city_color)) %>% 
        layout(yaxis = list(title = selected_indicator_legend), 
               xaxis = list(title = 'Cities',categoryorder = "total descending"),
               annotations = list(
                 x = 10,
                 y = cities_indicator_avg+cities_indicator_avg*0.1, 
                 text = paste("Cities' average: ", 
                              cities_indicator_avg, 
                              city_wide_indicator_value_unit, 
                              sep = ""),
                 showarrow = FALSE,
                 xanchor = "right"
               ))
      
      add_trace(fig,
                y = cities_indicator_avg, 
                type='scatter',
                mode = 'lines',
                name = 'Average', 
                showlegend = F,
                line = list(color = 'black', 
                            dash = 'dot',
                            width = 1)) 
      
    })
    
    # benchmark data to download
    output$downloadDataBenchmark <- downloadHandler(
      filename = function() {
        paste("Benchmark-", input$indicator,"-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(indicators_comparison,
                  file,
                  row.names=FALSE,
                  fileEncoding = "latin1")
      }
    )
    

    
    #########################################
    ### Indicator definition text  ----
    
    indicator_def_text = indicators_definitions %>% 
      filter(indicator_label == selected_indicator_label) %>% 
      pull(indicator_definition)
    
    indicator_data_sources = indicators_definitions %>% 
      filter(indicator_label == selected_indicator_label) %>%  
      pull(data_sources)
    
    indicator_importance = indicators_definitions %>% 
      filter(indicator_label == selected_indicator_label) %>%  
      pull(importance)
    
    indicator_methods = indicators_definitions %>% 
      filter(indicator_label == selected_indicator_label) %>%  
      pull(methods)
    
    # plot text 
    output$indicator_definition <- renderText({
      paste("<right>","<font size=3px; weight=100; color=\"#2A553E\"><b>",
            "<font color=\"#2A553E\"><b>", " ", "<br>",
            "<font color=\"#2A553E\"><b>","Definition: ",
            "<font color=\"#242456\"><b>", indicator_def_text,
            "<br/>",
            "<font color=\"#2A553E6\"><b>", " ", "<br>",
            "<font color=\"#2A553E\"><b>","Data sources: ",
            "<font color=\"#242456\"><b>", indicator_data_sources,
            "<br/>",
            "<font color=\"#2A553E\"><b>", " ", "<br>",
            "<font color=\"#2A553E\"><b>","Importance: ",
            "<font color=\"#242456\"><b>", indicator_importance,
            "<br/>",
            "<font color=\"#2A553E\"><b>", " ", "<br>",
            "<font color=\"#2A553E\"><b>","Methods: ",
            "<font weight=50; color=\"#242456\"><b>", indicator_methods
      )
    })
    
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)