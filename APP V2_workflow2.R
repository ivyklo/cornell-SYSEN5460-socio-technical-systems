library(shiny)
library(dplyr) # data wrangling
library(readr) # reading data
library(ggplot2) # data vizualization
library(scales) # for rescaling values
library(RSQLite) #database connection
library(stringr) #string manipulation
library(lubridate) #date manipulation
library(tidyr)
library(shinydashboard) #styled boxes
library(sf) # to read/work with sf data
library(ggspatial) # for plotting maps
library(cowplot)
library(leaflet)
library(plotly)

sta_bg <- read_rds("data/stationbg_dataset.rds") %>%
  select(1:17)
boston <- read_sf("data/bounds.geojson")
blocks <- read_rds("data/bgdataset.rds") %>%
  select(1:16)
neighborhood <- read_sf("data/boston_neighborhood_boundaries.json") %>%
  st_make_valid() %>%
  select(name, sqmiles, geometry)

#store the data in global environment
assign("sta_bg", sta_bg, envir = .GlobalEnv)
assign("boston", boston, envir = .GlobalEnv)
assign("blocks", blocks, envir = .GlobalEnv)
assign("neighborhood", neighborhood, envir = .GlobalEnv)

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Spatial Analysis of Boston Blue Bike Stations"),
  
  layout_columns(
    layout_columns(
      card(card_header("Map Category"),
           radioButtons(inputId = "map_type",
                        label = "Select Map to View:",
                        choices = c("By County" = "sel_region", "By Density" = "sel_density", "By Income" = "sel_income"), # Map display names to data values
                        selected = "sel_county")), # Select AM by default
      
      card(card_header("Plot Details"),
           checkboxInput(inputId = "select_bikes",
                         label = "Blue Bike Stations on Map",
                         value = TRUE),
           uiOutput("adaptive_checkboxes")),
      col_widths = c(6,6)),
    
    card(card_header("Qualities of Interest"),
        # valueBoxOutput("stat1"),
        # valueBoxOutput("stat2"),
         plotlyOutput("bar_chart")),
    
    card(card_header("Plot of Interest"),
         plotOutput("desired_plot")))
)

Server <- function(input, output) {
  
  # Initial Calculations:
  
  # For plot by county ----
  # Identify which block groups are in the Boston boundary
  bgdataset <- blocks %>%
    select(geoid, geometry) %>%
    mutate(
      is_boston = lengths(st_intersects(geometry, boston)) > 0,
      location_group = case_when(
        is_boston ~ "Boston",
        str_sub(geoid, 3, 5) == "025" ~ "Suffolk (Includes Boston)",
        TRUE ~ "Other Areas"))
  
  # Extract Suffolk County (Boston + Not Boston)
  suffolk_geom <- bgdataset %>%
    filter(location_group %in% c("Boston", "Suffolk (Not Boston)")) %>%
    st_union()  # unify Suffolk County geometry
  
  # Calculate centroid of Suffolk
  suffolk_centroid<- st_centroid(suffolk_geom)
  
  
  # Create 15 km buffer from Suffolk centroid
  buffer_15km <- st_buffer(suffolk_centroid, dist = 15 * 1000)
  
  # Filter block groups within 15 km
  bg_within_15km <- bgdataset[st_intersects(bgdataset, buffer_15km, sparse = FALSE), ]
  
  # Prepare BlueBikes dataset with label
  stationbg_dataset <- sta_bg %>%
    select(geometry, geoid) %>%
    mutate(type = "BlueBikes Station")
  
  # Filter stations within 15 km
  stations_within_15km <- stationbg_dataset[st_intersects(stationbg_dataset, buffer_15km, sparse = FALSE), ]
  
  # Assign county names based on GEOID prefix
  
  assign_county <- bg_within_15km %>%
    mutate(
      county_fips = str_sub(geoid, 1, 5),
      county_name = case_when(
        is_boston ~ "Boston",
        county_fips == "25025" ~ "Suffolk (Includes Boston)",
        county_fips == "25017" ~ "Middlesex",
        county_fips == "25021" ~ "Norfolk",
        TRUE ~ "Other" ))
  #_____________________________________________________________________________
  # For plot by density: 
  sta_in <- sta_bg %>%
    select(geometry, geoid) %>%
    st_join(boston, left = FALSE)       # Filter to bike stations in Boston
  
  blocks_in <- blocks %>%
    select(geometry, geoid) %>%
    st_join(sta_in, left = FALSE) %>%   # Census blocks inside Boston w/Bike stations
    select(-geoid.y)
  
  blocks_bos <- blocks %>%
    st_join(boston, left = FALSE)       # All census blocks inside Boston
  
  # Convert areas in neighborhood data.frame to units of km^2
  area_km2 <- neighborhood %>%
    mutate(area = sqmiles*2.58999) %>%
    st_drop_geometry() %>%
    select(name, area)
  
  # Add area column to sta_neigh data.frame
  sta_neigh <- neighborhood %>%
    st_join(sta_in, left = FALSE, join = st_intersects) %>%
    select(name.x, geometry) %>%
    arrange(name.x) %>%
    add_count(name.x, name = 'n') %>%
    distinct(name.x, .keep_all = TRUE) %>%
    rename(name = name.x) %>%
    left_join(area_km2, by = "name")
  
  # Calculate the density of blue bike stations in each neighborhood, arrange highest to lowest
  dens_neigh <- sta_neigh %>%
    mutate(density = n / area) %>%
    arrange(desc(density))
  
  # WANT TO OUTPUT THESE MAX, MIN, MEDIAN VALUES
  # Find min, median, max densities 
  min_dens <- dens_neigh %>% filter(density == min(density))    # Hyde Park, 0.253 stations/km^2
  dens_neigh %>% filter(density == median(density))             # Allston, 2.72 stations/km^2
  max_dens <- dens_neigh %>% filter(density == max(density))    # Downtown, 11.8 stations/km^2
  #____________________________________________________________________________
  # For plot by income:
  
  
  
  # ____________________________________________________________________________
  
  # Dynamically generated checkboxes based on map type
  output$adaptive_checkboxes <- renderUI({
    if(input$map_type == "sel_region") {
      checkboxGroupInput("sel_county", "Select Counties:",
                         choices = sort(unique(assign_county$county_name)))
    } elseif(input$map_type == "sel_density"){
      checkboxGroupInput("sel_neighborhood", "Select Neighborhoods:",
                         choices = sort(unique(dens_neigh$name)))
    } elseif(input$map_type == "sel_income"){
      checkboxGroupInput("sel_incomeBracket", "Select Income Bracket:",
                         choices = c("High", "Low"))
    } else {NULL}
      
  }) %>% bindEvent({ input$map_type })  # Updates with map type selection
  
  
  
  # Reactive variables:
  
  selected_county = reactive({
    
    if(input$map_type == "sel_region") {
      data_b <- assign_county %>%
        mutate(county_name = factor(input$county_name))
    } else {NULL}
           
    return(data_b)
    
  })%>% bindEvent({ input$map_type; input$county_name })  # Updates with map type selection
  
  income_bos = reactive({
    
    data_c <- blocks_bos %>%
      rowwise() %>% # Process row by row
      mutate(
        # Create a named vector of income values
        income_values = list(c(
          pop_0_40000_2019 = pop_0_40000_2019,
          pop_40001_60000_2019 = pop_40001_60000_2019,
          pop_60001_100000_2019 = pop_60001_100000_2019,
          pop_100000_plus_2019 = pop_100000_plus_2019
        )),
        # Find the maximum value among the income columns
        max_income_value = max(unlist(income_values), na.rm = TRUE),
        # Determine the category label
        income_category = if (max_income_value >= 0.5) {
          # Find the name of the column with the maximum value
          names(income_values)[which.max(income_values)]
        } else {
          "Below Threshold" # Assign "Below Threshold" if max value is less than 0.5
        }
      ) %>%
      ungroup() # Ungroup the data
    
    selected_data <- data_c %>%
      if(input$sel_incomeBracket == "High") {
        filter(max_income_value > 0.6)
      } elseif(input$sel_incomeBracket == "Low"){
        filter(max_income_value < 0.6)
      } else {NULL}
    
    return(selected_data)
    
  }) %>% bindEvent({ input$map_type; input$sel_incomeBracket})  # Updates with map type selection
  
 
  # Desired plot based on selected map type and checkbox selections
  
  output$desired_plot <- renderPlot({
    
    # Create the plot based on selected graph type
    if (input$map_type == "sel_region") {
      p <- ggplot() +
        geom_sf(data = selected_county, aes(fill = county_name), color = "grey70", size = 0.2) +
        geom_sf(data = buffer_15km, fill = NA, color = "black", size = 0.6) +
        geom_sf(data = suffolk_centroid, aes(shape = "Centroid"), color = "darkblue", size = 2) +
        if(input$select_bikes == TRUE){   ### input$select_bikes
          geom_sf(data = stations_within_15km, aes(shape = type), color = "blue", fill = "lightblue", size = 1, stroke = 0.3)} +   # Plot bike stations 
        scale_fill_manual(
          values = c(
            "Boston" = "firebrick",
            "Suffolk (Includes Boston)" = "orange",
            "Middlesex" = "lightgreen",
            "Norfolk" = "gold",
            "Other" = "gray90"),
          name = "County / City") +
        scale_shape_manual(
          values = c("BlueBikes Station" = 21, "Centroid" = 19),
          name = NULL) +
        labs(
          title = "Bluebikes Stations and Block Groups within 15 km of Suffolk Centroid",
          subtitle = "(Boston is part of Suffolk County)",
          x = "Longitude", y = "Latitude") +
        ggspatial::annotation_scale(location = "bl", width_hint = 0.4) +
        theme_minimal() +
        guides(
          fill = guide_legend(order = 1),
          shape = guide_legend(
            override.aes = list(size = c(3, 3), fill = c("lightblue", NA), color = c("blue", "darkblue")),
            order = 2))
      # Add text using cowplot
      ggdraw(p) +
        draw_label(
          label = paste("Total Bluebikes stations in this map:", station_count),
          x = 0.97, y = 0.03,  # adjust for position (x = right, y = bottom)
          hjust = 1,
          size = 10,
          fontface = "plain")
      
    } elseif(input$map_type == "sel_density"){
         base_plot <- ggplot() +
            geom_sf(data = dens_neigh, aes(fill = density), color = "black", size = 0) +  #Plot Neighborhoods
            geom_sf(data = blocks_bos, fill = NA, color = "white", size = 0.4) +      # Plot all census blocks in Boston 
            geom_sf(data = neighborhood, fill = NA, color = "black", size = 0.5) +   # Plot neighborhood boundaries
      
      scale_fill_gradientn(
        colors = c("lightblue", "#FCB97D", "#A84268"),
        limits = c(min_dens$density, max_dens$density),
        # oob = scales::squish,
        labels = scales::label_number(suffix =" Stations/km^2"),
        name = "Density of Blue Bike Stations") +
      if(input$select_bikes == TRUE){   ### input$select_bikes
        geom_sf(data = sta_in, color = "#241571", size = 1.75, alpha = 0.5)} # Plot bike stations 
    
    # Crop the map, and reorder legend
    main_plot <- base_plot +
      geom_sf(data = box, fill = NA, color = "#8B0000", linewidth = .5)+
      coord_sf( xlim = c(-71.20, -70.90),
                ylim = c(42.23, 42.42)) +
      labs(title = "Location and Density of Blue Bike Stations in Boston Neighborhoods",
           subtitle = "Data Source: 'Station_bg.rdg' by T. Fraser, et. all.",
           x = "Longitude",
           y = "Latitude")+
      annotation_scale(location = "bl")+
      annotation_north_arrow(location = "bl", which_north = "true",
                             style = north_arrow_fancy_orienteering,
                             pad_x = unit(0.08, "npc"),
                             pad_y = unit(0.07, "npc")) +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.justification = c(0, 1),
        legend.position = c(.05, 0.95),
        legend.background = element_rect(fill = NA, color = NA)
      )
    main_plot
    
  } elseif(input$map_type == "sel_income"){
   
    # Define a custom color palette for the income categories
    income_colors <- c(
      "pop_0_40000_2019" = "#DC267F",        
      "pop_40001_60000_2019" = "purple",    
      "pop_60001_100000_2019" = "green",   
      "pop_100000_plus_2019" = "steelblue2",
      "Below Threshold" = "wheat"       
    )
    
    # Plot them
    ggplot() +
      # Plot income_bos polygons with fill based on income_category
      geom_sf(data = income_bos, aes(fill = income_category), color = NA, linewidth = 0.1, alpha = 0.5) +  #census group
      geom_sf(data = income_bos, fill = NA, color = "black", linewidth = 0.2) + # Outline of smaller areas
      geom_sf(data = boston, fill = NA, color = "black", alpha = 0.8, linewidth = 1) + # Suffolk outline
      if(input$select_bikes == TRUE){   ### input$select_bikes
        geom_sf(data = sta_in, color = "#241571", size = 1.75, alpha = 0.5)}+ # Plot bike stations 
      
      # Use scale_fill_manual for the income categories
      scale_fill_manual(values = income_colors,
                        name = "Dominant Income\nCategory (> 50%)",
                        # Add labels for the legend keys
                        labels = c(
                          "pop_0_40000_2019" = "$0 - $40k",
                          "pop_40001_60000_2019" = "$40k - $60k",
                          "pop_60001_100000_2019" = "$60k - $100k",
                          "pop_100000_plus_2019" = "$100k+",
                          "Below Threshold" = "Less than 50% in any income category"
                        ),
                        # Ensure all categories appear in the legend
                        breaks = names(income_colors)) +
      scale_color_manual(values = c("Bike Station" = "black",shape=2,size = 2 ), name = "Legend:",
                         guide = guide_legend(override.aes = list(linetype = c("blank"), shape = c(16)))) +
      # Manually set color and legend title for bike stations
      labs(
        title = "Boston Bike Sharing Map by Dominant Income Category",
        x = "Longitude",
        y = "Latitude",
        caption = "Figure 1: This visual is showing a concentration of low income \nneighboorhood in the center of Boston. \nThe surrounding area of Boston contans \n neighborhood that are making more than $100K annually"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 8), # Adjust size for x-axis text
        axis.text.y = element_text(size = 8), # Adjust size for y-axis text
        legend.box = "vertical" # Arrange multiple legends vertically
      ) +
      annotation_scale(location = "br", style = "tick", text_cex=0.7, height = unit(0.5, "cm")) # Adds a scale bar
    
  } else {NULL}
    
  }) %>% bindEvent({input$map_type; input$select_bikes})
    
}
  
  
    
    
    
    
    
  )