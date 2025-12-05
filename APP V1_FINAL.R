## Team 7-11 APP V1 FINAL
## SYSEN 5460

global = function(){    
  
  library(dplyr) # data wrangling
  library(readr) # reading data
  library(ggplot2) # data vizualization
  library(scales) # for rescaling values
  library(shiny) # main shiny app package
  library(RSQLite) #database connection
  library(stringr) #string manipulation
  library(lubridate) #date manipulation
  library(tidyr)
  library(shinydashboard) #styled boxes
  
  #load the data so it is available for ui and server
  
  rush_db = dbConnect(RSQLite::SQLite(), "data/bluebikes/bluebikes.sqlite")
  tally_rush_data = dbReadTable(rush_db, "tally_rush")
  dbDisconnect(rush_db)
  
  tally_rush_data$day = as.Date(tally_rush_data$day)  #convert "day" column into Date format
  tally_rush_data$year = lubridate::year(tally_rush_data$day)  #Extract year into a new "year" column
  tally_rush_data$day_of_year = yday(tally_rush_data$day) #extract day for plotting
  
  
  #store the data in global environment
  assign("tally_rush_data", tally_rush_data, envir = .GlobalEnv)
  
}

ui = function(){
  
  # Load up the database
  tally_rush_data = get("tally_rush_data", envir = .GlobalEnv)
  
  # Filter to get unique year from the year column
  available_years = sort(unique(tally_rush_data$year))
  
  fluidPage(
    titlePanel("Bluebikes Boston Rider Dashboard"),
    sidebarLayout( # Use sidebar Layout for a common dashboard layout
      sidebarPanel(
        # Input for Year Selection
        selectInput(inputId = "selected_year",
                    label = "Select Year:",
                    choices = available_years,
                    selected = available_years[1]), # Select the first available year by default
        
        # Input for AM/PM Selection
        radioButtons(inputId = "rush_period",
                     label = "Select Rush Period:",
                     choices = c("AM" = "am", "PM" = "pm"), # Map display names to data values
                     selected = "am"), # Select AM by default
        
        # Input for Graph Type (Line or Bar)
        radioButtons(inputId = "graph_type",
                     label = "Select Graph Type:",
                     choices = c("Line Plot" = "line", "Bar Plot" = "bar"),
                     selected = "line"), # Select Line Plot by default
        
        br(),
        h4("Average Monthly Ride Count Comparisons and Year-to-Year Trends"),
        tableOutput(outputId = "monthly_stats")
        
      ),
      mainPanel(
        h3("Key Statistics"),
        fluidRow(
          valueBoxOutput("vb1", width = 6),
          valueBoxOutput("vb2", width = 6)
        ),
        br(),
        plotOutput(outputId = "rider_count_plot"),
        br(),
        plotOutput(outputId = "distribution_plot"),
        br(),
        h4("Median Ride Counts per Month"),
        tableOutput(outputId = "monthly_median")
      )
      
    )
  )
}

server = function(input, output, session){
  
  # Access the data loaded in the global function
  tally_rush_data = get("tally_rush_data", envir = .GlobalEnv)
  
  # Reactive expression to filter data based on user inputs
  filtered_data = reactive({
    data = tally_rush_data %>%
      filter(year == as.integer(input$selected_year)) %>% # Filter by selected year (ensure types match)
      filter(rush == input$rush_period) %>% # Filter by selected rush period (am/pm)
      mutate(month = month(day, label = TRUE, abbr = TRUE)) %>% #Create a month column
      # Arrange by the 'day' column (which is now a Date) for correct plotting order
      arrange(day)  
    return(data)
  })
  
  #New Value Boxes and Text Boxes (no border around the value boxes)
  
  # Text Boxes
  #output$text_summary_1 <- renderText({
  #  paste("Total Riders in", input$selected_year, toupper(input$rush_period), "Rush Hour")
  # })
  
  # output$text_summary_2 <- renderText({
  #  paste("Average Riders per Day in", input$selected_year)
  # })
  
  # Value Boxes
  #output$value_box_1 <- renderText({
  # format(sum(filtered_data()$count, na.rm = TRUE), big.mark = ",")
  #  })
  
  #output$value_box_2 <- renderText({
  #round(mean(filtered_data()$count, na.rm = TRUE), 1)
  #})
  
  
  #Value Boxes with border, text included & without ‘[1]’
  output$vb1 <- renderValueBox({
    valueBox(
      value = format(sum(filtered_data()$count, na.rm = TRUE), big.mark = ","),
      subtitle = paste("Total Riders in", input$selected_year, toupper(input$rush_period), "Rush Hour"),
      color = "aqua",
      icon = icon("bicycle")
    )
  })
  
  output$vb2 <- renderValueBox({
    valueBox(
      value = round(mean(filtered_data()$count, na.rm = TRUE), 1),
      subtitle = paste("Average Riders per Day in", input$selected_year),
      color = "light-blue",
      icon = icon("chart-line")
    )
  })
  
  
  # Calculate Statisics for Output Table
  stats = reactive({
    
    sel_year = as.integer(input$selected_year)
    prev_year = as.integer(input$selected_year)-1
    
    data2 = tally_rush_data %>%
      filter(year %in% c(prev_year, sel_year)) %>%
      filter(rush == input$rush_period) %>%
      mutate(month = factor(month(day, label = TRUE, abbr = TRUE), levels = month.abb, ordered = FALSE)) %>%
      group_by(year, month) %>%
      summarize(avg_count = mean(count), .groups = "drop") %>%
      complete(year = c(sel_year, prev_year), 
               month = factor(month.abb, levels = month.abb), 
               fill = list(avg_count = NA)) %>%
      tidyr::pivot_wider(names_from = year, values_from = avg_count,
                         names_prefix = "Avg_")
    # Name the Columns
    colnames(data2) <- c("Month",
                         paste0("Avg ",toupper(input$rush_period), " Count ", prev_year),
                         paste0("Avg ",toupper(input$rush_period), " Count ", sel_year))
    
    # Reorder the Columns to list selected year then previous
    col_order <- c("Month",
                   paste0("Avg ",toupper(input$rush_period), " Count ", sel_year),
                   paste0("Avg ",toupper(input$rush_period), " Count ", prev_year))
    data2 <- data2[, col_order]
    
    # Replace "NA" with "No Data"
    data2 <- data2 %>%
      mutate(across(starts_with("Avg Count"), ~ ifelse(is.na(.), "No Data", round(.,1))))
    
    return(data2)
    
  })
  
  # Calculate the Median for the Output Table
  month_med = reactive({
    
    data3 <- tally_rush_data %>%
      filter(year == input$selected_year) %>%
      filter(rush == input$rush_period) %>%
      mutate(month = factor(month(day, label = TRUE, abbr = TRUE), levels = month.abb, ordered = FALSE)) %>%
      group_by(month) %>%
      summarize(med_count = median(count))
    
    #Transpose Table
    wide_data <- data3 %>%
      tidyr::pivot_wider(names_from = month, values_from = med_count)
    
    data3 <- rbind(names(wide_data), as.character(unlist(wide_data))) %>%
       cbind(c(input$selected_year,"Median"), .)
       
    data3 <- as_tibble(data3, .name_repair = "minimal")
      
    return(data3)

  })
  
  
  # Render the plot based on filtered data and selected graph type
  output$rider_count_plot = renderPlot({
    
    data_to_plot = filtered_data()
    
    
    # Check if data is available to plot
    if (is.null(data_to_plot) || nrow(data_to_plot) == 0) {
      return(ggplot() + annotate("text", x = 0, y = 0, label = "No data available for selection"))
    }
    
    # Determine the x-axis variable for plotting within a year
    # Using the full date 'day' column as the x-axis
    x_var = "day"
    x_label = "Date"
    
    # Determine graph color based on selected rush hour period
    graph_color = if (input$rush_period == "am") "#2E8B57" else "#630436"
    
    # Create the plot based on selected graph type
    if (input$graph_type == "line") {
      ggplot(data_to_plot, aes_string(x = x_var, y = "count", group = 1)) + # Use aes_string with variable names
        geom_line(color = graph_color) +
        geom_point() + # Add points to the line plot
        labs(title = paste("Rider Count For Year", input$selected_year,"In", toupper(input$rush_period), "Rush Hour"),
             x = x_label,
             y = "Total Riders") +
        theme_minimal() +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %d") # Format x-axis for dates
    } else { # Bar plot
      ggplot(data_to_plot, aes_string(x = x_var, y = "count")) + # Use aes_string
        geom_col(fill = graph_color) +
        labs(title = paste("Rider Count For Year", input$selected_year,"In", toupper(input$rush_period), "Rush Hour"),
             x = x_label,
             y = "Total Riders") +
        theme_minimal() +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %d") # Format x-axis for dates
    }
    
  }) %>% bindEvent({ input$selected_year; input$rush_period; input$graph_type }) # Update plot when these inputs change
  
  # Output for the Box Plot Distribution
  output$distribution_plot = renderPlot({
    
    data_to_plot = filtered_data() 
    
    # Create total riders medians to data_to_plot for fill colors
    medians = data_to_plot %>% 
      group_by(month) %>% 
      summarise(med = median(count))
    
    # Left Join medians to data to plot
    data_to_plot = data_to_plot %>% 
      left_join(medians, by = "month")
    
    
    # Check if data is available to plot
    if (is.null(data_to_plot) || nrow(data_to_plot) == 0) {
      return(ggplot() + annotate("text", x = 0, y = 0, label = "No data available for selection"))
    }
    
    ggplot(data_to_plot, aes(x = month, y = count, fill = med)) +
      geom_boxplot(varwidth = TRUE) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = paste("Monthly Distribution of", toupper(input$rush_period), "Rush Hour Ride Counts in", input$selected_year) ,
           x = "Month",
           y = "Total Riders",
           fill = "Total Riders Median") +
      theme_minimal()
    
  }) %>% bindEvent({ input$selected_year; input$rush_period}) # Update plot when these inputs change
  
  # Output table for summary of monthly medians
  output$monthly_median <- renderTable({
    month_med()
  }, digits = 1)
  
  
  # Output table for summary of monthly averages
  output$monthly_stats <- renderTable({
    stats()
  }, digits = 1)
  
} %>% bindEvent({input$selected_year; input$rush_period}) # Updates table

# Run app
shiny::shinyApp(ui = ui, server = server, onStart = global)

