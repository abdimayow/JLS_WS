library(shiny)
library(shinydashboard)
library(fontawesome)
library(leaflet)
library(ggrepel)
library(dplyr)
library(palmerpenguins)
library(tidyverse)
library(stringr)

# Load and clean the water sources data
water_sources <- read.csv("JLS_WS.csv")

# Clean column names to ensure compatibility
names(water_sources) <- make.names(names(water_sources))
water_sources$REGION <- str_to_title(water_sources$REGION)
water_sources$REGION[water_sources$SOURCE.NAME == "Hassan Abdi karin"] <- "Lower Juba"
water_sources$SOURCE.TYPE[water_sources$SOURCE.NAME == "Hassan Abdi karin"] <- "Water Pan"

# Define UI for the application
ui <- dashboardPage(
  title = "Water Resource",
  skin = 'green',
  dashboardHeader(title = "MoEWR Jubaland"),
  dashboardSidebar(
    sidebarMenu(
      sidebarSearchForm('searchText', 'buttonSearch', 'Search'),
      menuItem("Dashboard", tabName = "dashboard", icon = icon('dashboard')),
      menuSubItem("Borehole Dashboard", tabName = "boreholes", icon = icon("water")),
      menuSubItem("Shallow Well Dashboard", tabName = "shallowwells", icon = icon("tint")),
      menuSubItem("Barkad/Pan Dashboard", tabName = "barkadsPans", icon = icon("hand-holding-water")),
      selectInput("region", "Select By Region", choices = NULL)  # choices set in server
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("boreholeCount"),
                valueBoxOutput("shallowWellCount"),
                valueBoxOutput("barkadWaterPanCount")
              ),
              fluidRow(
                tabBox(tabPanel("Bar Chart", plotOutput("sourceTypeBar")),
                       tabPanel("Pie Chart", plotOutput("sourceTypePie"))
                ),
                tabBox(tabPanel("Map", leafletOutput("wsmap")))
              )
      ),
      tabItem(tabName = "boreholes", fluidRow(tabBox(tabPanel("Bar chart", plotOutput("boreholes")),
                                                     tabPanel("Pie chart", plotOutput("boreholesPie"))
      ),
      tabBox(tabPanel("Map", leafletOutput("boreholeloc"))))),
      tabItem(tabName = "shallowwells", fluidRow(tabBox(tabPanel("Bar chart", plotOutput("shallowwells")),
                                                        tabPanel("Pie chart", plotOutput("shallowWellsPie"))
      ),
      tabBox(tabPanel("Map", leafletOutput("shallowloc"))))),
      tabItem(tabName = "barkadsPans", fluidRow(tabBox(tabPanel("Bar chart", plotOutput("barkadsPans")),
                                                       tabPanel("Pie chart", plotOutput("barkadsWaterPansPie"))
      ),
      tabBox(tabPanel("Map", leafletOutput("barpanmap")))))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  ###################################################################
  ###################################################################
  ###################################################################
  
  # Populate the selectInput choices for region dynamically
  observe({
    regions <- c("All", unique(water_sources$REGION))
    updateSelectInput(session, "region", choices = regions)
  })
  
  ########
  # Value box for Borehole count
  output$boreholeCount <- renderValueBox({
    borehole_count <- water_sources %>%
      filter(SOURCE.TYPE == "Borehole") %>%
      filter(REGION == input$region | input$region == "All") %>%
      nrow()
    
    valueBox(
      value = borehole_count,
      subtitle = "Number of Boreholes",
      icon = icon("water"),
      color = "blue"
    )
  })
  ###########
  # Value box for Shallow Well count
  output$shallowWellCount <- renderValueBox({
    shallow_well_count <- water_sources %>%
      filter(SOURCE.TYPE == "Shallow Well") %>%
      filter(REGION == input$region | input$region == "All") %>%
      nrow()
    
    valueBox(
      value = shallow_well_count,
      subtitle = "Number of Shallow Wells",
      icon = icon("tint"),
      color = "green"
    )
  })
  #############
  #Value box to show the number of Barkads and Water Pans
  output$barkadWaterPanCount <- renderValueBox({
    barkad_water_pan_count <- water_sources %>%
      filter(SOURCE.TYPE %in% c("Barkad", "Water Pan")) %>%
      filter(REGION == input$region | input$region == "All") %>%
      nrow()
    
    valueBox(
      value = barkad_water_pan_count,
      subtitle = "Number of Barkads and Water Pans",
      icon = icon("water"),
      color = "purple"
    )
  })
  
  ##########
  #Bar chart for SOURCE TYPE filtered by region
  output$sourceTypeBar <- renderPlot({
    # Filter data based on selected region
    source_type_counts <- water_sources %>%
      filter(REGION == input$region | input$region == "All") %>%
      group_by(SOURCE.TYPE) %>%
      summarise(Count = n())
    
    # Plot the bar chart using ggplot2
    ggplot(source_type_counts, aes(x = SOURCE.TYPE, y = Count, fill = SOURCE.TYPE)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Count), vjust = -0.3, size = 4) +  # Add text labels above each bar
      labs(
        title = paste("Water Sources Types in", input$region, "Region(s)"),
        x = "Source Type",
        y = "Count"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12)
      )
  })
  
  ##########
  # Plot the bar chart of boreholes in Jubaland districts using ggplot2
  output$boreholes <- renderPlot({
    # Filter data based on selected region and boreholes
    filtered_data <- water_sources %>%
      filter(SOURCE.TYPE == "Borehole") %>%
      filter(REGION == input$region | input$region == "All") %>%  # Apply region filter
      
      group_by(DISTRICT) %>%
      summarise(Count = n())
    
    # Plot the bar chart using ggplot2
    ggplot(filtered_data, aes(x = DISTRICT, y = Count, fill = DISTRICT)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Count), vjust = -0.3, size = 4) +  # Add text labels above each bar
      labs(
        title = paste("Boreholes in", input$region, "Region(s)"),
        x = "District",
        y = "Count"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",                           # Remove legend as it's redundant here
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Bold and center title
        axis.title.x = element_text(size = 14, face = "bold"),  # Bold and increase font size for x-axis label
        axis.title.y = element_text(size = 14, face = "bold"),  # Bold and increase font size for y-axis label
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Adjust x-axis text for readability
        axis.text.y = element_text(size = 12)               # Increase font size for y-axis variables
      )
  })
  
  
  ###########
  # Plot the bar chart of shallow wells in Jubaland districts using ggplot2
  output$shallowwells <- renderPlot({
    filtered_data <- water_sources %>%
      filter(SOURCE.TYPE == "Shallow Well") %>%
      filter(REGION == input$region | input$region == "All") %>%
      group_by(DISTRICT) %>%
      summarise(Count = n())
    
    ggplot(filtered_data, aes(x = DISTRICT, y = Count, fill = DISTRICT)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Count), vjust = -0.3, size = 4) +
      labs(title = paste("Shallow wells in", input$region, "Region(s)"), x = "District", y = "Count") +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12)
      )
  })
  
  ###########
  # Plot the bar chart of Barkad/Pans in Jubaland districts using ggplot2
  output$barkadsPans <- renderPlot({
    filtered_data <- water_sources %>%
      filter(SOURCE.TYPE %in% c("Barkad", "Water Pan")) %>%
      filter(REGION == input$region | input$region == "All") %>%
      group_by(DISTRICT, SOURCE.TYPE) %>%
      summarise(Count = n())
    
    ggplot(filtered_data, aes(x = DISTRICT, y = Count, fill = DISTRICT)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.3, size = 4) +
      labs(title = paste("Barkads and Water Pans in", input$region, "Region(s)"), x = "District", y = "Count") +
      theme_minimal() +
      theme(
        legend.position = "top",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12)
      )
  })
  
  ##########
  # Render the pie chart for source types, filtered by region
  output$sourceTypePie <- renderPlot({
    # Filter data based on selected region
    source_type_counts <- water_sources %>%
      filter(REGION == input$region | input$region == "All") %>%
      group_by(SOURCE.TYPE) %>%
      summarise(Count = n()) %>%
      filter(Count > 0) %>%  # Ensure there are counts for the selected region
      mutate(Percentage = Count / sum(Count) * 100)  # Calculate percentage
    
    # Check if source_type_counts is empty
    if (nrow(source_type_counts) == 0) {
      return(NULL)  # Do not render the plot if no data is available
    } else {
      # Plot the pie chart using ggplot2
      ggplot(source_type_counts, aes(x = "", y = Count, fill = SOURCE.TYPE)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +  # Convert bar chart to pie chart
        labs(
          title = paste("Water Source Types in", input$region, "Region(s)")
        ) +
        geom_text_repel(
          aes(label = paste0(round(Percentage, 1), "%")),
          position = position_stack(vjust = 0.5),
          fontface = "bold",        # Make labels bold
          box.padding = 0.3,        # Space around each label
          point.padding = 0.2,      # Space between label and connecting line
          segment.color = "grey50"  # Color of the connecting lines
        ) +
        theme_void() +  # Remove unnecessary plot elements for a cleaner pie chart
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          legend.title = element_blank(),
          legend.text = element_text(face = "bold")  # Make legend text bold
        )
    }
  })
  
  ##########
  # Pie chart of Boreholes by District, filtered by selected region
  output$boreholesPie <- renderPlot({
    # Filter data for boreholes and selected region
    borehole_counts <- water_sources %>%
      filter(SOURCE.TYPE == "Borehole") %>%
      filter(REGION == input$region | input$region == "All") %>%
      group_by(DISTRICT) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = Count / sum(Count) * 100)  # Calculate percentage
    
    # Plot the pie chart using ggplot2
    ggplot(borehole_counts, aes(x = "", y = Count, fill = DISTRICT)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +  # Convert bar chart to pie chart
      labs(
        title = paste("Boreholes in", input$region, "Region(s)")
      ) +
      geom_text_repel(
        aes(label = paste0(round(Percentage, 1), "%")),
        position = position_stack(vjust = 0.5),
        fontface = "bold",        # Make labels bold
        box.padding = 0.3,        # Space around each label
        point.padding = 0.2,      # Space between label and connecting line
        segment.color = "grey50"  # Color of the connecting lines
      ) +
      theme_void() +  # Remove unnecessary plot elements for a cleaner pie chart
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(face = "bold")  # Make legend text bold
      )
  })
  
  #######
  # Pie chart of Shallow Wells by District, filtered by selected region
  output$shallowWellsPie <- renderPlot({
    # Filter data for shallow wells and selected region
    shallow_well_counts <- water_sources %>%
      filter(SOURCE.TYPE == "Shallow Well") %>%
      filter(REGION == input$region | input$region == "All") %>%
      group_by(DISTRICT) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = Count / sum(Count) * 100)  # Calculate percentage
    
    # Plot the pie chart using ggplot2
    ggplot(shallow_well_counts, aes(x = "", y = Count, fill = DISTRICT)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +  # Convert bar chart to pie chart
      labs(
        title = paste("Shallow Wells in", input$region, "Region(s)")
      ) +
      geom_text_repel(
        aes(label = paste0(round(Percentage, 1), "%")),
        position = position_stack(vjust = 0.5),
        fontface = "bold",        # Make labels bold
        box.padding = 0.3,        # Space around each label
        point.padding = 0.2,      # Space between label and connecting line
        segment.color = "grey50"  # Color of the connecting lines
      ) +
      theme_void() +  # Remove unnecessary plot elements for a cleaner pie chart
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(face = "bold")  # Make legend text bold
      )
  })
  
  ########
  # Pie chart of Barkads and Water Pans by District, filtered by selected region
  output$barkadsWaterPansPie <- renderPlot({
    # Filter data for Barkads and Water Pans and selected region
    barkads_water_pans_counts <- water_sources %>%
      filter(SOURCE.TYPE %in% c("Barkad", "Water Pan")) %>%
      filter(REGION == input$region | input$region == "All") %>%
      group_by(DISTRICT) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = Count / sum(Count) * 100)  # Calculate percentage
    
    # Plot the pie chart using ggplot2
    ggplot(barkads_water_pans_counts, aes(x = "", y = Count, fill = DISTRICT)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +  # Convert bar chart to pie chart
      labs(
        title = paste("Barkads and Water Pans in", input$region, "Region(s)")
      ) +
      geom_text_repel(
        aes(label = paste0(round(Percentage, 1), "%")),
        position = position_stack(vjust = 0.5),
        fontface = "bold",        # Make labels bold
        box.padding = 0.3,        # Space around each label
        point.padding = 0.2,      # Space between label and connecting line
        segment.color = "grey50"  # Color of the connecting lines
      ) +  # Add percentage labels
      theme_void() +  # Remove unnecessary plot elements for a cleaner pie chart
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(face = "bold")  # Make legend text bold
      )
  })
  
  
  #############
  # Render the leaflet map
  output$wsmap <- renderLeaflet({
    # Filter data based on selected region
    filtered_data <- water_sources %>%
      filter(REGION == input$region | input$region == "All")
    
    # Check if filtered_data is empty
    if (nrow(filtered_data) == 0) {
      leaflet() %>% addTiles()  # Return an empty map if no data is available
    } else {
      # Define icon and color for each source type using mutate and case_when
      filtered_data <- filtered_data %>%
        mutate(
          icon = case_when(
            SOURCE.TYPE == "Borehole" ~ "tint",
            SOURCE.TYPE == "Shallow Well" ~ "circle",
            SOURCE.TYPE == "Water Pan" ~ "cloud",
            SOURCE.TYPE == "Barkad" ~ "drop",
            TRUE ~ "star"  # Default icon for any other source type
          ),
          markerColor = case_when(
            SOURCE.TYPE == "Borehole" ~ "blue",
            SOURCE.TYPE == "Shallow Well" ~ "green",
            SOURCE.TYPE == "Water Pan" ~ "purple",
            SOURCE.TYPE == "Barkad" ~ "orange",
            TRUE ~ "red"  # Default color for any other source type
          )
        )
      
      # Create icons using awesomeIcons
      icons <- awesomeIcons(
        icon = filtered_data$icon,
        iconColor = "white",
        markerColor = filtered_data$markerColor
      )
      
      # Set up the map with Awesome Markers
      leaflet(data = filtered_data) %>%
        addTiles() %>%
        addAwesomeMarkers(
          lng = ~Longitude, lat = ~Latitude,
          icon = icons,
          popup = ~paste(
            "<strong>Source Type:</strong>", SOURCE.TYPE, "<br>",
            "<strong>Name:</strong>", SOURCE.NAME, "<br>",
            "<strong>District:</strong>", DISTRICT, "<br>",
            "<strong>Settlement:</strong>", SETTLEMENT
          )
        ) %>%
        addLegend(
          position = "bottomright",
          colors = c("blue", "green", "purple", "orange", "red"),  # Colors used for each source type
          labels = c("Borehole", "Shallow Well", "Water Pan", "Barkad", "Other"),  # Corresponding source types
          title = "Source Type",
          opacity = 1
        )
    }
  })
  
  
  ##########
  # Render the leaflet map for boreholes only, colored by region
  output$boreholeloc <- renderLeaflet({
    # Filter data for boreholes only
    filtered_data <- water_sources %>%
      filter(SOURCE.TYPE == "Borehole") %>%
      filter(REGION == input$region | input$region == "All") %>%
      na.omit()  # Remove any rows with missing values
    
    # Check if filtered_data is empty
    if (nrow(filtered_data) == 0) {
      leaflet() %>% addTiles()  # Return an empty map if no data is available
    } else {
      # Mutate filtered_data to add markerColor column based on region
      filtered_data <- filtered_data %>%
        mutate(
          markerColor = case_when(
            REGION == "Lower Juba" ~ "green",
            REGION == "Gedo" ~ "purple",
            TRUE ~ "red"  # Default color for any other region
          )
        )
      
      # Set up the map with Awesome Markers using the markerColor column
      leaflet(data = filtered_data) %>%
        addTiles() %>%
        addAwesomeMarkers(
          lng = ~Longitude, lat = ~Latitude,
          icon = ~awesomeIcons(
            icon = "tint",
            iconColor = "white",
            markerColor = markerColor  # Use the markerColor column for color
          ),
          popup = ~paste(
            "<strong>Source Type:</strong>", SOURCE.TYPE, "<br>",
            "<strong>Name:</strong>", SOURCE.NAME, "<br>",
            "<strong>Region:</strong>", REGION, "<br>",
            "<strong>District:</strong>", DISTRICT, "<br>",
            "<strong>Settlement:</strong>", SETTLEMENT
          )
        ) %>%
        addLegend(
          "bottomright", 
          colors = c("green", "purple", "red"), 
          labels = c("Lower Juba", "Gedo", "Other"),
          title = "Region",
          opacity = 1
        )
    }
  })
  
  
  ###########
  # Render the leaflet map for shallow wells only with specific marker colors by region
  output$shallowloc <- renderLeaflet({
    # Filter data for shallow wells only
    filtered_data <- water_sources %>%
      filter(SOURCE.TYPE == "Shallow Well") %>%
      filter(REGION == input$region | input$region == "All") %>%
      na.omit()  # Remove any rows with missing values
    
    # Check if filtered_data is empty
    if (nrow(filtered_data) == 0) {
      leaflet() %>% addTiles()  # Return an empty map if no data is available
    } else {
      # Mutate filtered_data to add markerColor column based on region
      filtered_data <- filtered_data %>%
        mutate(
          markerColor = case_when(
            REGION == "Lower Juba" ~ "green",
            REGION == "Gedo" ~ "purple",
            TRUE ~ "red"  # Default color for any other region
          )
        )
      
      # Set up the map with Awesome Markers using the circle icon and region-based color
      leaflet(data = filtered_data) %>%
        addTiles() %>%
        addAwesomeMarkers(
          lng = ~Longitude, lat = ~Latitude,
          icon = ~awesomeIcons(
            icon = "tint",          # Use a whand-holding-water icon for shallow wells
            iconColor = "white",
            markerColor = markerColor # Use the markerColor column for color
          ),
          popup = ~paste(
            "<strong>Source Type:</strong>", SOURCE.TYPE, "<br>",
            "<strong>Name:</strong>", SOURCE.NAME, "<br>",
            "<strong>Region:</strong>", REGION, "<br>",
            "<strong>District:</strong>", DISTRICT, "<br>",
            "<strong>Settlement:</strong>", SETTLEMENT
          )
        ) %>%
        addLegend(
          "bottomright", 
          colors = c("green", "purple", "red"), 
          labels = c("Lower Juba", "Gedo", "Other"),
          title = "Region",
          opacity = 1
        )
    }
  })
  
  
  ##########
  # Render the leaflet map for Barkad and Water Pan only with specific marker colors by region
  output$barpanmap <- renderLeaflet({
    # Filter data for Barkad and Water Pan only
    filtered_data <- water_sources %>%
      filter(SOURCE.TYPE %in% c("Barkad", "Water Pan")) %>%
      filter(REGION == input$region | input$region == "All") %>%
      na.omit()  # Remove any rows with missing values
    
    # Check if filtered_data is empty
    if (nrow(filtered_data) == 0) {
      leaflet() %>% addTiles()  # Return an empty map if no data is available
    } else {
      # Mutate filtered_data to add markerColor column based on region
      filtered_data <- filtered_data %>%
        mutate(
          markerColor = case_when(
            REGION == "Lower Juba" ~ "green",
            REGION == "Gedo" ~ "purple",
            TRUE ~ "red"  # Default color for any other region
          )
        )
      
      # Set up the map with Awesome Markers using the tint icon and region-based color
      leaflet(data = filtered_data) %>%
        addTiles() %>%
        addAwesomeMarkers(
          lng = ~Longitude, lat = ~Latitude,
          icon = ~awesomeIcons(
            icon = "tint",              # Using tint icon for Barkad and Water Pan
            iconColor = "white",
            markerColor = markerColor   # Use the markerColor column for color
          ),
          popup = ~paste(
            "<strong>Source Type:</strong>", SOURCE.TYPE, "<br>",
            "<strong>Name:</strong>", SOURCE.NAME, "<br>",
            "<strong>Region:</strong>", REGION, "<br>",
            "<strong>District:</strong>", DISTRICT, "<br>",
            "<strong>Settlement:</strong>", SETTLEMENT
          )
        ) %>%
        addLegend(
          "bottomright", 
          colors = c("green", "purple", "red"), 
          labels = c("Lower Juba", "Gedo", "Other"),
          title = "Region",
          opacity = 1
        )
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
