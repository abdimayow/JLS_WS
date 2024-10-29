library(shiny)
library(shinydashboard)
library(fontawesome)
library(leaflet)
library(ggrepel)

shinyUI(
  dashboardPage(title = "Water Resource", skin = 'green',
                dashboardHeader(title = "MoEWR Jubaland"
                ),
                
                
                dashboardSidebar(
                  
                  sidebarMenu(
                    sidebarSearchForm('searchText','buttonSearch','Search'),
                    menuItem("Dashboard", tabName = "dashboard",
                             icon = icon('dashboard')),
                    menuSubItem("Borehole Dashboard", tabName = "boreholes",
                                icon = icon("water")),
                    menuSubItem("Shallow Well Dashboard", tabName = "shallowwells",
                                icon = icon("tint")),
                    menuSubItem("Barkad/Pan Dashboard", tabName = "barkadsPans",
                                icon = icon("hand-holding-water")),
                    selectInput("region", "Select By Region", choices = NULL)  # choices are set in server.R

                  )
                ),
                dashboardBody(
                  tabItems(
                    tabItem(
                      tabName = "dashboard",
                     
                      fluidRow(
                        # Value box is an info box where value comes first then title
                        valueBoxOutput("boreholeCount"),  # Value box for number of boreholes
                        valueBoxOutput("shallowWellCount"),
                        valueBoxOutput("barkadWaterPanCount")
                        
                      ),
                      fluidRow(
                        
                        tabBox(
                          
                          tabPanel(title = 'Bar Chart',status = 'success', solidHeader = T, background = 'fuchsia',
                                   plotOutput("sourceTypeBar")
                          ),
                          tabPanel(title = 'Pie Chart',status = 'success', solidHeader = T, background = 'fuchsia',
                                   plotOutput("sourceTypePie")
                          )
                          
                         ),
                        tabBox(
                          
                          tabPanel(title = 'Map',status = 'success', solidHeader = T, background = 'fuchsia',
                                   leafletOutput("wsmap")
                          )
                        )
                      )
                     
                        
                      ),
                    tabItem(
                      tabName = "boreholes",
                      fluidRow(
                        tabBox(
                          tabPanel(title = 'Bar chart',status = 'success', solidHeader = T, background = 'fuchsia',
                                   plotOutput("boreholes")
                          ),
                          tabPanel(title = 'Pie chart',status = 'success', solidHeader = T, background = 'fuchsia',
                                   plotOutput("boreholesPie")
                          )
                        ),
                        tabBox(
                          
                          tabPanel(title = 'Map',status = 'success', solidHeader = T, background = 'fuchsia',
                                   leafletOutput("boreholeloc")
                          )
                        )
                      )
                    ),
                    tabItem(
                      tabName = "shallowwells",
                      fluidRow(
                        tabBox(
                          tabPanel(title = 'Bar chart',status = 'success', solidHeader = T, background = 'fuchsia',
                                   plotOutput("shallowwells")
                          ),
                          tabPanel(title = 'Pie chart',status = 'success', solidHeader = T, background = 'fuchsia',
                                   plotOutput("shallowWellsPie")
                          )
                        ),
                        tabBox(
                          
                          tabPanel(title = 'Map',status = 'success', solidHeader = T, background = 'fuchsia',
                                   leafletOutput("shallowloc")
                          )
                        )
                      )
                    ),
                    tabItem(
                      tabName = "barkadsPans",
                      fluidRow(
                        tabBox(
                          tabPanel(title = 'Bar chart',status = 'success', solidHeader = T, background = 'fuchsia',
                                   plotOutput("barkadsPans")
                          ),
                          tabPanel(title = 'Pie chart',status = 'success', solidHeader = T, background = 'fuchsia',
                                   plotOutput("barkadsWaterPansPie")
                          )
                        ),
                        tabBox(
                          
                          
                          tabPanel(title = 'Map',status = 'success', solidHeader = T, background = 'fuchsia',
                                   leafletOutput("barpanmap")
                          )
                        )
                      )
                    )
                    )
                    
                    
                  )
                  
                )
  )
