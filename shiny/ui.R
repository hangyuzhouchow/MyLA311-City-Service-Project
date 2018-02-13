library(shinydashboard)
library(leaflet)
library(raster)
library(RColorBrewer)
library(dplyr)
library(htmltools)
library(rgdal)
library(lubridate)
library(shinyjs)
header = dashboardHeader(title="MyLA 311 Requests")
type <- c(
  "Graffiti Removal",
  "Bulky Items",
  "Illegal Dumping Pickup",
  "Metal/Household Appliances",
  "Electronic Waste",
  "Single Streetlight Issue",  
  "Homeless Encampment",
  "Multiple Streetlight Issue",
  "Dead Animal Removal",
  "Report Water Waste",
  "Feedback",
  "Other"
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Distribution of Requests", tabName = "distribution"),
    menuItem("Compare to other CD", tabName = "comparison"),
    menuItem("Trend of requests of CDs",tabName = "history")
  )
)

body <- dashboardBody(
  useShinyjs(),
  tabItems(
    # -----------Tab 1 distribution---------
    tabItem("distribution",
        fluidRow(
          # output map
            box(width = 8,
              status = "info", solidHeader = TRUE,
              title = "Distribution",
              leafletOutput("map")),
            
            box(width = 4,
                # input date range
              dateRangeInput("dates", label = "Date Range",
                             start="2016-08-22",
                             end="2016-09-21"),
              # input service type
              selectInput("serviceType", "Service Type", choices = type, selected = type[1])
              ),
              #actionButton("submit",label = "Update Data"),
              #selectInput("cdSelect", "Council District", choices = c("All",c(1:15)), selected = "1"),
            
            # output performance table
            box(width=4, status = "info",
                  htmlOutput("serviceText"),
                  dataTableOutput("requestPerform")))),
    # -----------Tab 2 comparison---------                
    tabItem("comparison",
            fluidRow(
              # output map
              box(width = 8,
                  selectizeInput("compare",label = "Choose districts to compare",
                                 choices = c(1:15),options=list(maxItems = 2)))
              )
            ),
    # -----------Tab 3 historical data---------  
    tabItem("history",
            fluidRow(
              box(width = 8,
                  plotOutput("trend1"),
                  plotOutput("trend2")),
              box(width = 4,
                  dateRangeInput("histDates", label = "Date Range",
                                 start="2016-08-22",
                                 end="2016-09-21"),
                  selectInput("oneDistrict",label = "Choose One District",
                              selected = 1,choices = c(1:15)),
                  checkboxGroupInput("serviceTypeAll", "Service Type",
                                                         choices = type,
                                                        selected = type[c(1:3)]
                                     )
                  )
            ))
           
           
  ))

dashboardPage(
  header,
  sidebar,
  body,
  skin="blue"
)

