## Description of document ----
# Author: Luuk Nijs
# August 2023
# Capita Selecta Assignment Operations Research in Healthcare
# Supervisor: Sebastian Rachuba

## Packages ----
library(shiny)
library(shinythemes)
library(readxl)
library(shinycssloaders)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(chron)
library(shinydashboard)
library(shinyBS)
library(ggpubr)
library(plotly)
library(shinyWidgets)
library(shinyjs)
library(rsconnect)


## UI ----
ui <- tagList(
  shinyjs::useShinyjs(),
  # Enable shinyjs
  navbarPage(
    theme = shinytheme("united"),
    "ED and ward occupation correlation dashboard",
    ## Intro page where you can upload and inspect your data ----
    tabPanel("Input",
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Inputs"),
                 conditionalPanel(
                   condition = "input.help == 1",
                   h5(
                     "Below you can upload your two data files for the ED and Ward.
                   Make sure you pay attention to the upload specifications and that all datatypes are used correctly.
                      It should contain 3 columns with the following format:"
                   )
                 ),
                 conditionalPanel(condition = "input.help == 1",
                                  h5("1. Arrival date-time, format: Date-Time")),
                 conditionalPanel(condition = "input.help == 1",
                                  h5(
                                    "2. Departure date-time, format: Date-Time"
                                  )),
                 conditionalPanel(condition = "input.help == 1",
                                  h5("3. Urgency, format: Text")),
                 br(),
                 fileInput("EDdata", "Upload your ED data here:"),
                 fileInput("Warddata", "Upload your Ward data here:"),
                 conditionalPanel(
                   condition = "input.help == 1",
                   h5(
                     "Here you can select what you want to do with missing values in the Arrival-Date-Time and the Departure-Date-Time. NA values in the Arrival-Date-Time column will always be deleted"
                   )
                 ),
                 radioButtons(
                   "missingvalues",
                   label = h4("What to do with missing values?"),
                   choices = list(
                     "Remove observation" = 1,
                     "Set departure time to 23:59 on day of arrival" = 2
                   ),
                   selected = 1
                 ),
                 radioButtons(
                   "help",
                   label = h4("Do you want extra helptext on every object?"),
                   choices = list("Show" = 1, "Do not show" = 2),
                   selected = 1
                 ),
                 width = 3
               ),
               mainPanel(fluidRow(
                 h4("Below you can inspect your uploaded data."),
                 column(
                   5,
                   h2("ED data"),
                   conditionalPanel(
                     condition = "input.help == 1",
                     h5(
                       "Inspect the uploaded ED data here. If you see anything strange, go back to your excel file and adapt to the specifications."
                     )
                   ),
                   conditionalPanel(condition = "output.EDtable == null",
                                    h3(HTML(
                                      "<em>No data uploaded yet</em>"
                                    ))),
                   tableOutput("EDtable") %>% withSpinner(color =
                                                            "#0dc5c1"),
                 ),
                 column(
                   5,
                   h2("Ward data"),
                   conditionalPanel(
                     condition = "input.help == 1",
                     h5(
                       "Inspect the uploaded Ward data here. If you see anything strange, go back to your excel file and adapt to the specifications."
                     )
                   ),
                   conditionalPanel(condition = "output.Wardtable == null",
                                    h3(HTML(
                                      "<em>No data uploaded yet</em>"
                                    ))),
                   tableOutput("Wardtable") %>% withSpinner(color =
                                                              "#0dc5c1"),
                 )
               ))
             )),
    
    ## ED Occupation tab where you can analyse your ED data ----
    tabPanel("ED Occupation",
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Settings"),
                 conditionalPanel(
                   condition = "input.help == 1",
                   h5(
                     "Below you can select the date range on which you want to inspect your data. The analysis changes automatically."
                   )
                 ),
                 dateRangeInput("EDdates", "Date range", start = "2019-01-01", end = "2019-12-31"),
                 br(),
                 conditionalPanel(
                   condition = "input.help == 1",
                   h5(
                     "Below you can select the capacity of your ED. On the analysis part calculations will be shown based on this value. Also the red dotted line in the occupancy graph represents this capacity."
                   )
                 ),
                 numericInput("EDCapacity", "ED Capacity", value = 70),
                 br(),
                 conditionalPanel(
                   condition = "input.help == 1",
                   h5(
                     "Below you can select urency filters based on the data you uploaded. The analysis will update automatically when you select new filters."
                   )
                 ),
                 selectizeInput(
                   'UrgencyInput',
                   'Urgency filter:',
                   choices = NULL,
                   options = list(create = TRUE),
                   multiple = TRUE
                 ),
                 
                 width = 3
               ),
               mainPanel(
                 fluidRow(
                   h3("Maximum presence per day"),
                   conditionalPanel(
                     condition = "input.help == 1",
                     h5(
                       "This graph shows the maximum occupancy per day. On a 5 minute interval the capacity is calculated.
                     Per day we take the maximum of these occupancy levels and they are displayed in the graph.
                     The red line shows your determined capacity. The peaks above it could represent congestion or overoccupancy of your ED facility."
                     )
                   ),
                   plotOutput("EDOccupationPlot") %>% withSpinner(color =
                                                                    "#0dc5c1")
                 ),
                 fluidRow(
                   h3("Capacity Indicators"),
                   conditionalPanel(
                     condition = "input.help == 1",
                     h5(
                       "In this section capacity indicators are shown.
                     The number of days that your selected capacity is exceeded is presented in the first KPI.
                     In the second KPI the congestion rate is presented.
                     This represents the number of days that your capacity is exceeded compared to the total number of days in your selected period."
                     )
                   ),
                   box(
                     width =  12,
                     valueBoxOutput("CapacityExcDays", width = 4),
                     infoBoxOutput("CongestionRate", width = 4)
                   )
                 ),
                 plotlyOutput("EDOccupationBoxPlot"),
                 br(),
                 br(),
                 br(),
                 fluidRow(
                   h3("Key Performance Indicators"),
                   conditionalPanel(
                     condition = "input.help == 1",
                     h5(
                       "Below three KPI's are shown.The first is the busiest day of your selected period.
                     The second shows how many patients were present on that day.
                     And the third shows the total number of patients that visited the ED in the selected period."
                     )
                   ),
                   box(
                     width =  12,
                     valueBoxOutput("EDBusyDay", width = 4),
                     infoBoxOutput("EDMostPatientsPresent", width = 4),
                     infoBoxOutput("EDTotalNumPatients", width = 4)
                   )
                 ),
                 br(),
                 conditionalPanel(
                   condition = "input.help == 1",
                   h5(
                     "Below a boxplot of the maximum occupation is shown for the selected period.
                     The plot is interactive and shows how the limits are calculated.
                     Also mean and outlier values are shown."
                   )
                 ),
                 br(),
                 br(),
                 br(),
                 fluidRow(
                   h3("Arrival patterns"),
                   conditionalPanel(
                     condition = "input.help == 1",
                     h5(
                       "In this section arrival patterns are shown.
                     The first one shows the number of arrivals per day of the week. The second graph shows the number of arrivals per hour of the day"
                     )
                   ),
                   plotOutput("EDArrivalPatternWeekDays") %>% withSpinner(color =
                                                                            "#0dc5c1"),
                   plotOutput("EDArrivalPatternHour") %>% withSpinner(color =
                                                                        "#0dc5c1")
                 )
               )
             )),
    
    ## Ward Occupation tab where you can analyse your ED data ----
    tabPanel("Ward Occupation",
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Settings"),
                 conditionalPanel(
                   condition = "input.help == 1",
                   h5(
                     "Below you can select the date range on which you want to inspect your data. The analysis changes automatically."
                   )
                 ),
                 dateRangeInput("Warddates", "Date range", start = "2019-01-01", end = "2019-12-31"),
                 width = 3
               ),
               mainPanel(
                 fluidRow(
                   h3("Maximum presence per day"),
                   conditionalPanel(
                     condition = "input.help == 1",
                     h5(
                       "This graph shows the maximum occupancy per day. On a 5 minute interval the capacity is calculated.
                     Per day we take the maximum of these occupancy levels and they are displayed in the graph.
                     The red line shows your determined capacity. The peaks above it could represent congestion or overoccupancy of your ED facility."
                     )
                   ),
                   plotOutput("WardOccupationPlot") %>% withSpinner(color =
                                                                      "#0dc5c1")
                 ),
                 br(),
                 br(),
                 br(),
                 fluidRow(
                   h3("Key Performance Indicators"),
                   conditionalPanel(
                     condition = "input.help == 1",
                     h5(
                       "Below three KPI's are shown.The first is the busiest day of your selected period.
                     The second shows how many patients were present on that day.
                     And the third shows the total number of patients that visited the Ward in the selected period."
                     )
                   ),
                   box(
                     width =  12,
                     infoBoxOutput("WardBusyDay", width = 4),
                     infoBoxOutput("WardMostPatientsPresent", width = 4),
                     infoBoxOutput("WardTotalNumPatients", width = 4)
                   )
                 ),
                 br(),
                 conditionalPanel(
                   condition = "input.help == 1",
                   h5(
                     "Below a boxplot of the maximum occupation is shown for the selected period.
                     The plot is interactive and shows how the limits are calculated.
                     Also mean and outlier values are shown."
                   )
                 ),
                 plotlyOutput("WardOccupationBoxPlot"),
                 br(),
                 br(),
                 br(),
                 fluidRow(
                   h3("Arrival patterns"),
                   conditionalPanel(
                     condition = "input.help == 1",
                     h5(
                       "In this section arrival patterns are shown.
                     The first one shows the number of arrivals per day of the week. The second graph shows the number of arrivals per hour of the day"
                     )
                   ),
                   plotOutput("WardArrivalPatternWeekDays") %>% withSpinner(color =
                                                                              "#0dc5c1"),
                   plotOutput("WardArrivalPatternHour") %>% withSpinner(color =
                                                                          "#0dc5c1")
                 )
               )
             )),
    
    ## ED and ward max occupation correlation analysis tab
    tabPanel(
      "Correlation Analysis",
      sidebarLayout(
        sidebarPanel(
          titlePanel("Settings"),
          conditionalPanel(
            condition = "input.help == 1",
            h5(
              "Below you can select the date range on which you want to inspect your data. The analysis changes automatically."
            )
          ),
          dateRangeInput(
            "Correlationdates",
            "Date range",
            start = "2019-01-01",
            end = "2019-12-31"
          ),
          conditionalPanel(
            condition = "input.help == 1",
            h5(
              "Below you can select which type of correlation method you would like to chose."
            )
          ),
          radioButtons(
            "correlationmethod",
            label = h4("What correlation method to use?"),
            choices = list(
              "Pearson" = "pearson",
              "Kendall" = "kendall",
              "Spearman" = "spearman"
            ),
            selected = "pearson"
          ),
          width = 3
        ),
        mainPanel(
          fluidRow(
            h3("Correlation analysis"),
            conditionalPanel(
              condition = "input.help == 1",
              h5(
                "Below the correlation graph of the maximum ED occupation per day and the maximum ward occupation per day is shown.
                        A straight diagonal line from the left lower corner to the right upper corner suggest positive correlation.
                        A straight line from the left upper corner to the right lower corner suggests negative correlation.
                        A straight horizontal line suggest no correlation.
                        To evaluate if ther is indeed correlation, one should evaluate the correlationvalue, which we will in the section below."
              )
            ),
            plotOutput("CorrelationPlot") %>% withSpinner(color =
                                                            "#0dc5c1")
          ),
          fluidRow(
            h3("Correlation Evaluation"),
            br(),
            textOutput("correlationvalue"),
            br(),
            
            conditionalPanel(
              condition = "input.help == 1",
              tags$style(
                HTML(
                  "
                                h5, p {
                                  color: #888; /* Change the color to a lighter shade, adjust as needed */
                                }
                              "
                )
              ),
              h5(
                "When evaluating correlations, you have the option to choose from various correlation methods. Each method provides insights into different aspects of the data relationships. Here's a breakdown of these methods:"
              ),
              HTML("<h5><strong>Pearson Correlation:</strong></h5>"),
              p(
                "   - Measures the strength and direction of linear relationships between two variables."
              ),
              p("   - Assumes that the data follows a normal distribution."),
              HTML("<h5><strong>Kendall Correlation:</strong></h5>"),
              p(
                "   - Measures the strength and direction of ordinal associations between two variables."
              ),
              p("   - Suitable for assessing monotonic (non-linear) relationships."),
              p("   - Does not make assumptions about the data distribution."),
              p("   - Uses rankings of data points rather than actual values."),
              HTML("<h5><strong>Spearman Correlation:</strong></h5>"),
              p(
                "   - Calculates a non-parametric rank correlation for monotonic relationships."
              ),
              p("   - Similar to Kendall but based on rank order."),
              p("   - Also does not assume a specific data distribution."),
              p("   - Robust to outliers."),
            )
            ,
          )
        )
      )
    )
  )
)


## Server part of the application ----
## Here all the calculations take place
server <- function(input, output, session) {
  ## Input calculations ----
  
  # This part translates the input of the excel file into a usable data frame
  EDInput <- reactive({
    inFile <- input$EDdata
    if (is.null(inFile))
      return(NULL)
    df1 <- readxl::read_excel(inFile$datapath)
    
    print(input$help)
    
    # Rename columns
    colnames(df1)[1] = "ArrivalDateTime"
    colnames(df1)[2] = "DepartureDateTime"
    colnames(df1)[3] = "Urgency"
    
    # Assuming the date-time columns are the first and second columns
    datetime_columns <- c(1, 2)
    
    # Define the time to add
    time_to_add <- dhours(23) + dminutes(59)
    
    if (input$missingvalues == 1) {
      df1 <- df1[complete.cases(df1[, datetime_columns]),]
    } else {
      # Check for NA values in the "DepartureDateTime" column of df1
      na_indices <- is.na(df1$DepartureDateTime)
      
      if (any(na_indices)) {
        # Replace NA values with the original date from ArrivalDateTime and time 23:59
        new_departure <-
          as.Date(df1$ArrivalDateTime[na_indices]) + time_to_add
        
        # Check for finite values
        new_departure[!is.finite(new_departure)] <- NA
        df1$DepartureDateTime[na_indices] <- new_departure
        
        #remove na values
        df1 <- df1[complete.cases(df1[, datetime_columns]),]
      }
    }
    
    for (col_index in datetime_columns) {
      df1[[col_index]] <-
        as.POSIXct(df1[[col_index]], format = "%Y-%m-%d %H:%M:%S")
      df1[[col_index]] <-
        format(df1[[col_index]], "%Y-%m-%d %H:%M:%S")
    }
    
    # Format the "Urgency" column as character
    df1$Urgency <- as.character(df1$Urgency)
    
    selected_urgencies <- input$UrgencyInput
    
    if (!is.null(selected_urgencies) &&
        length(selected_urgencies) > 0) {
      df1 <- df1 %>%
        filter(Urgency %in% selected_urgencies)
    }
    
    df1
  })
  
  # Extract the ugencies from the excel files to use as a filter
  EDUrgencies <- reactive({
    inFile <- input$EDdata
    if (is.null(inFile))
      return(NULL)
    df1 <- readxl::read_excel(inFile$datapath)
    colnames(df1)[3] = "Urgency"
    
    # Format the "Urgency" column as character
    df1$Urgency <- as.character(df1$Urgency)
    
    # Extract unique urgency options
    unique_urgencies <- unique(df1$Urgency)
    
    return(unique_urgencies)
  })
  
  # Observes the changes in urgency filters; makes the page change automatically
  observe({
    updateSelectizeInput(session, 'UrgencyInput', choices = EDUrgencies())
  })
  
  # This part translates the input of the excel file into a usable data frame
  WardInput <- reactive ({
    inFile <- input$Warddata
    if (is.null(inFile))
      return(NULL)
    df2 <- readxl::read_excel(inFile$datapath)
    
    # Rename columns
    colnames(df2)[1] = "ArrivalDateTime"
    colnames(df2)[2] = "DepartureDateTime"
    
    # Assuming the date-time columns are the first and second columns
    datetime_columns <- c(1, 2)
    
    for (col_index in datetime_columns) {
      df2[[col_index]] <-
        as.POSIXct(df2[[col_index]], format = "%Y-%m-%d %H:%M:%S")
      df2[[col_index]] <-
        format(df2[[col_index]], "%Y-%m-%d %H:%M:%S")
    }
    
    if (input$missingvalues == 1) {
      # Remove rows with NA values in column one or two
      df2 <- df2[complete.cases(df2[, datetime_columns]),]
    }
    df2
  })
  
  # Calculate the maximum occupation per day based on the given uploaded data
  EDOccupation <- reactive({
    df_data <- EDInput()
    
    presence_data <- df_data %>%
      mutate(Date = as.Date(ArrivalDateTime)) %>%
      group_by(Date) %>%
      arrange(ArrivalDateTime) %>%
      mutate(
        IntervalStart = ArrivalDateTime,
        IntervalEnd = lead(
          ArrivalDateTime,
          order_by = ArrivalDateTime,
          default = max(DepartureDateTime)
        )
      ) %>%
      summarise(Presence = sum(
        IntervalStart >= ArrivalDateTime &
          IntervalEnd <= DepartureDateTime
      ))
    
    max_presence_per_day <- presence_data %>%
      group_by(Date) %>%
      summarise(Max_Presence = max(Presence))
    
    return(
      data.frame(
        Date = max_presence_per_day$Date,
        Presence = presence_data$Presence,
        Max_Presence = max_presence_per_day$Max_Presence
      )
    )
  })
  
  # Calculate the maximum occupation per day based on the given uploaded data
  WardOccupation <- reactive({
    df_data <- WardInput()
    
    presence_data <- df_data %>%
      mutate(Date = as.Date(ArrivalDateTime)) %>%
      group_by(Date) %>%
      arrange(ArrivalDateTime) %>%
      mutate(
        IntervalStart = ArrivalDateTime,
        IntervalEnd = lead(
          ArrivalDateTime,
          order_by = ArrivalDateTime,
          default = max(DepartureDateTime)
        )
      ) %>%
      summarise(Presence = sum(
        IntervalStart >= ArrivalDateTime &
          IntervalEnd <= DepartureDateTime
      ))
    
    max_presence_per_day <- presence_data %>%
      group_by(Date) %>%
      summarise(Max_Presence = max(Presence))
    
    return(
      data.frame(
        Date = max_presence_per_day$Date,
        Presence = presence_data$Presence,
        Max_Presence = max_presence_per_day$Max_Presence
      )
    )
  })
  
  
  ## Outputs for input tab ----
  
  # Output ED table
  output$EDtable <- renderTable({
    EDInput()
  })
  
  # Output ward table
  output$Wardtable <- renderTable({
    WardInput()
  })
  
  ## Outputs for ED Occupation tab ----
  # Plot the maximum occupation graph
  output$EDOccupationPlot <- renderPlot({
    er_occupation_data <- EDOccupation()
    
    p <- ggplot(er_occupation_data, aes(x = Date, y = Presence)) +
      geom_line(color = "blue", size = 1) +
      labs(x = "Date", y = "Presence", title = "Presence Over Time") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        panel.border = element_rect(
          color = "black",
          fill = NA,
          size = 1
        )
      ) +
      coord_cartesian(xlim = as.Date(c(input$EDdates[1], input$EDdates[2]))) +
      scale_y_continuous(
        limits = c(0, max(er_occupation_data$Presence), expand = c(0, 0)),
        breaks = seq(0, max(er_occupation_data$Presence), by = 10),
        # Custom y-axis breaks
        labels = scales::comma_format(scale = 1),
        # Format y-axis labels
        minor_breaks = seq(0, max(er_occupation_data$Presence), by = 2)  # Add minor gridlines
      ) +
      scale_x_date(date_labels = "%b %d, %Y", date_breaks = "1 month") +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 10
        ),
        axis.text.y = element_text(size = 10)
      )
    
    threshold_value <-
      input$EDCapacity  # Set your threshold value here
    p <-
      p + geom_hline(yintercept = threshold_value,
                     linetype = "dashed",
                     color = "red")
    p
  })
  
  # Output infobox of the busiest day
  output$EDBusyDay <- renderInfoBox({
    er_occupation_data <- EDOccupation()
    
    # Filter the data based on date input range
    filtered_data <-
      er_occupation_data[er_occupation_data$Date >= as.Date(input$EDdates[1]) &
                           er_occupation_data$Date <= as.Date(input$EDdates[2]),]
    
    # Find the busiest day within the filtered data
    busiest_day <-
      filtered_data[which.max(filtered_data$Max_Presence), "Date"]
    
    infoBox(
      "Busiest day = ",
      format(busiest_day, "%d-%m-%Y"),
      icon = icon("calendar"),
      color = "purple",
      fill = TRUE
    )
  })
  
  # Output infobox of maximum number of patients present
  output$EDMostPatientsPresent <- renderInfoBox({
    er_occupation_data <- EDOccupation()
    
    # Filter the data based on input$EDdates[]
    filtered_data <-
      er_occupation_data[er_occupation_data$Date >= as.Date(input$EDdates[1]) &
                           er_occupation_data$Date <= as.Date(input$EDdates[2]),]
    
    # Find the busiest day and its corresponding maximum presence within the filtered data
    busiest_day_row <-
      filtered_data[which.max(filtered_data$Max_Presence),]
    busiest_day <- busiest_day_row$Date
    max_patients <- busiest_day_row$Max_Presence
    
    infoBox(
      "Busiest day max patients = ",
      max_patients,
      icon = icon("hospital"),
      color = "red",
      fill = TRUE
    )
  })
  
  # Output infobox of total number of patients evaluated
  output$EDTotalNumPatients <- renderInfoBox({
    er_arrival_data <- EDInput()
    
    # Filter the data based on input$EDdates[]
    filtered_data <-
      er_arrival_data[er_arrival_data$ArrivalDateTime >= as.Date(input$EDdates[1]) &
                        er_arrival_data$ArrivalDateTime <= as.Date(input$EDdates[2]),]
    
    filtered_data_count <- nrow(filtered_data)
    
    infoBox(
      "Total number of patients = ",
      filtered_data_count,
      icon = icon("signal", lib = "glyphicon"),
      color = "blue",
      fill = TRUE
    )
  })
  
  # Output boxplot of max occupation
  output$EDOccupationBoxPlot <- renderPlotly({
    df <- EDOccupation()
    
    # Filter the data based on input$EDdates[]
    filtered_data <- df[df$Date >= as.Date(input$EDdates[1]) &
                          df$Date <= as.Date(input$EDdates[2]),]
    
    # Create a box plot
    box <-
      plot_ly(
        data = filtered_data,
        x = ~ Max_Presence,
        type = "box",
        name = "Values"
      )
    
    # Customize the layout
    box <- box %>%
      layout(
        title = "Maximum Occupation Box Plot",
        xaxis = list(title = "Maximum Occupation per Day"),
        yaxis = list(title = ""),
        showlegend = FALSE  # Remove legend
      )
    
    # Return the plotly object
    box
  })
  
  # Output infobox with the number of days the capacity is exceeded
  output$CapacityExcDays <- renderInfoBox({
    df <- EDOccupation()
    
    threshold <- input$EDCapacity
    
    days_exceeded <- sum(df$Max_Presence > threshold)
    
    infoBox(
      "Number of days capacity is exceeded = ",
      days_exceeded,
      icon = icon("exclamation-sign", lib = "glyphicon"),
      color = "blue",
      fill = TRUE
    )
  })
  
  # Output infobox with congestion rate
  output$CongestionRate <- renderInfoBox({
    df <- EDOccupation()
    
    threshold <- input$EDCapacity
    days_exceeded <- sum(df$Max_Presence > threshold)
    total_days <- nrow(df)
    
    percentage_exceeded <- (days_exceeded / total_days) * 100
    percentage_formatted <- sprintf("%.2f%%", percentage_exceeded)
    
    infoBox(
      "Congestion rate = ",
      percentage_formatted,
      icon = icon("remove-circle", lib = "glyphicon"),
      color = "blue",
      fill = TRUE
    )
  })
  
  # Output graph of arrivals per weekday
  output$EDArrivalPatternWeekDays <- renderPlot({
    er_arrival_data <- EDInput()
    
    # Filter the data based on input$EDdates[]
    filtered_data <-
      er_arrival_data[er_arrival_data$ArrivalDateTime >= as.Date(input$EDdates[1]) &
                        er_arrival_data$ArrivalDateTime <= as.Date(input$EDdates[2]),]
    
    # Extract the weekday and hour
    filtered_data$Weekday <-
      factor(
        weekdays(as.Date(filtered_data$ArrivalDateTime)),
        levels = c(
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday",
          "Sunday"
        )
      )
    
    ggplot(filtered_data, aes(x = Weekday)) +
      geom_bar() +
      labs(x = "Weekday", y = "Number of Arrivals", title = "Arrivals Per Weekday") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        # Increase the size to 20 (adjust as needed)
        panel.border = element_rect(
          color = "black",
          fill = NA,
          size = 1
        ),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 10
        ),
        axis.text.y = element_text(size = 10)
      )
    
  })
  
  # Output of the arrival pattern
  # Output of the arrival pattern
  output$EDArrivalPatternHour <- renderPlot({
    er_arrival_data <- EDInput()
    
    # Filter the data based on input$EDdates[]
    filtered_data <-
      er_arrival_data[er_arrival_data$ArrivalDateTime >= as.Date(input$EDdates[1]) &
                        er_arrival_data$ArrivalDateTime <= as.Date(input$EDdates[2]),]
    
    # Extract the hour and convert to numeric for filtering
    filtered_data$Hour <- hour(filtered_data$ArrivalDateTime)
    
    ggplot(filtered_data, aes(x = Hour)) +
      geom_bar() +
      labs(x = "Hour of the Day", y = "Number of Arrivals", title = "Arrivals Per Hour of the Day") +
      scale_x_continuous(breaks = 0:23, labels = sprintf("%02d:00", 0:23)) +  # Specify breaks and labels
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        # Increase the size to 20 (adjust as needed)
        panel.border = element_rect(
          color = "black",
          fill = NA,
          size = 1
        ),
        axis.text.x = element_text(hjust = 0.5, size = 10),
        # Center the x-axis labels
        axis.text.y = element_text(size = 10),
      )
  })
  
  
  
  
  
  ## Outputs for Ward Occupation tab ----
  # Plot of maximum ward occupancy
  output$WardOccupationPlot <- renderPlot({
    ward_occupation_data <- WardOccupation()
    
    ggplot(ward_occupation_data, aes(x = Date, y = Presence)) +
      geom_line(color = "red", size = 1) +
      labs(x = "Date", y = "Presence", title = "Presence Over Time") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        panel.border = element_rect(
          color = "black",
          fill = NA,
          size = 1
        )
      ) +
      coord_cartesian(xlim = as.Date(c(input$Warddates[1], input$Warddates[2]))) +
      scale_y_continuous(
        limits = c(0, max(ward_occupation_data$Presence), expand = c(0, 0)),
        breaks = seq(0, max(ward_occupation_data$Presence), by = 10),
        # Custom y-axis breaks
        labels = scales::comma_format(scale = 1),
        # Format y-axis labels
        minor_breaks = seq(0, max(ward_occupation_data$Presence), by = 2)  # Add minor gridlines
      ) +
      scale_x_date(date_labels = "%b %d, %Y", date_breaks = "1 month") +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 10
        ),
        axis.text.y = element_text(size = 10)
      )
  })
  
  
  # Output infobox with busiest day
  output$WardBusyDay <- renderInfoBox({
    ward_occupation_data <- WardOccupation()
    
    # Filter the data based on date input range
    filtered_data <-
      ward_occupation_data[ward_occupation_data$Date >= as.Date(input$Warddates[1]) &
                             ward_occupation_data$Date <= as.Date(input$Warddates[2]),]
    
    # Find the busiest day within the filtered data
    busiest_day <-
      filtered_data[which.max(filtered_data$Max_Presence), "Date"]
    
    infoBox(
      "Busiest day = ",
      format(busiest_day, "%d-%m-%Y"),
      icon = icon("calendar"),
      color = "purple",
      fill = TRUE
    )
  })
  
  # Output infobox with maximum occupancy on busiest day
  output$WardMostPatientsPresent <- renderInfoBox({
    ward_occupation_data <- WardOccupation()
    
    # Filter the data based on input$EDdates[]
    filtered_data <-
      ward_occupation_data[ward_occupation_data$Date >= as.Date(input$Warddates[1]) &
                             ward_occupation_data$Date <= as.Date(input$Warddates[2]),]
    
    # Find the busiest day and its corresponding maximum presence within the filtered data
    busiest_day_row <-
      filtered_data[which.max(filtered_data$Max_Presence),]
    busiest_day <- busiest_day_row$Date
    max_patients <- busiest_day_row$Max_Presence
    
    infoBox(
      "Busiest day max patients = ",
      max_patients,
      icon = icon("hospital"),
      color = "red",
      fill = TRUE
    )
  })
  
  # Output total number of evaluated patients
  output$WardTotalNumPatients <- renderInfoBox({
    ward_arrival_data <- WardInput()
    
    # Filter the data based on input$Warddates[]
    filtered_data <-
      ward_arrival_data[ward_arrival_data$ArrivalDateTime >= as.Date(input$Warddates[1]) &
                          ward_arrival_data$ArrivalDateTime <= as.Date(input$Warddates[2]),]
    
    filtered_data_count <- nrow(filtered_data)
    
    infoBox(
      "Total number of patients = ",
      filtered_data_count,
      icon = icon("signal", lib = "glyphicon"),
      color = "blue",
      fill = TRUE
    )
  })
  
  # Output boxplot of max occupation
  output$WardOccupationBoxPlot <- renderPlotly({
    df <- WardOccupation()
    
    # Filter the data based on input$EDdates[]
    filtered_data <- df[df$Date >= as.Date(input$Warddates[1]) &
                          df$Date <= as.Date(input$Warddates[2]),]
    
    # Create a box plot
    box <-
      plot_ly(
        data = filtered_data,
        x = ~ Max_Presence,
        type = "box",
        name = "Values"
      )
    
    # Customize the layout
    box <- box %>%
      layout(
        title = "Maximum Occupation Box Plot",
        xaxis = list(title = "Maximum Occupation per Day"),
        yaxis = list(title = ""),
        showlegend = FALSE  # Remove legend
      )
    
    # Return the plotly object
    box
  })
  
  #
  output$WardArrivalPatternWeekDays <- renderPlot({
    ward_arrival_data <- WardInput()
    
    # Filter the data based on input$EDdates[]
    filtered_data <-
      ward_arrival_data[ward_arrival_data$ArrivalDateTime >= as.Date(input$Warddates[1]) &
                          ward_arrival_data$ArrivalDateTime <= as.Date(input$Warddates[2]),]
    
    # Extract the weekday and hour
    filtered_data$Weekday <-
      factor(
        weekdays(as.Date(filtered_data$ArrivalDateTime)),
        levels = c(
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday",
          "Sunday"
        )
      )
    
    ggplot(filtered_data, aes(x = Weekday)) +
      geom_bar() +
      labs(x = "Weekday", y = "Number of Arrivals", title = "Arrivals Per Weekday") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        # Increase the size to 20 (adjust as needed)
        panel.border = element_rect(
          color = "black",
          fill = NA,
          size = 1
        ),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 10
        ),
        axis.text.y = element_text(size = 10)
      )
    
  })
  
  # Output of the Ward arrival pattern
  output$WardArrivalPatternHour <- renderPlot({
    ward_arrival_data <- WardInput()
    
    # Filter the data based on input$Warddates[]
    filtered_data <-
      ward_arrival_data[ward_arrival_data$ArrivalDateTime >= as.Date(input$Warddates[1]) &
                          ward_arrival_data$ArrivalDateTime <= as.Date(input$Warddates[2]),]
    
    # Extract the hour and convert to numeric for filtering
    filtered_data$Hour <- hour(filtered_data$ArrivalDateTime)
    
    ggplot(filtered_data, aes(x = Hour)) +
      geom_bar() +
      labs(x = "Hour of the Day", y = "Number of Arrivals", title = "Arrivals Per Hour of the Day") +
      scale_x_continuous(breaks = 0:23, labels = sprintf("%02d:00", 0:23)) +  # Specify breaks and labels
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        # Increase the size to 20 (adjust as needed)
        panel.border = element_rect(
          color = "black",
          fill = NA,
          size = 1
        ),
        axis.text.x = element_text(hjust = 0.5, size = 10),
        # Center the x-axis labels
        axis.text.y = element_text(size = 10),
      )
  })
  
  
  
  ## Correlation analysis calculation and analysis -------------------------------
  
  CorrelationAnalysis <- reactive ({
    ED <- EDOccupation()
    Ward <- WardOccupation()
    
    CorrelationOutput <- merge(ED, Ward, by = "Date")
    
    CorrelationOutput
  })
  
  
  output$CorrelationTable <- renderTable({
    CorrelationAnalysis()
  })
  
  output$CorrelationPlot <- renderPlot({
    my_data <- CorrelationAnalysis()
    
    # Filter the data based on input$EDdates[]
    filtered_data <-
      my_data[my_data$Date >= as.Date(input$Correlationdates[1]) &
                my_data$Date <= as.Date(input$Correlationdates[2]),]
    
    ggscatter(
      filtered_data,
      x = "Max_Presence.x",
      y = "Max_Presence.y",
      add = "reg.line",
      conf.int = TRUE,
      cor.coef = TRUE,
      cor.method = input$correlationmethod,
      xlab = "Max presence ED",
      ylab = "Max presence ward"
    )
  })
  
  output$correlationvalue <- renderText({
    my_data <- CorrelationAnalysis()
    
    # Filter the data based on input$Correlationdates[]
    filtered_data <-
      my_data[my_data$Date >= as.Date(input$Correlationdates[1]) &
                my_data$Date <= as.Date(input$Correlationdates[2]),]
    
    correlation <-
      round(
        cor(
          filtered_data$Max_Presence.x,
          filtered_data$Max_Presence.y,
          method = input$correlationmethod
        ),
        digits = 2
      )
    
    correlation_message <- ""
    
    if (abs(correlation) > 0.8) {
      if (correlation > 0) {
        correlation_message <-
          "There is a strong positive correlation between ED occupancy and Ward occupancy. This suggests that when there is a high occupancy of patients to the ED, there is also a corresponding increase in the occupancy of hospital wards. This might indicate that ED occupancy are a reliable predictor of ward occupancy."
      } else {
        correlation_message <-
          "There is a strong negative correlation between ED occupancy and Ward occupancy. This implies that when the ED experiences a surge in occupancy, ward occupancy tends to decrease. Further investigation is needed to understand the reasons behind this negative relationship."
      }
    } else if (abs(correlation) > 0.6) {
      if (correlation > 0) {
        correlation_message <-
          "There is a moderate positive correlation between ED occupancy and Ward occupancy. This indicates that an increase in ED occupancy is generally associated with higher ward occupancy, but the relationship is not as strong as in cases of strong correlation."
      } else {
        correlation_message <-
          "There is a moderate negative correlation between ED occupancy and Ward occupancy. This suggests that a rise in ED occupancy is somewhat linked to a decrease in ward occupancy, though the correlation is not very strong. Further investigation is recommended to understand this relationship better."
      }
    } else {
      correlation_message <-
        "There is a weak or no correlation between ED occupancy and Ward occupancy. This means that changes in ED occupancy do not significantly impact ward occupancy, or there may be other factors at play that affect ward occupancy independently of ED occupancy"
    }
    
    paste("The correlation coefficient =",
          correlation,
          ".",
          correlation_message)
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
