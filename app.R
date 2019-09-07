library(shiny)
library(ggplot2)
library(DT)
library(tools)
library(dplyr)
library(shinythemes)
load("/Users/jiayingshi/Desktop/R Shiny/Shiny/Sold_Fleet_Equipment.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("paper"),
  
   # Application title
   titlePanel("Sold Fleet Equipments in Seattle"),
   
   # Sidebar
   sidebarLayout(
     
     # Inputs: Select variables to plot ------------------------------
     sidebarPanel(
       
       # Select variable for equipment make- ----------------------------------
       selectInput(inputId = "make", 
                   label = "Select Equipment Make",
                   choices = sort(unique(df1$MAKE)), 
                   selected = "CHEVROLET"),
       
       # Select which year to plot ------------------------
       checkboxGroupInput(inputId = "saleyear",
                          label = "Select Sale Year",
                          choices = c("2016", "2017", "2018","2019"),
                          selected = c("2016", "2017", "2018","2019")),
       
       # Set point size ----------------------------------------------
       sliderInput(inputId = "size", 
                   label = "Size:", 
                   min = 0, max = 5, 
                   value = 3),
       
       # Show data table ---------------------------------------------
       checkboxInput(inputId = "show_data",
                     label = "Show data table",
                     value = TRUE),
       
       # Select sample size ----------------------------------------------------
       numericInput(inputId = "n_samp", 
                    label = "Sample size:", 
                    min = 1, max = nrow(df1), 
                    value = 100),
       
       # Download Button
       downloadButton("downloadData", "Download Data")
     ),
     
     # Output --------------------------------------
     mainPanel(
       
       #Barplot
       plotOutput(outputId = "barplot"),
       br(),
       #boxplot
       plotOutput(outputId = "boxplot"),
       br(),
       #Scatterplot
       plotOutput(outputId = "scatterplot"),
       br(),
       #Data Table
       DT::dataTableOutput(outputId = "equipmenttable")
     )
   )
)
  
# Define server logic required to draw plots
server <- function(input, output, session) {
  
  # Create a subset of data filtering for selected sale year ------
  year_subset <- reactive({
    req(input$saleyear) 
    filter(df1, SALE_YEAR %in% input$saleyear)
  })
  
  # Update the maximum allowed n_samp for selected year ------
  observe({
    updateNumericInput(session, 
                       inputId = "n_samp",
                       value = min(100, nrow(year_subset())),
                       max = nrow(year_subset())
    )
  })
  
  # Create new df that is n_samp obs from selected year ------
  year_sample <- reactive({ 
    req(input$n_samp) 
    sample_n(year_subset(), input$n_samp)
  })
  
  # Create a subset of data filtering for selected equipment make ------
  make_subset <- reactive({
    req(input$make) 
    filter(df1, MAKE %in% input$make)
  })
  
  # Create boxplot object for equipment year vs. sale price
  output$boxplot <- renderPlot({
    ggplot(make_subset(), 
           aes(x = input$make, y = SALE_PRICE))+
      geom_boxplot() +
      labs(title="Sale Price Distribution by Equipment Make", x="Equipment Make", y = "Sale Price")+
      scale_fill_brewer(palette="Blues")+
      theme(plot.title = element_text(color="blue", size = 14,face = "bold"))
  }
 )
  
  # Create scatterplot object for equipment year vs. sale price
  output$scatterplot <- renderPlot({
    ggplot(year_sample(), 
           aes(x = YEAR, 
               y = SALE_PRICE))+ 
      geom_point(size = input$size) + 
      labs(title="Price VS. Equipment Year", x="Equipment Year", y = "Sale Price")+
      theme(plot.title = element_text(color="blue", size = 14,face = "bold"))
  }
)
  
  #Create barplot object by department
  output$barplot <- renderPlot({
    ggplot(year_subset(), 
           aes(DEPT))+ 
      geom_bar(aes(fill = SALE_YEAR))+
      labs(x = paste("Department"),
           y = "Count",
           title = paste("Number of Sold Fleet Equipment by Department"))+
      theme(plot.title = element_text(color="blue", size = 14,face = "bold"))
  }
)
  
  # Print data table if checked -------------------------------------
  output$equipmenttable <- DT::renderDataTable(
  if(input$show_data){
    DT::datatable(data = year_subset()[, 1:8], 
                  options = list(pageLength = 5), 
                  rownames = FALSE)
    }
  )

  #Download function
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Sold Fleet Equipment", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df1, file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)