
# Load required libraries
library(shiny)      # For building the web application
library(bslib)      # For enhancing Shiny UI with Bootstrap themes
library(visNetwork) # For visualizing association rules as network graphs


# Import required R scripts
source("C:/Users/Asus/Desktop/programming/dsProject/R/installing.R")
source("C:/Users/Asus/Desktop/programming/dsProject/R/data_cleaning.R")
source("C:/Users/Asus/Desktop/programming/dsProject/R/visualization.R")
source("C:/Users/Asus/Desktop/programming/dsProject/R/clustering.R")
source("C:/Users/Asus/Desktop/programming/dsProject/R/association.R")

# Home page UI - File input for importing data and displaying sample data
home_page <- card(
  fileInput("data_input", "Import Data To Start Analysis:", accept = ".csv", width = "100%"),
  uiOutput("imported_data_sample")
)

# Shiny's main UI layout
ui <- page_fillable(
  navset_card_tab(
    # Homepage tab
    nav_panel("Homepage", home_page),

    # Dashboard tab
    nav_panel("Dashboard", uiOutput("dashboard_page")),

    # Clustering tab
    nav_panel("Clustering", uiOutput("clustering_page")),

    # Association tab
    nav_panel("Association", uiOutput("association_page")),

    # More menu with additional tabs
    nav_menu(
      "More",
      nav_panel("Association Graph", uiOutput("association_graph_page"))
    )
  )
)

# Shiny server logic
server <- function(input, output) {

  # Reactive data cleaning and processing
  data <- reactive({
    req(input$data_input) # Ensure input file is provided
    data_cleaning(
      read.csv(
        input$data_input$datapath,
        header = TRUE,
        stringsAsFactors = FALSE
      )
    )
  })

  # Check if data has been uploaded
  data_exist <- reactive({
    !is.null(input$data_input) # Returns TRUE if data exists
  })

  # Display sample of imported data
  output$imported_data_sample <- renderUI({
    if (data_exist()) {
      card(
        card_header("Sample of Data imported:"),
        renderTable({
          head(data()) # Display the first few rows of the data
        })
      )
    } else {
      "No data imported yet."
    }
  })

  # Dashboard UI rendering
  output$dashboard_page <- renderUI({
    if (data_exist()) {
      card(
        fluidRow(
          column(6, card(plotlyOutput('age_spending_plot'))),
          column(6, card(plotlyOutput('city_spending_plot')))
        ),
        fluidRow(
          column(6, card(plotOutput('pay_type_plot'))),
          column(6, card(plotlyOutput('spending_distribution_plot')))
        )
      )
    } else {
      "No data imported yet."
    }
  })

  # Generate age-spending plot
  output$age_spending_plot <- renderPlotly({
    age_spending_visualization(
      age_spending_summray(data())
    )
  })

  # Generate city-spending plot
  output$city_spending_plot <- renderPlotly({
    city_spending_visualization(
      city_spending_summray(data())
    )
  })

  # Generate payment type plot
  output$pay_type_plot <- renderPlot({
    pay_type_visualization(
      pay_type_summray(data())
    )
  })

  # Generate spending distribution plot
  output$spending_distribution_plot <- renderPlotly({
    spending_distribution_visualization(
      data()
    )
  })

  # Clustering page UI
  output$clustering_page <- renderUI({
    if (data_exist()) {
      card(
        card_header(
          numericInput("k_number", label = "Input Number of Clusters k [2 to 4]:", value = 3, min = 2, max = 4, step = 1)
        ),
        plotlyOutput('clustering_plot')
      )
    } else {
      "No data imported yet."
    }
  })

  # Generate clustering plot
  output$clustering_plot <- renderPlotly({
    clustering(
      data(),
      input$k_number
    )
  })

  # Association rules reactive logic
  association_rules_reactive <- reactive({
    if (length(input$min_confidence) == 0) return(list()) # No data if no confidence is set
    association_rules(data(), input$min_confidence, input$min_support)
  })

  # Association page UI
  output$association_page <- renderUI({
    if (data_exist()) {
      card(
        card_header(
          fluidRow(
            column(6, card(numericInput("min_confidence", label = "Min Confidence [0.001 to 1]:", value = 0.5, min = 0.001, max = 1, step = 0.001))),
            column(6, card(numericInput("min_support", label = "Min Support [0.001 to 1]:", value = 0.01, min = 0.001, max = 1, step = 0.001)))
          )
        ),
        uiOutput("association_rules_table")
      )
    } else {
      "No data imported yet."
    }
  })

  # Display association rules table
  output$association_rules_table <- renderUI({
    rules <- association_rules_reactive()
    if (association_rules_exist(rules)) {
      renderTable({
        association_Data_frame(rules)
      })
    } else {
      "No association rules found according to current min confidence and support."
    }
  })

  # Association graph page UI
  output$association_graph_page <- renderUI({
    if (data_exist()) {
      rules <- association_rules_reactive()
      if (length(rules) == 0) {
        card(
          card_header("Association rules in graph view:"),
          "Choose min confidence and support in the Association tab first."
        )
      } else {
        if (association_rules_exist(rules)) {
          card(
            card_header("Association rules in graph view:"),
            renderVisNetwork({
              association_graph(rules)
            })
          )
        } else {
          "No association rules found according to current min confidence and support."
        }
      }
    } else {
      "No data imported yet."
    }
  })
}

# Run the Shiny application
runApp(shinyApp(ui = ui, server = server))


