library(shiny)
library(rugarch)
library(shinythemes)
library(bslib)

# Define UI for application
ui <- fluidPage(theme = shinytheme("united"),
#   shinythemes::themeSelector(),
# Custom CSS for styling
tags$head(
  tags$style(HTML("
      .title-panel {
        background-color: rgb(165, 30, 55);
        padding: 15px;
        text-align: center;
        color: white;
        font-size: 32px;
        font-weight: bold;
        margin-bottom: 30px; /* Add space between title and main panel */
      }
      .sidebar-panel {
        background-color: rgb(208 . 207 . 210);
        padding: 15px;
        border-radius: 10px;
      }
      .box {
        background-color: rgb(240 . 235 . 224);
        padding: 15px;
        border-radius: 10px;
        box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1);
        margin-bottom: 20px; /* Add space between boxes */
      }
      .plot-box {
        margin-bottom: 20px;
      }
      .center-table {
        display: flex;
        justify-content: center;
        align-items: center;
        width: 100%;
        padding: 20px;
      }
      .table-box {
        width: 50%; /* Table box width */
        margin: 0 auto;
        background-color: rgb(240 . 235 . 224);
        padding: 20px;
        border-radius: 10px;
        box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1);
      }
      table {
        width: 100%;  /* Ensure the table fits inside the box */
        margin: 0 auto;  /* Ensures that the table is centered properly */
        border-collapse: collapse;
      }
      th, td {
        padding: 10px;
        text-align: center;
        border: 1px solid #ddd;
      }
    "))
),

# Title Panel
div(class = "title-panel", "DGP with GARCH Variants and Different Distributions"),

sidebarLayout(
  sidebarPanel(
    class = "sidebar-panel",
    radioButtons("checkModel", "Select Model Type:",
                 choices = list("sGARCH" = "m1",
                                "EGARCH" = "m2",
                                "GJR-GARCH" = "m3"),
                 selected = "m1"),
    
    radioButtons("checkGroup", "Select Distribution:",
                 choices = list("Normal Distribution" = "opt1",
                                "Student's t Distribution" = "opt2",
                                "Skewed Student Distribution" = "opt3"),
                 selected = "opt1"),
    
    sliderInput("omega", "Omega:", min = 0, max = 0.1, value = 0.02),
    sliderInput("alpha", "Alpha:", min = 0, max = 0.1, value = 0.05),
    sliderInput("beta", "Beta:", min = 0, max = 0.9, value = 0.9),
    
    # Conditional sliders
    conditionalPanel(
      condition = "input.checkGroup == 'opt2' || input.checkGroup == 'opt3'",
      sliderInput("shape", "Shape (Degrees of Freedom):", min = 3, max = 30,
                  value = 5)
    ),
    conditionalPanel(
      condition = "input.checkGroup == 'opt3'",
      sliderInput("skew", "Skewness:", min = 0.5, max = 2, value = 1)
    ),
    conditionalPanel(
      condition = "input.checkModel == 'm2'",
      sliderInput("gamma", "Gamma", min = -1, max = 1, value = 0.2, step = 0.1)
    ),
    conditionalPanel(
      condition = "input.checkModel == 'm3'",
      sliderInput("gamma2", "Gamma", min = 0, max = 1, value = 0, step = 0.1)
    )
    
  ),
  
  mainPanel(
    fluidRow(
      column(6, div(class = "box plot-box", plotOutput("plot1"))),
      column(6, div(class = "box plot-box", plotOutput("plot2")))
    ),
    div(class = "center-table",
        div(class = "table-box", tableOutput("table"))
    )
  )
)
)

# Define server logic
server <- function(input, output) {
  
  # Reactive value to hold simulation results
  sim_data <- reactiveVal(NULL)
  
  observe({
    req(input$checkGroup, input$omega, input$alpha, input$beta, input$checkModel)
    
    # GJR-GARCH Stationarity check
    if (input$checkModel == "m3") {
      req(input$gamma2)
      # Enforce stationarity: alpha + beta + gamma2 / 2 < 1
      if (input$alpha + input$beta + input$gamma2 / 2 >= 1) {
        showNotification("Invalid parameters: Ensure (alpha + beta + gamma/2) < 1",
                         type = "error")
        return()
      }
    }
    
    spec_params <- list(
      variance.model = list(garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(0, 0)),
      distribution.model = "norm",
      fixed.pars = list(mu = 0, omega = input$omega,
                        alpha1 = input$alpha,
                        beta1 = input$beta)
    )
    
    if (input$checkModel == "m1") {
      spec_params$variance.model$model <- "sGARCH"
    } else if (input$checkModel == "m2") {
      req(input$gamma)  # Only required for eGARCH
      spec_params$variance.model$model <- "eGARCH"
      spec_params$fixed.pars$gamma1 <- input$gamma
    } else if (input$checkModel == "m3") {
      req(input$gamma2)
      spec_params$variance.model$model <- "gjrGARCH"
      spec_params$fixed.pars$gamma1 <- input$gamma2
    }
    
    if (input$checkGroup == "opt2") {
      req(input$shape)
      spec_params$distribution.model <- "std"
      spec_params$fixed.pars$shape <- input$shape
    } else if (input$checkGroup == "opt3") {
      req(input$shape, input$skew)
      spec_params$distribution.model <- "sstd"
      spec_params$fixed.pars$shape <- input$shape
      spec_params$fixed.pars$skew <- input$skew
    }
    
    # Define the GARCH model
    spec <- do.call(ugarchspec, spec_params)
    
    # Run the simulation
    garch.sim <- ugarchpath(spec, n.sim = 1000, rseed = 42)
    
    # Extract the simulated returns and conditional variance
    sim_data(data.frame(
      time = 1:1000,
      returns = as.numeric(garch.sim@path$seriesSim),
      variance = as.numeric(garch.sim@path$sigmaSim)
    ))
  })
  
  # Plot 1: Return Series
  output$plot1 <- renderPlot({
    req(input$checkGroup)
    df <- sim_data()
    plot(df$time, df$returns, type = "l", col = rgb(50, 65, 75, maxColorValue = 255), 
         main = "GARCH Simulated Return Series", ylab = "Returns", xlab = "Time")
  })
  
  # Plot 2: Distribution of Cond. Variance
  output$plot2 <- renderPlot({
    req(sim_data())
    df <- sim_data()

    # Compute histogram data
    hist_data <- hist(df$variance, col = rgb(172, 55, 66, maxColorValue = 255), probability = TRUE, plot = FALSE)

    # Compute density data
    dens <- density(df$variance)
    
    # Define y-axis limits
    ylim_max <- max(hist_data$density, dens$y) * 1.1
    
    # Plot histogram
    hist(df$variance, col = rgb(172, 55, 66, maxColorValue = 255), probability = TRUE, 
         main = "Histogram of GARCH Simulated Conditional Variance",
         xlab = "Variance", border = "white", ylim = c(0, ylim_max))
    
    # Overlay density curve
    lines(dens, col = rgb(50, 65, 75, maxColorValue = 255), lwd = 2)
    
  })
  
  # Output Table
  output$table <- renderTable({
    df <- sim_data()
    
    summary_returns <- summary(df$returns)
    summary_variance <- summary(df$variance)
    
    # Combine summaries into a data frame
    summary_df <- data.frame(
      Statistic = names(summary_returns),
      Returns = as.numeric(summary_returns),
      Variance = as.numeric(summary_variance)
    )
    
    return(summary_df) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



# Skew and Shape should do something but they do not do anything. Check it out tmrw
