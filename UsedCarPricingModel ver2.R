library(shiny)
library(xtable)
library(lattice)
library(ggplot2)
library (Hmisc)
library(DT)

## ----------------------------------------------------------------------------

ui <- fluidPage(
    
    # Application title
    titlePanel("Mazda CX-5 Used Car Buyer"),
    
    sidebarLayout(
        sidebarPanel(
            # Select the Model Year for the Linear Regression pricing model.
            radioButtons(inputId = "modelyear", 
                         label = "Model Year:", 
                         c("2016" = "2016", 
                           "2015" = "2015", 
                           "2014" = "2014",
                           "2013" = "2013"),
                         selected = "2015",
                         inline = TRUE),
    
            br(),
    
            # Set the mileage for the Linear Regression pricing model.
            sliderInput(inputId = "mileage",
                        label = "Vehicle Mileage:",
                        value = 25000, min = 0, max = 80000, step = 100),
            
            br(),
            
            textOutput(outputId = "newprice")
            ),
    
        # Show a tabset that includes a plot, summary, and table view
        # of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs", 
                        tabPanel("Plot", 
                                 plotOutput("plot")), 
                        tabPanel("Model Summary", 
                                 verbatimTextOutput("summary")), 
                        tabPanel("Data Table", 
                                 DT::dataTableOutput("table"))
                        )
        )
    )
    
)

## ----------------------------------------------------------------------------

server <- function(input, output) {
    
    ## Linear model for predicting a price of an out-of-sample used car based
    ## on user-defined Model.Year and Miles.
    
    usedcars = read.csv("carprices.csv", header = TRUE)
    usedcars$Asking <- as.numeric(usedcars$Asking)
    
    fit <- lm(Asking ~ Model.Year + Miles, data = usedcars)

    output$newprice <- renderPrint({
        
        newdata <- data.frame(as.integer(input$modelyear), 
                              as.integer(input$mileage))
        names(newdata) <- c("Model.Year", "Miles")
        
        price <- predict(fit, newdata)
        
        cat(sprintf("Predicted Price: $%.0f", price))
        #cat(format(price, trim = false, nsmall = 0))
    })
    
    # TABSET: Plot Tab - Generate a plot of the data on the using inputs
    # to build the plot.  A simple linear regression model is used to 
    # predict the price of a new vehicle.
    
    output$plot <- renderPlot({
        
        newdata <- data.frame(as.integer(input$modelyear), 
                              as.integer(input$mileage))
        names(newdata) <- c("Model.Year", "Miles")
        
        price <- predict(fit, newdata)
        
        ggplot(usedcars, aes(x = Miles, y = Asking, 
                             colour = factor(Model.Year))) + 
            geom_point(shape = 18, size = 3) +
            theme_bw() + 
            theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  text = element_text(size = 10),
                  legend.position = "top",
                  legend.key = element_blank(),
                  legend.title = element_blank()) +
            scale_size(guide = "none") +
            scale_color_discrete(breaks=c("2013", "2014", "2015", "2016")) +
            geom_vline(xintercept = input$mileage, color = "red") +
            geom_hline(yintercept = price, color = "red") +
            geom_smooth(method=lm, color = "red", size = .8, linetype = "dotted") + 
            ggtitle("Linear Regression Pricing Model for Mazda CX-5 SUV") +
            theme(plot.title = element_text(lineheight=.8, face="bold")) +
            xlim(0, 80000) + 
            ylim(15000, 35000)
        
    })

    # TABSET: Summary Tab - Prints a formatted summary of the used car pricing
    # linear regression model using the Hmisc library and Latex.

    output$summary <- renderPrint({summary(fit)})
    
    # TABSET: Table Tab - Generates an HTML table view of the dataset using 
    # the DT library.
    
    output$table <- DT::renderDataTable(usedcars, server = FALSE)                
}

shinyApp(ui = ui, server = server)
